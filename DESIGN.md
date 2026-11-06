# Design: Cistern, an OCaml ORM modeled on Toasty (version 1: MySQL and MariaDB on weft)

## Context

You want an OCaml ORM and database library that works like Toasty (github.com/tokio-rs/toasty):

- You define models as structs with a derive macro (`#[key]`, `#[auto]`, `#[unique]`, `#[index]`, `#[has_many]`, `#[belongs_to]`).
- The macro generates query methods only for indexed access paths.
- Relation fields use `Deferred<T>`. `create!` makes a parent and its children in one call.

Name: **Cistern** (the water store of a caravanserai, which windcatchers cool). Repository: `github.com/ocaml-caravanserai/cistern`. The opam name `cistern` is free.

Cistern uses weft for concurrency, the new weft libraries `weft.flow` and `weft-tls` for byte streams and TLS (see "Prerequisite PRs"), windtrap for all tests, and `flake.nix` for all dependencies. Cistern does not depend on caravan. `ocaml-caravanserai/weft`, `caravan` and `windtrap` exist on GitHub. The old names `bikallem/*` redirect to them (I verified this with `gh`).

Your decisions for version 1:

- Servers: MySQL 8.4, MariaDB 11.4 and MariaDB 11.8, with one client. PostgreSQL, SQLite and DynamoDB come later.
- Protocol: the text protocol only (`COM_QUERY`), with hex literals for strings and bytes. Prepared statements come later.
- Cancel: close the connection, then `KILL CONNECTION` on a control connection after an identity check.
- No weft PR for blocking calls (`Blocking.run`) in version 1.
- Move generic transport code from caravan to weft (`weft.flow` and the package `weft-tls`).
- Relations: Toasty style (one recursive type declaration, `Deferred.t` fields, `include_`, nested create).
- All dependencies come from `flake.nix`.
- Use the OCaml 5.5 features (module-dependent functions, polymorphic parameters) in hand-written code.

Five review rounds found defects in drafts 4, 5, 9, 10 and 11. This draft (12) fixes the round 5 findings. The section "Review history" lists them.

Note: claims about server behavior that nobody verified are marked "not verified". The phase 1.1 tests check them, before the code depends on them.

## Answer: is the SQL made at run time or at compile time?

At run time, for each statement:

- The typed API builds a model-level AST. `cistern.sql` serializes it to SQL text for the dialect of the connection, with the literals in place.
- Version 1 has no cache of SQL text. With the text protocol, the serializer costs microseconds, and a round trip costs much more. The server parses each query.
- The same pass also makes a redacted copy of the SQL (each literal becomes `?`) for the query hook, so that no value goes to the logs.
- Prepared statements and a statement cache come in a later version.

## Scope of version 1

| Server | Version | Default authentication plugin |
|---|---|---|
| MySQL | 8.4 (LTS) | `caching_sha2_password` |
| MariaDB | 11.4 (LTS) | `mysql_native_password` (not verified) |
| MariaDB | 11.8 (LTS) | `mysql_native_password` (not verified) |

In version 1:

- One pure OCaml client for MySQL and MariaDB on `Weft_flow.Flow.t`, text protocol only.
- The ppx: models, `has_many`, `belongs_to`, `Deferred.t`, `include_`, nested create of one level.
- Typed filters (comparisons, `IS NULL`, `IN`, `starts_with`, `contains`), `order_by` (several keys), `limit`, `offset`, `to_list`, `first`, `count`, `exists`, `for_update`, `for_share`, bulk `update` and `delete`.
- `Query.where` on a relation scope (for example the open todos of one user).
- Raw SQL from fragments (`Raw`), in and outside transactions.
- The pool, deadlines, cancel with `KILL`, transactions with savepoints.
- TLS with `ssl-mode`, a CA file, or a pinned certificate fingerprint (`ssl-fingerprint`).
- `push_schema` and a reduced `check_schema`.
- A query hook for logs (`?on_query`).

Other servers, for example MySQL 5.7 or a fork, are not supported. A server with no `mariadb` in its version or comment gets the rules of MySQL. MySQL 5.7 refuses the session setup with error 1273, because it has no `utf8mb4_0900_bin`.

Later versions:

- PostgreSQL (see "Later versions: PostgreSQL notes"), SQLite (needs a weft API for blocking calls), DynamoDB.
- Prepared statements and the binary protocol, with a statement cache.
- `has_one`, `via`, nested create of more than one level, `Query.fold`, composite keys, UUID and timestamp `[@auto]`, upsert (`ON DUPLICATE KEY UPDATE`), ordering by a relation, `references` to a `[@unique]` field that is not the key.
- `[@codec]`, variant columns, `DECIMAL` fields, `JSON`, `ENUM`, unsigned integers.
- A full `check_schema` (indexes, foreign keys, collations), migrations, a CLI.
- Galera and Group Replication error handling, read replicas.
- MariaDB `ed25519` and `parsec` (11.6 and later), the MariaDB certificate check without a CA (from the password hash), the RSA path of `caching_sha2_password`, packets of 16 MiB and more, MariaDB `INSERT ... RETURNING`.
- Session state tracking (`CLIENT_SESSION_TRACK`), connection recovery by drain, a batch API, pagination with cursors.
- A signature deriver for `.mli` files.

## Main design decisions

| Topic | Cistern | Reason |
|---|---|---|
| Model definition | `[@@deriving cistern]` on one recursive record declaration | Toasty style |
| Generated code | Model modules at top level, a namespace module for stage 2, then stage 3 | A structure cannot define a module name two times |
| Concurrency | weft, direct style, all domains | No monad, no context value |
| Client | One pure OCaml client. The greeting decides only the framing and the capabilities. The dialect, all queries and the identity come after the authentication. | The greeting is plaintext, and proxies rewrite it |
| Protocol | Text protocol; strings and bytes as hex literals; SQL text made for each statement | Safe in every `sql_mode`; no cache in version 1 |
| Raw SQL | Fragments (`Raw.sql "..." ++ Raw.v value`), no placeholders | A placeholder lexer depends on `NO_BACKSLASH_ESCAPES` |
| Transport | `weft.flow` (Flow, Tcp, Reader, Writer) and `weft-tls` | The `caravan` package brings HTTP dependencies |
| TLS | MySQL `ssl-mode` names, and `ssl-fingerprint`. By default, the clear-text password goes only to a server with a verified name or a pinned certificate. | `REQUIRED` and `VERIFY_CA` do not check the name |
| Filters | Phantom types. Inside `Q.( ... )`: `==.`, `<.`, ... and `&&`, `||`, `not` over expressions | `&&.` and `||.` had the precedence of `==.` |
| Execution | All operations of one statement run in order on one connection | Parallel operations needed many connections |
| Timeouts | Flow read deadlines; a default timeout of 30 s; Cistern sets no server timeout | The client deadline and `KILL` stop a query |
| Cancel | Close, then `KILL CONNECTION` after an exact identity check (server and start time) | A close does not stop a running query, and thread ids are not unique across servers and restarts |
| Transactions | `Tx.t` with `Tx.exec`. Only errors that end the server transaction fail the `Tx`. A failed `Tx` refuses more statements. | Savepoints must be useful, and a failed `Tx` must not commit writes |
| Session reset | At the next checkout, under the deadline of that caller, not at checkin | A checkin must do no I/O after the result is known |
| Tests | windtrap: unit, property, stateful, expect, mutation; weft simulator | |
| Build | `flake.nix` | Your decision |

## Limits and defaults

All numbers of version 1 are in this table. The other sections refer to it.

| Item | Value |
|---|---|
| `pool_size` | 10 |
| `checkout_timeout` | 30 s, gives `Pool_exhausted`. A checkout in `exec` waits until the earlier of this limit and the `exec` deadline. The `exec` deadline gives `Timeout`. |
| Default `?timeout` of `Db.exec` | 30 s, gives `Timeout` or `Outcome_unknown` |
| Default `?timeout` of `Db.transaction` | 30 s for the whole transaction. `Tx.exec` with no `?timeout` uses the transaction deadline. |
| Margin: a statement with less remaining time is not sent | 50 ms (not for `COMMIT` and `ROLLBACK`) |
| `COMMIT` and `ROLLBACK` deadline | The remaining time, at least 5 s and at most 30 s |
| `max_lifetime` | 30 min, with ±15% jitter |
| Idle limit: an idle connection older than this is closed at checkout | The smaller of 30 s and `@@wait_timeout` − 5 s |
| Connect timeout (pool and control connection) | 10 s |
| Concurrent connects | 2 |
| Connect backoff | 100 ms to 5 s |
| Kill queue | `pool_size` entries |
| One `KILL` | 2 s |
| Age limit of a `Killing` slot | 30 s (more than one connect plus `pool_size` kills), then the slot is free |
| Kill drain at the end of `with_db` | 15 s (one connect plus a few kills) |
| Longest wait of an outer cancel of `with_db` | About 45 s: a `COMMIT` in its shield (30 s), then the drain (15 s). Documented. |
| Keys in one `IN (...)` | 1000 |
| Identifier length | 64 characters |
| Statement size (write) | Less than the smaller of 16 MiB − 1 and `@@max_allowed_packet` (read at connect). The client checks the length before it sends. A hex literal doubles the size of a value, so one value can have at most about 8 MiB. |
| Row size (read) | Less than 16 MiB − 1 for each packet |

## Repository and build

### flake.nix

```nix
inputs = {
  nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
  windtrap = { url = "github:ocaml-caravanserai/windtrap"; inputs.nixpkgs.follows = "nixpkgs"; };
  weft = {
    url = "github:ocaml-caravanserai/weft";
    inputs.nixpkgs.follows = "nixpkgs";
    inputs.windtrap.follows = "windtrap";
  };
};
```

- All dependencies come from the flake. Cistern does not use `opam install` or Docker. dune makes the `.opam` files only for the package metadata.
- The OCaml scope is `ocaml-ng.ocamlPackages_5_5.overrideScope` with `weft = callPackage weft.lib.package {}`, `windtrap = callPackage windtrap.lib.windtrap {}`, `ppx_windtrap = callPackage windtrap.lib.ppx_windtrap {}`. This is the pattern of `~/projects/caravan/flake.nix`. Phase 1.4 (TLS) adds `weft-tls = callPackage weft.lib.tlsPackage {}`, the output of weft PR 1c (weft main has `lib = { inherit package tlsPackage; }`). Before phase 1.4, the flake does not refer to `lib.tlsPackage`, because no code of Cistern uses TLS before then.
- The `weft` input is the main branch of weft. weft PRs 1a and 1b are merged, and `flake.lock` pins one commit. If the codec of phase 1.1 needs a change of `Reader` or `Writer`, a normal weft PR makes it, and an update of `flake.lock` takes it. weft PR 1c is merged. Phase 1.4 updates `flake.lock` and takes `lib.tlsPackage` from weft main.
- Systems: `x86_64-linux`, `aarch64-linux`, `aarch64-darwin`. nixpkgs dropped `x86_64-darwin`.
- OCaml libraries from nixpkgs: `ppxlib`, `digestif` (SHA-1 and SHA-256), `mirage-crypto-rng`, `ptime`, `fmt`. `weft-tls` brings `tls` and `x509`. The tests also use `mirage-crypto-ec` (test certificates).
- Outputs: `lib.package`, `lib.ppxPackage`, `overlays.default`, `packages.<system>.cistern`, `packages.<system>.ppx_cistern`, `devShells.default`, `checks` (build and tests that need no database server), and `packages.<system>.server-mysql84`, `server-mariadb114` and `server-mariadb118` (the nixpkgs servers `mysql84` 8.4.11, `mariadb_114` 11.4.13 and `mariadb_118` 11.8.8, verified on 2026-11-06). The servers are not in the dev shell, so a `nix develop` does not download them. The server outputs come in phase 0 with `test/certgen` and the `db-up` targets, not with the first flake.
- The dev shell uses `inputsFrom = [ cistern ppx_cistern ]`, so it has every library dependency of the two packages. It adds OCaml 5.5, dune, findlib, `windtrap`, `ppx_windtrap`, `ocaml-lsp`, `odoc`, and `ocamlformat_0_29_0` (pinned, as in weft).
- Each PR adds a dependency together with its first user. For example, `ptime` comes with `Value`, and `x509` and `mirage-crypto-ec` come with `test/certgen`. At the end of version 1, the packages depend on `weft`, `ppxlib`, `digestif`, `mirage-crypto-rng`, `ptime`, `fmt` and (from phase 1.4) `weft-tls`, and the dev shell adds `mirage-crypto-ec` and `x509` for the tests.
- `flake.lock` pins nixpkgs, weft and windtrap. The server versions follow nixpkgs.

### Database servers for the tests

`make db-up SERVER=<name>`:

1. Gets the server with `nix build --no-link --print-out-paths .#server-<name>`.
2. Makes a test CA and a server certificate with `test/certgen`, a small OCaml program that uses `x509` and `mirage-crypto-ec` (the method of caravan's `test/tls_support.ml`). No `openssl` is necessary.
3. Initializes the data directory in `.db/<name>` (not `_build`, because `dune clean` deletes `_build`). The path of the Unix socket must have less than 104 bytes on macOS.
   - MySQL: `mysqld --no-defaults --basedir=<store path> --datadir=... --initialize-insecure`, then the server with `--no-defaults --bind-address=127.0.0.1 --port=<free port> --socket=... --pid-file=... --log-error=... --secure-file-priv=... --mysqlx=OFF --ssl-ca=... --ssl-cert=... --ssl-key=...`.
   - MariaDB: `mariadb-install-db --no-defaults --basedir=<store path> --datadir=... --auth-root-authentication-method=normal --skip-test-db`, then `mariadbd` with the same kind of flags.
   - `--no-defaults` must be the first option, so that the server does not read `/etc/my.cnf` or `~/.my.cnf`.
   - `--bind-address=127.0.0.1` is necessary, because root has no password. The server certificate from `test/certgen` has the name `localhost` and the IP address `127.0.0.1` in its subject-alternative names.
4. Makes the test users with the client of the same store path: one with `caching_sha2_password` on MySQL, one with `mysql_native_password` on MariaDB, with the grants for the tests (also `RELOAD` for the test that empties the authentication cache).

`mysql_native_password` is tested only on MariaDB, because MySQL 8.4 disables it by default.

`make integration SERVER=<name>` runs the integration tests. `make db-down` stops the servers.

CI jobs:

- `test`, on Linux and macOS: the build, the suites that need no server, and `nix flake check`. On Linux also: the format checks, the `.opam` files, the OCaml 5.5 check of the mutation backend, and on a pull request the mutation gate.
- Phase 1.1 adds one integration job for each server. MySQL 8.4 and MariaDB 11.8 run on each PR. MariaDB 11.4 runs on `main` and every night.
- The test servers of phase 0 come with a separate workflow. It starts each server and runs a smoke check, on `workflow_dispatch` and every night, not on each PR.

### Libraries (dependencies go one way)

```
cistern/
  flake.nix, flake.lock, dune-project, Makefile, test/certgen
  names/    (cistern.names)   The rules of names, the names of indexes (only the stdlib)
  core/     (cistern.core)    Value, Dialect, literals, Schema, model AST, Decoder, Error, Event, MODEL
  sql/      (cistern.sql)     SQL AST, compiler from the model AST, serializer for each dialect, DDL
  mysql/    (cistern.mysql)   Config and URL parser, connect, TLS, authentication, text protocol, sessions
  cistern/  (cistern)         Typed API, Raw, pool, killer, Tx, execution  (depends on core, sql, mysql)
  ppx/      (ppx_cistern)     The deriver
  test/  unit, prop, stateful, expect, sim, integration, nounix, readme, typeerror, ocaml55
```

- Two opam packages: `cistern` (the five libraries) and `ppx_cistern`.
- `cistern.names` has an empty `libraries` field, thus dune refuses a dependency. The ppx of phase 3 can link it with no weft and no ptime. An application links `cistern`.
- With one client, version 1 has no `CONN` interface and no `Driver.t`. A later PostgreSQL driver brings the abstraction back.

## User API

### Model definition

```ocaml
open Cistern

type user = {
  id : int64; [@key] [@auto]
  name : string;
  email : string; [@unique]
  todos : todo list Deferred.t; [@has_many]
}

and todo = {
  id : int64; [@key] [@auto]
  user_id : int64;
  user : user Deferred.t; [@belongs_to { key = user_id; references = id }]
  title : string;
  done_ : bool; [@default false] [@column "done"]
}
[@@deriving cistern]
```

| Attribute | Effect |
|---|---|
| `[@key]` | Primary key. One field in version 1. |
| `[@auto]` | Only on an `int64` key. `AUTO_INCREMENT`. |
| `[@unique]` | Unique index. Generates `get_by_<field>` and `find_by_<field>`. |
| `[@index]` | Index. Generates `filter_by_<field>`. |
| `[@has_many]`, `[@belongs_to { key; references }]` | Relations. The field type is `'a Deferred.t`. `belongs_to` makes a `FOREIGN KEY ... ON DELETE RESTRICT`, and it acts as `[@index]` on the key field (one index, and `filter_by_<key>`). An explicit `[@index]` on that field adds no second index. A key field that is the `[@key]` or has `[@unique]` gets no plain index, because its index serves the foreign key. In version 1, `references` must be the `[@key]` field of the other model. |
| `[@default v]` | The create argument becomes optional. The client puts the default in the `INSERT`. The DDL has no `DEFAULT` clauses. |
| `[@column "x"]`, `[@@table "x"]` | Names in the database. The default table name is the type name. The rules (`Cistern_names.valid_identifier`): valid UTF-8, 1 to 64 characters (not bytes), each in U+0001 to U+FFFF (the servers keep names in utf8mb3), no backquote, and no ASCII white space at the end. A table name is also a file name: InnoDB encodes each character other than `[0-9A-Za-z_]` in 3 or 5 bytes, with a limit of 251 bytes. `Cistern_names.valid_table_name` counts 5 bytes for each such character, thus 64 × `€` is a valid column name but not a valid table name. A table name must not start with `#mysql50#`: MariaDB 11.4 refuses it (1103), and MariaDB 11.8 uses another table. The ppx and `Schema.check` apply these rules. |

The ppx refuses field names that clash with generated names:

- For each field (the fields are values in `Q`): the names of `Expr.OPS` (`is_null`, `in_`, `starts_with`, `contains`).
- For a relation field (it is also a value of the model module): `all`, `where`, `update`, `delete`, `draft`, `create`, `model`, `get_by_*`, `find_by_*`, `filter_by_*`.

Thus a plain column can have a name such as `model` or `update`.

Field types in version 1:

| OCaml | Column type |
|---|---|
| `int64`, `int` | `BIGINT` |
| `string` | `VARCHAR(255)` if indexed or a key, else `MEDIUMTEXT`. Indexed: `[@unique]`, `[@index]`, or the key field of a `belongs_to`. |
| `bool` | `TINYINT(1)`. Decoding: 0 is `false`, any other value is `true`. |
| `float` | `DOUBLE` |
| `bytes` | `VARBINARY(255)` if indexed or a key, else `MEDIUMBLOB` |
| `Ptime.t` | `DATETIME(6)` in UTC. Timestamps are truncated to 1 µs on the client. A timestamp before the year 1000 gives `Invalid_value` on the client, because the documented range of `DATETIME` is the years 1000 to 9999. |
| `'a option` | nullable |

- A value that is too long or out of range gives `Invalid_value`, because the session uses `STRICT_ALL_TABLES`.
- String comparison uses a binary collation, so it is case-sensitive. For example, two emails that differ only in case are two different unique values (documented).

### Generated code (three stages)

OCaml refuses two definitions of one module name in one structure. Thus, for a group whose first type is `user`:

1. Model modules at top level, one for each type, in declaration order: `module Cistern_model__user : MODEL with type t = user and type key = int64 and type draft = ...`. Before them, `module Cistern_drafts__user = struct type user = {...} and todo = {...} ... end`. It also holds the functions that need only types: the draft encoders, the record builders, and the setters, in one submodule for each type (`Cistern_drafts__user.Set_todo.user_id` sets the key of a `Drafts.todo`, and `Cistern_drafts__user.Set_user.todos` sets a `Loaded` field). Type names are unique in a group, so these names cannot clash, and they do not hide values of the user. A draft has only the column fields of its type, not the relation fields.
2. Stage 2 in the namespace module `module Cistern_gen__user = struct module User = struct ... end module Todo = struct ... end end`: `Q`, `get_by_*`, `find_by_*`, `filter_by_*`, `all`, `where`, `update`, `delete`, `draft`. These are thin wrappers around the generic operations, for example `let get_by_id k = Cistern.Private.Model.get (module Cistern_model__user) k`.
3. Stage 3 after the namespace module: `module User = struct include Cistern_gen__user.User ... end` and the same for `Todo`. It adds `create` (nested drafts), the relation scopes (`User.todos`), `Rel` (relation descriptors), `model` and `Drafts`. Stage 3 refers to other models through the namespace module or the model modules, because the stage-3 `Todo` comes after the stage-3 `User`.

Rules:

- All generated code calls the library through `Cistern.Private.*`, so a user type named `model`, `query` or `field` does not hide it.
- The model modules have top-level names (`Cistern_model__todo`), so a later version can permit relations between two groups.
- Two groups with the same first type name in one file clash (documented).
- An expect test compiles a file with two deriving groups.

The children step of `create`:

```ocaml
val Model.child :
  (module C : MODEL) -> fk:(C.draft -> 'k -> C.draft) -> set:('p -> C.t list -> 'p)
  -> C.draft list -> ('p, 'k) child

(* stage 3 of User *)
let create ~name ~email ?(todos = []) () =
  Cistern.Private.Model.create (module Cistern_model__user) { name; email }
    ~children:[ Cistern.Private.Model.child (module Cistern_model__todo)
                  ~fk:Cistern_drafts__user.Set_todo.user_id
                  ~set:Cistern_drafts__user.Set_user.todos todos ]
```

`fk` and `set` are setters of stage 1. Thus `create` gives the parent with `Loaded` children. Because `references` is always the key of the parent in version 1, `'k` is `M.key` of the parent, and all children of one parent have one type `('p, 'k) child`.

Implementation note (compiled on OCaml 5.5.0 in round 5): the definition of a module-dependent function needs a full type annotation, `let child : (module C : MODEL) -> ... = fun (module C) ~fk ~set ds -> ...`. The short form `let child (module C : MODEL) ~fk ...` gives the error "The module C would escape its scope".

Rules for drafts:

- In `Todo.draft`, each `belongs_to` key is an optional argument (`?user_id`). A nested create fills the key of its parent. A missing key gives `Constraint_violation Not_null` from the database.
- `User.draft` has no relation arguments, because a draft holds only columns. Only `User.create` takes `?todos`.

```ocaml
module User : sig
  type t = user
  module Drafts = Cistern_drafts__user
  type draft = Drafts.user
  val model : (module Cistern.MODEL)
  module Q : sig
    val id : (t, int64) Field.t
    val name : (t, string) Field.t
    val email : (t, string) Field.t
    include Expr.OPS   (* ==. <>. <. >. <=. >=. is_null in_ starts_with contains, and && || not *)
  end
  module Rel : sig val todos : (t, todo) Relation.t end
  val get_by_id : int64 -> t Stmt.t
  val find_by_id : int64 -> t option Stmt.t
  val get_by_email : string -> t Stmt.t
  val find_by_email : string -> t option Stmt.t
  val all : t Query.t
  val where : (t, bool) Expr.t -> t Query.t
  val draft : name:string -> email:string -> unit -> draft
  val create : name:string -> email:string -> ?todos:Drafts.todo list -> unit -> t Stmt.t
  val update : ?name:string -> ?email:string -> t -> t Stmt.t
  val delete : t -> unit Stmt.t
  val todos : t -> todo Query.t
end
```

Filters:

- `&&`, `||` and `not` change meaning only inside `X.Q.( ... )`, where the field names already hide other names. `Expr.and_` and `Expr.or_` take lists.
- On a field with an `option` type, the filters follow OCaml equality, also for `NULL`:
  - `f ==. v` gives `f <=> v` (the NULL-safe operator of the two servers, which can use an index). `f ==. None` gives `IS NULL`.
  - `f <>. v` gives `NOT (f <=> v)`. Thus `f <>. Some 1` keeps the rows where `f` is `NULL`, as OCaml keeps `None`.
  - `in_ f [None; Some 1]` gives `(f IN (1) OR f IS NULL)`.
  - `<.`, `>.`, `<=.` and `>=.` with `None` raise `Invalid_argument` when the query is built.
- `in_ f []` gives `FALSE`. `IN ()` is a syntax error.
- `starts_with` and `contains` give `LIKE <hex literal> ESCAPE '!'`. The client escapes `%`, `_` and `!` in the pattern with `!`. The escape character does not depend on `NO_BACKSLASH_ESCAPES`.

### Deferred values

`type 'a Deferred.t = Unloaded | Loaded of 'a` (immutable, no closures, so `=` and printing work).

- `get_by_*`, `find_by_*` and queries give `Unloaded` relations, unless `include_` loads them.
- `create` gives `Loaded` children. Each child has an `Unloaded` back-reference.
- `update` keeps the relations of its input record. If the update changes a `belongs_to` key, that relation becomes `Unloaded`.
- `include_` never fills the inverse relation, so no data is cyclic.

### Usage

```ocaml
open Weft
open Cistern

let () =
  Mirage_crypto_rng_unix.use_default ();   (* the application seeds the TLS generator *)
  Weft.run (module Weft_os) @@ fun () ->
  let config =
    Config.of_url "mysql://app:secret@db.example.com/app?ssl-mode=VERIFY_IDENTITY&ssl-ca=/etc/ssl/db-ca.pem"
    |> Result.get_ok
  in
  Db.with_db ~pool_size:16 config @@ fun db ->
  Db.push_schema db [ User.model; Todo.model ];
  let user =
    User.create ~name:"John Doe" ~email:"john@example.com"
      ~todos:Todo.[ draft ~title:"Make pizza" (); draft ~title:"Sleep" () ] ()
    |> Db.exec_exn db
  in
  let uid = user.id in
  let _open_todos =
    Todo.where Todo.Q.(user_id ==. uid && not (done_ ==. true))
    |> Query.order_by Todo.Q.id `Asc |> Query.limit 10 |> Query.to_list
    |> Db.exec_exn ~timeout:(Span.s 2) db
  in
  let _same_todos =
    User.todos user |> Query.where Todo.Q.(not (done_ ==. true)) |> Query.to_list
    |> Db.exec_exn db
  in
  let _users = User.all |> Query.include_ User.Rel.todos |> Query.to_list |> Db.exec_exn db in
  Db.transaction db (fun tx ->
      let u = User.where User.Q.(id ==. uid) |> Query.for_update |> Query.first |> Tx.exec_exn tx in
      let u = User.update ~name:"Jane" (Option.get u) |> Tx.exec_exn tx in
      User.delete u |> Tx.exec_exn tx)
  |> Result.get_ok
```

Note: the field names of `Q` hide user values with the same names under `Todo.Q.( ... )`. Bind the values outside, as `uid` above.

### Core user types

```ocaml
module Db : sig
  type t      (* the pool. Safe across fibers and domains. Any use after with_db raises Invalid_argument *)
  val with_db :
    ?pool_size:int -> ?max_lifetime:Weft.Span.t -> ?checkout_timeout:Weft.Span.t
    -> ?on_query:(Event.t -> unit) -> ?random:Random.State.t
    -> Config.t -> (t -> 'a) -> 'a   (* owns a Fiber.scope *)
  val push_schema : t -> (module MODEL) list -> unit   (* checks the relations of the models first *)
  val check_schema : t -> (module MODEL) list -> (unit, string list) result
  val exec : ?timeout:Weft.Span.t -> t -> 'a Stmt.t -> ('a, Error.t) result   (* default timeout 30 s *)
  val exec_exn : ?timeout:Weft.Span.t -> t -> 'a Stmt.t -> 'a          (* raises Cistern_error *)
  val transaction : ?timeout:Weft.Span.t -> t -> (Tx.t -> 'a) -> ('a, Error.t) result
end

module Tx : sig
  type t      (* one connection. Only the fiber of the callback can use it *)
  val exec : ?timeout:Weft.Span.t -> t -> 'a Stmt.t -> ('a, Error.t) result
  val exec_exn : ?timeout:Weft.Span.t -> t -> 'a Stmt.t -> 'a
  val savepoint : t -> (t -> 'a) -> ('a, Error.t) result
end

module Raw : sig
  type t
  val sql : string -> t                  (* fixed SQL text from the program, never from users *)
  val v : Value.t -> t                   (* a literal, with a space before and after it *)
  val ( ++ ) : t -> t -> t
  type result = { columns : string array; rows : Value.t array list }
  val query : t -> result Stmt.t
  val exec : t -> Exec_result.t Stmt.t   (* affected rows, last insert id *)
end

module Query : sig
  type 'm t
  val where : ('m, bool) Expr.t -> 'm t -> 'm t   (* AND with the filter of the query *)
  val order_by : ('m, _) Field.t -> [ `Asc | `Desc ] -> 'm t -> 'm t   (* each call adds a key *)
  val limit : int -> 'm t -> 'm t
  val offset : int -> 'm t -> 'm t       (* without limit: LIMIT 18446744073709551615 *)
  val include_ : ('m, _) Relation.t -> 'm t -> 'm t
  val for_update : 'm t -> 'm t          (* only in a Tx; Db.exec raises Invalid_argument *)
  val for_share : 'm t -> 'm t           (* MySQL FOR SHARE, MariaDB LOCK IN SHARE MODE *)
  val to_list : 'm t -> 'm list Stmt.t
  val first : 'm t -> 'm option Stmt.t
  val count : 'm t -> int Stmt.t         (* with limit or offset: a derived table *)
  val exists : 'm t -> bool Stmt.t
  val update : 'm Update.t -> 'm t -> int Stmt.t      (* matched rows *)
  val delete : 'm t -> int Stmt.t
end

module Update : sig
  type 'm t
  val set : ('m, 'a) Field.t -> 'a -> 'm t
  val ( & ) : 'm t -> 'm t -> 'm t
end

module Stmt : sig
  type 'a t
  val map : ('a -> 'b) -> 'a t -> 'b t
  val to_sql : Dialect.t -> 'a t -> string list
  (* for snapshot tests. A value that comes from an earlier step of the
     statement shows as a placeholder: <last_insert_id>, <keys> *)
end

module Error : sig
  type kind = Unique | Foreign_key | Not_null | Check | Other_constraint
  type t =
    | Not_found
    | Constraint_violation of { kind : kind; message : string }
    | Invalid_value of { code : int option; message : string }
    | Retryable of [ `Deadlock | `Lock_timeout | `Rolled_back ]
    | Statement_timeout     (* the server stopped the statement (3024, 1969); the connection is good *)
    | Timeout               (* a read or an unsent statement passed its deadline *)
    | Pool_exhausted        (* no connection before checkout_timeout *)
    | Outcome_unknown of [ `Statement | `Commit ]   (* a write was sent, and no reply came *)
    | Implicit_commit       (* in a Tx, the server committed the transaction before its end *)
    | Connection_lost       (* before a write was sent, or during a read *)
    | Read_only             (* 1290 with --read-only or --super-read-only, 1792, 1836, 1874 *)
    | Connect of string     (* the last connect error of the pool *)
    | Decode of string
    | Driver of { code : int; sqlstate : string; message : string }
  val interrupts : Dialect.t -> int -> bool      (* 1053, 1317; 4031 on MySQL; 1927 on MariaDB *)
  val of_server : Dialect.t -> code:int -> sqlstate:string -> message:string -> t
  val pp_summary : Format.formatter -> t -> unit   (* no message: for logs *)
  val pp : Format.formatter -> t -> unit           (* the summary and the message *)
  val to_string : t -> string                      (* the text of pp *)
end
exception Cistern_error of Error.t   (* its printer (Printexc.register_printer) prints the summary *)
```

Rules:

- `Query.update` and `Query.delete` with `offset`, `include_`, `for_update` or `for_share` raise `Invalid_argument` (MySQL has no `UPDATE ... OFFSET`). `count` and `exists` ignore `include_`, `for_update` and `for_share`.
- A raw statement marks its connection, which then gets a session reset at checkin (see "Session").
- `Value.t` has `Null`, `Int of int64`, `Float of float`, `Text of string`, `Bytes of string`, `Datetime of Ptime.t`, and `Decimal of Decimal.t` (for raw results such as `SUM`). `Decimal.t` is abstract. `Decimal.of_string` accepts only `-?[0-9]+(\.[0-9]+)?`, with at most 65 digits in total and at most 38 after the point (the `DECIMAL` limits of the servers: 65 digits, and a scale of at most 38 on MariaDB and 30 on MySQL), so that `Raw.v (Decimal d)` cannot carry SQL text.
- The exception is `Cistern_error`, not `Error`, so that `open Cistern` does not hide the `Error` constructor of `result`.
- A cancel of the fiber is never an `Error.t`. weft's `Cancelled` always goes up, also when a write was in flight. Then the `on_query` event of the statement records the unknown outcome, and the `Tx` (if any) fails.
- A failure in a hook or in the checkin never changes the result of the statement.

Order of the error rules:

- A write (`INSERT`, `UPDATE`, `DELETE`, any raw statement, `COMMIT`) that was sent with no clear reply gives `Outcome_unknown`. "No clear reply" is a deadline, the end of the stream, or an error for which `Error.interrupts` is true followed by the end of the stream: 1053 or 1317 (the two servers), 4031 (MySQL) or 1927 (MariaDB). This rule is the same for `COMMIT` (it gives `Outcome_unknown `Commit`) and for a write in autocommit mode. The user must not retry it blindly.
- `Error.of_server` gives `Connection_lost` for such an error also when the stream did not end. Then the client closes the connection, because its state is not known, and a write that it sent gives `Outcome_unknown`.
- `Timeout` and `Connection_lost` are only for reads and for statements that were not sent.
- At the level of `Db.transaction`: if the client closed the connection before it sent `COMMIT`, the server rolled back. Then `Db.transaction` gives `Timeout` or `Connection_lost`, not `Outcome_unknown `Statement`, because a retry of the whole transaction is safe.

Error mapping. The key is (server, error number). A number that the table does not have uses the SQLSTATE: class `23` gives `Constraint_violation Other_constraint`, and `40001` gives `Retryable `Deadlock`.

| Error | MySQL | MariaDB |
|---|---|---|
| `Unique` | 1062, 1586, 1022, 1169, 1859, 1761, 1762 | the same |
| `Foreign_key` | 1451, 1452, 1216, 1217 | 1451, 1452, 1216, 1217 |
| `Not_null` | 1048, 1364, 3673 (`ER_BAD_NULL_ERROR_NOT_IGNORED`) | 1048, 1364 |
| `Check` | 3819 | 4025 (`ER_CONSTRAINT_FAILED`, verified in round 5) |
| `Invalid_value` | 1406, 1264, 1265, 1365, 1366, 1292, 1690, 1367, 1301, and 1153 (see the note that follows) | the same |
| `Retryable `Deadlock` | 1213 | 1213 |
| `Retryable `Lock_timeout` | 1205 | 1205 (also for `NOWAIT`, not verified) |
| `Retryable `Rolled_back` | 3101 (`ER_TRANSACTION_ROLLBACK_DURING_COMMIT`) | none (3101 is a placeholder number) |
| `Statement_timeout` | 3024 | 1969 (`ER_STATEMENT_TIMEOUT`, verified in round 5) |
| `Connection_lost` (a read or an unsent statement; a sent write gives `Outcome_unknown`) | 4031, 1053, end of stream | 1927, 1053, end of stream |
| `Read_only` | 1290 when the message names the option `--read-only` or `--super-read-only`, 1792, 1836 (`ER_READ_ONLY_MODE`), 1874 (`ER_INNODB_READ_ONLY`) | the same |
| `Driver` | all others, also 1317, 1052 (`ER_NON_UNIQ_ERROR`, an ambiguous column, with SQLSTATE 23000), 1290 for other options, 1036 (`ER_OPEN_AS_READONLY`, one table) and 3572 (`ER_LOCK_NOWAIT`) | the same, with no 3572 |

The server gives no structured constraint name, so `Constraint_violation` has only the message.

`perror` of the pinned packages (MySQL 8.4.11, MariaDB 11.4.13 and 11.8.8) confirmed each number of the table and its name on 2026-11-06. A number of one server is another error on the other server (MySQL 4025, MariaDB 3024, 3101 and 4031), or no error (MySQL 1927 and 1969, MariaDB 3819 and 3572). Thus the key is the pair. 2013 and 2006 are codes of the client library: a server never sends them. `Error.of_server` gives the meaning of an error packet alone. The rules that need the state of the connection ("Order of the error rules", "Transactions") are in the client.

Error 1153 (packet too large): the server did not run the statement, but it closes the connection after the error. Thus the client gives `Invalid_value`, closes the connection, and fails the `Tx` (if any). The client size check (see "Limits and defaults") prevents this error in most cases.

## Internal architecture

### Schema, AST and SQL

- One schema (`Schema`): models, fields, column names, types, the key, indexes and relations. `push_schema` and `check_schema` call `Schema.check` first. Its rules:
  1. The model names are unique. The table names are unique after the ASCII fold (the file system of macOS ignores case).
  2. Each table name and each column name obeys the rules of names ("Model definition").
  3. Each model has exactly one `[@key]` field. The key is not an `option` and not a `float`.
  4. `[@auto]` is only on the key, and only on an `int64` field.
  5. The column names of a model are unique after the ASCII fold.
  6. The key has no `[@unique]` and no `[@index]`. A field does not have `[@unique]` and `[@index]` together.
  7. The relation names of a model are unique, and differ from the field names.
  8. `belongs_to`: the key is a field, the other model is in the list, `references` is its key, and the two fields have the same type. Two `belongs_to` of a model do not use one key. A `belongs_to` to its own model does not use the `[@key]` as its key: MySQL refuses `FOREIGN KEY (id) REFERENCES t (id)` (6125), and on MariaDB each `INSERT` fails (1452).
  9. `has_many`: the other model is in the list, with exactly one `belongs_to` to this model (version 1 has no `via`).
  10. The names of the unique and plain indexes of a table are unique after the ASCII fold.
  11. The foreign key names of all the models are unique after the ASCII fold, because a constraint name is unique in the database. `check` cannot see the foreign keys of tables outside the list.

  A group of equal names gives one message with all the members. A rule that needs a part that another rule refuses does not run. The fold is ASCII only: the servers also fold other letters (`É` to `é`), thus two such names pass `check`, and the server refuses the DDL with 1060 (documented). An index name with a non-ASCII character always has the hash, thus no 1061. The fold of the servers differs: MariaDB 11.8 accepts the Kelvin sign (U+212A) and `k` as two columns. The messages quote names with `Cistern_names.quote`: valid UTF-8 stays as it is, and control characters and invalid bytes become escapes.
- The typed API builds a model-level untyped AST. `cistern.sql` compiles it to a SQL AST and serializes it for the dialect of the connection.
- Each `SELECT` has an explicit, qualified column list in the field order of the model. The decoders read columns by position.
- Identifiers are always quoted with backquotes.

### Literals (cistern.core)

No value from a user is ever between quotes:

| Value | Literal |
|---|---|
| `int64`, `int` | Decimal digits, with a sign. `Int64.min_int` is in the property tests. |
| `float` | Cistern's own method: the shortest correctly rounded `%.{p}e` form that reads back to the same bits. For p from 0 to 16, the client formats the float with `%.{p}e`, takes the sign, the digits and the exponent from the result, skips one byte for the point, and writes its own point. No locale can change the literal, because the OCaml 5.5 runtime formats and reads floats in the "C" locale (`runtime/floats.c`: `caml_format_float` and `caml_float_of_string`, with `USE_LOCALE` and `strtod_l`). Always with an exponent and with no `+`, for example `1.5e0`, `1e-1` and `-0e0`, so that the server reads a `DOUBLE` and keeps the sign of `-0` (not verified). The form is not always the shortest decimal: 2^-1017 gives `7.1202363472230444e-307`. NaN and infinities give `Invalid_value` on the client. |
| `bool` | `0` or `1` |
| `string` | `_utf8mb4 X'<hex>'`. The client checks that the string is valid UTF-8 first. |
| `bytes` | `X'<hex>'` |
| `Ptime.t` | `'YYYY-MM-DD HH:MM:SS.ffffff'`, made by the client from the number, with no user text. A timestamp before the year 1000 gives `Invalid_value` on the client. |
| `Decimal.t` (raw only) | The digits that `Decimal.of_string` checked, as they are. More than 30 digits after the point give `Invalid_value` on the client, because MySQL cuts such a literal with no error. |
| `None` | `NULL` |

- The literals do not depend on `NO_BACKSLASH_ESCAPES` or another session setting.
- A comparison between a column and a literal uses the collation of the column, because a literal has a higher coercibility. A review found no path to error 1267 or 1271.
- `Raw` joins only fixed program text and these literals, so it needs no lexer. It puts a space before and after each literal, so that `Raw.sql "LIMIT" ++ Raw.v (Int 5L)` gives `LIMIT 5 `, not `LIMIT5`.

### DDL

- `CREATE TABLE IF NOT EXISTS ... ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=<collation> ROW_FORMAT=DYNAMIC`. `ROW_FORMAT=DYNAMIC` permits unique indexes on `VARCHAR(255)` in `utf8mb4` (1020 bytes).
- Collation: binary with no trailing-space padding. MySQL: `utf8mb4_0900_bin`. MariaDB: `utf8mb4_nopad_bin`.
- Indexes and foreign keys are inside `CREATE TABLE`, because MySQL has no `CREATE INDEX IF NOT EXISTS`. Each foreign key has an explicit index with a known name. The name of an index or a constraint (`Cistern_names.index_name`) is `<table>_<column>_<kind>`, with the kind `key` (unique), `idx` (plain) or `fk` (foreign key), for example `user_email_key`, `todo_user_id_idx` and `todo_user_id_fk`. If this name has more than 64 characters or a non-ASCII character, it becomes `<prefix>_<kind>_<hash>`: `<hash>` is the 8 lowercase hex digits of the FNV-1a 32 hash of the full name, and `<prefix>` is `<table>_<column>`, cut on a character boundary to 64 characters in total. The hash on non-ASCII names prevents equal names on MySQL, which compares constraint names with no accents.
- No `DEFAULT` clauses (MySQL refuses them on `MEDIUMTEXT` and `MEDIUMBLOB`). `[@default]` works on the client.
- `push_schema` runs on its own connection with `SET foreign_key_checks = 0`, so the order of the tables and cycles of references do not matter (mysqldump uses the same method). The connection is closed after the DDL. This connection is outside `pool_size`, so the pool can have one more session during `push_schema` (documented).
- `push_schema` never changes a table that exists. To change a table, use a raw `ALTER TABLE`, then `check_schema` (documented).
- `check_schema` (version 1) compares the tables, the column names, `DATA_TYPE` and `IS_NULLABLE` from `information_schema`. Indexes, foreign keys and collations come later.
- `push_schema` and `check_schema` from a fiber that holds a `Tx.t` raise `Invalid_argument`: DDL commits the open transaction, and it waits for the metadata lock of the transaction.

### Execution of one statement

1. Serialize the SQL for the dialect of the connection. If the statement is too large (see "Limits and defaults"), give `Invalid_value` and send nothing.
2. Take one connection from the pool. Run all operations of the statement in order on this connection.
3. Read all rows before the connection goes back to the pool. Typed row decoders that the ppx generates put the text values straight into records.
4. If a decoder fails, the client drops the rows and reads up to the end of the result set, and gives `Decode`. If that fails, the connection closes.
5. An error packet can come after some rows (for example 3024). The client drops the partial rows and gives the error.

Relations and writes:

| Operation | Method |
|---|---|
| `include_` (has_many and belongs_to) | One more query with `IN (...)`. The keys are unique on the client, in chunks (see "Limits and defaults"). No keys: no query. |
| Nested create (one level) | Parent `INSERT`, the last insert id from the OK packet, then one `INSERT` for each child. Outside a `Tx`, all of it runs in a transaction. In a `Tx`, it runs in a `SAVEPOINT` (a `START TRANSACTION` in a transaction commits the open transaction). |
| `create` gives the record | The input values, the defaults of the model and the last insert id. No `SELECT`. |
| `update` gives the record | The input record with the new values (`CLIENT_FOUND_ROWS`: 0 matched rows give `Not_found`). It does not see changes from the server, for example from triggers (documented). |

Nobody verified that one multi-row insert gets consecutive ids, so the children go in one row at a time.

### OCaml 5.5 features

OCaml 5.5 has module-dependent functions and polymorphic parameters. Tests on 2026-11-06 showed:

- OCaml 5.5.1 compiles and runs the two features.
- ppxlib 0.38 uses the OCaml 5.2 AST inside. It keeps a module-dependent arrow type as an encoded extension (`[%ppxlib.migration.ptyp_functor_5_5 ...]`). A file with this type compiles through a ppxlib driver, and the result runs. A polymorphic parameter fits the 5.2 AST directly.
- ocamlformat 0.29.0 formats the two features.
- A review compiled a prototype of the generated code of draft 10 (model modules, namespace module, stage 3, the usage expressions). It compiled.

Use in Cistern:

| Feature | Place | Gain |
|---|---|---|
| Module-dependent functions | Generic model operations on `module type MODEL` (types `t`, `key`, `draft`, `update`; the schema, the encoders and the decoders): `Model.get : (module M : MODEL) -> M.key -> M.t Stmt.t`, and the same for `find`, `create`, `child`, `update`, `delete`, `where` | Signatures without a value with four type parameters. The ppx makes less code. |
| Polymorphic parameters | Generic loops over the fields of a record (encode a record to literals, `check_schema`): `iter_fields : ('a. ('m, 'a) Field.t -> 'a -> unit) -> 'm -> unit` | No wrapper records `{ f : 'a. ... }` |
| Polymorphic parameters | Tests: one suite runs on `Db.exec` and on `Tx.exec` through a runner: `run : (('a. 'a Stmt.t -> ('a, Error.t) result) -> unit) -> unit` | No functor and no wrapper record |
| Module-dependent functions | The integration suite: `suite : (module S : SERVER) -> Windtrap.test` | No functor |

Rules:

- Use the two features in hand-written library code and in tests.
- The ppx does not generate a module-dependent arrow type, because it would need the internal encoding of ppxlib, which can change. The generated code only calls the generic operations with `(module Cistern_model__user)` (plain 5.2 AST).
- A polymorphic argument from a partial application must be eta-expanded, for example `(fun s -> Tx.exec tx s)`.
- Phase 0 checks that `ppx_windtrap.mutate` (a ppxlib driver) works on files with module-dependent arrow types.
- weft already needs OCaml 5.5, so the two features do not narrow the set of users.

## The MySQL and MariaDB client (cistern.mysql)

### Configuration

`Config.of_url` takes `mysql://` (`mariadb://` is an alias): user, password (percent-decoded), host, port, database, `socket=<path>`, `ssl-mode`, `ssl-ca=<path>`, `ssl-fingerprint=sha256:<hex>`, `allow-cleartext=socket`, `kill=true|false`. The database is necessary: a URL without it gives an error, because the session reset needs it for `COM_INIT_DB`. It gives `(Config.t, string) result`. `Config.make` takes the same values, and also `?authenticator:X509.Authenticator.t`. The CA file is read with `Weft.File`. The printers of `Config.t` and the `Connect` events never show the password.

`ssl-mode` uses the MySQL names:

| `ssl-mode` | TLS | Certificate check |
|---|---|---|
| `DISABLED` | No | None |
| `REQUIRED` | Yes | None. Documented: no protection against an attacker in the middle. |
| `VERIFY_CA` | Yes | The chain only, with `ssl-ca` or the authenticator. No peer name is set. |
| `VERIFY_IDENTITY` | Yes | The chain and the name: `Tls.Config.peer` for a host name, `Tls.Config.ip` for an IP address |

`ssl-fingerprint=sha256:<hex>` pins the SHA-256 hash of the server certificate (`X509.Authenticator.server_cert_fingerprint`). It needs TLS, and it is a verified identity for the clear-text rule, the same as `VERIFY_IDENTITY`.

Defaults:

- A Unix socket: no TLS.
- TCP (also on the loopback address) with `ssl-ca` or an authenticator: `VERIFY_IDENTITY`.
- TCP with `ssl-fingerprint`: TLS with the pinned certificate.
- TCP without them: `Config.of_url` gives an error, unless the URL gives `ssl-mode` explicitly.
- The certificate that MySQL 8.4 makes (`auto_generate_certs`) has the name `MySQL_Server_<version>_Auto_Generated_Server_Certificate` and no host name, so `VERIFY_IDENTITY` refuses it. MariaDB 11.4 makes a self-signed certificate. The documentation tells users to make a server certificate with the host name, or to pin the certificate with `ssl-fingerprint`.

### Handshake

1. Read the server greeting (protocol v10): capabilities, the authentication plugin, the 20-byte scramble, and the MariaDB extended capabilities when the `CLIENT_MYSQL` bit is clear. The greeting decides only the framing and the capabilities.
2. Client capabilities: `CLIENT_PROTOCOL_41`, `CLIENT_SECURE_CONNECTION`, `CLIENT_PLUGIN_AUTH`, `CLIENT_DEPRECATE_EOF`, `CLIENT_FOUND_ROWS`, `CLIENT_TRANSACTIONS`, `CLIENT_CONNECT_WITH_DB`, `CLIENT_MULTI_RESULTS`, and `CLIENT_SSL` with TLS. Not set: `CLIENT_MULTI_STATEMENTS`, `CLIENT_LOCAL_FILES`, `CLIENT_SESSION_TRACK`, `MARIADB_CLIENT_PROGRESS`.
3. Character set byte in the handshake: 45 (`utf8mb4_general_ci`), which exists on the two servers.
4. TLS: the client sends the SSL request packet (sequence id 1; on MariaDB with the same extended capabilities in bytes 28 to 31), then starts `Weft_tls.client`. The reader must have no buffered bytes at this point. If it has, the client fails. The handshake response then has sequence id 2. Under `REQUIRED` and stronger modes, a server without `CLIENT_SSL` fails.
5. The client checks at creation that the application set the TLS random generator (`mirage-crypto-rng`).

### Authentication

| Plugin | Method |
|---|---|
| `caching_sha2_password` | Fast path: the SHA-256 scramble. The server answers `0x01 0x03` and then an OK packet. Full path: the server answers `0x01 0x04`, and the client sends the password in clear text. |
| `mysql_native_password` | The SHA-1 scramble. |

The clear-text password of the full path goes only:

- under `VERIFY_IDENTITY`;
- with `ssl-fingerprint`;
- on a Unix socket with `allow-cleartext=socket`.

The socket rule is an explicit opt-in for every socket. A check of the path is not safe: on macOS `/tmp` is a link to `/private/tmp`, and other directories (for example `/dev/shm`) are also writable by all users. While the server is down, a local user can make a socket at such a path. The client does not have the owner of the directory (`Weft.File.stat` gives `perm` but no owner), so it cannot check the directory.

In other cases the client fails with `Connect "the server asks for full authentication; use VERIFY_IDENTITY, ssl-fingerprint, or allow-cleartext=socket"`. Thus a server or an attacker that answers `0x04` cannot get the password. With `DISABLED` over TCP, the first connect after each server restart needs the full path, so it fails with this message (documented).

- `AuthSwitchRequest` to one of the two plugins: the client uses the first 20 bytes of the new plugin data (20 bytes and a NUL). Another length fails. A switch to another plugin gives `Connect "unsupported authentication plugin <name>"`. For example, MariaDB `root` uses `unix_socket` by default.
- The integration tests run with TLS (`VERIFY_IDENTITY` with the test CA, and `ssl-fingerprint`) or a Unix socket with `allow-cleartext=socket`, and one test empties the authentication cache first.

### Session

Three round trips after the authentication:

1. `SELECT CONNECTION_ID(), @@version, @@version_comment, @@hostname, @@port, @@server_id, @@wait_timeout, @@innodb_rollback_on_timeout, @@max_allowed_packet`. All names exist on the two servers. The dialect comes from `@@version` and `@@version_comment` (`Dialect.of_version`): the text `mariadb` in one of them, with no regard to case, gives MariaDB, else MySQL. The pinned MySQL build has the comment `Source distribution`, thus the comment cannot identify MySQL. `@@version` has no `5.5.5-` prefix on the pinned servers. A version that does not start with `<major>.<minor>.<patch>` gives `Driver`.
2. `SET NAMES utf8mb4 COLLATE <collation of the dialect>, time_zone = '+00:00', sql_mode = 'STRICT_ALL_TABLES,NO_BACKSLASH_ESCAPES,NO_ZERO_IN_DATE,NO_ZERO_DATE,ERROR_FOR_DIVISION_BY_ZERO,NO_ENGINE_SUBSTITUTION', autocommit = 1`. The `sql_mode` is a complete value, because some global modes break Cistern (for example MariaDB `EMPTY_STRING_IS_NULL`).
3. Identity, in one `SELECT` that the dialect chooses (with no `CLIENT_MULTI_STATEMENTS`, one round trip has one statement):
   - MySQL: `SELECT @@server_uuid, UNIX_TIMESTAMP() - (SELECT VARIABLE_VALUE FROM performance_schema.global_status WHERE VARIABLE_NAME = 'Uptime')`.
   - MariaDB: `SELECT UNIX_TIMESTAMP() - (SELECT VARIABLE_VALUE FROM information_schema.GLOBAL_STATUS WHERE VARIABLE_NAME = 'UPTIME')`.
   - In one statement, `UNIX_TIMESTAMP()` and `Uptime` come from the same start time of the query, so the difference is exactly the start time of the server (verified in round 5 in MySQL 8.4 `sql/mysqld.cc`, `show_starttime`, and in the MariaDB 11.4 source). Thus the identity check uses exact equality, with no tolerance.
   - Any user can read `performance_schema.global_status` on MySQL 8.4 (verified in round 5: `table_global_status.cc` uses `pfs_truncatable_world_acl`).
   - If the query fails, `KILL` is off for this pool, and an event says why.
   - If the thread id of the greeting (4 bytes) is not the low 32 bits of `CONNECTION_ID()`, a proxy is probably between the client and the server. Then `KILL` is off for this pool, and an event says why. Not verified: that each common proxy (ProxySQL, MaxScale, MySQL Router) gives such a difference. `kill=false` stays the documented setting for proxies.

The identity of a connection is (`@@server_uuid` on MySQL, or (`@@hostname`, `@@port`, `@@server_id`) on MariaDB; the start time). `@@server_uuid` stays the same after a MySQL restart, so the start time is always part of the identity. With the exact start time, (identity, `CONNECTION_ID()`) names one session. Thread ids are not used again in one server lifetime before they wrap (believed, not verified). Thus version 1 has no `PROCESSLIST` check before `KILL`.

Session reset. At checkin, the client marks the connection "dirty" if it ran a raw statement, or if the last OK packet had `SERVER_STATUS_IN_TRANS` outside a `Tx`. The checkin does no I/O. At the next checkout of a dirty connection, before the statement and under the deadline of that caller, the client runs:

1. `COM_RESET_CONNECTION`. It sets all session variables back to their global values.
2. Step 2 again (the complete `SET`).
3. `COM_INIT_DB <database of the configuration>`, because a reset probably keeps the database of a raw `USE` (not verified; the MySQL `THD::cleanup_connection` does not change the database).

If any of these fails, the connection closes, and the checkout takes another connection. No statement was sent, so this is safe. Version 1 does not use session state tracking, because the typed API does not change session state.

### Commands and packets

- `COM_QUERY` (with a loop while `SERVER_MORE_RESULTS_EXISTS` is set), `COM_PING`, `COM_RESET_CONNECTION`, `COM_INIT_DB`, `COM_QUIT`.
- A result header of `0xFB` (a `LOCAL INFILE` request) closes the connection with an error.
- At checkout, the client calls `read_now` on the flow (not `readable`, because a TLS record with no data makes `readable` true). If the stream ended, or a packet came that the client did not ask for (for example error 4031, or 1053, or 1927), the client closes the connection and takes another one. No command was sent, so this is safe. This applies only at checkout. In a `Tx`, or between the operations of one statement, the same condition gives `Connection_lost`.
- If the stream ends after a write, the write was sent: a write gives `Outcome_unknown`, and a read is tried one more time on another connection (only outside a `Tx`).
- `Weft_flow.Writer` with packet frames (3-byte length and sequence id). One flush for each round trip. `Weft_flow.Reader` with `ensure`, integer reads and views at offsets.

## Concurrency with weft

### Pool

- `Pool_state` is pure: no effects, no I/O. Its operations take the time as an argument and give the hand-offs as data: `checkout : now -> [ `Conn of conn | `Wait of id | `Wait_and_open of id ]`, `checkin`, `cancel_wait`, `open_started`, `opened`, `open_failed`, `killing`, `killed`, `tick`, `next_timer`, `close_all`. It holds the counts of pending and running connects, the stored connect error, and the `Random.State.t` for the jitter.
- A pure state sees the time only when an operation runs. Thus `next_timer` gives the earliest time at which the state changes without an operation: the age limit of a `Killing` slot, or the end of the connect backoff. Without it, a waiter can wait for a slot that is already free. `tick now` applies these changes and gives the connects to start.
- Each `Killing` slot has a unique token. `killing` gives the token, and `killed tok` does nothing for a slot that already passed its age limit. Thus a slot is never freed two times, and the pool never has more than `pool_size` sessions from slots.
- A stdlib `Mutex` protects `Pool_state`. No effect happens while the mutex is held (weft `docs/design.md` §2.5, locks).
- Order in the weft shell, for each operation: (1) read the clock and set the flow deadlines, outside the mutex; (2) take the mutex, run the `Pool_state` operation, release the mutex; (3) fill Ivars and ring doorbells. At checkin, the deadlines are set to `None` in step 1, before the connection becomes visible to another fiber.
- A waiter is an `Ivar` that holds `Got of conn | Failed of Error.t | Abandoned`. `Ivar.try_fill` decides who wins:
  - Checkin pops waiters until `Ivar.try_fill w (Got c)` succeeds. If none succeeds, it pushes `c` on the idle stack (LIFO).
  - After a timeout or a cancel, the waiter calls `Ivar.try_fill w Abandoned`. If that fails, it takes `c` from `Ivar.peek w` and checks it in again. Only then does it give its error or raise `Cancelled`.
- Doorbells: `Fiber.Semaphore.create ~max:1 0` with `try_release`. `try_release` is not a cancellation point, so a cancelled fiber can ring it.
- Opener: a daemon in the `with_db` scope. It syncs on `choose [ acquire doorbell; Time.at next_timer ]`, calls `tick`, and starts the pending connects (at most 2 at a time) as plain `Fiber.fork` fibers in a nested scope, each with the connect timeout (`Fiber.with_timeout`). A connect fiber catches all exceptions except a `Cancelled` of its own chain, always calls `opened` or `open_failed`, and rings the doorbell of the opener when it ends. Because it catches the other exceptions, it needs no `fork_isolated`.
- Rule for every daemon of Cistern (the opener, the killer) and each connect fiber: a loop catches all exceptions except `Cancelled`. A `Cancelled` ends the loop. A catch-all that loops back to a `sync` after a cancel spins without end, because the cancel is sticky, and then the join of the scope never ends.
- Connect errors: the waiters keep waiting, and the opener tries again after the backoff. A waiter whose deadline passes while a connect error is stored gets `Connect msg`, not `Timeout` or `Pool_exhausted`. A successful connect clears the error.
- `max_lifetime` and the idle limit: see "Limits and defaults". An idle connection older than the idle limit is closed at checkout, not pinged.
- At checkout, a dirty connection gets the session reset (see "Session") before the statement, under the deadline of the caller.
- The end of `with_db`: `close_all` fails all waiters with `Failed Connection_lost`, closes the idle connections, and sets the closed flag. A connection that comes back later is closed. Any later use of `Db.t` raises `Invalid_argument "Db.t used after with_db"`.

### Deadlines

- `Db.exec` has one absolute `Mono` deadline from `?timeout` (default 30 s). `Tx.exec` uses the earlier of its `?timeout` and the transaction deadline.
- If the remaining time is less than the margin, `exec` gives `Timeout` and sends nothing.
- A read deadline that passes raises `Flow.Timeout` inside the wait, before bytes are read. A write deadline that passes can leave half a packet, and it fails a TLS flow. In the two cases the state of the connection is not known, and the cancel steps follow.
- PR 1a adds a keep-alive option to `Weft_flow.Tcp.connect` (weft has `Net.Keep_alive`). Cistern sets it, so that a half-open connection ends.

### Cancel

After a deadline, a fiber cancel or a lost reply in the middle of a statement, the connection state is not known. A close does not stop a running query on the server, and the query keeps its locks. Thus:

1. The client closes the connection (`close` never waits, so a cancelled fiber can call it).
2. `Pool_state` keeps the slot as `Killing`, with a token, so the pool does not open more sessions than `pool_size` while the old query runs. A `Killing` slot is free after its age limit, also if the `KILL` did not happen. The opener sees this time through `next_timer` (see "Pool").
3. The client puts (`CONNECTION_ID()`, the identity of the connection, the token) in the kill queue and rings the doorbell of the killer. The queue has no duplicates of (id, identity). If the queue is full, the entry is dropped, and the slot is free at once.
4. The killer is a separate daemon. It opens its control connection at the start of `with_db`, with the connect function of the pool, outside the slot accounting, with the connect timeout. If this connect fails, it tries again after the backoff, so the control connection is usually open before the first cancel. The control connection is never killed. It reads its own server identity at connect.
5. Before each entry, the killer applies the idle limit and the `read_now` check of the pool to the control connection (see "Commands and packets"). A control connection that is too old or closed by the server (error 4031 after `wait_timeout`) is opened again first.
6. For each entry, in `Fiber.shield` with `Fiber.with_timeout` of the `KILL` limit:
   - The killer drops the entry if its server identity (server id and exact start time) is not the identity of the control connection.
   - It sends `KILL CONNECTION <id>`. Error 1094 (no such thread) means that the query is already gone.
   - It frees the slot (`killed tok`), also when it drops the entry.
7. If the control connection fails during an entry, or the `KILL` limit passes, the killer closes the control connection, because its state is not known. It opens a new one and tries the entry one more time. Only after the second failure does it drop the entry and free the slot.
8. Events (`Kill`, `Kill_dropped`) go to `on_query` after the shield, in the killer fiber.
9. `kill=false` in the URL turns `KILL` off (for proxies such as ProxySQL, MaxScale or MySQL Router). Then the slot is free at once. Documented: behind a proxy, a cancelled query runs until it ends.
10. The end of `with_db`: `with_db` is `match Fiber.scope body with v -> drain (); v | exception e -> drain (); raise e`. When the body ends, weft cancels the daemons, and the join of the scope waits for them. The join ignores cancels (weft `docs/design.md`, "Lifetime"), and a shielded `KILL` has a time limit, so the join ends. Then `drain` runs after the scope, in `Fiber.shield` with `Fiber.with_timeout` of the drain limit. It opens a control connection if necessary and sends the remaining entries. It catches all exceptions, so `with_db` gives the result or the exception of the body. This needs no stop step and no `Ivar`.
11. `Statement_timeout` (3024, 1969): the server stopped the statement. The connection is good. No close and no `KILL`.

Phase 1.1 to phase 1.4 ship only the `kill=false` behavior: close the connection, and the slot is free at once. Phase 1.5 adds steps 2 to 10.

### Transactions

- `Tx.t` holds one connection, an `Atomic` active flag, a failed flag with the first error, and the id of the fiber of the callback (`Fiber.self`). Use from another fiber, or after the end, raises `Invalid_argument`.
- `Db.t` keeps an `Atomic` set of the fiber ids that hold a `Tx.t`. `Db.exec`, `Db.transaction`, `push_schema` and `check_schema` from such a fiber raise `Invalid_argument`, because they would wait for a second connection. A child fiber that the callback forks is not in the set (documented).
- Errors. In MySQL and MariaDB, a failed statement rolls back only the statement, except some errors. Thus:
  - These errors fail the `Tx`: 1213; 1205 when `@@innodb_rollback_on_timeout` is on; `Read_only`; `Timeout`; `Outcome_unknown`; `Connection_lost`; 1153; a cancel (the `Tx` fails, and `Cancelled` goes up).
  - An OK packet (also the OK at the end of a result set) with `SERVER_STATUS_IN_TRANS` clear means that the server committed the transaction, for example after a raw DDL statement, `TRUNCATE` or `LOCK TABLES`. The writes before it are committed. The `Tx` fails with `Implicit_commit`, never with a `Retryable` error, so that the user does not run the committed writes again. Documented: raw statements with an implicit commit end the `Tx`.
  - `Tx.exec` refuses (`Invalid_argument`) a raw statement whose first fragment, after leading white space, does not start with a letter, or starts with `BEGIN`, `START`, `COMMIT`, `ROLLBACK`, `SAVEPOINT`, `RELEASE`, `XA`, `LOCK`, `UNLOCK` or `SET` (any case). A raw `START TRANSACTION` commits the open transaction and sets `SERVER_STATUS_IN_TRANS` again, so the flag cannot show it. This is a check of the first word, not a lexer.
  - After an error whose number is not in the list of statement-only errors (constraint, `Invalid_value`, `Statement_timeout` numbers), the client sends `COM_PING` before it gives the error. An ERR packet has no status flags, and the OK of the ping shows `SERVER_STATUS_IN_TRANS`. If it is clear, the `Tx` fails. Verified in round 5 for MySQL (`sql_parse.cc`: `COM_PING` answers `my_ok`, and `trans_begin` sets the flag). Not verified for MariaDB.
  - Other errors (constraint violations, `Invalid_value`, `Statement_timeout`, `Not_found`, `Decode`) leave the `Tx` usable. The user can handle them.
  - A failed `Tx` gives its first error for each later `Tx.exec` and `Tx.savepoint`, with no I/O. Thus a write after a server rollback cannot run in autocommit mode.
- `Tx.savepoint` and the nested create in a `Tx` use `SAVEPOINT`. The scope gets the same `Tx.t`.
  - The callback returns: `RELEASE SAVEPOINT`, and the value.
  - The callback raises `Cistern_error e`: `ROLLBACK TO SAVEPOINT`, and `Error e`.
  - The callback raises another exception: `ROLLBACK TO SAVEPOINT`, then the exception again.
  - An `Error` that `Tx.exec` gave and that the callback handled does not roll back the scope.
  - If the savepoint is gone (error 1305 after a deadlock), the `Tx` fails.
  - If the `Tx` failed or the fiber was cancelled, the scope ends with no I/O: no `RELEASE` and no `ROLLBACK TO SAVEPOINT`. The first error (or `Cancelled`) goes up.
- The end of the callback:
  - Before the end, the client calls `Fiber.check_cancel ()`. If the fiber was cancelled, the client closes the connection without `COMMIT` (the server rolls back an idle session when the connection closes), and `Cancelled` goes up. A statement in flight goes to the killer.
  - It returns after the transaction deadline: `ROLLBACK`, and `Timeout`.
  - It returns, and the `Tx` did not fail: `COMMIT`.
  - It returns, and the `Tx` failed: `ROLLBACK` if the connection is open, and the first error.
  - It raises `Cistern_error e`: `ROLLBACK`, and `Error e`.
  - It raises another exception: `ROLLBACK`, then the exception again.
- `COMMIT` and `ROLLBACK` run in `Fiber.shield` with their own deadline (see "Limits and defaults"), so the shield always has a time limit.
- Replies to `COMMIT`:
  - OK: success.
  - 1213, 1205, 1180 (the two servers), 3101 (MySQL): `Retryable `Rolled_back`.
  - An error for which `Error.interrupts` is true (1053 or 1317; 4031 on MySQL; 1927 on MariaDB) followed by the end of the stream, or no reply: `Outcome_unknown `Commit`, and the connection closes.
  - Another error: `ROLLBACK`, and `Driver`.
- The cleanup uses `match ... with exception`, not `Fun.protect`, so that a failed `ROLLBACK` does not become `Fun.Finally_raised`.

### Shared state and the query hook

- No `Hashtbl` is shared between domains. The transaction set is an `Atomic` value. The jitter uses the `Random.State.t` in `Pool_state`, under its mutex.
- `on_query` gets events with a kind (`Query`, `Connect`, `Kill`, `Kill_dropped`, `Kill_off`), the redacted SQL, the duration, the number of rows and the error. Cistern catches its exceptions.
  - `Query` events outside a `Tx` run after the checkin, in the fiber of the statement. In a `Tx`, they run after each statement. An event never changes the result of the statement.
  - `Connect` events run in the connect fiber. `Kill` events run in the killer, outside its shields.
- `Error.t` keeps the full server message for the program. A server message can hold a value of a row, for example 1062 "Duplicate entry 'x' for key 'email'". `Error.pp_summary` prints only the constructor, the kind of a constraint and the error number. The printer of the exception `Cistern_error`, which logging libraries call through `Printexc.to_string`, uses `Error.pp_summary`. Only an explicit `Error.pp` or `Error.to_string` gives the full message.
- `Event.pp`, and any default log output of events, prints only the kind of the event, the duration, the number of rows, the address, the redacted SQL and `Error.pp_summary`. Thus no server message and no value goes to the logs (decision Q9 of phase 0). The producer of a `Query` event puts only redacted SQL in it: each literal is `?`. A query has the outcome `Done` (with the rows), `Failed` (with the error) or `Cancelled` (with a flag: was the statement sent).
- The events of `KILL` (`Kill`, `Kill_dropped`, `Kill_off`) come with the killer in phase 1.5. Their reasons are variants, not text, so that an event holds no free text. A `KILL` that gives 1094 (the query is already gone) is a success.
- No stdlib I/O: Cistern does not call `Unix`, `open_in` or `Sys.time`. The guard `test/nounix` links the five libraries of the package `cistern` (not `ppx_cistern`, which runs in the compiler) with the list of caravan's `test/nounix/dune`: `(forbidden_libraries unix threads.posix weft.posix weft.sim weft.os)`.

## Tests (windtrap)

- Unit tests: the URL parser and the `ssl-mode` defaults, the schema checks, the SQL compiler for the two dialects (also `IS NULL`, `<=>` on `option` fields, `in_` with `None`, `in_ []`, `LIKE` escaping, `count` with `limit`, `Query.where`), the literals (also `Decimal.of_string` refuses SQL text), the first-word check of raw statements in a `Tx`, the error mapping, the two authentication scrambles with known values, and the parsers of recorded server rows (`@@version` strings, `information_schema` rows), so that the mutation gate reaches this code.
- Property tests (`prop`): round trips of each type through literals and the text decoders (also `Int64.min_int` and `-0.0`), length-encoded integers, any OCaml string gives a literal that the test parser reads back without a change (also with `'`, `\` and NUL).
- Stateful tests: a sequential `stateful` test of `Pool_state` against a model (no connection goes to two holders, no slot is lost, `Killing` slots come back after their age limit through `tick`, `killed` with an old token frees nothing, `next_timer` is never later than the next change). The races of the weft shell are tested in the simulator.
- Expect tests (`ppx_windtrap`): the generated code and the errors of the ppx (with `ppxlib` and `Pprintast`), a file with two deriving groups, the refused field names, and the SQL of each statement for each dialect (`Stmt.to_sql`).
- Type-error test: a dune rule compiles a file that uses a field of another model in a filter, with `(with-accepted-exit-codes 2 ...)`, and windtrap `expect_file` checks the compiler message. No cram test, so that all tests are windtrap tests.
- README test: a dune build rule compiles the examples of `README.md`, as in caravan.
- Mutation tests: `make mutate` as in weft, on the suites unit, prop, stateful, expect and sim. Each phase adds its suites to `SUITES` of the Makefile. `test/ocaml55` stays in `SUITES` as the check of the mutation backend. The integration suites are not in the mutation runs. The rule is the rule of weft (no surviving mutant on changed lines), with `[@mutate off]` and a reason where a mutant cannot be killed.
  - The gate gives the output of `git diff -U0` to windtrap (`--diff`). On a local machine, the base is the merge base with `origin/main`. In CI, the base is `HEAD^1`: the checkout of a pull request is a merge commit, and its first parent is the tip of the base branch at the time of the event.
  - Unlike weft, a mutant on a changed line that no test reaches fails the gate. Code that no suite can reach gets `[@mutate off]` with a reason.
  - Each library with the mutation backend and a `.ml` file is in the `libraries` of a test stanza of a suite of `SUITES`, and each such test stanza links with `-linkall`. `make check-scope` examines this, so that the catalogue of the suites has each mutation site. `ppx_cistern` has no mutation backend until phase 3.
  - Before the mutation run, the suites run with no mutant armed, and a failure stops the gate. In the mutation run, each suite writes a verdict file or tells that no mutant is in its scope. A suite that does neither stopped early, and the gate fails.
- The integration suites run under their own dune alias, not under `@runtest`. Thus `dune runtest` and the `checks` of the flake never need a database server.
- Simulator tests (`Weft_sim`):
  - The weft shell of the pool: 1000 fibers, 4 virtual domains, 8 connections, timeouts and cancels at the same time as checkins; a connect fiber that raises does not stop `with_db`.
  - Deadlines with no real time, and deadlines that do not stay on a reused flow.
  - Cancel: the `KILL` goes to the control connection with the right id; a changed identity drops it; the slot is free after its age limit, and a waiter gets it without another operation; a control connection that the server closed (4031) is opened again, and the `KILL` is sent; an outer cancel of `with_db` still sends the queued `KILL`s; `with_db` ends when the killer is cancelled during a `sync`.
  - Transactions: a deadlock rollback followed by a `Tx.exec` gives the first error, and no write runs. A raw `TRUNCATE` in a `Tx` gives `Implicit_commit`. A cancel after an OK of `COMMIT` does not change the result.
  - Security: under `REQUIRED`, a fake server answers `0x01 0x04`, and the client does not send the password. Bytes after the SSL request make the client fail.
  - Session reset: a raw `SET sql_mode = ''` and a raw `USE other`, then checkin; the checkin does no I/O; the next checkout resets the session, and the statement sees the complete `sql_mode` and the configured database. A reset that fails makes the checkout take another connection.
  - Protocol faults: a peer fiber plays a transcript that a test packet encoder makes, so a change of the protocol code needs no new recording. Version 1 has no recorder of real server traffic; the integration tests cover the real servers. The test injects resets, short reads, latency, and an error 4031 on an idle connection.
  - TLS: `Weft_tls.server` with test certificates from `test/certgen`.
- Integration tests (`Weft_os`): `suite : (module S : SERVER) -> Windtrap.test` runs the tests through the typed API on each server. The `Tx.exec` runner runs on MySQL 8.4 and MariaDB 11.8.
- Verification of claims (phase 1.1), on the real servers: the MariaDB default plugins; the implicit commit of `START TRANSACTION` in a transaction; `COM_RESET_CONNECTION` and the database; `SERVER_STATUS_IN_TRANS` in the OK of `COM_PING` on MariaDB; the exact start time from one identity `SELECT` on the two servers; 1053 and 1927 after an autocommit write; the greeting bit `CLIENT_MYSQL` (0 on MariaDB) as a cross-check of `Dialect.of_version`; the text forms of `DOUBLE` that the decoders read (the `my_gcvt` model of `test/prop/server_text.ml`); the literals `-0e0` (the sign stays), `5e-324` and `-9223372036854775808` on a `DOUBLE` and a `BIGINT` column; if a `utf8mb4` column can store an encoded surrogate (`ED A0 80`), which `Decoder.string` refuses; the errors 1874 (`--innodb-read-only`), 1836 (`ER_READ_ONLY_MODE`), 1265 and 1365 (in strict mode) on the two servers, 3101 on MySQL, and 1205 for `NOWAIT` on MariaDB; the errors 1052, 1169, 1859, 1761 and 1762 on the two servers, and 3673 on MySQL. 1557 (`ER_FOREIGN_DUPLICATE_KEY_OLD_UNUSED` on MySQL, `ER_UNUSED_4` on MariaDB) has no row: the servers do not send it. The grammar of MariaDB 11.4.13 and 11.8.8 (`sql/sql_yacc.yy`) has no `FOR SHARE`, thus Cistern uses `LOCK IN SHARE MODE` on MariaDB. A review ran the pinned servers for the rules of names (phase 0b-3): a name of 65 characters gives 1059 on MySQL and 1103 on MariaDB (a table); a 4-byte character gives 1300 on MariaDB, and MySQL stores `a?` (a second `a😀` then gives 1060); 64 × `€` is a valid column name but not a valid table name (the InnoDB file name); MySQL `CHECK` constraint names are unique in the schema (version 1 has no `CHECK`). Phase 1.1 tests these errors again. It also tests that the collations do not pad: `SELECT 'a ' = 'a' COLLATE utf8mb4_0900_bin` (MySQL) and `COLLATE utf8mb4_nopad_bin` (MariaDB) give 0. MariaDB has no `PAD_ATTRIBUTE` column in `information_schema.COLLATIONS` (1054). Round 5 verified in the source code: error numbers 4025 and 1969, the read rights for `performance_schema.global_status`, and `COM_PING` on MySQL. The tests check these too.

## Prerequisite PRs

These PRs move or add generic transport code in weft, so that any protocol on weft can use it.

| PR | Content | Depends on |
|---|---|---|
| weft PR 1a | `Flow`, `Tcp`, `Deadline` from caravan into the library `weft.flow`. `Tcp.Listener` takes a constant backlog instead of caravan's `Config`. `Tcp.connect` gets a keep-alive option. caravan's `Cancel` stays internal to `weft.flow` (weft already has `Fiber.check_cancel`). The flow-level tests move from caravan onto weft's simulator helpers. | — |
| weft PR 1b | A new binary `Weft_flow.Reader` and `Weft_flow.Writer`: `ensure`, integer reads and writes (`int8` to `int64`, little-endian and big-endian, 3-byte lengths), views at offsets, `consume`, frames with a length field that is filled after the body. Its two users (the MySQL packet codec, and caravan's WebSocket frame headers with 16- and 64-bit big-endian lengths, `caravan/websocket/frame.ml`) are outside weft. Thus the PR has property tests with a model of the two framings. The owner merged it on 2026-11-06 (ocaml-caravanserai/weft#56), before phase 1.1. | 1a |
| weft PR 1c | The TLS flow (`client`, `server`) in a separate package `weft-tls`, with `weft-tls.opam`, the flake output `lib.tlsPackage`, and `tls`, `x509`, `mirage-crypto-rng` and `mirage-crypto-ec` in weft's dev shell. It includes caravan's `tls_support` (certificates made in code). The `https` and engine tests stay in caravan. Merged on 2026-11-06 (ocaml-caravanserai/weft#58). | 1a |
| caravan PR | caravan uses `weft.flow` and `weft-tls`, and moves its WebSocket frames onto the binary `Reader` and `Writer`. caravan keeps its HTTP `Buf_reader` and `Buf_writer`. Done on 2026-11-06 (ocaml-caravanserai/caravan#36): caravan main uses `weft.flow` and `weft-tls`. | 1a, 1b, 1c; not on the critical path of Cistern |

PRs 1b and 1c can run in parallel. Each weft PR includes the work that the weft rules require:

- The mutation gate on all new and moved lines. The new suites go into `SUITES` of weft's `Makefile` (at this time `test/core test/sim`). The flow library does not go into `LINUX_LIBS`, because that variable is only for code that only `test/linux` reaches, and the flow tests run on the simulator.
- The new library in `test/nounix`.
- A section in `docs/design.md`.
- PR 1c: the library is `(optional)`, and its tests have `(enabled_if %{lib-available:tls})`. The FreeBSD and Windows jobs of weft's `.github/workflows/ci.yml` run `dune build` and `dune runtest --force` with no package filter, so `(package weft-tls)` alone does not stop them from building the tests (verified in round 5). Those runners do not install `tls`. The TLS session uses two `Fiber.Mutex` locks and adds no new shared protocol. Thus, as for `Var` (weft `docs/design.md`, "Why no TLA+ model"), a note and a `dscheck` test replace a TLA+ model.

## Phases of implementation

The phase steps have the names 1.1 to 1.5, so that they are not confused with the weft PRs 1a to 1c.

0. Foundations: weft PRs 1a and 1b (merged); the flake on weft main, with the server packages, `test/certgen` and the `Makefile` targets; `cistern.core` (Value, Decimal, Dialect, literals, Schema, Decoder, Error, Event); a check that `ppx_windtrap.mutate` works on 5.5 syntax. No reference to `lib.tlsPackage`. `MODEL` moves to phase 2, because its first users are there.
1. A usable raw client, in five steps. Each step ends with tests that pass.
   - 1.1: One connection, no pool. TCP and Unix socket without TLS; `mysql_native_password`; the `caching_sha2_password` fast path, and the full path on a Unix socket with `allow-cleartext=socket`; the session setup; `Raw` on one connection; the error mapping. The tests of the "not verified" claims on MySQL 8.4 and MariaDB 11.8. Result: the claims are checked early.
   - 1.2: `Pool_state`, the weft shell, the opener, the deadlines, the session reset at checkout, `Db.with_db`, `Db.exec`, the events. Cancel with the `kill=false` behavior. Simulator tests.
   - 1.3: `Db.transaction`, `Tx.exec` with the error rules, savepoints. Result: a raw SQL client with a pool and transactions. MySQL 8.4 only on a Unix socket (with `DISABLED` over TCP, the full path fails after each restart); MariaDB also on plain TCP.
   - 1.4: TLS with `ssl-mode`, `ssl-ca`, `ssl-fingerprint` and the authenticator; the full path over verified TLS; MariaDB 11.4. Needs PR 1c. The flake adds `weft-tls`.
   - 1.5: Cancel with `KILL`: `Killing` slots with tokens, `next_timer`, the killer, its control connection, the identity step of the session, the drain.
2. `cistern.sql`, `module type MODEL`, and the typed API with one hand-written model module (an internal step for the tests, not a result for users): get, find, create, update, delete, `all`, `where`, `Query.where`, order, limit, offset, `to_list`, `first`, count, exists, for_update, for_share, `IS NULL`, `in_`, `starts_with`, `contains`, bulk `update` and `delete`; `push_schema` and `check_schema`.
3. `ppx_cistern` for models without relations. Result: a usable ORM for single tables.
4. Relations in Toasty style (three stages, `Deferred.t`, `include_`, nested create of one level).
5. Hardening and documentation: the README and odoc, the limits, the security notes.

## Later versions: PostgreSQL notes

The reviews of drafts 4 and 5 produced this PostgreSQL design. Keep it for the later PostgreSQL driver:

- Startup with `client_encoding=UTF8`, `DateStyle=ISO`, `TimeZone=UTC`.
- `SSLRequest`: read exactly 1 byte for the answer, and fail if the reader has more bytes (CVE-2021-23222).
- SCRAM-SHA-256 without channel binding, with a cache of `ClientKey` and `ServerKey`. Test with the RFC 7677 vector.
- Extended protocol with named statements (`s_<hash of the SQL>`): Parse with explicit parameter OIDs, Describe, Bind with binary results, Execute and Sync in one write. Cache a statement only after `ParseComplete`. PgBouncer 1.21 or later, or `?prepared:false`.
- After an `ErrorResponse`, discard messages until `ReadyForQuery`. Read its transaction status byte (`E` means a failed transaction).
- Nested create in one statement: `unnest` of array parameters `WITH ORDINALITY`, with the parent row through a `LEFT JOIN`.
- `include_` with `WHERE fk = ANY($1)` and one array parameter.
- Cancel with `CancelRequest` on a new socket and the `BackendKeyData`.
- Error codes: 23505, 23503, 23502, 23514, 40P01, 40001, 55P03, 57014.
- The `CONN` interface and `Driver.t` come back with this driver.

## Review history

| Round | Finding | Fix |
|---|---|---|
| 1, 2 | `module rec` refuses the cycle of the generated modules | Generated code in stages |
| 1, 2 | A weft `Chan` holds no connection; a slot leaks on a failed open | `Pool_state` and the opener daemon |
| 1, 2 | A checkin can fill the Ivar of a waiter that timed out | `Ivar.try_fill` with `Abandoned` |
| 1, 2 | An effect under the pool lock breaks a weft rule | Pure `Pool_state`; effects outside the mutex |
| 1, 2 | `&&.` and `||.` parse with the precedence of `==.` | `&&`, `||`, `not` over expressions inside `Q` |
| 1, 2 | Parallel operations need many connections | Operations in order on one connection |
| 1, 2 | `SELECT *` with position decoders | Explicit column lists |
| 1, 2 | A nested create in a MySQL transaction commits it | `SAVEPOINT` in a `Tx` |
| 1, 2 | A shared `Db.t` in a transaction lets two fibers use one connection | `Tx.t` with a fiber check |
| 3 | Stage 3 redefines a module name | A namespace module |
| 3 | A clear-text password goes to an attacker under unverified TLS | Full path only to a verified server |
| 3 | Escaping depends on `NO_BACKSLASH_ESCAPES` | Hex literals |
| 3 | `KILL` can stop a connection of another client | Identity check, `kill=false` |
| 3 | Session state leaks between users of a connection | Session reset at checkin |
| 3 | The dialect comes from the plaintext greeting | The dialect comes from `@@version` |
| 3 | `mysql80` and `x86_64-darwin` removed from nixpkgs | MySQL 8.4, MariaDB 11.4 and 11.8; three systems |
| 4 | The session reset ran the identity `SELECT` instead of the `SET` | Reset, then the `SET`, then `COM_INIT_DB` |
| 4 | `@@server_uuid` stays the same after a MySQL restart; no common table for `Uptime`; the greeting chose a query | Start time with a tolerance in every identity; a third round trip chosen by the dialect; a `PROCESSLIST` check before `KILL` |
| 4 | A failed `Tx` could commit later writes in autocommit mode | A failed `Tx` refuses statements; `COM_PING` after unknown errors; `IN_TRANS` on every OK |
| 4 | An error reply to `COMMIT` does not always mean a rollback | A table of `COMMIT` replies |
| 4 | A sent write that timed out gave `Timeout` | `Outcome_unknown` for every sent write without a reply |
| 4 | `VERIFY_CA` and sockets in `/tmp` can leak the clear-text password | Clear text by default only under `VERIFY_IDENTITY` or a safe socket |
| 4 | Session tracking gives false resets | No session tracking in version 1 |
| 4 | Raw SQL parameters needed a lexer | `Raw` fragments |
| 4 | Index on `MEDIUMBLOB`; the 16 MiB limit was wrong; `DEFAULT` on `MEDIUMTEXT` | `VARBINARY(255)` for indexed bytes; the limits table; no `DEFAULT` clauses |
| 4 | The killer's drain was lost on an outer cancel; serial kills could block the pool | Drain in the `with_db` fiber; `Killing` age limit; queue rules |
| 4 | A connect fiber that raised failed `with_db` | `fork_isolated`, catch all, doorbell at the end |
| 4 | `COMMIT` in a shield without a time limit; a cancelled `Tx` could commit | Deadline from 5 s to 30 s; `check_cancel` before `COMMIT` |
| 4 | `Query` combinations gave invalid SQL; `in_ []`; `offset` without `limit`; no `IS NULL` or string search | Rules in the `Query` section |
| 4 | Generated code hygiene and the children step | `Cistern.Private` paths, top-level model modules, refused names, `Model.child` with setters |
| 4 | The template map added complexity with no gain | No SQL cache in version 1 |
| 4 | Servers in every dev shell; `db-up` flags; no tool for the test CA | Server packages, explicit flags, `test/certgen` |
| 4 | weft PR 1b had no user in weft | MySQL and caravan WebSocket codecs |
| 4 | Phase 1a transactions could run nothing | `Raw` runs through `Db.exec` and `Tx.exec` |
| 4 | No default timeout; the ping conflicted with `wait_timeout` | Default timeout 30 s; close idle connections; keep-alive |
| 5 | The session reset at checkin did I/O after the result, with no deadline; a cancel there turned a committed `Tx` into an error | Mark the connection dirty; reset at the next checkout under the caller's deadline |
| 5 | A catch-all loop in the killer spins after a cancel, so `with_db` never ends | Daemons catch all exceptions except `Cancelled` |
| 5 | `Value.Decimal of string` let SQL text into `Raw.v` | Abstract `Decimal.t` with a checked constructor |
| 5 | A cancel gave `Outcome_unknown` in one rule and `Cancelled` in another | A cancel always raises `Cancelled`; the event records the outcome |
| 5 | The `Killing` age limit (5 s) was shorter than one connect; no timer freed the slot; `killed` was not idempotent | Age limit 30 s; `next_timer` and `tick`; tokens |
| 5 | The idle control connection went stale, and the next cancel dropped all entries | Idle limit and `read_now` before each entry; one retry on a new connection |
| 5 | The drain used an `Ivar` and a stop step that an outer cancel skipped | Drain after `Fiber.scope` returns |
| 5 | Start time across two statements with ±2 s; two `SELECT`s in one round trip; `CURRENT_USER()` is not the `PROCESSLIST` user | One identity `SELECT` with an exact start time; no `PROCESSLIST` check |
| 5 | The `/tmp` rule was easy to get around (macOS `/private/tmp`, `/dev/shm`) | `allow-cleartext=socket` for every socket |
| 5 | MySQL's own certificate has no host name, so the default `VERIFY_IDENTITY` refuses it | `ssl-fingerprint` |
| 5 | An implicit commit (raw DDL, `START TRANSACTION`) in a `Tx` was not reported correctly | `Implicit_commit`; first-word check of raw statements in a `Tx` |
| 5 | `option` filters did not follow OCaml equality for `NULL` | `<=>`, `in_` with `None`, refuse order comparisons with `None` |
| 5 | `Model.child` does not type for `references` to a unique field (compiled); `User.draft ~todos` dropped children; setter names could clash | `references` only to the key; no relations in drafts; setters in submodules |
| 5 | 1053 and 1927 on an autocommit write gave `Connection_lost`; 1153 closes the connection; `COM_INIT_DB` without a database | One `Outcome_unknown` rule; 1153 closes and fails the `Tx`; the database is necessary |
| 5 | Phase 1a needed the killer and PR 1c; the flake referred to an output that does not exist; one input cannot follow two branches | Phases 1.1 to 1.5; `kill=false` until 1.5; TLS in 1.4; one integration branch |
| 5 | weft CI builds all packages, so `(package weft-tls)` does not protect the FreeBSD and Windows jobs | `(enabled_if %{lib-available:tls})` |
| 5 | No transaction timeout default; no checkout deadline rule; no filter on a relation scope; no ppx flake output; test servers on all interfaces | Rows in the limits table; `Query.where`; `lib.ppxPackage`; `--bind-address=127.0.0.1` |

## Status

Draft 12, 2026-11-06. It fixes the findings of review round 5 on draft 11. You approved it on 2026-11-06. Phase 0 started on 2026-11-06: weft PR 1a (ocaml-caravanserai/weft#55) and weft PR 1b (ocaml-caravanserai/weft#56) are merged. The owner merged PR 1b before phase 1.1, so the flake uses weft main. weft PR 1c (`weft-tls`) is merged too (ocaml-caravanserai/weft#58, 6f05d17), and weft main has `lib.tlsPackage`. Cistern takes it in phase 1.4. The caravan PR is done (ocaml-caravanserai/caravan#36, 2026-11-06): caravan main uses `weft.flow` and `weft-tls`. Thus all four prerequisite PRs are done: weft #55, #56 and #58, and caravan #36.

Round 5 found 3 critical defects. Two of them were in fixes of round 4 (the session reset and the killer drain), and one was a gap from an earlier draft (`Decimal`). Most of the critical and major findings are in the cancel path with `KILL`. Thus phases 1.1 to 1.4 ship without `KILL` (`kill=false`), and phase 1.5 adds it. The remaining risk is in server behavior that only a real server can show (the "not verified" claims). Phase 1.1 tests these claims first.
