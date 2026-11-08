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
| MariaDB | 11.4 (LTS) | `mysql_native_password` |
| MariaDB | 11.8 (LTS) | `mysql_native_password` |

The greeting of each server names its default plugin, and `CREATE USER ... IDENTIFIED BY` gives the same plugin (verified on MySQL 8.4.11, MariaDB 11.4.13 and 11.8.8 on 2026-11-07).

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

All numbers of version 1 are in this table. The other sections refer to it. The module `Cistern_core.Limits` holds each number of this table that the code uses as a constant (also the guard of `COMMIT` and the limit of the polite closes of the 52 s row), and the code takes it from there (phase 5a). `Cistern_names` cannot depend on `cistern.core`: a unit row ties its cut of index names to `identifier_length`. The page `doc/limits.mld` is the copy for the user, with no reasons. A unit row (`test/unit/test_limits.ml`) compares each named row of that page with `Cistern_core.Limits`.

| Item | Value |
|---|---|
| `pool_size` | 10 |
| `checkout_timeout` | 30 s, gives `Pool_exhausted`. A checkout in `exec` waits until the earlier of this limit and the `exec` deadline. The `exec` deadline gives `Timeout`. |
| Default `?timeout` of `Db.exec` | 30 s, gives `Timeout` or `Outcome_unknown` |
| Default `?timeout` of `Db.transaction` | 30 s for the whole transaction. `Tx.exec` with no `?timeout` uses the transaction deadline. |
| Margin: a statement with less remaining time is not sent | 50 ms (not for `COMMIT` and `ROLLBACK`) |
| `COMMIT` and `ROLLBACK` deadline | The remaining time, at least 5 s and at most 30 s |
| `max_lifetime` | 30 min, with ±15% jitter |
| Idle limit: an idle connection older than this is closed at checkout | The smaller of 30 s and `@@wait_timeout` − 5 s, at least 1 s. With `wait_timeout = 1`, the limit equals the server timeout, and the `read_now` check catches a close |
| Connect timeout (pool and control connection) | 10 s |
| Concurrent connects | 2 |
| Connect backoff | 100 ms to 5 s |
| Kill queue | `pool_size` entries |
| One `KILL` | 2 s |
| Age limit of a `Killing` slot | 30 s (more than one connect plus `pool_size` kills), then the slot is free |
| Kill drain at the end of `with_db` | 15 s (one connect plus a few kills) |
| Sessions of one pool on the server | At most `pool_size` + 1: the control connection of the killer is outside the slots. Each `push_schema` that runs adds one more session for its time (phase 2e). A `Killing` slot that its age limit frees while its session still runs can add more for a short time (the plan of 1.5, D14) |
| Longest wait of an outer cancel of `with_db` | About 52 s: a `COMMIT` in its shield (30 s, and the guard 1 s later), a shielded try of the killer in the join of the scope (2 s), the drain (15 s, and at most 2 s more for a try that runs at its limit), and the polite closes (2 s). Documented. |
| Keys in one `IN (...)` | 1000 |
| Identifier length | 64 characters |
| Statement size (write) | The payload of `COM_QUERY` (1 byte and the SQL text) must be less than the smaller of `0xFFFFFF` and `@@max_allowed_packet` (read at connect), because the server refuses a payload equal to `max_allowed_packet` (verified on the pinned servers). Thus the SQL text has at most the smaller of the two, minus 2 bytes. The client checks the length before it sends (`Packet.query_fits`). A hex literal doubles the size of a value, so one value can have at most about 8 MiB. |
| Row size (read) | At most `0xFFFFFE` bytes for each packet. A packet with the length `0xFFFFFF` (16 MiB or more) gives `Decode`, and the connection breaks |

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
- The OCaml scope is `ocaml-ng.ocamlPackages_5_5.overrideScope` with `weft = callPackage weft.lib.package {}`, `windtrap = callPackage windtrap.lib.windtrap {}`, `ppx_windtrap = callPackage windtrap.lib.ppx_windtrap {}`. This is the pattern of `~/projects/caravan/flake.nix`. Phase 1.4 (TLS) adds `weft-tls = callPackage weft.lib.tlsPackage {}`, the output of weft PR 1c (weft main has `lib = { inherit package tlsPackage; }`), with its first user (PR 1.4b1, the TLS flow of `Connection`). PR 1.4a adds `tls`, `x509`, `domain-name` and `ipaddr`, which `Config` and `Tls_setup` use.
- The `weft` input is the main branch of weft. weft PRs 1a and 1b are merged, and `flake.lock` pins one commit. If the codec of phase 1.1 needs a change of `Reader` or `Writer`, a normal weft PR makes it, and an update of `flake.lock` takes it. weft PR 1c is merged. The pinned commit (8520b5f, weft main on 2026-11-07) has `lib.tlsPackage`, thus phase 1.4 needs no update of `flake.lock`.
- Systems: `x86_64-linux`, `aarch64-linux`, `aarch64-darwin`. nixpkgs dropped `x86_64-darwin`.
- OCaml libraries from nixpkgs: `ppxlib`, `digestif` (SHA-1 and SHA-256), `ptime`, `tls`, `x509`, `domain-name`, `ipaddr` (phase 1.4a), `mirage-crypto-rng` (phase 1.4b1) and `fmt`. The tests also use `mirage-crypto-ec` and `mirage-crypto-rng` (test certificates).
- Outputs: `lib.package`, `lib.ppxPackage`, `overlays.default`, `packages.<system>.cistern`, `packages.<system>.ppx_cistern`, `devShells.default`, `checks` (build and tests that need no database server), and `packages.<system>.server-mysql84`, `server-mariadb114` and `server-mariadb118` (the nixpkgs servers `mysql84` 8.4.11, `mariadb_114` 11.4.13 and `mariadb_118` 11.8.8, verified on 2026-11-06). The servers are not in the dev shell, so a `nix develop` does not download them. The server outputs come in phase 0 with `test/certgen` and the `db-up` targets, not with the first flake.
- The dev shell uses `inputsFrom = [ cistern ppx_cistern ]`, so it has every library dependency of the two packages. It adds OCaml 5.5, dune, findlib, `windtrap`, `ppx_windtrap`, `ocaml-lsp`, `odoc`, and `ocamlformat_0_29_0` (pinned, as in weft).
- Each PR adds a dependency together with its first user. For example, `ptime` comes with `Value`, and `x509` and `mirage-crypto-ec` come with `test/certgen`. At the end of version 1, the packages depend on `weft`, `ppxlib`, `digestif`, `mirage-crypto-rng`, `ptime`, `fmt`, and (from phase 1.4) `weft-tls`, `tls`, `x509`, `domain-name` and `ipaddr`. The dev shell adds `mirage-crypto-ec` for the tests.
- `flake.lock` pins nixpkgs, weft and windtrap. The server versions follow nixpkgs.

### Database servers for the tests

`make db-up SERVER=<name>`:

1. Gets the server with `nix build --no-link --print-out-paths '.#server-<name>^out'`. The flake has `packages.<system>.server-mysql84`, `server-mariadb114` and `server-mariadb118`. They are not in the dev shell, and `nix flake check` does not build them.
2. Makes a test CA and a server certificate with `test/certgen`, a small OCaml program that uses `x509` and `mirage-crypto-ec` (the method of weft's `test/tls/tls_support.ml`). No `openssl` is necessary. The keys are ECDSA P-256: the three servers accept them, and use TLS 1.3. The server certificate has the names `localhost` and `127.0.0.1`, and is valid from 2020 to 2040. `certgen` also writes `server-cert.sha256` (the SHA-256 of the DER of the server certificate, in lowercase hex: the value of `ssl-fingerprint`) and `other-ca.pem` (a second CA with no certificate in use, for the tests of a CA of another PKI). A directory of an older `certgen` with no such file is made again. `test/certgen` is test code: it is not in the package, and the mutation gate does not examine it. `dune runtest` runs its test.
3. Initializes a new data directory in `.db/<name>` on each `db-up`. The data is not in `_build`, because `dune clean` deletes `_build`.
   - `DB_DIR` changes `.db`. `db-up` removes or uses a directory only if it has the file `.cistern-db`, which `db.sh` writes. Else `db-up` stops.
   - The directories `.db/<name>` and `.db/certs` have the mode 700. Thus only the owner can use the socket of root. In `.db/certs`, `ca.pem` and `server-cert.pem` have the mode 644, and `server-key.pem` has the mode 600.
   - The path of the Unix socket has at most 107 bytes on Linux (`sun_path` has 108 bytes) and 103 bytes on macOS (`sun_path` has 104 bytes). A longer path stops `db-up` before the start.
   - The ports are fixed: 13384 (`mysql84`), 13414 (`mariadb114`) and 13418 (`mariadb118`). `DB_PORT` changes the port.
   - `DB_SERVER_ARGS` adds server options, for example `--max-connections=50`. On MySQL 8.4, `--innodb-read-only` makes `db-up` fail with error 1836, because `db-up` makes the test user.
   - The script `test/db/db.sh` is POSIX `sh`, for Linux (dash) and macOS.
   - MySQL: `mysqld --no-defaults --basedir=<store path> --datadir=... --initialize-insecure`. Then the server starts with `--no-defaults --bind-address=127.0.0.1 --skip-name-resolve --port=<DB_PORT> --socket=... --pid-file=... --log-error=... --secure-file-priv=... --mysqlx=OFF --ssl-ca=... --ssl-cert=... --ssl-key=...`, then `--user=root` if the user is root, then `DB_SERVER_ARGS`.
   - MariaDB: `mariadb-install-db --no-defaults --basedir=<store path> --datadir=... --auth-root-authentication-method=normal --skip-test-db`. Then `mariadbd` starts with the same options, without `--mysqlx=OFF`.
   - `--no-defaults` must be the first option. Then the server does not read `/etc/my.cnf` or `~/.my.cnf`.
   - Root has no password. Thus `--bind-address=127.0.0.1` is necessary. With `--skip-name-resolve`, the account `root@localhost` matches only the socket.
   - MariaDB also makes root at `127.0.0.1`, `::1` and the host name, with no password. `db-up` removes these accounts after the start. Then a TCP login of root gets error 1045 on the three servers.
   - The server certificate from `test/certgen` has the name `localhost` and the IP address `127.0.0.1` in its subject alternative names.
   - Each client call gets `--no-defaults` and `--protocol`. The MySQL 8.4 clients read `~/.mylogin.cnf` also with `--no-defaults`. Thus the script sets `MYSQL_TEST_LOGIN_FILE=/dev/null`.
   - The script also removes `MYSQL_PWD`, `MYSQL_HOST`, `MYSQL_TCP_PORT`, `MYSQL_UNIX_PORT` and `MYSQL_HOME`.
   - The MariaDB client asks for TLS by default, also on the socket. Thus the root route gives it `--skip-ssl`, so that a server with no TLS can start.
   - The wait for the server uses a query, not `mysqladmin ping`. `mysqladmin ping` gives status 0 also for "Access denied".
   - The errors 2002 and 2003 tell that the server is not ready. Another client error stops the wait at once, with the error and the end of the error log.
   - If the server process stops, `db-up` prints the end of the error log, for example for a busy port.
   - A failure, `SIGINT`, `SIGTERM` or `SIGHUP` after the start stops the server with `SIGTERM`.
   - The server refuses root with no `--user`. Thus the script adds `--user=root` for root (not tested).
4. Makes the test users with the client of the same store path:
   - MySQL: `cistern_sha2` with `caching_sha2_password`.
   - MariaDB: `cistern_native` with `mysql_native_password` (`IDENTIFIED VIA mysql_native_password USING PASSWORD(...)`).
   - Each user has all rights on the database `cistern_test`, and `RELOAD` for the test that empties the authentication cache.
   - The users have the host `%` and the fixed password `cistern`. The bind address keeps them local. A connection on the Unix socket also gets `<user>@%`.
5. Writes `.db/<name>/env`, with shell assignments of these values:
   - `DB_SERVER`, `DB_STORE`, `DB_VERSION` (from `@@version`) and `DB_PID`.
   - `DB_PORT`, `DB_SOCKET`, `DB_USER`, `DB_PASSWORD`, `DB_DATABASE`, `DB_CA`, `DB_FINGERPRINT` (the hex of `server-cert.sha256`), `DB_OTHER_CA` and `DB_PLUGIN`.
   - `DB_ADMIN_USER`: root on `DB_SOCKET`, with no password. These rights are necessary for some claims of phase 1.1, for example `read_only` and `SHUTDOWN`.

`mysql_native_password` is tested only on MariaDB, because MySQL 8.4 disables it by default.

`make db-check SERVER=<name>` is the smoke check of a server that runs. Each step prints `ok`, or `FAIL` with the expected and the actual result. The steps:

- The series of `@@version` (`8.4.`, `11.4.` or `11.8.`). An incorrect package fails.
- MySQL only: the paths of `caching_sha2_password`. Root runs `FLUSH PRIVILEGES` first, thus the cache is empty. Then:
  1. With no TLS, the login gives error 2061.
  2. On the socket, the full path passes.
  3. With no TLS, the fast path passes.
  4. The test user runs `FLUSH PRIVILEGES` (`RELOAD`).
  5. With no TLS, the login gives error 2061 again.
  6. With TLS (`VERIFY_IDENTITY`), the full path passes.
  7. With no TLS, the fast path passes.
- TCP with TLS and the client of the server. MySQL uses `--ssl-mode=VERIFY_IDENTITY --ssl-ca`, and MariaDB uses `--ssl-verify-server-cert --ssl-ca`. The step expects the version, `<user>@%` and `Ssl_version` `TLSv1.3`.
- The identity of the certificate, with the MySQL 8.4 client and `VERIFY_IDENTITY` on the three servers.
  - The MariaDB client checks the chain of the certificate. It does not check the name or the IP address of a connection to `127.0.0.1`.
  - The MariaDB client also connects with no CA file.
  - Probably MariaDB 11.4 and later treat a local connection as safe, or check the certificate with the password hash. We did not examine the source.
  - Thus a certificate with no `127.0.0.1` name passes the MariaDB client. Cistern's own client does the real check in phase 1.4.
- The test user on the Unix socket.
- The plugin of the test user (as root).
- No variable from a defaults file.
  - MySQL: `performance_schema.variables_info` has no `VARIABLE_SOURCE` other than `COMPILED`, `COMMAND_LINE` and `DYNAMIC`.
  - MariaDB: `information_schema.SYSTEM_VARIABLES` has no `GLOBAL_VALUE_ORIGIN` `CONFIG`.
  - A comparison of values does not find a defaults file, because the command line sets the compared values.

`make db-errors` checks the error numbers of "Error mapping" against the server packages. A running server is not necessary.

- `test/db/errors.tsv` has each pair (dialect, number) of `Error.of_server` and `Error.interrupts`, with its name. An example: `mariadb 4025 ER_CONSTRAINT_FAILED`.
- The test `test/db/error_codes` runs in `dune runtest`. It is not a suite of the mutation gate.
- The test makes sure that each row has a name, and that the file has each number of the table and no other number.
- It probes the numbers 0 to 65535 with the SQLSTATE `23000` and with `HY000`. A number outside the table gives the fallback of the SQLSTATE: `Constraint_violation Other_constraint` or `Driver`.
- A number is in the table if at least one of the two probes does not give its fallback.
- `make db-errors` compares each name with `perror` of `server-mysql84`, or of `server-mariadb114` and `server-mariadb118`.
- An empty name, and a number that `perror` does not know, are mismatches. MariaDB `perror` gives status 0 for a number that it does not know.
- Thus a nixpkgs update that changes an error number fails the workflow.

`make integration SERVER=<name>` runs the integration tests against a server that runs (phase 1.1).

`make db-down [SERVER=<name>]` stops the servers. It sends `SIGTERM` to the pid of the env file and to the pid of the server's pid file. It stops a process only if its command line has the pid file of this `DB_DIR`. After 60 s, it sends `SIGKILL` and gives status 1.

CI jobs:

- `test`, on Linux and macOS: the build, the suites that need no server, the slow tests (`make sim-slow`), and `nix flake check`. On Linux also: the format checks, the `.opam` files, the OCaml 5.5 check of the mutation backend, and on a pull request the mutation gate.
- The job `integration` of `ci.yml` runs `make db-up`, `make integration` and `make db-down` on Linux: MySQL 8.4, MariaDB 11.4 and MariaDB 11.8 on each PR and each push to `main` (MariaDB 11.4 on each PR from phase 1.4). It keeps the logs of the server if a step fails or the job is cancelled. The timing claims of phase 1.1 run only in `db.yml`.
- The workflow `db.yml` of the test servers runs every night, on `workflow_dispatch`, and on a PR that changes its files. These files are `db.yml`, `test/db/`, `test/certgen/`, `test/integration/`, `Makefile`, `flake.nix`, `flake.lock` and `core/error.ml`.
  - The job `server` runs `make db-up`, `make db-check`, `make integration`, `make integration-timing`, `make integration-shutdown` (last, because it stops the server) and `make db-down` for each server on Linux (`ubuntu-24.04`) and macOS (`macos-15`). Thus MariaDB 11.4 and macOS run the integration suite each night, and the timing claims run only there.
  - The make targets run outside the dev shell. Thus `db.sh` runs with dash on Linux, and with the `sh` and the BSD tools of macOS. Only the build of `test/certgen` uses `nix develop -c dune`.
  - If a step fails or the job is cancelled, the job keeps the logs of the server.
  - If a server package does not build on aarch64-darwin, the macOS leg of that server fails at `make db-up`.
  - The job `errors` runs `make db-errors` on Linux.
  - The job `slow` runs `make sim-slow` on Linux (no server).

### Libraries (dependencies go one way)

```
cistern/
  flake.nix, flake.lock, dune-project, Makefile, test/certgen
  names/    (cistern.names)   The rules of names, the names of indexes (only the stdlib)
  schema/   (cistern.schema)  Schema: the description of the models and its checks (only cistern.names)
  core/     (cistern.core)    Value, Dialect, literals, Schema (an alias), model AST, Decoder, Error, Event, MODEL, Limits
  sql/      (cistern.sql)     SQL AST, compiler from the model AST, serializer for each dialect, DDL
  mysql/    (cistern.mysql)   Config and URL parser, Tls_setup, connect, TLS, authentication, text protocol, sessions
  cistern/  (cistern)         Typed API, Raw, pool, killer, Tx, execution  (depends on core, sql, mysql)
  ppx/      (ppx_cistern)     The deriver (cistern.names, cistern.schema and ppxlib)
  doc/                        The pages of the doc of the user: index, limits, security (phase 5a)
  examples/ (cistern_examples) The examples of README.md, not installed (phase 5b)
  test/  unit, prop, stateful, expect, ppx, sim, integration, nounix, readme, typeerror, ocaml55,
         model (hand-written models), model_ppx (derived models), model_mli (an interface on a derived group)
```

- Two opam packages: `cistern` (the six libraries) and `ppx_cistern`. `ppx_cistern` depends on `cistern` of the same version (phase 3a), because it links `cistern.names` and `cistern.schema`. It never links `cistern.core`, so the compiler does not load weft or ptime.
- `cistern.names` has an empty `libraries` field, thus dune refuses a dependency. The ppx of phase 3 can link it with no weft and no ptime. An application links `cistern`.
- `cistern.schema` (phase 3a0) holds `Schema` with no dependency but `cistern.names`, so that the ppx runs the same `Schema.check` at compile time as `push_schema` at run time. `Cistern_core.Schema` is an alias of it, so no path changes.
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
| `[@column "x"]`, `[@@table "x"]` | Names in the database. The default table name is the type name. The rules (`Cistern_names.valid_identifier`): valid UTF-8, 1 to 64 characters (not bytes), each in U+0001 to U+FFFF (the servers keep names in utf8mb3), no backquote, and no ASCII white space at the end. A table name is also a file name: InnoDB encodes each character other than `[0-9A-Za-z_]` in 3 or 5 bytes, with a limit of 251 bytes. `Cistern_names.valid_table_name` counts 5 bytes for each such character, thus 64 × `€` is a valid column name but not a valid table name. A table name must not start with `#mysql50#`: MariaDB 11.4 refuses it (1103), and MariaDB 11.8 uses another table. A table name has no uppercase ASCII letter (phase 2a). The ppx and `Schema.check` apply these rules. |

The ppx refuses field names that clash with generated names:

- For each field (the fields are values in `Q`): the names of `Expr.OPS` (`is_null`, `in_`, `starts_with`, `contains`, `starts_with_opt`, `contains_opt`, `not`).
- For a relation field (it is also a value of the model module): `all`, `where`, `update`, `delete`, `draft`, `create`, `model`, `get_by_*`, `find_by_*`, `filter_by_*`.

Thus a plain column can have a name such as `model` or `update`. The prefix `cistern__` is also refused on a field and on a type, because the generated names use it.

The attributes are in the namespace `cistern` (phase 3a): `[@key]` and `[@cistern.key]` are one attribute. In a file with another deriver that reads a bare name (for example `[@key "x"]` of a JSON deriver), use the long forms: ppxlib gives a bare `[@key]` to each deriver that declares the name `key`, and the other deriver then refuses its payload (probe P2 of the plan of phase 3). An attribute of another namespace, such as `[@yojson.key "x"]`, goes only to its deriver.

OCaml gives an attribute after the `;` of a field to that field, not to the next field. Thus write `id : int64; [@key]` or `id : int64 [@key];`. The ppx refuses an attribute on a later line than its field and on the line of the name of the next field (`name : string;` then `[@key] code : string;`), because the user wants it on the next field. An attribute on a line of its own after a field goes to that field.

The ppx applies the ASCII fold of `Schema.check` (see "Schema, AST and SQL"). Thus two names that differ only in the case of a non-ASCII letter (`aé` and `aÉ`, which the three servers refuse with 1060), or in a letter that a server folds (`i` and the dotless `ı`, two different letters, which only MariaDB 11.8 refuses as one column), pass the ppx, and the server refuses the DDL (the probes of the plan reviews of phase 3).

The ppx examines a group in this order, and stops at the first error. Each error has the location of its cause, and its message names the type and the field (`Cistern_names.quote`):

1. The shape: each type of the group is a record type with no parameter, not private and with no manifest.
2. The attributes of each type: only `[@@table "x"]`.
3. The type names. A type name starts with an ASCII lowercase letter and has only ASCII characters. A type name with an uppercase letter needs `[@@table]`, and the message gives the lowercase name. The ppx refuses the prefix `cistern__`, the name `cistern` (its module `Cistern` would hide the library), each name that starts with `cistern` (the prefix of the generated modules), each name with `__` (the separator of the generated names), `stdlib`, `ptime` (the module `Ptime` would change the spelling `Ptime.t`), `unit`, and the names `int64`, `int`, `string`, `bool`, `float`, `bytes` and `option`, because such a type changes the field types of the rest of the file.
4. The attributes of each field and of its type:
   - A bare name near a Cistern name gets "did you mean". The distance is the optimal string alignment distance (a transposition counts as 1), and the limit is 1 for `key` and `auto` (4 letters or fewer) and 2 for the longer names. Thus `[@kye]`, `[@keys]` and `[@atuo]` are errors, and short names of other derivers, such as `[@gen]`, `[@sep]`, `[@hex]` and `[@at]`, pass. The spellings of other ORMs also get "did you mean": `[@col]`, `[@unique_key]`, `[@unique_index]`, `[@primary_key]`, `[@primary]`, `[@pk]`, `[@auto_increment]` and `[@autoincrement]`. The check never examines a name with a namespace (`[@x.key]` passes), but each unknown `[@cistern.x]` is an error.
   - A Cistern attribute in the type of a field, as in `(string [@unique])`, is an error. So is a field attribute on a type, and `[@@table]` on a field.
   - `[@key]`, `[@auto]`, `[@unique]` and `[@index]` take no payload. `[@column]` and `[@@table]` take one string, and `[@default]` takes one expression.
   - An attribute two times on one node is an error, also `[@key] [@cistern.key]`.
   - The payload of `[@has_many]` and `[@belongs_to]`: see 6.
5. The fields: no name of `Expr.OPS` and no prefix `cistern__`, no `mutable` field (`Field.set` gives a copy), and a field type of the table that follows, by its syntax only. Thus a type alias of the program, `Int64.t`, `int list` and `int option option` are errors, and so is a polymorphic type. A field with a type of the group or a `Deferred.t` type and no relation attribute is an error.
6. The relation fields (phase 4f, the plan of phase 4, E12): a field with `[@has_many]` or `[@belongs_to]` is not a column. `[@has_many]` takes no payload, on the type `<t> list Deferred.t`. `[@belongs_to]` takes the record `{ key = <field>; references = <field> }` (each label one time, no other label, no `with`, each value a field name), on the type `<t> Deferred.t`, or `<t> option Deferred.t` exactly when the key is an `option`. `<t>` is a type of the group, and `Deferred.t` or `Cistern.Deferred.t` is accepted by its syntax only. A relation field has no other Cistern attribute (`[@key]`, `[@auto]`, `[@unique]`, `[@index]`, `[@column]`, `[@default]`), one relation attribute, and a name that is not a value of the model module (the list above). `Desc.schema` gives the relations to `Schema.faults`: a fault of a relation goes to the relation field, or for a belongs_to to its attribute (the payload names the key and the referenced field), E15. Rule 11 can name one type two times: two belongs_to of one type on two long key columns can give one foreign key name, because the hash of a cut name can collide (for example the columns `c`×57 + `mwjz` and `c`×57 + `tdimd`, with `[@unique]` on one of them so that rule 10 does not come first). That fault goes to the attribute of the second belongs_to of the two. Phase 4g generates the code of relations (see "Generated code"). `[@default]` on a key is an error: each row needs its own key, and the server gives the value of an `[@auto]` key.
7. `Schema.faults` of the `Schema.model` list of the group, which one function of the ppx builds. A fault of one field goes to the field, and a fault of two or more fields (with all the fields in the message) to the second of them, the first that makes the conflict. A fault of a field goes to its `[@column]` attribute if the column name is the cause. A fault of the table name goes to the `[@@table]` attribute, else to the type. Another fault of a model goes to the type. A fault of the list (two types with one name, or two equal tables) goes to the second type of its models (`Schema.fault.models`), or to the group if no such type exists. A name that is too long and that comes from the OCaml name gets the hint `[@column "..."]` or `[@@table "..."]`.

The ppx raises a located error (`Location.raise_errorf`), and the driver of ppxlib turns it into an error node, so that Merlin and the LSP also show the error. Probe P4 of the plan of phase 3 shows that the compiler gives only the first error node of a file, so error nodes in place of the exception give no more errors in one compile. Thus the user sees the first error only.

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

1. First `module Cistern_fields__user = struct module User = struct ... end module Todo = struct ... end end`: the `Schema.model` and the typed fields (`Field.make`) of each type (phase 2d1). It comes first because the drafts module hides the record types with the draft types of the same names, and the fields need the record types. Then `module Cistern_drafts__user = struct type user = {...} and todo = {...} ... end`, and the model modules at top level, one for each type, in declaration order: `module Cistern_model__user : MODEL with type t = user and type key = int64 and type draft = ...`. The model modules take `schema`, `key` and `fields` from `Cistern_fields__user`. It also holds the functions that need only types: the draft encoders, the record builders, and the setters of the draft keys, in one submodule for each type with a belongs_to (`Cistern_drafts__user.Set_todo.user_id` sets the key of a `Drafts.todo`). The setters of the relation fields, which set `Loaded`, are in `Cistern_fields__user` (`Cistern_fields__user.Set_user.todos`, phase 4c1, E3), because a draft type can have a label with the name of a relation of another type (C-M3). A draft encoder takes the column and the encoder of each field from `Cistern_fields__user` (`(Field.column f, Field.encode f v)`), so that one source holds them. Type names are unique in a group, so these names cannot clash, and they do not hide values of the user. A draft has only the column fields of its type, not the relation fields.
2. Stage 2 in the namespace module `module Cistern_gen__user = struct module User = struct ... end module Todo = struct ... end end`: `Q`, `get_by_*`, `find_by_*` (only for `[@unique]` fields and the key: for another field the server would choose one of the rows), `filter_by_*`, `all`, `where`, `update`, `delete`, `draft`. These are thin wrappers around the generic operations, for example `let get_by_id k = Cistern.Private.Model.get (module Cistern_model__user) k`.
3. Stage 3 after the namespace module: `module User = struct include Cistern_gen__user.User ... end` and the same for `Todo`. It adds `create` (nested drafts), the relation scopes (`User.todos`), `Rel` (relation descriptors), `model` and `Drafts`. Stage 3 refers to other models through the namespace module or the model modules, because the stage-3 `Todo` comes after the stage-3 `User`.

The form of the generated code (phase 3b for stage 1, phase 3c for stages 2 and 3), for a group whose first type is `kinds`:

0. `let cistern__default__kinds__<n> () : <type of the field> = e` for each `[@default e]`, where `<n>` is the index of the field in the group. The thunks come first, so that a type error of a payload comes before any use of the library (a row of `test/typeerr`). Then `module Cistern__private__kinds = Cistern.Private`, the one name of the library in the code of the group.
1. `module Cistern_fields__kinds = struct module Kinds = struct module Schema = struct ... end ... end end`. `Schema` has one value for each field (the `Schema.field` record) and `cistern__model` (the `Schema.model`). These records are the lift of the one value of `Desc.schema` that the ppx checks. After `Schema`, one typed field for each field: `Field.make ~model:Schema.cistern__model ~field:Schema.<f> ~encode ~decode ~get:(fun (cistern__r : kinds) -> cistern__r.<f>) ~set:(fun (cistern__r : kinds) cistern__v -> {cistern__r with <f> = cistern__v})`. The setter of a `Ptime.t` field cuts the value (`Encode.cut` or `Encode.cut_opt`).
2. `module Cistern_drafts__kinds = struct type kinds = {...} ... let encode_kinds (cistern__d : kinds) = [(Field.column f, Field.encode f cistern__d.<f>); ...] end`. A draft has the fields with no `[@auto]`, with the type expressions of the source. A type with no such field has `type t = Cistern__private__<first>.Encode.empty` and `let encode_t () = []`.
3. `module Cistern_model__kinds : Cistern.MODEL with type t = kinds and type key = <type of the key> and type draft = Cistern_drafts__kinds.kinds = struct type nonrec t = kinds ... end`. `decode` binds `cistern__0`, `cistern__1`, ... in the order of the fields, because the order of the evaluation of a record is not specified. `of_draft` takes an `[@auto]` key from the server (`Encode.auto`) and cuts each `Ptime.t`.
4. `module Cistern_gen__kinds = struct module Kinds = struct type nonrec t = kinds module Q = struct ... include Cistern__private__kinds.Expr.Ops end ... end end`, with these values:
   - `get_by_<key>` and `find_by_<key>`, and `get_by_<f>` and `find_by_<f>` for each `[@unique]` field. For a nullable one, the value has no `option` (`Encode.some`).
   - `filter_by_<f>` for each `[@index]` field, with no local open: `Model.where (module Cistern_model__kinds) (Cistern__private__kinds.Expr.Ops.( ==. ) Q.<f> cistern__v)`.
   - `all`, `where` and `delete`.
   - `update`, with a label `?<f>` for each field that is not the key: `Model.update (module M) (Encode.updates [Encode.set Q.<f> <f>; ...]) cistern__r`.
   - `draft`, with `~<f>` for a field with no default and no `option`, `?<f>` for an `option` field, and `?(<f> = cistern__default__kinds__<n> ())` for a `[@default]`. An empty draft has `draft () = ()`.
5. `module Kinds = struct include Cistern_gen__kinds.Kinds module Drafts = Cistern_drafts__kinds type draft = Drafts.kinds let model : (module Cistern.MODEL) = (module Cistern_model__kinds) let create ... = ... end`. `create` has the labels of `draft` (a default field has `?<f>`, and `draft` gives the default), and it calls `draft` by its path (`Cistern_gen__kinds.Kinds.draft`).

Each item has `[@@ocaml.warning "-23-27-30-32-34-37-40-41-42-60-69"]`. The code of each field has the location of its field, so that a type error points at the source. The generated code names each helper through `Cistern.Private.Encode` (`int64`, `int`, `text`, `bool`, `float`, `bytes`, `ptime`, `opt`, `cut`, `cut_opt`, `some`, `set`, `updates`, `auto` and the type `empty = unit`), and it names no module of the stdlib. Each local name has the prefix `cistern__`. It writes no type annotation of its own, only copies of the type expressions of the source. A user module named `Cistern` before a group hides the library. In the same way, a user type with the constructors `[]` and `(::)` (for example `type l = [] | (::) of int * l`) before a group breaks the lists of the generated code (`fields`, the draft encoders and the list of `update`). The value `Some` of `get_by_*` comes from the library (`Encode.some`), so a user constructor `Some` does not break it.

Rules:

- All generated code calls the library through `Cistern.Private.*`, so a user type named `model`, `query` or `field` does not hide it.
- The model modules have top-level names (`Cistern_model__todo`), so a later version can permit relations between two groups.
- Two groups with the same first type name in one file clash (documented). A user module with the name of a stage-3 module (for example a module `M` and a type `m`) in the same structure is an error of the compiler, because OCaml refuses two modules of one name in one structure.
- The warnings (probe P3 of the plan of phase 3): the hand-written stages of `kinds` and `auto_only` behind an interface with only a value `model` give only the warnings 32 (an unused value, 24 times) and 34 (an unused type, 3 times) in the dev profile. With `-w +a`, the generated code of 3b also gives the warnings 30 (two draft types of one group with the same labels), 40 and 42 (labels that the type selects), and the attribute covers 41 for the same cause. Thus the generated code has a warning attribute that disables 23, 27, 30, 32, 34, 37, 40, 41, 42, 60 and 69. The library `test/ppx/driver` compiles the generated code with `-w @a-70`, so that each other warning is an error.
- Phase 2 writes the three stages by hand (`test/model`: the group of `user` and `todo`, `kinds` with each scalar, and `auto_only`), as the reference of the ppx. Phase 3b changes stage 1 of `kinds` and `auto_only` to the form of the ppx, puts the attributes on their type declarations, and adds the group `person` (an `[@auto]` key, `[@unique]` fields, an `[@index]` field, an `option` field, a `[@default]` and `[@@table]`). The group of `user` and `todo` keeps its form until phase 4. Stage 1 also has `Cistern_fields__user` with the `Schema.model` and the typed fields of each type, because the drafts module hides the record types with the draft types of the same names. `Q` binds each field of it, then includes `Cistern.Private.Expr.Ops`.
- `get_by_<field>` and `find_by_<field>` of a nullable `[@unique]` field take the value with no `option`, because `None` names no row: `get_by_nick v` calls `Model.get_by (module M) Q.nick (Some v)`, and `Model.get_by` raises `Invalid_argument` for a `Null` value.
- An expect test compiles a file with two deriving groups.

The children step of `create`:

```ocaml
val Model.child :
  (module C : MODEL) -> fk:(C.draft -> 'k -> C.draft) -> set:('p -> C.t list -> 'p)
  -> C.draft list -> ('p, 'k) child

(* stage 3 of User *)
let create ~name ~email ?todos () =
  Cistern.Private.Model.create (module Cistern_model__user) { name; email }
    ~children:(match todos with
               | None -> []
               | Some ds -> [ Cistern.Private.Model.child (module Cistern_model__todo)
                                ~fk:Cistern_drafts__user.Set_todo.user_id
                                ~set:Cistern_fields__user.Set_user.todos ds ])
```

`fk` and `set` are setters of stage 1: `fk` sets a draft key (in `Cistern_drafts__<first>`), and `set` a relation field (in `Cistern_fields__<first>`, phase 4c1, E3). Thus `create` gives the parent with `Loaded` children. With children, `create` is one atomic statement (see "Execution of one statement"): the parent `INSERT`, the key (the last insert id, or the key of the draft), then one `INSERT` for each child in the order of the list. No child (no group, or only empty lists) gives the `create` of phase 2, which is not atomic. `Stmt.to_sql` shows the key of an `[@auto]` parent in each child as `<last_insert_id>`, and the redacted SQL of the events shows `?`. A value with no literal in the parent or in a child gives `Invalid_value` before the checkout. Because `references` is always the key of the parent in version 1, `'k` is `M.key` of the parent, and all children of one parent have one type `('p, 'k) child`.

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
    include Expr.OPS   (* ==. <>. <. >. <=. >=. is_null in_ starts_with contains
                          starts_with_opt contains_opt, and && || not *)
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

The code of relations (phase 4g, the plan of phase 4, section 3): only a group with a relation gets it, thus the code of a group with no relation does not change.

- Stage 1: the lifted `Schema.model` has the relations. The setter of the typed field of a belongs_to key calls `Encode.reset` (E9). `Cistern_fields__<first>.Set_<type>` has the setters of the relation fields. In the draft, the key of a belongs_to that is not nullable is an `option` (E13), its encoder gives `NULL` for `None`, and `Cistern_drafts__<first>.Set_<type>` has its setter (`Encode.some`), the `fk` of the nested create. `decode` and `of_draft` give `Unloaded` for each relation field, and `of_draft` takes the key of the draft with `Encode.draft_key`.
- Stage 2: `filter_by_<key>` for the key of each belongs_to with no `[@index]`, `[@unique]` or `[@key]` (it acts as `[@index]`). The key is an optional label of `draft` and `update`.
- Stage 3: `create` has a label `?<has_many>` for each has_many. It passes `~children:(Encode.somes [Encode.group (Model.child (module C) ~fk ~set) <has_many>; ...])`, thus only the lists that the caller gave make groups, with no name of the stdlib (`fk` is the setter of the key of the one belongs_to of the child to the parent, rule 9). Then the scopes, and `module Rel`. The setter of a belongs_to with a key that is not nullable goes through `Encode.parent`: `None` gives `Unloaded`.
- The hand-written `user`/`todo` (`test/model/user_todo.ml`) and the tree `node` (`test/model/tree.ml`) have the form of the generator (E14), with the equality and the test of D2. The hostile relation group of `test/ppx/driver/relations.ml` (relation names `c`, `r`, `k`, `fields` and `decode`, a child with two belongs_to, a self relation, a nullable key, a relation and a column of another type with one name, `[@column]` on the key of a parent and of a child, and user modules `List`, `Option`, `Fun` and a constructor `Some` before the group) compiles through the driver with `-check`, and `test/ppx` runs its scopes, each `Rel` with `include_`, and each label of `create`.

The relation scopes and `Rel` (phase 4c2, E2 and E11):

- A has_many gives `User.todos : t -> todo Query.t`: `Todo.where Todo.Q.(user_id ==. r.id)` with the key of the parent. For a nullable key of the child, the filter takes `Encode.some` of the key (`parent_id <=> 4`). `Query.where`, `Query.order_by` and the other functions of `Query` act on the scope.
- A belongs_to gives `Todo.user : t -> user Stmt.t`: `Model.get` of the key (`Not_found` if the row is gone). For a nullable key it gives `t -> user option Stmt.t` through `Model.get_parent`: `None` gives a pure `None` (no checkout, no command, no event), and `Some k` gives `get` as `Some`, with `Not_found` if the row is gone.
- `Rel` has one value for each relation, built at module initialization with `Cistern.Private.Relation.has_many` and `belongs_to` (which check the schema). The key function of a key that is not nullable is `Encode.some` of the field. The setter of a belongs_to takes the parent as an option: for a key that is not nullable, `None` (no parent row) gives `Unloaded`; for a nullable key, `None` gives `Loaded None`.

Filters:

- `&&`, `||` and `not` change meaning only inside `X.Q.( ... )`, where the field names already hide other names. `Expr.and_` and `Expr.or_` take lists.
- Each filter is two-valued (phase 2a): on each row it is true or false, never SQL `NULL`. Thus `not p` selects the rows that `p` does not select, and `&&`, `||` and `not` act as the booleans of OCaml. An atom on a nullable field that SQL can make `NULL` gets the guard `IS NOT NULL AND`. The property test of `Expr` evaluates the model AST with the three-valued logic of SQL, over rows in OCaml, and compares each filter with its OCaml rule.
- On a field with an `option` type, the filters follow OCaml equality, also for `NULL`:
  - `f ==. v` gives `f <=> v` (the NULL-safe operator of the two servers, which can use an index). `f ==. None` gives `IS NULL`.
  - `f <>. v` gives `NOT (f <=> v)`. Thus `f <>. Some 1` keeps the rows where `f` is `NULL`, as OCaml keeps `None`.
  - `in_ f [Some 1; Some 2]` gives `(f IS NOT NULL AND f IN (1, 2))`, `in_ f [None; Some 1]` gives `(f IN (1) OR f IS NULL)`, and `in_ f [None]` gives `f IS NULL`.
  - `f <. Some 1` gives `(f IS NOT NULL AND f < 1)`, and the same for `>.`, `<=.` and `>=.`. Thus an order never selects `None`. With `None`, these operators raise `Invalid_argument` when the query is built.
- On a field that is not an `option`, `==.` gives `=` and `<>.` gives `<>`, with no guard.
- `in_ f []` gives `FALSE`. `IN ()` is a syntax error.
- `starts_with` and `contains` give `LIKE <hex literal> ESCAPE '!'`. The client escapes `%`, `_` and `!` in the pattern with `!`. The escape character does not depend on `NO_BACKSLASH_ESCAPES`. They take a `string` field. On a `string option` field, `starts_with_opt` and `contains_opt` give `(f IS NOT NULL AND f LIKE ...)`: a `NULL` row does not match. The compiler refuses each of the four on a field of another type.
- Each comparison has a column on one side and a literal on the other side, never two literals. A literal has the case-insensitive collation of the session, and the binary collation of the column wins.

### Deferred values

`type 'a Deferred.t = Unloaded | Loaded of 'a` (immutable, no closures, so `=` and printing work). `Deferred.get` gives `Some` of a loaded value and `None` for `Unloaded`, and `Deferred.get_exn` raises `Invalid_argument` for `Unloaded` (phase 4a).

- `get_by_*`, `find_by_*` and queries give `Unloaded` relations, unless `include_` loads them.
- `create` gives `Loaded` children for each has_many whose list the caller gave, also for an empty list (`Loaded []`, because a new row has no child), and `Unloaded` for a has_many with no list (phase 4c1, the review of 4c1, L1). Each child has an `Unloaded` back-reference.
- `update` keeps the relations of its input record. If the update changes a `belongs_to` key, that relation becomes `Unloaded` (phase 4c1, E9): the setter of the key field calls `Cistern.Private.Encode.reset`, which compares the encoded keys with `Value.equal`, and keeps the relation for the same key. `Field.set` of the key field does the same, because it uses this setter.
- The relation fields are not columns: `MODEL.fields`, `Q`, the drafts and `update` have no relation field, and `decode` and `of_draft` give `Unloaded`. The setters of the relation fields (`Set_user.todos`, `Set_todo.user`, which set `Loaded`) are in `Cistern_fields__<first>` (phase 4c1, E3).
- `include_` never fills the inverse relation, so no data is cyclic.

### Usage

README.md has the usage, and each of its OCaml blocks is a region of a file of `examples/` (phase 5b): the quick start, the models, queries, writes, transactions with a retry loop, relations, raw SQL, errors and events. The examples compile in each build, and the integration suite runs them on the three servers.

Note: the field names of `Q` hide user values with the same names under `Todo.Q.( ... )`. Bind the values outside, as `uid` above.

### Core user types

The module `Cistern` gives the types of the user: `Db`, `Tx`, `Stmt`, `Raw`, `Error`, `Event`, `Value`, `Decimal`, `Dialect`, `Exec_result`, `Config`, `Field`, `Expr`, `Update`, `Deferred`, `Relation` (phase 4a), `module type MODEL` and the exception `Cistern_error`. The public `Relation` is an abstract `('m, 'r) Relation.t` (the relation from the model `'m` to the record type `'r` of the other model) with `name` and `kind`. `Cistern.Private.Relation.has_many` and `belongs_to` make one from the two model modules, the key of the parent of a child (`C.t -> P.key option`) and the setter of the relation field, and they raise `Invalid_argument` if the schema has no such relation (the message names the relation that the schema has: another kind, or another target). Inside the library, `Relation.t` is a GADT with the two models, the key and the setter, for `Query.include_`; the public `Cistern.Relation` gives only the type. The public `Field`, `Expr`, `Update` and `Query` have the restricted signatures that follow: `Field.make`, `Expr.Ops`, `Update.assignments`, `Update.apply` and `Query.Private.make` are only in `Cistern.Private`, with `Schema`, `Decoder` and `Model` (the generic operations of the generated code). Thus a program cannot make a query of one model from the schema of another. The pool modules (`Pool`, `Pool_state`, `Exec`) are internal: only the tests use them, through `Cistern.Private`.

```ocaml
module Db : sig
  type t      (* the pool. Safe across fibers and domains. Any use after with_db raises Invalid_argument *)
  val with_db :
    ?pool_size:int -> ?max_lifetime:Weft.Span.t -> ?checkout_timeout:Weft.Span.t
    -> ?on_query:(Event.t -> unit) -> ?random:Random.State.t
    -> Config.t -> (t -> 'a) -> 'a   (* owns a Fiber.scope *)
  val push_schema : ?timeout:Weft.Span.t -> t -> (module MODEL) list -> unit
    (* Schema.check first; its own connection; raises Cistern_error *)
  val check_schema : t -> (module MODEL) list -> (unit, string list) result
    (* Error: the messages of Schema.check, or the differences; raises Cistern_error *)
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
(* Model.update : (module M : MODEL) -> M.t Update.t list -> M.t -> M.t Stmt.t
   (the empty list gives r with no statement), and Query.update takes one
   Update.t. An Update.t that sets the key, or that sets one column two times,
   raises Invalid_argument when the statement is made. *)

module Field : sig
  type ('m, 'a) t   (* abstract: a field of one model does not type as a field of another *)
  val table : (_, _) t -> string
  val column : (_, _) t -> string
  val nullable : (_, _) t -> bool
  val get : ('m, 'a) t -> 'm -> 'a
  val set : ('m, 'a) t -> 'm -> 'a -> 'm
end
(* Cistern.Private.Field.make ~model ~field ~encode ~decode ~get ~set makes a
   field from its Schema.model and Schema.field (the column, the scalar and
   nullable come from the Schema.field) *)

module Expr : sig
  type ('m, 'a) t   (* abstract *)
  val and_ : ('m, bool) t list -> ('m, bool) t   (* and_ [] is TRUE *)
  val or_ : ('m, bool) t list -> ('m, bool) t    (* or_ [] is FALSE *)
  module type OPS   (* the operators of X.Q: see "Filters" *)
end

module type MODEL = sig
  type t
  type key
  type draft
  val schema : Schema.model
  val key : (t, key) Field.t
  val fields : t Field.any list           (* the column fields, in the order of the record *)
  val decode : Decoder.cursor -> t        (* the columns in the order of fields *)
  val encode_draft : draft -> (string * Value.t) list
  val of_draft : draft -> key option -> t
end
(* no type update and no key_of: Field.get M.key gives the key *)

module Stmt : sig
  type 'a t   (* the commands of one statement, on one connection *)
  val map : ('a -> 'b) -> 'a t -> 'b t   (* f runs after the checkin and the event *)
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

- `Query.update` and `Query.delete` with `offset`, `include_`, `for_update` or `for_share` raise `Invalid_argument` (MySQL has no `UPDATE ... OFFSET`), when the statement is made. They give the matched rows (`CLIENT_FOUND_ROWS`). With no filter (`User.all`), they change the whole table (documented). `count` and `exists` ignore `include_`, `for_update` and `for_share`.
- `Query.limit` and `Query.offset` with a negative number raise `Invalid_argument` when the query is built.
- `for_update` (phase 2d2): the statements of `to_list` and `first` carry a flag in their meta, and `Stmt.map` keeps it. `Db.exec` raises `Invalid_argument` for it before any event, because the lock would end with the statement. `Tx.exec` runs it. `for_share` is permitted in `Db.exec`: its lock ends with the statement there (documented).
- `count` and `exists` give `Decode` for a reply with another count of rows than 1.
- A raw statement marks its connection dirty, which then gets a session reset at its next checkout (see "Session").
- `Value.t` has `Null`, `Int of int64`, `Float of float`, `Text of string`, `Bytes of string`, `Datetime of Ptime.t`, and `Decimal of Decimal.t` (for raw results such as `SUM`). `Decimal.t` is abstract. `Decimal.of_string` accepts only `-?[0-9]+(\.[0-9]+)?`, with at most 65 digits in total and at most 38 after the point (the `DECIMAL` limits of the servers: 65 digits, and a scale of at most 38 on MariaDB and 30 on MySQL), so that `Raw.v (Decimal d)` cannot carry SQL text.
- The fragments of `Raw` (`sql`, `v`, `++` and `to_string`), `Raw.result` and `Exec_result.t` are in `cistern.core` (phase 1.1). `Connection.query` and `Connection.exec` of `cistern.mysql` run a `Raw.t` on one connection. `Raw.query` and `Raw.exec` with `Stmt.t` come with the pool (phase 1.2). `Raw.to_string` gives `Error msg` for a value with no literal, and the client makes `Invalid_value` from it.
- The exception is `Cistern_error`, not `Error`, so that `open Cistern` does not hide the `Error` constructor of `result`.
- A cancel of the fiber is never an `Error.t`. weft's `Cancelled` always goes up, also when a write was in flight. Then the `on_query` event of the statement records the unknown outcome, and the `Tx` (if any) fails.
- A failure in a hook or in the checkin never changes the result of the statement.

Order of the error rules:

- A write (`INSERT`, `UPDATE`, `DELETE`, any raw statement, `COMMIT`) that was sent with no clear reply gives `Outcome_unknown`. "No clear reply" is a deadline, the end of the stream or a reset, a reply that the client cannot read, or an error that `Error.of_server` maps to `Connection_lost`: 1053 (the two servers), 4031 (MySQL) or 1927 (MariaDB). The client gives `Outcome_unknown` at once after such an error, and it closes the connection with no more read, because its state is not known. This rule is the same for `COMMIT` (it gives `Outcome_unknown `Commit`) and for a write in autocommit mode. The user must not retry it blindly.
- 1317 (`ER_QUERY_INTERRUPTED`, for example after `KILL QUERY`) gives `Driver`, and the connection stays usable. `Error.interrupts` names the four numbers. Its first user is the `COMMIT` rules of phase 1.3.
- `Timeout` and `Connection_lost` are only for reads and for statements that were not sent. A deadline in the flush of the command packet gives `Timeout` (the statement did not go). A deadline in a read after the flush gives `Outcome_unknown` for a write (see "Deadlines").
- A write that the server stops gives `Outcome_unknown`: `KILL CONNECTION` (the end of the stream, claim C8a) and `SHUTDOWN` (1053 then the end on MySQL, the end on MariaDB, claim C8c). `KILL QUERY` gives 1317 and leaves the connection usable (C8b).
- The write rule: a statement counts as sent when the flush of its command packet returned. If the flush raised (`EPIPE`, `ECONNRESET`, a closed flow), the server did not get the whole packet, and the client gives `Connection_lost`. The rule is the same on a TLS flow: a cancel at the write lock between two records leaves a part of the packet sent and the flow open, and a cancel or an error in a TCP write fails the TLS flow. In the two cases the client breaks the connection, the server drops the part of the packet, and a later `close` sends no `COM_QUIT`.
- Rule R-SEQ0: an ERR with the sequence id 0 where the first packet of the reply (id 1) is expected came before the command (for example 4031 at the idle timeout). The client gives `Connection_lost` and closes the connection, also for a write: the server did not read the command. Thus 4031 before the command is not `Outcome_unknown`. Claim C9 shows the two rules: after the idle timeout, a `ping` and an `INSERT` give `Connection_lost` on the two servers, on TCP (MySQL sends 4031) and on the socket (the write fails), and the row is not written.
- `Outcome.decide` (`cistern.mysql`) is the table of these rules for one command. Each event breaks the connection, except two. A server error that `Error.of_server` does not map to `Connection_lost` leaves the connection usable (also 1317), except 1153, which breaks it. A value that does not decode leaves it usable too, because the client read the whole reply. A malformed packet gives `Outcome_unknown` for a write also after the column count: the statement ran, but a `CALL` can run more statements after its first result set, and the client cannot read their outcome. A `LOCAL INFILE` request gives `Decode`: the server waits for a file, thus it did not run the statement.
- At the level of `Db.transaction`: if the client closed the connection before it sent `COMMIT`, the server rolled back. Then `Db.transaction` gives `Timeout` or `Connection_lost`, not `Outcome_unknown `Statement`, because a retry of the whole transaction is safe. This holds only if each write statement of the transaction was DML: a DDL statement or a `CALL` can commit by itself, and then `Outcome_unknown `Statement` stays ("Transactions").

Error mapping. The key is (server, error number). A number that the table does not have uses the SQLSTATE: class `23` gives `Constraint_violation Other_constraint`, and `40001` gives `Retryable `Deadlock`.

| Error | MySQL | MariaDB |
|---|---|---|
| `Unique` | 1062, 1586, 1022, 1169, 1859, 1761, 1762 | the same |
| `Foreign_key` | 1451, 1452, 1216, 1217 | 1451, 1452, 1216, 1217 |
| `Not_null` | 1048, 1364, 3673 (`ER_BAD_NULL_ERROR_NOT_IGNORED`) | 1048, 1364 |
| `Check` | 3819 | 4025 (`ER_CONSTRAINT_FAILED`, verified in round 5) |
| `Invalid_value` | 1406, 1264, 1265, 1365, 1366, 1292, 1690, 1367, 1301, and 1153 (see the note that follows) | the same |
| `Retryable `Deadlock` | 1213 | 1213 |
| `Retryable `Lock_timeout` | 1205 | 1205 (also for `NOWAIT`, claim C15) |
| `Retryable `Rolled_back` | 3101 (`ER_TRANSACTION_ROLLBACK_DURING_COMMIT`) | 1020 (`ER_CHECKREAD`, with `innodb_snapshot_isolation`, the default of 11.8: a row changed after the snapshot of the transaction, and the server rolled back the whole transaction, claim C37; phase 4a0). 3101 is a placeholder number |
| `Statement_timeout` | 3024 | 1969 (`ER_STATEMENT_TIMEOUT`, verified in round 5) |
| `Connection_lost` (a read or an unsent statement; a sent write gives `Outcome_unknown`) | 4031, 1053, end of stream | 1927, 1053, end of stream |
| `Read_only` | 1290 when the message names the option `--read-only` or `--super-read-only`, 1792, 1836 (`ER_READ_ONLY_MODE`), 1874 (`ER_INNODB_READ_ONLY`) | the same |
| `Driver` | all others, also 1317, 1052 (`ER_NON_UNIQ_ERROR`, an ambiguous column, with SQLSTATE 23000), 1290 for other options, 1036 (`ER_OPEN_AS_READONLY`, one table) and 3572 (`ER_LOCK_NOWAIT`) | the same, with no 3572 |

The server gives no structured constraint name, so `Constraint_violation` has only the message.

`perror` of the pinned packages (MySQL 8.4.11, MariaDB 11.4.13 and 11.8.8) confirmed each number of the table and its name on 2026-11-06. A number of one server is another error on the other server (MySQL 4025, MariaDB 3024, 3101 and 4031), or no error (MySQL 1927 and 1969, MariaDB 3819 and 3572). Thus the key is the pair. 2013 and 2006 are codes of the client library: a server never sends them. `Error.of_server` gives the meaning of an error packet alone. The rules that need the state of the connection ("Order of the error rules", "Transactions") are in the client.

Notes of the claims of phase 1.1:

- `--innodb-read-only` gives `Driver` for a write (1015 on MySQL, 1036 on MariaDB) and for DDL (1005 on MariaDB): 1015 and 1005 are generic numbers, and 1036 is `Driver` for one table. The failover case is `read_only` and `super_read_only` (1290) and a read-only transaction (1792), which give `Read_only` (claim C14). 1836 and 1874 stay `Read_only`. The suite cannot make them, because they need `--innodb-read-only` (a restart). The probe of the plan review made them on MySQL 8.4 with this option: 1836 for `CREATE TABLE` and `CREATE DATABASE`, 1874 for `CREATE TEMPORARY TABLE`.
- 1300 (a string that is not valid in its character set) gives `Driver`. It comes only from raw SQL text or from DDL names, because the `Text` literal is valid UTF-8 and `Schema.check` refuses such names.
- 1030 (MySQL) and 1005 (MariaDB) for a table name that is too long as a file name give `Driver`. `Schema.check` refuses such a name.
- 1927 and 1053 keep `Connection_lost`. The claims did not make 1927: `KILL CONNECTION` gives the end of the stream with no error packet on the two servers (C8a). MariaDB sends no 1053 at `SHUTDOWN` (C8c).
- 1762, 1169, 1859 and 3673 keep their rows, with `perror` as the proof: the claims did not make them on MySQL 8.4 and MariaDB 11.8 (C17).
- A zero `DATETIME` or `TIMESTAMP` gives `Decode` (the session `sql_mode` has `NO_ZERO_DATE`, thus only old data has it).
- The counts of an OK packet above `Int64.max_int` keep their 64 bits as a negative `int64`, because a `Decode` would hide a write that ran.
- MariaDB stores an encoded surrogate in a `utf8mb4` column. A row with one gives `Decode` (C12).

Error 1153 (packet too large): the server did not run the statement, but it closes the connection after the error. Thus the client gives `Invalid_value`, closes the connection, and fails the `Tx` (if any). The client size check (see "Limits and defaults") prevents this error in most cases.

## Internal architecture

### Schema, AST and SQL

- One schema (`Schema`): models, fields, column names, types, the key, indexes and relations. `push_schema` and `check_schema` call `Schema.check` first. Its rules:
  1. The model names are unique. The table names are unique after the ASCII fold (the file system of macOS ignores case).
  2. Each table name and each column name obeys the rules of names ("Model definition"). A table name has no uppercase ASCII letter (phase 2a), so that the name is the same on Linux and on macOS (`lower_case_table_names` 2), where `check_schema` finds a table by its name.
  3. Each model has exactly one `[@key]` field. The key is not an `option` and not a `float`.
  4. `[@auto]` is only on the key, and only on an `int64` field.
  5. The column names of a model are unique after the ASCII fold.
  6. The key has no `[@unique]` and no `[@index]`. A field does not have `[@unique]` and `[@index]` together.
  7. The relation names of a model are unique, and differ from the field names.
  8. `belongs_to`: the key is a field, the other model is in the list, `references` is its key, and the two fields have the same type. Two `belongs_to` of a model do not use one key. A `belongs_to` to its own model does not use the `[@key]` as its key: MySQL refuses `FOREIGN KEY (id) REFERENCES t (id)` (6125), and on MariaDB each `INSERT` fails (1452).
  9. `has_many`: the other model is in the list, with exactly one `belongs_to` to this model (version 1 has no `via`).
  10. The names of the unique and plain indexes of a table are unique after the ASCII fold.
  11. The foreign key names of all the models are unique after the ASCII fold, because a constraint name is unique in the database. `check` cannot see the foreign keys of tables outside the list.

  `Schema.faults` gives the same messages as structured faults (phase 3a0): each fault has the model (or none, for a fault of the list), the models of its cause (the model, or for a fault of the list each model of the cause: two models with one name, two equal table names, two equal foreign key names), the list of the fields of its cause (none, one, or more: two columns equal after the fold, several keys, two equal index names), the relation of its cause (rules 7 to 9, phase 4a: the relation that a rule refuses, or each `Belongs_to` after the first on one key, each with its own fault), and a flag for a fault of the table name. `check` formats them. The ppx puts each fault at the location of its first field, of the `[@@table]` attribute, or of the type.

  A group of equal names gives one message with all the members. A rule that needs a part that another rule refuses does not run. The fold is ASCII only: the servers also fold other letters (`É` to `é`), thus two such names pass `check`, and the server refuses the DDL with 1060 (documented). An index name with a non-ASCII character always has the hash, thus no 1061. The fold of the servers differs: MariaDB 11.8 accepts the Kelvin sign (U+212A) and `k` as two columns. The messages quote names with `Cistern_names.quote`: valid UTF-8 stays as it is, and control characters and invalid bytes become escapes.
- The typed API builds a model-level untyped AST. `cistern.sql` compiles it to a SQL AST and serializes it for the dialect of the connection.
- Each `SELECT` has an explicit, qualified column list in the field order of the model. The decoders read columns by position.
- Identifiers are always quoted with backquotes. A backquote in a name is doubled (`Schema.check` refuses it anyway).
- The compiler (`Compile`, phase 2b) makes the literals when the program makes the statement: the SQL AST holds the text of each literal. Thus a value with no literal gives `Invalid_value` before the checkout, and only the clause of a shared lock waits for the dialect. One serializer (`Print`) gives the SQL and the redacted copy of the events, with `?` in place of each literal: an `in_` list of n values gives n `?`. The numbers of `LIMIT` and `OFFSET` stay in the redacted copy: they are numbers of the program, not values of a row. Thus paging gives one event text for each offset, as `in_` lists give one for each length.
- The shapes (`<cols>` is the qualified column list):
  - `` SELECT <cols> FROM `t` [WHERE ...] [ORDER BY `t`.`c` ASC|DESC, ...] [LIMIT n [OFFSET m]] [FOR UPDATE | FOR SHARE | LOCK IN SHARE MODE] ``. An offset with no limit gives `LIMIT 18446744073709551615`.
  - With a limit or an offset, the reads (`to_list`, `first`, `count`, `exists`) and the bulk writes append the key as the last key of `ORDER BY` (`ASC`), if the order does not have it yet. Thus two pages never hold the same row, also over equal values.
  - `first`: `LIMIT 1`, or `LIMIT 0` if the query has it. The offset stays.
  - `count`: `` SELECT COUNT(*) FROM `t` [WHERE ...] ``, with no order. With a limit or an offset: `` SELECT COUNT(*) FROM (SELECT 1 FROM `t` ... ORDER BY ... LIMIT ...) AS `t` ``. `exists`: `` SELECT EXISTS (SELECT 1 FROM `t` [WHERE ...]) ``, with the order, the limit and the offset only for a query with a limit or an offset. The two ignore the lock.
  - `get` and `find`: `` SELECT <cols> FROM `t` WHERE `t`.`key` = <k> LIMIT 1 ``.
  - `create`: `` INSERT INTO `t` (`a`, ...) VALUES (<lit>, ...) ``, or `` INSERT INTO `t` () VALUES () `` for a model with only an `[@auto]` key.
  - `update` and `delete` of one row: `` UPDATE `t` SET `a` = <lit>, ... WHERE `t`.`key` = <k> `` and `` DELETE FROM `t` WHERE `t`.`key` = <k> ``. The columns of `SET` have no table, because the statement has one table.
  - `Query.update` and `Query.delete`: `` UPDATE `t` SET ... [WHERE ...] [ORDER BY ...] [LIMIT n] `` and `` DELETE FROM `t` [WHERE ...] [ORDER BY ...] [LIMIT n] ``. An offset or a lock raises `Invalid_argument` when the statement is made. With no filter, they change the whole table (documented).
  - A filter: `AND` and `OR` are always between parentheses, and `NOT` is `NOT (...)`. Each atom is `<column> <op> <literal>`, never two literals. A model AST from other code than `Expr` can have an empty `In`, `And` or `Or`: the compiler gives `FALSE`, `TRUE` and `FALSE`, thus the SQL never has `IN ()`. A negative limit or offset raises `Invalid_argument` in the compiler too.

### Literals (cistern.core)

No value from a user is ever between quotes:

| Value | Literal |
|---|---|
| `int64`, `int` | Decimal digits, with a sign. `Int64.min_int` is in the property tests. |
| `float` | Cistern's own method: the shortest correctly rounded `%.{p}e` form that reads back to the same bits. For p from 0 to 16, the client formats the float with `%.{p}e`, takes the sign, the digits and the exponent from the result, skips one byte for the point, and writes its own point. No locale can change the literal, because the OCaml 5.5 runtime formats and reads floats in the "C" locale (`runtime/floats.c`: `caml_format_float` and `caml_float_of_string`, with `USE_LOCALE` and `strtod_l`). Always with an exponent and with no `+`, for example `1.5e0`, `1e-1` and `-0e0`, so that the server reads a `DOUBLE`. MySQL keeps the sign of `-0`. MariaDB 11.4 and 11.8 store and read `-0.0` as `+0`, also in a `SELECT` of the literal (`SELECT -0e0` gives `0`), thus a `-0.0` comes back as `0.0` from MariaDB (the integration suite, and claim C11). The form is not always the shortest decimal: 2^-1017 gives `7.1202363472230444e-307`. NaN and infinities give `Invalid_value` on the client. |
| `bool` | `0` or `1` |
| `string` | `_utf8mb4 X'<hex>'`. The client checks that the string is valid UTF-8 first. |
| `bytes` | `X'<hex>'` |
| `Ptime.t` | `'YYYY-MM-DD HH:MM:SS.ffffff'`, made by the client from the number, with no user text. A timestamp before the year 1000 gives `Invalid_value` on the client. The models cut a `Ptime.t` to 1 µs (not rounded, `Ptime.truncate ~frac_s:6`) in `of_draft` (each `create` passes through it, also with a draft record that the program builds) and in the setter of the field (phase 2d1), thus the record that `create` and `update` give is the value that the server keeps. |
| `Decimal.t` (raw only) | The digits that `Decimal.of_string` checked, as they are. More than 30 digits after the point give `Invalid_value` on the client, because MySQL cuts such a literal with no error. |
| `None` | `NULL` |

- The literals do not depend on `NO_BACKSLASH_ESCAPES` or another session setting.
- A comparison between a column and a literal uses the collation of the column, because a literal has a higher coercibility. A review found no path to error 1267 or 1271. A text literal has the case-insensitive collation of the session (for example `_utf8mb4 X'61' = _utf8mb4 X'41'` is 1). Thus the compiler never compares two literals, and the binary collation of the column makes each comparison case-sensitive.
- `Raw` joins only fixed program text and these literals, so it needs no lexer. It puts a space before and after each literal, so that `Raw.sql "LIMIT" ++ Raw.v (Int 5L)` gives `LIMIT 5 `, not `LIMIT5`.

### DDL

- `CREATE TABLE IF NOT EXISTS ... ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=<collation> ROW_FORMAT=DYNAMIC`. `ROW_FORMAT=DYNAMIC` permits unique indexes on `VARCHAR(255)` in `utf8mb4` (1020 bytes).
- The text (`Ddl.create_table` of `cistern.sql`, phase 2c): one column for each field in order, `` `c` <type> NULL|NOT NULL [AUTO_INCREMENT] ``, then `PRIMARY KEY`, the `UNIQUE KEY` and `KEY` of `Schema.indexes`, then `` CONSTRAINT `n` FOREIGN KEY (`c`) REFERENCES `t` (`k`) ON DELETE RESTRICT `` for each foreign key of `Schema.foreign_keys`. The text has no value, thus it is also the SQL of the event.
- Collation: binary with no trailing-space padding. MySQL: `utf8mb4_0900_bin`. MariaDB: `utf8mb4_nopad_bin`.
- Indexes and foreign keys are inside `CREATE TABLE`, because MySQL has no `CREATE INDEX IF NOT EXISTS`. Each foreign key has an explicit index with a known name. The name of an index or a constraint (`Cistern_names.index_name`) is `<table>_<column>_<kind>`, with the kind `key` (unique), `idx` (plain) or `fk` (foreign key), for example `user_email_key`, `todo_user_id_idx` and `todo_user_id_fk`. If this name has more than 64 characters or a non-ASCII character, it becomes `<prefix>_<kind>_<hash>`: `<hash>` is the 8 lowercase hex digits of the FNV-1a 32 hash of the full name, and `<prefix>` is `<table>_<column>`, cut on a character boundary to 64 characters in total. The hash on non-ASCII names prevents equal names on MySQL, which compares constraint names with no accents.
- No `DEFAULT` clauses (MySQL refuses them on `MEDIUMTEXT` and `MEDIUMBLOB`). `[@default]` works on the client.
- `push_schema` runs on its own connection with `SET foreign_key_checks = 0`, so the order of the tables and cycles of references do not matter (mysqldump uses the same method). The connection is closed after the DDL. This connection is outside `pool_size`: each `push_schema` that runs at the same time adds one session (documented).
- `push_schema` never changes a table that exists. To change a table, use a raw `ALTER TABLE`, then `check_schema` (documented).
- `check_schema` (version 1) reads `TABLE_NAME`, `COLUMN_NAME`, `DATA_TYPE`, `IS_NULLABLE`, `CHARACTER_MAXIMUM_LENGTH`, `DATETIME_PRECISION`, `COLLATION_NAME`, `COLUMN_TYPE LIKE '%unsigned%'` and `EXTRA LIKE '%auto_increment%'` from `information_schema.COLUMNS`. A pure function (`Ddl.compare`, phase 2c) gives one message for each difference: a missing table, a missing column, an extra column, another `DATA_TYPE` (then no other message for this column), another nullability, another length, another precision, another collation, an unsigned column, an `[@auto]` key with no `AUTO_INCREMENT` (each insert with no key then fails with 1364), and an `AUTO_INCREMENT` column with no `[@auto]`. The column names match after the ASCII fold. The expected values are the values of the DDL on the three servers: for example `varchar` 255 with the binary collation, `mediumtext` and `mediumblob` 16777215, `datetime` with the precision 6, and no collation on the columns that are not text. Thus `check_schema` finds a shorter `VARCHAR`, a `DATETIME` with no fraction, a case-insensitive collation and an unsigned column, which `DATA_TYPE` alone does not show. It never compares the whole `COLUMN_TYPE`, because MariaDB writes `bigint(20)` and MySQL 8.4 writes `bigint`. Indexes and foreign keys come later.
- `push_schema` and `check_schema` from a fiber that holds a `Tx.t` raise `Invalid_argument`: DDL commits the open transaction, and it waits for the metadata lock of the transaction.
- `push_schema` in detail (phase 2e):
  1. The checks of misuse (a use after `with_db`, the fiber of a `Tx`), then `Schema.check`: its messages raise `Invalid_argument`, with no connect. An empty list does nothing.
  2. Its own connection with the configuration of the pool and `kill=false` (`Config.without_kill`: no round trip 3), and one `Connect` event. One deadline, `?timeout` (default 30 s), covers the connect and the statements: the connect runs in `Fiber.with_timeout`, and each statement has the margin check and the deadline of the flow.
  3. `SET foreign_key_checks = 0, lock_wait_timeout = <n>`. `<n>` is whole seconds: the remaining time minus 1 s, rounded up, at least 1 (MariaDB reads 0 as "no wait"). Thus the server gives 1205 for a metadata lock (MariaDB waits for one in `CREATE TABLE IF NOT EXISTS` of a table that exists) before the deadline of the flow.
  4. One `CREATE TABLE IF NOT EXISTS` for each model, in order, each with a `Query` event through `Pool.emit` (redacted as each event). The statements run as reads of the outcome rules: they are idempotent, thus a lost reply gives `Timeout` or `Connection_lost`, not `Outcome_unknown`.
  5. The polite close. After an error or a cancel, the abort.

  Each error raises `Cistern_error`: `Connect` for a failed connect, `Timeout` at the deadline, `Retryable `Lock_timeout` for 1205, and the error of the server for another failure.
  - `lock_wait_timeout` comes from the time left at the `SET`. For a later `CREATE TABLE`, less time is left, thus the deadline of the flow can come before the lock error, and the result is then `Timeout`.
  - After an error, the tables of the statements before it exist. `push_schema` can run again: `CREATE TABLE IF NOT EXISTS` skips them.
- `check_schema` sees only the tables that the user of the pool can see in `information_schema`. A table with no privilege of this user shows as missing ("the table ... does not exist").
- `check_schema` reads the columns with one statement through the pool (`Db.exec`, with its default deadline): `SELECT TABLE_NAME, COLUMN_NAME, DATA_TYPE, IS_NULLABLE, CHARACTER_MAXIMUM_LENGTH, DATETIME_PRECISION, COLLATION_NAME, COLUMN_TYPE LIKE '%unsigned%', EXTRA LIKE '%auto_increment%' FROM information_schema.COLUMNS WHERE TABLE_SCHEMA = DATABASE() AND TABLE_NAME IN (...) ORDER BY TABLE_NAME, ORDINAL_POSITION`, with the names of the tables as literals (`?` in the event). It is not raw: no session reset after it. An error of the pool, the connection or the server raises `Cistern_error`, because it is not a difference. The messages of `Schema.check` are the `Error`, with no statement. An empty list gives `Ok ()`.

### Execution of one statement

1. Serialize the SQL for the dialect of the connection. A raw statement makes its text one time, when the program makes the statement. A value with no literal gives `Invalid_value` before the checkout. The size check (see "Limits and defaults") needs the `@@max_allowed_packet` of the connection, thus it runs after the checkout. A statement that is too large gives `Invalid_value` and sends nothing.
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
| `update` gives the record | The input record with the new values (`CLIENT_FOUND_ROWS`: 0 matched rows give `Not_found`). It does not see changes from the server, for example from triggers (documented). An update with no field makes the checks of misuse of each statement (a use after `with_db`, `Db.exec` in a `Tx`, the first error of a failed `Tx`), then gives the record with no checkout, no statement and no event (`Stmt.Private.pure`). |
| `delete` of one row | 0 rows give `Not_found`. |
| `get` | No row gives `Not_found`, and the event has `Failed Not_found`. `find` gives `None`. |
| The kinds | A read (`get`, `find`, the queries) with no reply gives `Connection_lost`. A write (`create`, `update`, `delete`, the bulk forms) gives `Outcome_unknown`. A typed statement is not raw: it does not mark its connection dirty, thus it needs no session reset. In a `Tx`, it counts as DML. |

Nobody verified that one multi-row insert gets consecutive ids, so the children go in one row at a time.

Queries with `include_` (phase 4d, the plan of phase 4, E7, E8 and E17): after the main `SELECT` of `to_list` or `first`, one more `SELECT` for each relation of `include_`, in order, with `IN (...)` of the keys of the rows. The keys are unique on the client (`Value.equal`), in chunks of 1000. No key gives no query.

- A has_many takes the children of the keys, in the order of the key of the child (`ORDER BY` the key), and groups them by the key column of the child: the column of the one `belongs_to` of the child to the parent (rule 9), through its field, because `[@column]` can change the column. A row with no child gets `Loaded []`.
- A belongs_to takes the parent of each key. A nullable key that is `None` gives `Loaded None`. A key whose parent row is gone (another session deleted it between the main query and the query of `include_`) leaves the relation `Unloaded`, with no error.
- `include_` never fills the inverse relation. Each query of `include_` takes the lock clause of its main query (`FOR UPDATE`, or `FOR SHARE` with the rule of the dialect), so that a locking main query and its children see the same rows. The commands are reads (`Not_write`): a lost reply gives `Connection_lost`.
- The isolation (E17). Cistern sets no isolation level: the level is the one of the server (the default is REPEATABLE READ on the three servers). With REPEATABLE READ, a plain read makes one snapshot for the rest of the `Tx`, thus the plain queries of `include_` in a `Tx` see the rows of that snapshot. With READ COMMITTED, each query sees the rows committed before it, thus the main query and the queries of `include_` are not one snapshot. A locking read (`for_update`, `for_share`) is not part of the snapshot: it reads the newest committed rows and locks them. On MariaDB 11.8 (`innodb_snapshot_isolation`, its default), a locking read of a row that another session changed after the snapshot gives 1020: `Retryable `Rolled_back`, and the server rolled back the whole `Tx` (claim C37, and the rows of 4e). Outside a `Tx`, each query of `include_` runs in its own autocommit transaction, thus the queries are not one snapshot. Because of this, a belongs_to whose parent another session deleted between the main query and the query of `include_` stays `Unloaded`.
- The statement has the flag `multi` in its meta: each command emits its own event (the redacted SQL of a chunk has one `?` for each key), during the hold, and the statement emits no event of its own, in `Db.exec` and in `Tx.exec`. A failure before the first command (the checkout, the margin, a failed reset, a cancel in the wait) gives one event with the SQL of the statement. In a `Tx`, the error of a command goes through the rules of `Tx.exec`. `Stmt.to_sql` shows the keys as `<keys>`.
- `count` and `exists` ignore `include_`. `Query.update` and `Query.delete` with `include_` raise `Invalid_argument`.

Atomic statements (phase 4b1, the plan of phase 4, E4, E5 and E7): a statement of several commands that must all commit or all roll back (the nested create, from phase 4c1) has the flag `atomic` in its meta. `Db.exec` runs it through the code of `Db.transaction`, with no second path of transactions:

- `START TRANSACTION`, the commands, and the shielded `COMMIT` with its reply table, the margin check before each command, and the holder rule of phase 1.5 at the end.
- The first error of a command ends it. A server error or `Decode` on a usable connection gives the `ROLLBACK`, then the error (also 1213 and 1020 of a command). An error of `START TRANSACTION` follows the begin rule, and an error of `COMMIT` the reply table (a 1213 of `COMMIT` gives no `ROLLBACK`).
- An `OK` of a command with `IN_TRANS` clear (an implicit commit) gives `Implicit_commit`, as in `Tx.exec`.
- A cancel, a deadline or a lost reply of a command aborts the connection (the `KILL` of a command in flight), and the server rolls back at the close: the cancel goes up, a deadline gives `Timeout` (the timer of the `Tx` or the flow deadline, whichever fires first), and a lost reply gives `Connection_lost` (not committed). A 1153 gives `Invalid_value` and a `LOCAL INFILE` request `Decode`, and they also abort the connection. As in `Tx.exec`, a cancel or a deadline in a command sends no `ROLLBACK` and emits no `ROLLBACK` event. A lost reply of `COMMIT` gives `Outcome_unknown `Commit`. An `OK` of `COMMIT` or `ROLLBACK` with `IN_TRANS` set makes the checkin abort the connection.
- The events: one `Query` event for each command, with its own redacted SQL, duration and outcome, sent through the emitter of the scope (`Exec.scope`), and the events of `START TRANSACTION` and of `COMMIT` or `ROLLBACK`. No joined event. A command after the first error has no event. The `sent` flag of a `Cancelled` event is of that command only.
- A value with no literal (`meta.text`) gives `Invalid_value` before the checkout, with one event and no transaction, as `Pool.exec` does. The checks of misuse come in the order of `Db.transaction`: the closed pool first, then the fiber of a `Tx`.
- In a `Tx` (phase 4b2, E6), `Tx.exec` runs an atomic statement through the savepoint code of `Tx.savepoint`: one counter (`cistern_sp_<n>`) and the same rules. `SAVEPOINT`, the commands, then `RELEASE SAVEPOINT`. Each command checks the margin of the statement deadline and emits its own event. The error of a command goes through the rules of `Tx.exec` (the table of classes in "Transactions"):
  - An error after which the server keeps the transaction (for example 1062 or 1406) gives `ROLLBACK TO SAVEPOINT` and the error, and the `Tx` continues.
  - An error that fails the `Tx` (1213, 1020 of MariaDB, `Implicit_commit`, a broken connection) gives no `ROLLBACK TO SAVEPOINT`. The statement and each later step give the first error of the `Tx`.
  - The margin of an earlier statement deadline fails only the statement (`ROLLBACK TO SAVEPOINT`), as in `Tx.exec`.
  - A failed `RELEASE SAVEPOINT` fails the `Tx`.
  - A cancel between `SAVEPOINT` and `RELEASE` fails the `Tx`. A cancel in a command breaks the connection, and the end aborts it. Between two commands the connection stays usable: the cancel goes up, and the end of `Db.transaction` aborts the connection; if the callback catches the cancel, the end sends `ROLLBACK`. In this case the first error of the `Tx` (each later step and the result of `Db.transaction`) is `Connection_lost`, also though the connection is healthy and goes back to the pool: it tells only "not committed", which is true.
- A failed nested create uses AUTO_INCREMENT values (gaps in the ids).

### OCaml 5.5 features

OCaml 5.5 has module-dependent functions and polymorphic parameters. Tests on 2026-11-06 showed:

- OCaml 5.5.1 compiles and runs the two features.
- ppxlib 0.38 uses the OCaml 5.2 AST inside. It keeps a module-dependent arrow type as an encoded extension (`[%ppxlib.migration.ptyp_functor_5_5 ...]`). A file with this type compiles through a ppxlib driver, and the result runs. A polymorphic parameter fits the 5.2 AST directly.
- ocamlformat 0.29.0 formats the two features.
- A review compiled a prototype of the generated code of draft 10 (model modules, namespace module, stage 3, the usage expressions). It compiled.

Use in Cistern:

| Feature | Place | Gain |
|---|---|---|
| Module-dependent functions | Generic model operations on `module type MODEL` (types `t`, `key`, `draft`; the schema, the encoders and the decoders): `Model.get : (module M : MODEL) -> M.key -> M.t Stmt.t`, and the same for `find`, `create`, `child`, `delete`, `where`; `Model.update : (module M : MODEL) -> M.t Update.t list -> M.t -> M.t Stmt.t` | Signatures without a value with four type parameters. The ppx makes less code. |
| Polymorphic parameters | Generic loops over the fields of a record (encode a record to literals, `check_schema`): `iter_fields : ('a. ('m, 'a) Field.t -> 'a -> unit) -> 'm -> unit`. Phase 2 has no user of it, thus it comes with its first user | No wrapper records `{ f : 'a. ... }` |
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

`Config.of_url` takes `mysql://` (`mariadb://` is an alias): user, password (percent-decoded), host, port (default 3306), database, `socket=<path>`, `ssl-mode`, `ssl-ca=<path>`, `ssl-fingerprint=sha256:<hex>`, `allow-cleartext=socket`, `kill=true|false` (default `true`). The rules of the URL:

- The scheme is in lowercase. The URL must not have a space or a control character: use `%20` and the other escapes. `+` stays `+`.
- In the URL, the characters `@`, `/`, `?` and `#` of the user and the password must be percent-encoded. The password can have `:`. A raw `@` in the database part is an error, because it is probably a part of a password that is not encoded.
- After the decode, no value (the user, the password, the database, the host, the socket path, the CA path) has a control character. A NUL ends a string in the handshake and in the socket address of weft.
- The host is an IPv4 address, a name, or an IPv6 address in brackets, with no zone id (`[fe80::1%25eth0]` is an error). `localhost` is TCP to 127.0.0.1, not the Unix socket (the MySQL client uses the socket for `localhost`). This is safe: TCP needs a TLS setting.
- An unknown key, a key two times, an empty value, or a fragment (`#`) is an error. The keys, `sha256:`, `socket`, `true` and `false` are in lowercase. The `ssl-mode` values have no case.
- `socket=` with a host or a port is an error. `socket=` refuses `ssl-ca`, `ssl-fingerprint` and each `ssl-mode` other than `DISABLED`: version 1 has no TLS on a Unix socket.
- `allow-cleartext=socket` is only for `socket=`. Thus the opt-in is a part of the socket address, and a TCP configuration cannot hold it.
- `ssl-mode=DISABLED` with `ssl-ca` or `ssl-fingerprint` is an error. `ssl-mode=REQUIRED` with `ssl-ca` is an error, because the CA would have no effect: use `VERIFY_CA` or `VERIFY_IDENTITY`. `VERIFY_CA` and `VERIFY_IDENTITY` need `ssl-ca` or `ssl-fingerprint` in a URL, because a URL has no authenticator.
- `Config.t` is abstract, so that no generic printer shows the password. No message of `of_url` has a part of the URL.
- Phase 1.1 parsed the TLS settings with these final rules, and refused each configuration that needs TLS until phase 1.4. Phase 1.4 has TLS ("Handshake").

The database is necessary: a URL without it gives an error, because the session reset needs it for `COM_INIT_DB`. `Config.of_url` gives `(Config.t, string) result`. `Config.make ?tls ?authenticator ?kill ~address ~user ~password ~database ()` takes the same values, with the same checks (one shared function). The rules of `?authenticator:X509.Authenticator.t` (phase 1.4):

- It needs TLS on TCP. A TCP address with an authenticator and no `tls` gives `VERIFY_IDENTITY`.
- `VERIFY_CA` and `VERIFY_IDENTITY` accept it in place of `ssl-ca` or `ssl-fingerprint`. With `ssl-ca`, or with `REQUIRED`, it is an error. With `ssl-fingerprint`, the two checks run.
- The check of the chain and of the name is the task of the authenticator. Cistern gives it the host or the IP address of the configuration under `VERIFY_IDENTITY`, and no name under `VERIFY_CA`.
- An authenticator alone never makes the session verified for the clear-text rule ("Authentication"), because it can accept each certificate (for example `X509.Authenticator.of_string "none"`). Version 1 has no flag to change this.
- A configuration with an authenticator holds a function: compare its fields, not the configuration with `=`. The CA file is read with `Weft.File`. The printers of `Config.t` and the `Connect` events never show the password.

`ssl-mode` uses the MySQL names:

| `ssl-mode` | TLS | Certificate check |
|---|---|---|
| `DISABLED` | No | None |
| `REQUIRED` | Yes | None, if the configuration has no `ssl-fingerprint`. Documented: no protection against an attacker in the middle. With `ssl-fingerprint`, the pin is checked. With `ssl-ca`, the configuration is an error |
| `VERIFY_CA` | Yes | The chain only, with `ssl-ca` or the authenticator. No peer name is set. |
| `VERIFY_IDENTITY` | Yes | The chain and the name: `~peer_name` for a host name (also `localhost`), `~ip` for an IP address. The name always comes from the host of the configuration, never from the resolved address |

`ssl-fingerprint=sha256:<hex>` pins the SHA-256 hash of the server certificate (`X509.Authenticator.cert_fingerprint`, x509 1.2.0). The pin is checked in each mode, also with a CA (the two checks run). It needs TLS, and it is a verified identity for the clear-text rule. The pin also checks the validity dates of the certificate: a pinned certificate that expired fails. `VERIFY_CA` with only a pin checks the pin alone, with no name. `VERIFY_IDENTITY` with only a pin also checks the name. Thus a pinned certificate with no host name (the certificate that MySQL makes) needs `REQUIRED` with the pin, the default of a URL with only `ssl-fingerprint`. Each check gets the time of the wall clock of weft (`Tls_setup.now`). If the time is not known, each check fails, because x509 checks no time then.

The configurations that are verified for the clear-text rule (`Tls_setup.make`, with a unit table for each row):

| Configuration | Check of the certificate | Verified |
|---|---|---|
| `REQUIRED` | None | No |
| `REQUIRED` with a pin | The pin, no name | Yes |
| `VERIFY_CA` with `ssl-ca` | The chain, no name | No |
| `VERIFY_CA` with a pin (and maybe `ssl-ca` or an authenticator) | The pin (and the chain), no name | Yes |
| `VERIFY_CA` or `VERIFY_IDENTITY` with only an authenticator | The authenticator | No |
| `VERIFY_IDENTITY` with `ssl-ca` | The chain and the name | Yes |
| `VERIFY_IDENTITY` with a pin (and maybe `ssl-ca` or an authenticator) | The pin and the name (and the chain) | Yes |

Defaults:

- A Unix socket: no TLS.
- TCP (also on the loopback address) with `ssl-ca` or an authenticator: `VERIFY_IDENTITY`.
- TCP with `ssl-fingerprint`: TLS with the pinned certificate.
- TCP without them: `Config.of_url` gives an error, unless the URL gives `ssl-mode` explicitly.
- The certificate that MySQL 8.4 makes (`auto_generate_certs`) has the name `MySQL_Server_<version>_Auto_Generated_Server_Certificate` and no host name, so `VERIFY_IDENTITY` refuses it. MariaDB 11.4 makes a self-signed certificate. The documentation tells users to make a server certificate with the host name, or to pin the certificate with `ssl-fingerprint`.

### Handshake

1. Read the server greeting (protocol v10): capabilities, the authentication plugin, the 20-byte scramble, and the MariaDB extended capabilities when the `CLIENT_MYSQL` bit is clear. The greeting decides only the framing and the capabilities.
2. Client capabilities: `CLIENT_PROTOCOL_41`, `CLIENT_SECURE_CONNECTION`, `CLIENT_PLUGIN_AUTH`, `CLIENT_DEPRECATE_EOF`, `CLIENT_FOUND_ROWS`, `CLIENT_TRANSACTIONS`, `CLIENT_CONNECT_WITH_DB`, `CLIENT_MULTI_RESULTS`, and `CLIENT_SSL` with TLS (phase 1.4), each only if the server has it. A server with no `CLIENT_PROTOCOL_41`, `CLIENT_SECURE_CONNECTION`, `CLIENT_PLUGIN_AUTH`, `CLIENT_DEPRECATE_EOF` or `CLIENT_CONNECT_WITH_DB` gives `Connect`. A greeting of another protocol also gives `Connect`. The first auth response uses the plugin of the greeting if it is one of the two plugins. Else it uses the native scramble with the name `mysql_native_password`, and the server then switches. Not set: `CLIENT_MULTI_STATEMENTS`, `CLIENT_LOCAL_FILES`, `CLIENT_SESSION_TRACK`, `MARIADB_CLIENT_PROGRESS`.
3. Character set byte in the handshake: 45 (`utf8mb4_general_ci`), which exists on the two servers.
4. TLS: the client sends the SSL request packet (sequence id 1). It is the first 32 bytes of the handshake response, from the same function (`Message.write_ssl_request`), thus the capabilities, the character set and bytes 28 to 31 (the MariaDB extended capabilities, 0) are the same in the two packets. Then the client starts `Weft_tls.client`. The reader must have no buffered bytes at this point. If it has, the client fails with `Connect "bytes from the server before the TLS handshake"`: the server must wait for the ClientHello (verified on the three servers, probe T1 of the plan of phase 1.4), thus a byte there is a sign of an attack, and the session would read it as if it came over TLS. After the handshake, the client reads and writes only the TLS flow. The handshake response has sequence id 2, and the first reply of the authentication sequence id 3. A server without `CLIENT_SSL` gives `Connect "the server has no TLS"`. A failure of TLS gives `Connect "TLS: <cause>"`, escaped, with its first and its last 100 bytes (`Wire.peer_text_ends`): x509 prints the whole certificate, then the cause (the name that the certificate does not have, or the two fingerprints). The CA file of `ssl-ca` is read at each connect, before the DNS resolve and inside the limit of 10 s: a file that is not regular or is larger than 1 MiB is refused, a missing file and a path through a file give short messages, and a file with no valid certificate gives a fixed message with no part of the file. MariaDB does not check that bytes 28 to 31 of the two packets are the same (probe T9): the rule is Cistern's.
5. The generator: `Db.with_db` (before any fiber) and `Connection.connect` (before the network) raise `Invalid_argument` for a configuration with TLS if no default generator of `mirage-crypto-rng` is set. A program calls `Mirage_crypto_rng_unix.use_default ()` one time: this generator is safe on many domains, and a Fortuna generator is not (the pool connects from fibers on many domains). The tests of the simulator use a seeded Fortuna: `Weft_sim` runs all virtual domains on one OS thread.

### Authentication

| Plugin | Method |
|---|---|
| `caching_sha2_password` | Fast path: the SHA-256 scramble. The server answers `0x01 0x03` and then an OK packet. Full path: the server answers `0x01 0x04`, and the client sends the password in clear text. |
| `mysql_native_password` | The SHA-1 scramble. |

The clear-text password of the full path goes only:

- under `VERIFY_IDENTITY` with `ssl-ca`;
- with a pinned certificate (`ssl-fingerprint`), in each mode;
- on a Unix socket with `allow-cleartext=socket`.

An authenticator of the program alone is not enough ("Configuration", the table of the verified configurations).

The socket rule is an explicit opt-in for every socket. A check of the path is not safe: on macOS `/tmp` is a link to `/private/tmp`, and other directories (for example `/dev/shm`) are also writable by all users. While the server is down, a local user can make a socket at such a path. The client does not have the owner of the directory (`Weft.File.stat` gives `perm` but no owner), so it cannot check the directory.

In other cases the client fails with `Connect "the server asks for full authentication; use VERIFY_IDENTITY with ssl-ca, a pinned certificate (ssl-fingerprint), or allow-cleartext=socket"`. Thus a server or an attacker that answers `0x04` cannot get the password. With `DISABLED` over TCP, the first connect after each server restart needs the full path, so it fails with this message (documented).

- One function, `may_send_cleartext`, decides at the one place that writes the password: a Unix socket with `allow-cleartext=socket` (the opt-in is a part of the socket address), or a TLS session that is verified (`VERIFY_IDENTITY` with `ssl-ca`, or a pin: the table of "Configuration"). An authenticator of the program alone is not verified, because it can accept each certificate. The password goes with a NUL at the end. When the gate refuses, the connection closes and nothing more goes.
- `AuthMoreData` is accepted only for `caching_sha2_password`, and only `0x01 0x03` (then an OK) and `0x01 0x04` (then the gate). Another byte, a public key, or `0x04` after `0x03` gives `Connect`.
- An empty password gives an empty auth response for the two plugins.
- Note for `ssl-mode=DISABLED`: over plain TCP, the two scrambles let a passive listener attack the password offline, and the OK of MariaDB after the authentication holds a hash of the password hash. Under `REQUIRED` with no pin, an active attacker in the middle gets the scramble of the fast path, with the same risk. MariaDB 11.4 and later can verify a self-signed certificate of the server with a hash that depends on the password. Cistern does not do this in version 1.
- `AuthSwitchRequest`, at most one, to one of the two plugins: the client uses the first 20 bytes of the new plugin data (20 bytes and a NUL). Another length fails. A switch to another plugin gives `Connect "unsupported authentication plugin <name>"`. A name that is not 1 to 64 printable ASCII bytes is escaped and cut (`Wire.peer_text ~limit:64`). The text of a TLS failure in a `Connect` message keeps its first and its last 100 bytes, each part escaped in the same way (`Wire.peer_text_ends`, phase 1.4): x509 prints the whole certificate of the peer, which an attacker makes, and puts the cause at the end. For example, MariaDB `root` uses `unix_socket` by default.
- The integration tests run with TLS (`VERIFY_IDENTITY` with the test CA, and `ssl-fingerprint`) or a Unix socket with `allow-cleartext=socket`, and one test empties the authentication cache first.

### Session

Three round trips after the authentication (the third only with `kill=true`):

1. `SELECT CONNECTION_ID(), @@version, @@version_comment, @@hostname, @@port, @@server_id, @@wait_timeout, @@innodb_rollback_on_timeout, @@max_allowed_packet`. All names exist on the two servers. The dialect comes from `@@version` and `@@version_comment` (`Dialect.of_version`): the text `mariadb` in one of them, with no regard to case, gives MariaDB, else MySQL. The pinned MySQL build has the comment `Source distribution`, thus the comment cannot identify MySQL. `@@version` has no `5.5.5-` prefix on the pinned servers. A version that does not start with `<major>.<minor>.<patch>` gives `Connect`.
2. `SET NAMES utf8mb4 COLLATE <collation of the dialect>, time_zone = '+00:00', sql_mode = 'STRICT_ALL_TABLES,NO_BACKSLASH_ESCAPES,NO_ZERO_IN_DATE,NO_ZERO_DATE,ERROR_FOR_DIVISION_BY_ZERO,NO_ENGINE_SUBSTITUTION', autocommit = 1, completion_type = 'NO_CHAIN'`. The `sql_mode` is a complete value, because some global modes break Cistern (for example MariaDB `EMPTY_STRING_IS_NULL`). `completion_type` is set because a global `CHAIN` makes each `COMMIT` start a new transaction, and its OK then has `SERVER_STATUS_IN_TRANS` set (claim C26), and a global `RELEASE` ends the session after each `COMMIT`.
3. Identity (phase 1.5b, only with `kill=true`), in one `SELECT` that the dialect chooses (with no `CLIENT_MULTI_STATEMENTS`, one round trip has one statement). It is one statement on one session, thus a multiplexing proxy cannot mix two sessions in it (review A of the plan of 1.5, M2):
   - MySQL: `SELECT CONNECTION_ID(), @@server_uuid, UNIX_TIMESTAMP() - (SELECT VARIABLE_VALUE FROM performance_schema.global_status WHERE VARIABLE_NAME = 'Uptime'), <grantee rows>, <admin rows>, <enabled roles>`.
   - MariaDB: `SELECT CONNECTION_ID(), @@server_uid, @@hostname, @@port, @@server_id, UNIX_TIMESTAMP() - (SELECT VARIABLE_VALUE FROM information_schema.GLOBAL_STATUS WHERE VARIABLE_NAME = 'UPTIME'), <grantee rows>, <admin rows>, <enabled roles>`. `@@server_uid` is a system variable that any user reads.
   - The grantee `g` is `CONCAT('''', LEFT(CURRENT_USER(), CHAR_LENGTH(CURRENT_USER()) - CHAR_LENGTH(SUBSTRING_INDEX(CURRENT_USER(), '@', -1)) - 1), '''@''', SUBSTRING_INDEX(CURRENT_USER(), '@', -1), '''')`: the host is the text after the last `@`, because a user name can have `@`. The `GRANTEE` text has no escape for a quote. `<grantee rows>` is `(SELECT COUNT(*) FROM information_schema.USER_PRIVILEGES WHERE GRANTEE = g)`, `<admin rows>` the same with `AND PRIVILEGE_TYPE IN ('SUPER', 'CONNECTION_ADMIN')` (`'CONNECTION ADMIN'` on MariaDB), and `<enabled roles>` is `(SELECT COUNT(ROLE_NAME) FROM information_schema.ENABLED_ROLES)` (MariaDB gives one row with `ROLE_NAME` NULL when no role is enabled).
   - In one statement, `UNIX_TIMESTAMP()` and `Uptime` come from the same start time of the query, so the difference is exactly the start time of the server (verified in round 5 in MySQL 8.4 `sql/mysqld.cc`, `show_starttime`, and in the MariaDB 11.4 source). Thus the identity check uses exact equality, with no tolerance.
   - Any user can read `performance_schema.global_status` on MySQL 8.4 (verified in round 5: `table_global_status.cc` uses `pfs_truncatable_world_acl`). With `performance_schema = OFF`, the query fails, and `KILL` goes off (fail safe).
   - The result is `info.identity`: `Checked identity`, or `Refused reason`. None of these fails the connect. The checks, in this order: a server error or a row that does not decode gives `Identity_failed`; a `CONNECTION_ID()` that is not the one of round trip 1, or whose low 32 bits are not the thread id of the greeting (4 bytes), gives `Proxy`; no row of the grantee gives `Grantee_unknown` (the form of `g` does not match the server: fail safe); a direct grant of `SUPER` or of the admin privilege gives `Privileged`; an enabled role gives `Role`, because `USER_PRIVILEGES` does not show the privileges of a role.
   - The first `Refused reason` of a pool turns `KILL` off for the pool, with one event `Kill_off {reason}` (an `Atomic` flag and a compare-and-set, also across domains). Not verified: that each common proxy (ProxySQL, MaxScale, MySQL Router) gives a difference of the ids. `kill=false` stays the documented setting for each proxy that multiplexes or changes sessions.
   - The rules for the operator: a cloned MySQL data directory must get a new `auto.cnf`, else two servers have one `@@server_uuid`. MariaDB servers must have a unique `@@server_id` or `@@hostname`, because the defaults (server id 1, a fixed container host name, port 3306) are often the same. `@@server_uid` is different for two servers on one host, but containers on different hosts can have the same value (not verified), thus the identity keeps the host name, the port and the server id. The pool user must not have `CONNECTION_ADMIN` or `SUPER`. On MySQL, `activate_all_roles_on_login = ON` or `mandatory_roles` gives an enabled role to each session, thus `KILL` goes off for each pool on that server, with `Kill_off Role`. Else use `kill=false`.

The rules of the connect:

- One timeout of 10 s covers the DNS resolve, the connect, the greeting, the authentication and the round trips of the session.
- A host name goes to `Weft.Dns.resolve`, and the client tries the addresses in order. It tries the next address only after a network error. Thus a first address that accepts and never answers uses all of the 10 s.
- An IP address and `localhost` go to the connect with no resolve.
- Each failure of the connect gives `Connect msg`. When a server error caused it, `msg` has the error number. It does not have the server message, which can hold a user name.
- After a failure of the transport, a bad packet, or a server error that ends the session, the connection is broken. Each later call gives `Connection_lost` before any I/O, and `close` sends no `COM_QUIT`.

The identity of a connection is (`@@server_uuid` on MySQL, or (`@@server_uid`, `@@hostname`, `@@port`, `@@server_id`) on MariaDB; the start time). `@@server_uuid` stays the same after a MySQL restart, so the start time is always part of the identity. With the exact start time, (identity, `CONNECTION_ID()`) names one session. Thread ids are not used again in one server lifetime before they wrap (believed, not verified). Thus version 1 has no `PROCESSLIST` check before `KILL`.

Session reset. At checkin, the client marks the connection "dirty" if it ran a raw statement. If the last OK packet had `SERVER_STATUS_IN_TRANS` outside a `Tx`, the connection has an open transaction with its locks: the pool aborts it at the checkin. After an ERR packet, the flag can be old (an ERR has no status), thus this rule is conservative. The checkin does no I/O. At the next checkout of a dirty connection, before the statement and under the deadline of that caller, the client runs:

1. `COM_RESET_CONNECTION`. It sets all session variables back to their global values: `time_zone` becomes `SYSTEM`, `sql_mode` the global mode, and the collation the default of the server. It removes the user variables, and it rolls back an open transaction (its OK has `IN_TRANS` clear).
2. Step 2 again (the complete `SET`), because of these changes.
3. `COM_INIT_DB <database of the configuration>`, because a reset keeps the database of a raw `USE` (seen on the pinned servers in the probes of the review of phase 1.1; claim C3 tests it).

`Connection.reset` (phase 1.1) runs the three steps, and it is the only reset that `cistern.mysql` exports. `COM_RESET_CONNECTION` alone is only for the tests. If the reset fails, the pool aborts the connection, and the checkout takes another connection. No statement was sent, so this is safe. After a second failed reset in one `exec`, the `exec` gives the error of the second reset. A reset that passes the deadline gives `Timeout`. A cancel in the reset aborts the connection, and its event tells `sent=false`.

The reset is at the next checkout, not at the checkin. Thus an idle dirty connection keeps the session state of its raw statement until then. A raw `LOCK TABLES` or `GET_LOCK` sets no status flag, thus the pool does not abort its connection. Its lock stays on the idle connection until the next checkout (the reset releases it) or the idle limit. Documented: release these locks in the same raw statements. A raw `BEGIN` through `Db.exec` has no effect: its OK has `SERVER_STATUS_IN_TRANS`, thus the checkin aborts the connection at once. A transaction of more than one statement needs `Db.transaction` (phase 1.3). Each raw statement costs a reset (three round trips) at the next checkout of its connection. A successful reset makes the entry clean, thus a statement that is not raw (phase 2) after it needs no reset. Version 1 does not use session state tracking, because the typed API does not change session state.

### Commands and packets

- `COM_QUERY` (with a loop while `SERVER_MORE_RESULTS_EXISTS` is set), `COM_PING`, `COM_RESET_CONNECTION`, `COM_INIT_DB`, `COM_QUIT`.
- A result header of `0xFB` (a `LOCAL INFILE` request) closes the connection with `Decode`.
- The results of a `COM_QUERY` (`Connection.query` and `Connection.exec`):
  - `query` gives the first result set, and reads the other results to the end. A statement with no result set gives no columns and no rows.
  - `exec` reads all the results, and gives the counts of the last OK.
  - An ERR in any part is the result, and the parts before it are dropped.
  - A value that does not decode gives `Decode` after the client read the whole reply. The connection stays usable.
- The client checks the payload rule ("Limits and defaults") and the literals of `Raw` before it sends a byte. A failure gives `Invalid_value`, and the connection stays usable.
- `Connection.status` gives the status flags of the last OK packet (for example `SERVER_STATUS_IN_TRANS`).
- The values of a text result set (`Column`). The type code, the `UNSIGNED` flag (0x20) and the character set of the column definition choose the `Value.t`. Only the character set 63 (binary) has a meaning.

  | Type code | `Value.t` |
  |---|---|
  | SQL `NULL` in any column, and 6 (`NULL`) | `Null` |
  | 1, 2, 3, 9, 13 (`YEAR`), and 8 (`BIGINT`) signed | `Int`. `TINYINT(1)` is an `Int`, because `Value.t` has no bool |
  | 8 `UNSIGNED` | `Int` up to `Int64.max_int`, else `Decimal` |
  | 4 (`FLOAT`), 5 (`DOUBLE`) | `Float`. The server sends a `FLOAT` with about 6 significant digits (3.4028234e38 comes back as `3.40282e38`), thus use `DOUBLE` for exact values |
  | 0, 246 (`DECIMAL`) | `Decimal` |
  | 7 (`TIMESTAMP`), 12 (`DATETIME`) | `Datetime`. A zero date gives `Decode` |
  | 10 (`DATE`), 11 (`TIME`), 14 | `Text` |
  | 16 (`BIT`), 255 (`GEOMETRY`) | `Bytes` |
  | 245 (`JSON`) | `Text`, for each character set (MySQL sends 63) |
  | Each other code | `Bytes` with the character set 63, else `Text`. An unknown code never gives an error |

  `Text` must be valid UTF-8, else the value gives `Decode`. The message names the column and the expected type, never the value.
- A result set is the column count (1 to 65535, alone in its packet), the column definitions, the rows (`Resultset` keeps them as `string option array`), and the terminator OK. A row cannot start with `0xFE`, because its first value would need a packet of 16 MiB. An ERR after the columns or after some rows is the reply, and the rows before it are dropped. An error of the reader tells if it came after the column count: the server then ran the statement. The end of a result set is decoded only as a terminator (`0xFE`), and an OK only as `0x00`, so that an `AuthSwitchRequest` (`0xFE`) is never an OK. The reader keeps all the rows of a result in memory, with no limit of the number of rows: the deadline of phase 1.2 bounds the time of a read.
- At checkout, the client calls `read_now` on the flow (not `readable`, because a TLS record with no data makes `readable` true). If the stream ended, or a packet came that the client did not ask for (for example error 4031, or 1053, or 1927), the client closes the connection and takes another one. No command was sent, so this is safe. `Connection.stale` is this check: it is true if the reader holds bytes, or `read_now` gives bytes or the end of the stream, or raises `Flow.Closed`, `Flow.Timeout` or `Weft.Net.Error`. On TLS: a record with no data (the two session tickets that each server sends after the handshake, probe T7) gives no bytes, thus it is not stale. The servers end a session with no `close_notify` (probe T8), and a TLS read then raises `Flow.Closed`: stale. A part of a record gives no bytes: then the next command gets R-SEQ0 or the end, and gives `Connection_lost`. Each other exception of `read_now` (a cancel at the lock of the TLS session) breaks the connection, then goes up. A stale connection is broken. This applies only at checkout. In a `Tx`, or between the operations of one statement, the same condition gives `Connection_lost`.
- If the stream ends after a write, the write was sent: a write gives `Outcome_unknown`, and a read is tried one more time on another connection (only outside a `Tx`).
- `Weft_flow.Writer` with packet frames (3-byte length and sequence id). One flush for each round trip. `Weft_flow.Reader` with `ensure` and integer reads. The packet layer reads the 4 bytes of the header, consumes them, then reads the payload: an `ensure` of the header and a payload near 16 MiB would raise `Reader.Too_large`. The writer has a maximum frame of `0xFFFFFE` bytes and the header.
- The server must have `CLIENT_PROTOCOL_41`, `CLIENT_SECURE_CONNECTION`, `CLIENT_PLUGIN_AUTH` and `CLIENT_DEPRECATE_EOF` (the three pinned servers have them). Thus no message is a classic EOF packet: the end of a result set is an OK packet that starts with `0xFE`, with the layout of an OK (the status flags before the warnings). The handshake response has 0 in the MariaDB extended capabilities, because `MARIADB_CLIENT_EXTENDED_METADATA` and `CACHE_METADATA` change the column definitions. The OK decoder ignores the bytes after the warnings (an info text, or the 66 bytes of MariaDB after the authentication). Affected rows and the last insert id are unsigned 64-bit values: a value above `Int64.max_int` is a negative `int64` with the same 64 bits.
- The sequence ids. Each command starts at 0 (the client), and each packet adds 1, modulo 256 (a result of 300 rows wraps). The authentication continues from the greeting (0) to the final OK, also across an `AuthSwitchRequest` and `AuthMoreData`. A reply packet with another id breaks the connection. An ERR with the id 0 where the first packet of the reply (id 1) is expected came before the command (rule R-SEQ0, for example 4031 at the idle timeout): see "Order of the error rules". Later in a reply (after the column count, or during the authentication), an ERR with the id 0 is a sequence error.

## Concurrency with weft

### Pool

- `Pool_state` (`cistern/pool_state.ml`) is pure: a persistent value, no effects, no I/O, and no operation raises. A time is a span from the origin of the pool, and no operation assumes that the time grows from one operation to the next (two fibers can read the clock in one order and run their operations in the other order). The `Random.State.t` of the jitter is not in the state: the shell draws the jitter under its mutex and gives it to `opened`.
- The slots: the idle entries (a stack, the last checkin first), the entries in use (by a fiber, or given to a waiter), the running connects, and the entries that wait for a polite close by the opener (`closing`, in two parts: to close and in close). The `Killing` slots (phase 1.5a): the slot of a connection that the shell aborted while its statement can still run on the server (see "Cancel"). Each keeps its slot, thus `idle + in_use + opening + closing + killing <= pool_size` always.
- The operations give the hand-offs as data, and a flag `wake`. `wake` is true if the opener has work (a connect to start, or an entry to close), or a timer to arm (`next_timer` gives a time), because the opener reads its timer only in a tick. The operations:
  - `checkout`: `Conn`, `Wait id` or `Closed`. All idle entries past their life or their idle limit move to `closing`, also those under a fresh entry of the stack. Then the fresh entry at the top goes out.
  - `cancel_wait`: it tells if the waiter was in the queue, and gives the stored connect error and `wake` (the removal of an expired `Killing` slot can let another waiter get a connect).
  - `checkin`: `Give` to the oldest waiter, `Idle`, `Closing` past its life, `Freed` for a connection that the shell aborted first, or `Abort` on a closed pool.
  - `tick`: the connects to start (at most 2 running, none before the end of the backoff), the entries to close, and the timer.
  - `opened`: `Give`, `Idle`, or `Close` on a closed pool. It clears the error and the backoff. It cuts the jitter `u` to −0.15 to 0.15, and NaN gives 0.
  - `open_failed`: it stores the error. The backoff is 100 ms, doubled, at most 5 s, and its end is a maximum.
  - `open_aborted`, `closed_done` and `close_all`.
  - The demand of connects is the number of waiters minus the running connects.
- A pure state sees the time only when an operation runs. Thus `next_timer now` is the earliest `t` later than `now` at which `tick`, with no other operation, starts a connect. The candidates are the end of the connect backoff and the age limits of the `Killing` slots. Thus an age limit before the end of the backoff gives the end of the backoff. If a `tick` at `now` starts a connect, `next_timer` gives `None`: that tick gives the timer of its new state. `next_timer` runs under the mutex after each operation, thus it has a closed form in O(k) for k `Killing` slots: `None` if the pool is closed, no waiter needs a connect, or two connects run; else, with a free slot, the end of the backoff if `now` is in the backoff; else the later of the end of the backoff and the earliest age limit (the free slots only grow with the time). The model of the stateful suite finds the timer with a search, as a check of this form. Without it, a waiter can wait for a slot that is already free; with a time in the past, the opener would spin.
- The stateful suite (`test/stateful`) compares `Pool_state` with a model (`test/stateful/model.ml`) after each operation, and checks the invariants: each entry that `opened` made in exactly one place (idle, one holder, to close, in close, or out of the state), the slot bound with the `Killing` slots, a unique token for each `Killing` slot, a token freed at most one time, no idle entry while a fiber waits, progress (a connect starts now or at the timer), the timer, no hand-out of an entry past its life or its idle limit, and the range of the jitter. The model derives the connects, the timer, `wake` and the checkout by another method than the code (an enumeration, a search, a tick of the model, a filter of the stack). Its free slots at a time count only the `Killing` slots whose age limit is later than that time. The commands `killing`, `killed` and the advances to an age limit (and a time before it) reach the expired slots, a checkout after an expired slot, and a timer at an age limit. The test also models what the opener knows (its doorbell, and the timer of its last tick), and requires that the opener knows each connect to start, each entry to close and each timer.
- The default run of the stateful suite (`dune runtest`, the mutation gate) uses one fixed root seed, thus a fixed list of 1000 cases that meet every cover demand. With `CISTERN_SLOW` set, the run uses a random root seed, and a test with the tag `slow` runs 10000 more cases.
- Each `Killing` slot has a unique token. `killing now e` takes the entry `e` from "in use", keeps its slot with a new token, and gives the token. The age limit of the slot is `now` + 30 s. `killed tok` frees the slot of `tok` and tells if the slot was there. It does nothing for a slot that already passed its age limit, or that `killed` already freed. Thus a slot is never freed two times, and the pool never has more than `pool_size` sessions from slots. `close_all` keeps the `Killing` slots: the drain frees them.
- A `Killing` slot past its age limit is free for each operation at its `now`: each operation first removes the `Killing` slots whose age limit is at or before its `now`. Thus a checkout long after the age limit, with no operation between, sees the free slot, and its `wake` starts a connect (review C of the plan of 1.5, H1).
- A stdlib `Mutex` protects `Pool_state`. No effect happens while the mutex is held (weft `docs/design.md` §2.5, locks).
- Order in the weft shell (`cistern/pool.ml`), for each operation: (1) Read the clock, outside the mutex. (2) Take the mutex, run the `Pool_state` operation (and draw the jitter for `opened`), and release the mutex. (3) The seam of the tests (`Pool.Private.after_op`, which does nothing by default). (4) The effects in this order: the fills of the waiters (a checkin on a closed pool aborts its entry at this step), the doorbell of the opener, the abort of a connection that `opened` gives on a closed pool (`Opened_close`), and the events. An abort of the holder rule comes before the operation. The holder sets the flow deadlines at the checkout, before its first wait. The deadline stays on the connection at the checkin: the next holder sets its own, and the `COM_QUIT` of a polite close waits only with a full send buffer.
- The holder rule: an entry that its holder cannot give back as good (stale, a failed reset, a cancel or another exception, a connection that is not usable, `SERVER_STATUS_IN_TRANS` outside a `Tx`) is aborted before the operation that frees its slot. Thus the client never has more sockets than `pool_size`. The shell counts its sockets. A test counter (`over_bound`) counts the opens that find `pool_size` sockets, and the tests require 0.
- `Pool.with_conn` (internal) is the checkout and the checkin around a function on the held connection: one statement (`Db.exec`), or a transaction (phase 1.3). The steps before the function (a waiter at its limit, the margin, a failed reset, a cancel in the wait or in the reset) come back as data, and the function gives its outcome as data (a value, an error, a user exception with its backtrace, or a cancel), with its pending event and the end of the entry (back as good, or discarded). The caller emits the event after the checkin, then gives the result or raises. Only a bug of Cistern raises out of the function: then the entry is discarded.
- A waiter is an `Ivar` that holds `Got of conn | Failed of Error.t | Abandoned`. `Ivar.try_fill` decides who wins:
  - Checkin pops waiters until `Ivar.try_fill w (Got c)` succeeds. If none succeeds, it pushes `c` on the idle stack (LIFO).
  - After a timeout or a cancel, the waiter calls `Ivar.try_fill w Abandoned`. If that fails, it takes `c` from `Ivar.peek w` and checks it in again. Only then does it give its error or raise `Cancelled`.
- Doorbells: `Fiber.Semaphore.create ~max:1 0` with `try_release`. `try_release` is not a cancellation point, so a cancelled fiber can ring it.
- Opener: a daemon in the `with_db` scope. It calls `tick`, starts the pending connects (at most 2 at a time) as plain `Fiber.fork` fibers in a nested scope, does the polite closes, and then syncs on `choose [ acquire doorbell; Time.at next_timer ]`. `wake` is also true when `next_timer` gives a time, because the opener reads its timer again only after a wake. A connect fiber uses the connect timeout of `Connection.connect`. It catches all exceptions except `Cancelled`, and calls `opened`, `open_failed`, or `open_aborted` (after a cancel, which only the end of the opener gives). It rings the doorbell when the operation gives `wake`. Because it catches the other exceptions, it needs no `fork_isolated`.
- No pre-open: `with_db` opens no connection before the first `exec`. Thus a connect error shows at the first `exec`, at its limit.
- A waiter waits until the earlier of its deadline and `checkout_timeout`. At this limit, it gets the stored connect error, else `Timeout` if its deadline passed, else `Pool_exhausted`, with one reading of the clock.
- Rule for every daemon of Cistern (the opener, the killer) and each connect fiber: a loop catches all exceptions except `Cancelled`. A `Cancelled` ends the loop. A catch-all that loops back to a `sync` after a cancel spins without end, because the cancel is sticky, and then the join of the scope never ends.
- Connect errors: the waiters keep waiting, and the opener tries again after the backoff. A waiter whose deadline passes while a connect error is stored gets `Connect msg`, not `Timeout` or `Pool_exhausted`. A successful connect clears the error.
- `max_lifetime` and the idle limit: see "Limits and defaults". An idle connection older than the idle limit is closed at checkout, not pinged.
- At each checkout (also of an entry that a checkin or an open gives to a waiter), `Connection.stale` examines the connection. The pool aborts a stale connection, and the checkout takes another, with no error. On TLS, the stale check can take the lock of the session, which raises a pending cancel: the pool takes it as a cancel at the checkout (`Cancelled_before`, with its event), and discards the entry. The stale check runs after the operation of the checkout, outside the mutex of the pool.
- At checkout, a dirty connection gets the session reset (see "Session") before the statement, under the deadline of the caller.
- The end of `with_db`: after the scope (the opener and its connects end first), the shell sets the closed flag, and `close_all` fails all waiters with `Failed Connection_lost`. The idle connections and the connections to close get a polite close, in `Fiber.shield` with one limit of 2 s for all of them. After the limit, each close runs in a cancelled fiber: a `COM_QUIT` that must wait raises at once, and the close closes the flow. A polite close never raises, and it closes the flow in each case. On TLS the close is the same (phase 1.4): `COM_QUIT` with the write timeout of 1 s, then the close of the flow, with no `close_notify` (the servers send none either). A cancel can come at the write lock of the TLS session before any byte, or in the TCP write. The two take the branch that closes the flow and raises, and the close of a TLS flow closes the TCP flow and never waits. A connection that comes back later is aborted at its checkin. Any later `Db.exec` raises `Invalid_argument "Db.t used after with_db"` before each other check, and emits no event.

### Deadlines

- `Db.exec` has one absolute `Mono` deadline from `?timeout` (default 30 s). `Tx.exec` uses the earlier of its `?timeout` and the transaction deadline.
- If the remaining time is less than the margin, `exec` gives `Timeout` and sends nothing. The pool examines it at the checkout of a dirty connection, before the reset, and again before the statement. The connection goes back as good. weft examines a deadline only in a wait, thus a deadline that passed does not stop a write: a writable socket takes the packet at once. Thus the pool runs the margin check immediately before the statement, with no wait between them.
- `Connection.set_deadline` sets the read and the write deadline of the flow (the pool sets them at the checkout, before the session reset). The pool does not clear them at the checkin, and this is safe: each holder sets its own deadline before its first wait, the stale check never waits, and the `COM_QUIT` of a polite close waits only with a full send buffer (then an old deadline makes the close abrupt). A read deadline that passes raises `Flow.Timeout` inside the wait, before bytes are read. A write deadline that passes can leave half a packet, and it fails a TLS flow. In the two cases the connection breaks, and the cancel steps follow.
- A write deadline gives `Timeout`. The writer raises `Flow.Timeout` only while it waits to write the rest of the packet, thus the server did not get the whole command. A server runs a command only when it has the whole packet (also the last frame of a payload of many frames), and the client closes the connection, so the server drops the part. The same holds for a TLS flow (phase 1.4).
- A read deadline gives `Outcome_unknown` for a write (the statement was sent), and `Timeout` for a read or a command that is not a write. The steps of the session reset are not writes, thus a read deadline there gives `Timeout`.
- PR 1a adds a keep-alive option to `Weft_flow.Tcp.connect` (weft has `Net.Keep_alive`). Cistern sets it, so that a half-open connection ends.

### Cancel

After a deadline, a fiber cancel or a lost reply in the middle of a statement, the connection state is not known. A close does not stop a running query on the server, and the query keeps its locks. Thus:

1. The client aborts the connection (`Connection.abort`: it sends nothing, never waits and never raises, so a cancelled fiber can call it). `Connection.close` writes `COM_QUIT`, and in a cancelled fiber that write raises `Cancelled` after the flow closes, thus `close` is not for a cancelled fiber.
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
10. The end of `with_db` (phase 1.5c1b): `with_db` is `match Fiber.scope body with v -> Pool.shutdown pool; v | exception e -> Pool.shutdown pool; raise e`. When the body ends, weft cancels the daemons, and the join of the scope waits for them. The join ignores cancels (weft `docs/design.md`, "Lifetime"), and a shielded try of the killer has its limit of 2 s, so the join ends. Then `shutdown`, in this order (review C of the plan, M1): the closed flag and `close_all` (the waiters fail at once with `Connection_lost`, and the `Killing` slots stay), then the drain, then the polite closes (2 s, also of the control connection). The drain runs in `Fiber.shield` with `Fiber.with_timeout` of 15 s. It uses the control connection and the state of the killer (its backoff and the token of a first failed try): the killer owns them while it runs, and the drain after the join, which gives the happens-before. It opens a control connection if none is open (with the backoff of the killer), and sends the entries with the rules of steps 5 to 7, until the queue is empty, also the entries that come during the drain (a holder of an outer scope, review C, M2). One mutex section ends it: with an empty queue, it sets `drained`; after the limit, it also drops the entries that remain (`Kill_dropped Drain_timeout`) and frees their slots. After `drained`, a kill request frees its slot at once with `Kill_dropped Closed`. The drain catches all exceptions, so `with_db` gives the result or the exception of the body. A try in its shield can pass the limit of the drain by at most 2 s. With `kill=false` the queue is always empty, and the drain opens nothing. This needs no stop step and no `Ivar`.
11. `Statement_timeout` (3024, 1969): the server stopped the statement. The connection is good. No close and no `KILL`.

Phase 1.1 to phase 1.4 ship only the `kill=false` behavior: abort the connection (step 1), and the slot is free at once. Phase 1.5 adds steps 2 to 10. Phase 1.5c1a has steps 2 to 9 as code:

- When a kill is necessary (the plan of 1.5, D1 and N2): `Connection.in_flight` is true when the last command flushed its packet and no whole reply came (no server error, no ERR before the command (R-SEQ0), no end of the reply). It is true only on a broken connection, and a command on a broken connection (no I/O) keeps it (review A of 1.5c1a, N1). At the end of a holder (`Pool.exec` and `Tx` alike), `with_conn` gives an entry back as good if it is usable and `IN_TRANS` is clear. Else, if a command is in flight, the entry goes to `kill`; else the abort and the free of the slot of 1.4. Thus a cancel after the flush, a read deadline, the end of the stream after the flush, and a `COMMIT` or `ROLLBACK` cut by its deadline get a `KILL`. A cancel before the flush, a server error, R-SEQ0, a failed reset (it does not reach the function), a stale entry and an idle transaction do not.
- `kill` (D4, D4a): the abort first (the holder rule). Then one mutex section reads `kill_off` and `drained`, takes the token (`Pool_state.killing`), and checks the bound (`pool_size` entries) and the duplicates of (id, identity). A full queue or a duplicate frees the slot in the same section (`Kill_dropped Queue_full` or `Duplicate`). With `kill=false`, a refused identity or `kill_off`, the slot is free at once with no event. After the shutdown took the queue (`drained`), the slot is free with `Kill_dropped Closed`. Then: the doorbell of the opener if `wake`, the doorbell of the killer, the events.
- `turn_kill_off` (D6a, review C2 M-a): the flag, the drop of the queue and the free of its slots in one mutex section, then the doorbell of the opener if `wake`, the doorbell of the killer (it closes its control connection), `Kill_off` and a `Kill_dropped Off` for each entry. The first refused identity (of the pool or of the control connection) and a 1095 call it.
- The killer (D5, D7 to D9): a daemon of `with_db`, only with `kill=true`. It opens its control connection at once, with the connect of the pool (also TLS) and its own backoff (100 ms, doubled, at most 5 s, again 100 ms after a success), with a `Connect` event. Before each entry: a control connection past its idle limit or stale is aborted and opened again. Each try runs in its own `Fiber.shield` with a limit of 2 s: the entry is still in the queue, the identity of the entry is the identity of the control connection, its id is not the id of the control connection (N1), then `KILL CONNECTION`, then for a final result `killed` and the removal of the entry by its token in one section (review C2 M-b), and the doorbell of the opener if `wake`. 1094 is a success (`Gone`). After a failure, the control connection is aborted and opened again outside the shields, then the second try runs; after the second failure, `Kill_dropped Failed`. A 1095 gives `Kill_dropped Not_owner` and turns `KILL` off. The events come after the shields, in the killer fiber.
- `abandon` rings the doorbell of the opener when `cancel_wait` gives `wake` (review C of 1.5a, L1).
- The end of `with_db`: see step 10.

### Transactions

`Db.transaction ?timeout db f` (`cistern/tx.ml`) holds one entry of the pool for the whole callback, through `Pool.with_conn`. `?timeout` (default 30 s) is one deadline for the checkout, `START TRANSACTION`, the callback and its statements. The rules are pure functions in `Tx_rules`, with one unit row for each row of the tables below.

- The fibers. `Tx.t` holds one connection and the id of the fiber of the callback (`Fiber.self`). `Tx.exec` and `Tx.savepoint` from another fiber, or after the end, raise `Invalid_argument`. `Db.t` keeps the set of the fibers that hold a `Tx` (`Set.Make (Fiber.Id)` in an `Atomic`). `Db.exec` and `Db.transaction` from such a fiber raise `Invalid_argument`, because they would wait for a second connection. One outer `match … with exception` removes the fiber from the set on each path. A child fiber of the callback is not in the set: its `Db.exec` waits for a second connection (with `pool_size = 1`, until `checkout_timeout`, then `Pool_exhausted`).
- The start. A cancel before the checkout raises `Cancelled` with no event. The steps before the function of `Pool.with_conn` (a waiter at its limit, the margin, a failed reset, a cancel) give an event with the SQL `START TRANSACTION`. `START TRANSACTION` comes right after the margin check of the checkout. Its OK must have `SERVER_STATUS_IN_TRANS` set, else the connection is aborted, and the result is `Connection_lost`. The session sets `completion_type = 'NO_CHAIN'` (claim C26), thus `COMMIT` never chains a new transaction.
- The callback runs in `Fiber.with_timeout` of the remaining transaction time, in the fiber of the caller (weft runs the body of `with_timeout` in the fiber of the caller).
- `Tx.exec`:
  - A failed `Tx` gives its first error, with no I/O and no event. Thus a write after a server rollback cannot run in autocommit mode.
  - The first-word check (`Raw.first_word`): a raw statement whose first word is `BEGIN`, `START`, `COMMIT`, `ROLLBACK`, `SAVEPOINT`, `RELEASE`, `XA`, `LOCK`, `UNLOCK`, `SET`, `EXECUTE` or `PREPARE` (any case), or that has no first word (a value, a comment or a parenthesis first, for example `(SELECT 1) UNION (SELECT 2)`), raises `Invalid_argument`. A raw `START TRANSACTION` commits the open transaction and sets the flag again (claim C2), so the flag cannot show it. A `CALL` of a procedure that commits is not detected (documented), and MariaDB permits `PREPARE … FROM 'START TRANSACTION'`.
  - The deadline is the earlier of `?timeout` and the transaction deadline. The margin check comes before the statement. A margin failure of an earlier statement deadline fails only the statement. A margin failure of the transaction deadline fails the `Tx`.
  - An OK with `SERVER_STATUS_IN_TRANS` clear (also at the end of a result set) means that the server committed (a raw DDL statement, `TRUNCATE`, `LOCK TABLES`): the `Tx` fails with `Implicit_commit`, and `Tx.exec` gives it. The writes before it are committed. It is never a `Retryable` error, so that the user does not run the committed writes again.
- The errors of a statement (an ERR has no status flags, thus the client's flags are old after an error):

| Class | Errors | `Tx` |
|---|---|---|
| Not usable | Any error after which the connection is not usable (for example `Decode` of a `LOCAL INFILE` request) | Fails, no ping |
| Fail | 1213, 1205 when `@@innodb_rollback_on_timeout` is on, 3101 (MySQL) and 1020 (MariaDB, phase 4a0), `Read_only`, `Timeout`, `Outcome_unknown`, `Connection_lost`, 1153 | Fails |
| Statement-only | `Constraint_violation`, `Invalid_value`, `Statement_timeout`, `Not_found`, `Decode` on a usable connection | Stays usable |
| Other | `Driver` (for example 1050, 1146, 1305), 1205 when `@@innodb_rollback_on_timeout` is off | `COM_PING`: `IN_TRANS` set keeps the `Tx`. `IN_TRANS` clear gives `Implicit_commit`: a failed DDL statement commits the transaction before it fails (1050, 1146 of `TRUNCATE`, 1051 on the pinned servers, claim C28). The result and the event then say `Implicit_commit`, and they do not show the error of the server (for example 1050). A later version can add it to the event |

- A cancel in a command of the `Tx` breaks the connection, and the server rolls back at the close. The `Tx` stores `Connection_lost` as its first error (`Outcome_unknown `Statement` for a sent statement that is not DML, and `Timeout` in place of `Connection_lost` if the transaction deadline passed, for a cancel by its timer), the event says `Cancelled`, and `Cancelled` goes to the callback. Only the cancel check at the end gives `Cancelled` from `Db.transaction`.
- `Tx.savepoint` and an atomic statement in a `Tx` (the nested create, phase 4b2) use `SAVEPOINT cistern_sp_<n>`, with one counter. The scope gets the same `Tx.t`. A failure of `SAVEPOINT` is a statement error (the table above). A failure of `RELEASE SAVEPOINT` or `ROLLBACK TO SAVEPOINT` (for example 1305) fails the `Tx`. A cancel in the scope fails the `Tx` with `Connection_lost` (phase 5a0), also when no command runs (a cancel of the code of the program in the scope, or at its end): no `ROLLBACK TO SAVEPOINT` goes, and the cancel goes up. If the callback catches it, each later step gives the error, and the end sends `ROLLBACK`. Before phase 5a0, such a cancel left the `Tx` usable, and a later `COMMIT` committed the part of the scope that ran.
- The end, after the callback, reads the time once (weft's `with_timeout` gives the value of a body that returns after its timer) and the cancel check. A do-not-retry first error is `Implicit_commit`, or an `Outcome_unknown` of a `Tx` with a statement that is not DML. The first row that applies:

| # | Condition | Commands | Result |
|---|---|---|---|
| 1 | The fiber is cancelled | Abort (the server rolls back an idle session when its connection closes) | `Cancelled` |
| 2 | The callback raised `x` that is not `Cistern_error` (also `Fiber.Timeout` before the deadline) | `ROLLBACK` if the connection is usable | raise `x` again |
| 3 | A do-not-retry first error | `ROLLBACK` if usable | that error |
| 4 | The callback raised `Cistern_error e` | `ROLLBACK` if usable | `Error e` |
| 5 | The deadline passed (a value or `Fiber.Timeout` after it) | `ROLLBACK` if usable | the first error (D5), else `Timeout` |
| 6 | A first error, the callback returned | `ROLLBACK` if usable | the first error (D5) |
| 7 | The `Tx` is good, a value before the deadline | `COMMIT` | the reply of `COMMIT` |

  D5: `Outcome_unknown `Statement` of a `Tx` whose writes were all DML (`INSERT`, `UPDATE`, `DELETE`, `REPLACE`, `SELECT`, `WITH`, `VALUES`, `TABLE`) becomes `Timeout` (a deadline) or `Connection_lost`: the client closed the connection before `COMMIT`, thus the server rolled back, and a retry is safe.

  The priority of row 3 changes the rule before phase 1.3, which gave `Error e` and the deadline `Timeout` in each case. A do-not-retry first error wins over them, because the writes before it can be committed, or the outcome of a statement that is not DML is not known. A program that gets `Timeout` or its own `Cistern_error e` can run the whole transaction again, and the committed writes then run two times: the case that `Implicit_commit` exists to prevent. A non-Cistern exception still goes up, because the program then sees an exception, not a result. The end of a savepoint uses the same rule (with no deadline row): a user exception (`ROLLBACK TO` if the `Tx` is good), a do-not-retry first error, `Cistern_error e` (`ROLLBACK TO` if good), the first error, else `RELEASE` and the value. An `Error` that `Tx.exec` gave and that the scope handled does not roll it back.
- `COMMIT` and `ROLLBACK` run in `Fiber.shield`. Their flow deadline is the remaining transaction time, at least 5 s and at most 30 s ("Limits and defaults"), so that the sent rule holds. An outer `Fiber.with_timeout` 1 s later bounds the shield. A `ROLLBACK` that fails aborts the connection and never changes the result. The replies to `COMMIT`:

| Reply | Result |
|---|---|
| OK, `IN_TRANS` clear | the value |
| OK, `IN_TRANS` set (not expected with `NO_CHAIN`) | the value, and the checkin aborts the connection |
| 1213, 1205, 1180, 3101 (MySQL), 1020 (MariaDB) | `Retryable `Rolled_back`. A ping reads the flags again, and the connection stays. The ping runs in the shield of the end, under its flow deadline, thus a cancel during `COMMIT` does not stop it. A failed ping breaks the connection, and the checkin aborts it |
| 1317 with the connection open | `Outcome_unknown `Commit`, and the connection is aborted. A `KILL QUERY` of a `COMMIT` that waits for a lock rolls back on the pinned servers (claim C27), but no fact covers each case of 1317 |
| 1053 (or 1927 on MariaDB) then the end, the end of the stream, a read deadline, the guard | `Outcome_unknown `Commit`, and the connection is aborted |
| 4031 with the sequence id 0 (MySQL), a flush that failed, a write deadline | `COMMIT` did not run, and the server rolls back at the close: `Connection_lost` or `Timeout` |
| Another error | `ROLLBACK`, and the error |

- The checkin after the end gives the entry back as good if the connection is usable and `IN_TRANS` is clear. It is dirty if a raw statement ran. The events of `COMMIT` and `ROLLBACK` come after the checkin. A `Tx` in a fiber of an outer scope can outlive `with_db`: it keeps its connection to its end, and the checkin aborts it (`with_db` does not own the socket of a holder that escaped).
- The cleanup uses `match ... with exception`, not `Fun.protect`, so that a failed `ROLLBACK` does not become `Fun.Finally_raised`.

### Shared state and the query hook

- No `Hashtbl` is shared between domains. The transaction set is an `Atomic` value. The jitter uses a `Random.State.t` of the shell (a split of `?random`), under the mutex of the pool.
- `on_query` gets events with a kind (`Query`, `Connect`, `Kill`, `Kill_dropped`, `Kill_off`), the redacted SQL, the duration, the number of rows and the error. Cistern catches its exceptions.
- An event holds no message of a server error packet and no value of a statement (phase 2a0). The error of a `Query` event and of a `Connect` event is redacted (`Error.redact`): the messages of `Constraint_violation`, `Invalid_value` and `Driver` are empty, because a message of the server can hold a value of a row (for example `Duplicate entry 'x' for key ...` of a 1062, or `Incorrect integer value: 'x12'` of a 1366). The kind, the error number and the SQLSTATE stay. `redact` also empties the messages of `Invalid_value` that the client makes (`Literal.add`, the size check): they hold no value, but one rule for each constructor is safer. The result of the statement (`Db.exec`, `Tx.exec`) keeps the full error. `Error.pp` prints no `": "` for an empty message.
  - "No value of a statement" holds for the values of `Raw.v` and the literals of the typed API (each is `?` in the SQL of the event). A literal that the program writes into `Raw.sql` stays in the event, because `Raw.sql` takes only program text.
  - The messages of `Connect` and `Decode` come from the client and hold no value of a row. Two `Connect` texts hold a short text that the server (or a peer in the middle) controls, escaped and cut (`Wire.peer_text`): the name of an authentication plugin (`unsupported authentication plugin <name>`), and the cause of a TLS failure, which can name the subject of the certificate.
  - On MySQL, a `Connect` event of an unknown user differs from the event of a wrong password: the server switches an unknown account to `sha256_password`, thus the event has `unsupported authentication plugin sha256_password` in place of `server error 1045`. An observer of the events can thus tell that the account does not exist. MariaDB gives 1045 in the two cases.
  - `Query` events outside a `Tx` run after the checkin, in the fiber of the statement. An atomic statement of `Db.exec` (phase 4b1) is a `Tx`: its events follow the rule of a `Tx`, with one event for each command of the statement. In a `Tx`, they run after each command (`START TRANSACTION`, each `Tx.exec`, the savepoint commands), while the fiber holds the entry, and the events of `COMMIT` and `ROLLBACK` run after the checkin. The ping after an error emits no event. An event never changes the result of the statement.
  - `Db.exec` of a statement that is not atomic and not multi emits exactly one `Query` event on each path (an atomic statement emits the events of its `Tx`, and one event for a value with no literal, with no `Tx`; a multi statement, a query with `include_`, emits one event for each command that ran, during the hold): also for an error before the checkout, a waiter at its limit, and a cancel. Only two paths emit no event: a misuse (`Invalid_argument`) and the exception of a bug. The function of `Stmt.map` runs after the event, thus its exception gives no second event.
  - `Connect` events run in the connect fiber. `Kill` events run in the killer, outside its shields.
- `Error.t` keeps the full server message for the program. A server message can hold a value of a row, for example 1062 "Duplicate entry 'x' for key 'email'". `Error.pp_summary` prints only the constructor, the kind of a constraint and the error number. The printer of the exception `Cistern_error`, which logging libraries call through `Printexc.to_string`, uses `Error.pp_summary`. Only an explicit `Error.pp` or `Error.to_string` gives the full message.
- `Event.pp`, and any default log output of events, prints only the kind of the event, the duration, the number of rows, the address, the redacted SQL and `Error.pp_summary`. Thus no server message and no value goes to the logs (decision Q9 of phase 0). The producer of a `Query` event puts only redacted SQL in it: each literal is `?`. A query has the outcome `Done` (with the rows), `Failed` (with the error) or `Cancelled` (with a flag: was the statement sent).
- The events of `KILL` (phase 1.5b): `Kill {connection_id; duration; outcome}` (`` `Killed `` or `` `Gone ``), `Kill_dropped {connection_id; reason}` (`` `Queue_full ``, `` `Duplicate ``, `` `Other_server ``, `` `Failed ``, `` `Not_owner ``, `` `Off ``, `` `Drain_timeout `` or `` `Closed ``) and `Kill_off {reason}` (`` `Identity_failed ``, `` `Proxy ``, `` `Privileged ``, `` `Role ``, `` `Grantee_unknown `` or `` `Not_owner ``). Their reasons are variants, not text, so that an event holds no free text. A `KILL` that gives 1094 (the query is already gone) is a success. Phase 1.5b emits only `Kill_off` (at a connect of the pool). The killer of 1.5c emits the others. `Connection.kill` sends `KILL CONNECTION <id>` with the whole 64-bit id, and gives `Killed`, `Gone` (1094) or `Not_owner` (1095).
- No stdlib I/O: Cistern does not call `Unix`, `open_in` or `Sys.time`. The guard `test/nounix` links the five libraries of the package `cistern` (not `ppx_cistern`, which runs in the compiler) with the list of caravan's `test/nounix/dune`: `(forbidden_libraries unix threads.posix weft.posix weft.sim weft.os)`.

## Tests (windtrap)

- Unit tests: the URL parser and the `ssl-mode` defaults, the schema checks, the SQL compiler for the two dialects (also `IS NULL`, `<=>` on `option` fields, `in_` with `None`, `in_ []`, `LIKE` escaping, `count` with `limit`, `Query.where`), the literals (also `Decimal.of_string` refuses SQL text), the first-word check of raw statements in a `Tx`, the error mapping, the two authentication scrambles with known values, and the parsers of recorded server rows (`@@version` strings, `information_schema` rows), so that the mutation gate reaches this code.
- Property tests (`prop`): the filters of `Expr` (phase 2a: each filter is two-valued and selects the rows of its OCaml rule, thus `not p` selects the complement of `p`), round trips of each type through literals and the text decoders (also `Int64.min_int` and `-0.0`), length-encoded integers, any OCaml string gives a literal that the test parser reads back without a change (also with `'`, `\` and NUL).
- Stateful tests: a sequential `stateful` test of `Pool_state` against a model (no connection goes to two holders, no slot is lost, `Killing` slots come back after their age limit at the next operation, `killed` with an old token frees nothing, `next_timer` is never later than the next change). The races of the weft shell are tested in the simulator.
- The odoc of the packages (phase 5a): `make doc` (`dune build @doc`) builds it, and the env stanza of the file `dune` at the root makes each warning of odoc an error, thus an unresolved or ambiguous reference fails the build. `checks.sh` and the CI step "Doc" of the job `test` run it. `nix flake check` does not build the doc: its source has no root `dune`, and odoc is not in its inputs. odoc has no documentation of the stdlib and of weft in this build, thus the doc comments name `Invalid_argument` and the modules of weft as code (`[Invalid_argument]`), not as references, and they use no `@raise` tag for them. The pages of `doc/` (`index.mld`, `limits.mld`, `security.mld`) are in the doc of the package `cistern`. The doc of `Cistern.Error` has what each error tells and what to do. Each fact of `security.mld` names its tests.
- The limits (phase 5a): a unit row compares each named row of `doc/limits.mld` with the constant of `Cistern_core.Limits`. Rows of the names and of the idle limit tie `identifier_length`, `idle_max`, `idle_offset` and `idle_min` to the code that uses them.
- Expect tests (`ppx_windtrap`): the suite `test/expect` of `SUITES` (phase 2b) prints the SQL of each statement of `Compile` for each dialect, with its redacted copy, the DDL of `Ddl.create_table` and the messages of `Ddl.compare` (phase 2c), and has a property: the SQL is the redacted copy with one literal of the statement in place of each `?`, and the parentheses are balanced. Later phases add the generated code of the ppx (with `ppxlib` and `Pprintast`), a file with two deriving groups, and the SQL of each statement for each dialect (`Stmt.to_sql`).
- The ppx suite `test/ppx` of `SUITES` (phase 3a) calls the expander as a function (`Ppx_cistern.Private.expand`) on sources that `Ppxlib.Parse` reads from strings. It prints the description of each accepted group (the fields, the types, the attributes and their locations) and its `Schema.model` list, and each error with its line, its characters and its text. A test checks that the expander marks each Cistern attribute as seen (`Ppxlib.Attribute.collect_unseen_errors`). Phases 3b and 3c add the generated code: for each of `kinds`, `auto_only` and `person` of `test/model`, the `Pprintast` text of the output of the expander for the type declaration of the file is the `Pprintast` text of the items of the file after the declaration. The hand-written code came from the generator, thus this equality detects changes only. The meaning comes from two other checks:
  - The test of D2: for each of these files, and for the derived groups of `test/ppx/driver`, `Desc.schema` of the type declaration (the value that the ppx checks and lifts) is the run-time `schema` of the value `model` of each model.
  - The run-time rows of the unit, sim and integration suites on these models, also `Field.get` and `Field.set` of each field.

  A test checks the locations of the code of each field. The library `test/ppx/driver` compiles groups with each attribute through the driver of ppxlib with `-check`, which refuses an attribute that no ppx marked. Phase 3c adds the hostile group of D17 to it: user modules `List`, `Option`, `Fun` and `Q` before the group (the defaults name `Q`), a type `unit` after the group, field names such as `c`, `r`, `v`, `k`, `d`, `draft`, `schema`, `model`, `update`, `key`, `all`, `where`, `delete`, `fields` and `decode`, the types `t`, `s`, `f`, `d`, `m` and `u`, and the types `a_` and `a` with two thunks. It compiles with each warning as an error but 70.
- The derived models (phase 3d): the library `test/model_ppx` has `kinds`, `auto_only` and `person` with the type declarations of `test/model` (a test of `test/ppx` checks that the declarations are equal), the hostile groups of D17 with SQL names (the table `order`, the columns `select`, `key`, `desc`, `a b`, `it's`, a column with `"` and `\`, `café`, and a column of 64 characters with `[@unique]`), and two groups in one file. These tests run on the derived models: the unit rows of `kinds`, `auto_only` and `person`; the sim rows of `test_model.ml` (`person` takes the place of the hand-written `user`); and in `typed_tests.ml` the filters with their complements, the order, the stable pages of `person`, `count`, `exists`, `first`, the bulk forms, the errors of a unique key and of a value too long, the events of a 1062, `for_update`, `for_share`, the round trip of `kinds` and `auto_only`, and an `update` of the last field of `person` with a read back. These tests stay on the hand-written `user` and `todo`, because they need the relation: the rows of `todo` in `typed_tests.ml` (`filter_by_user_id`, the pages and the bulk delete of `todo`), the errors of a foreign key, the error of `NOT NULL` (only the optional key of the draft of `todo` can give `NULL`), the delete of a parent with children, the user rows of `test_schema_ops.ml` and the first test of `schema_tests.ml` (the DDL of the foreign key), and `test_typed_query.ml` (its queries use `user` and `todo` together). Since phase 4h they run on the derived `user`/`todo` of `test/model_ppx` (the declaration of `test/model/user_todo.ml`, checked equal by `test/ppx`), and so do the other run-time tests of relations (`test_relations.ml`, the sim rows of the nested create, the scopes and `include_`, and `relation_tests.ml`), and the derived tree `node`. The hand-written `user_todo` and `tree` stay as the reference of the equality. The hand-written `Coded` (`test/model/coded.ml`, a parent with a key that is not `[@auto]`, two has_many and float fields) stays too: it tests `Model.child` and `Model.create` directly, with a NaN in the parent and in a child (a value with no literal), and only the parts that the nested create reads are real, which a derived group cannot give. `test/model_ppx/hostile_rel.ml` is the hostile relation group with SQL names (the tables `group`, `select`, `order by` and `where`, the key columns `a id`, `key`, `a key`, `order` and `from`, the relation names `c`, `r`, `k`, `fields` and `decode`, a child with two belongs_to, a nullable key and a tree, a parent `gt` with a key that is not `[@auto]`, and a child `gm` whose key column has a long name, so that its foreign key name gets the hash). The integration suite runs it on the three servers: `push_schema`, `check_schema`, the nested create of each has_many (each child gets the key of its parent in the column of the right key; the parent keys differ, so that a grouping by the column of another key gives other children), the nested create of `gt` with the key of the draft, each scope, each `Rel` with `include_`, and the foreign key of `gm`. One sim row and one integration row keep the list form of `Cistern.Private.Model.update`. The integration suite runs the hostile group on the three servers: `push_schema`, `check_schema`, `create` with the defaults, `get_by_*` and `find_by_*`, `filter_by_*`, a filter, an order, `update` and `delete`. A sim row checks the DDL of `push_schema` of the derived models. The library `test/model_mli` has a derived group behind an interface that shows only `model` and `create`, and it compiles in the dev profile with no warning (D13).
- Type-error tests (`test/typeerr`): a dune rule types each file that the compiler must refuse, with `(with-accepted-exit-codes 2 ...)`, and windtrap `expect_file` checks the compiler message, which names the two types. A file that types makes its rule fail. Phase 2a refuses a filter and an update with the fields of two models, `starts_with` on an `int64` field and on a `string option` field, and `starts_with_opt` on a `string` field. Phase 2d2 adds the files of the models: a filter of the fields of one model in the query of another, and `Query.order_by` with a field of another model. Since phase 3d, these two files use the derived models (`Person.Q` in `Kinds.where`, and a field of `Kinds` on `Person.all`). A third file refuses `Cistern.Query.Private`. Phase 3c adds a file of `[@@deriving cistern]` (through a driver of ppxlib with `ppx_cistern`) with `[@default 1]` on a `bool` field: the error is at the payload. No cram test, so that all tests are windtrap tests.
- README test (phase 5b): each OCaml block of `README.md` has a marker `<!-- example: <name> -->`, and `test/readme/gen.exe` puts the text of the region `<name>` of `examples/*.ml` in it. The rule in the default `runtest` alias compares the result with `README.md`, and `dune promote` updates `README.md`. `gen.exe` finds a fence of backquotes or tildes after any spaces, with the language in any case. It refuses an OCaml block (`ocaml` or `ml`) with no marker, a fence in a language other than `sh`, `nix`, `dune` and `text` (except after `<!-- no-check -->`), a fence after a prefix of spaces, `>` or list markers (in a list item or a blockquote), an indented code block, a `<pre>` block, CRLF line ends, an unknown, unused or repeated region, and a region with no end. The library `cistern_examples` compiles in `dune build`. The group `deterministic` of the integration suite runs each example (`example_tests.ml`): the quick start with its own `main` and a TLS URL, and the others on a pool with new tables.
- The soak (phase 5c): `make soak SERVER=<s> [N=50]` runs tests of the integration suite N times (`tools/soak.sh`; the default is the flaky TLS test "the pool on TLS: KILL CONNECTION of an idle connection" of cistern#39 and its socket sibling "a killed idle connection"). Each soak writes to a new directory `.soak/soak.XXXXXX` and removes no file. A failed run keeps its output: before the next run, its log moves to `soak-<i>.log`, and the files of `_build/_tests/integration` that the run wrote (newer than a mark before the run) are copied to `soak-<i>.tests`, with their paths. `N` must be 1 or more. The nightly `db.yml` runs 50 times on mysql84, on the two systems (ubuntu and macOS), and keeps `.soak/` on a failure. `test/soak` (in `runtest`) checks the script with a fake command that fails in one run of two.
- Mutation tests: `make mutate` as in weft, on the suites unit, prop, stateful, expect, ppx (phase 3a) and sim. Each phase adds its suites to `SUITES` of the Makefile. `test/ocaml55` stays in `SUITES` as the check of the mutation backend. The integration suites are not in the mutation runs. The rule is the rule of weft (no surviving mutant on changed lines), with `[@mutate off]` and a reason where a mutant cannot be killed.
  - The gate gives the output of `git diff -U0` to windtrap (`--diff`). On a local machine, the base is the merge base with `origin/main`. In CI, the base is `HEAD^1`: the checkout of a pull request is a merge commit, and its first parent is the tip of the base branch at the time of the event.
  - Unlike weft, a mutant on a changed line that no test reaches fails the gate. Code that no suite can reach gets `[@mutate off]` with a reason.
  - Each library with the mutation backend and a `.ml` file is in the `libraries` of a test stanza of a suite of `SUITES`, and each such test stanza links with `-linkall`. `make check-scope` examines this, so that the catalogue of the suites has each mutation site. `ppx_cistern` has the mutation backend since phase 3a, and the suite `test/ppx` links it.
  - Before the mutation run, the suites run with no mutant armed, and a failure stops the gate. In the mutation run, each suite writes a verdict file or tells that no mutant is in its scope. A suite that does neither stopped early, and the gate fails.
- The integration suite `test/integration` is an executable, not a `test` stanza and not a dune alias action. `make integration SERVER=<name>` builds it and runs it each time, with `CISTERN_DB_ENV`, the absolute path of `.db/<name>/env`. Thus `dune runtest` and the `checks` of the flake never need a database server, a cache cannot skip a run, and the mutation gate does not run it (it is not in `SUITES`).
- Simulator tests (`Weft_sim`), the suite `test/sim` of `SUITES`. A fake server listens on an address of the in-memory network and plays a script of packets that the test writes with its own encoders. The real `Connection.connect` connects to it, so that the address rules, the DNS, the timeout and each authentication path run in the gate. The fake server keeps each byte that it gets. At the end of each test, each fd of the simulator must be closed:
  - The weft shell of the pool: directed tests of each path and each event, and a storm. The storm has 8 connections, 4 virtual domains, raw statements (resets), `BEGIN` (an abort at checkin), server errors, deadlines, cancels at the same time as checkins, a short `max_lifetime` and 4031 faults, in three waves. Each fiber gets one permitted outcome, each `exec` gives one `Query` event, and no socket goes over the bound. After the storm, the pool is quiet (no entry in use, no connect, no waiter), and 8 fibers get a connection at once. After `with_db`, no socket and no slot stay. The race counters, as a sum over the seeds, are each above 0. Since phase 4b1, the storm also runs atomic statements (two commands in the transaction of `Db.transaction`): each atomic exec gives one `START TRANSACTION` event and one event for each command that ran, the other execs one event each, and no holder gets a session of the fake server in a transaction (the seam `Pool.Private.on_hold`). 6 of the 8 seeds run with the seam `Pool.Private.after_op` (a shielded sleep of 1 ms between the operation and its effects). The gate runs 200 fibers × 8 seeds. `make sim-slow` (`CISTERN_SLOW`) adds 1000 fibers × 20 seeds, in the CI test job and in the nightly `db.yml`.
  - Transactions (phase 1.3): directed tests of each row of the tables of "Transactions" against the fake server with a state of a transaction, and a storm of `Db.transaction` and `Db.exec`: 8 connections, 4 virtual domains, three waves, transactions of 1 to 3 statements with waits and deadlocks of the fake server, savepoints (some end with `ROLLBACK TO`), faults of `COMMIT` that change every 10 to 30 ms (1213, 1205, 1213 after 30 ms, a slow OK), cancels, and the seam in 6 of the 8 seeds. Each fiber gets a permitted outcome, the set of the fibers that hold a `Tx` is empty at the end, each statement that the fake server got has exactly one event (each statement has a text of its own), the counts of `COMMIT`, `ROLLBACK` and the savepoint commands agree with the events, each seed has at least 50 commits, and no socket and no slot stay after `with_db`. The gate runs 200 fibers × 8 seeds, and `make sim-slow` 1000 fibers × 20 seeds. The same storm also runs on TLS (phase 1.4, `Test_pool.with_pool ~tls`, with the TLS fault of `Fake_db` and a seeded Fortuna on the 4 virtual domains, which `Weft_sim` runs on one OS thread): 200 fibers × 4 seeds in the gate, and 1000 fibers × 8 seeds in `make sim-slow`. Each connection of the TLS storm runs TLS (the count of the TLS sessions of the fake server is the count of its connections). On TLS the fake server ends a connection that is idle for 25 ms with 4031, and half of the cancels come in 0.1 to 2 ms (in the seam after the checkout, thus before the stale check). The storm requires one event for each call, whatever its outcome (each `Db.exec` has a text of its own, and each `Db.transaction` one `START TRANSACTION`), and the counts of `COMMIT`, `ROLLBACK` and the savepoint commands only for the commands that reached the server. `make sim-slow` requires a cancel at the lock of the TLS session in the stale check (`Pool.Private` counter `cancel_in_stale`) as a sum over its seeds: 3 in the record. The fewest commits of a seed were 59. Since phase 4b2, 1 step of 10 is an atomic statement of two commands in a savepoint (E6), whose second command can be a deadlock. Each command of an atomic statement that the fake server got has exactly one event ("atomic: <command>"), and each seed runs atomic statements (with `KILL`, the sum over the seeds).
  - Deadlines with no real time, and deadlines that do not stay on a reused flow.
  - Cancel: the `KILL` goes to the control connection with the right id; a changed identity drops it; the slot is free after its age limit, and a waiter gets it without another operation; a control connection that the server closed (4031) is opened again, and the `KILL` is sent; an outer cancel of `with_db` still sends the queued `KILL`s; `with_db` ends when the killer is cancelled during a `sync`.
  - Transactions: a deadlock rollback followed by a `Tx.exec` gives the first error, and no write runs. A raw `TRUNCATE` in a `Tx` gives `Implicit_commit`. A cancel after an OK of `COMMIT` does not change the result.
  - Security: under `REQUIRED`, a fake server answers `0x01 0x04`, and the client does not send the password. Bytes after the SSL request make the client fail.
  - Session reset: a raw `SET sql_mode = ''` and a raw `USE other`, then checkin; the checkin does no I/O; the next checkout resets the session, and the statement sees the complete `sql_mode` and the configured database. A reset that fails makes the checkout take another connection.
  - Protocol faults: a peer fiber plays a transcript that a test packet encoder makes, so a change of the protocol code needs no new recording. Version 1 has no recorder of real server traffic; the integration tests cover the real servers. The test injects resets, short reads, latency, and an error 4031 on an idle connection.
  - TLS (phase 1.4, `test/sim/test_tls.ml`): `Fake_server.Start_tls` runs `Weft_tls.server` with the certificates of `test/certgen`, a seeded Fortuna and the time 2025 of the simulator. The tests: each row of the table of the verified configurations (with the sequence ids 1, 2 and 3), the full path under each row (the password goes only when verified), no `CLIENT_SSL`, a byte before the ClientHello, another CA, another name, another IP address, another pin, a certificate with a hostile name (the message is printable ASCII, with the first and the last 100 bytes, and the cause at the end), the cause of a wrong name and of a wrong pin in the message, the CA file (missing, a path through a file, a directory, larger than 1 MiB, empty, a key, a bad block, and a slow file system past the limit of 10 s), no generator (`Connection.connect` and `Db.with_db`), session tickets, `stale` (plaintext, the end, nothing, a pending cancel), `close` (a server that closed, a server that reads nothing, a cancel at the write lock, and a cancel in the TCP write of `COM_QUIT` after a command that filled the receive buffer of the server, as the deaf statement of 1.2), the write rule (a cancel at the write lock, a cancel in the TCP write of 100 KB), the deadlines, a TLS handshake past the limit of 10 s, the pool on TLS (`Db.exec`, `Db.transaction` and their events, and an idle TLS session that the server ends: the stale check finds it, and the next exec has a new connection with no error), and a cancel at the lock of the session in the stale check of the pool (`Db.exec` and `Db.transaction`, with the latch of 1.2 and the TLS fault of `Fake_db`: the event of the checkout comes). Each TLS simulation seeds the generator again, so that each test has the same bytes in each run.
  - The killer (phase 1.5c1a, `test/sim/test_kill.ml`): `Connection.in_flight` (an OK, a server error, R-SEQ0, the end of the stream, a read deadline, a cancel, after the connect, a broken connection, a reset whose second write fails); on `Fake_db` with `kill=true` (the faults `kill_ms`, `kill_close`, `kill_err`, `id_of`, `server_uuid`, `refuse`, `wait_timeout`, `idle_4031_ms`): a cancel after the flush (the slot stays `Killing` until the `KILL`), no `KILL` for a cancel in the wait, an error or an ended statement, a read deadline, a `Tx` (a cancel in a statement, a `COMMIT` past its flow deadline), no `KILL` for R-SEQ0, the backoff of the control connection (exact times to 5 s, and 100 ms again after a success), its idle limit (from the last `KILL`), a 4031 on it, one retry, two failures, the limit of 2 s, 1095 with the drop of the queue, another server, the id of the control connection, a duplicate, the drain (phase 1.5c1b: it sends the entries that remain, it ends at its limit of 15 s with `Kill_dropped Drain_timeout`, an outer cancel does not stop it, a waiter of an outer scope fails at once before it, and it takes a kill request that comes during it), the join of the scope while a try runs (within 2 s) and while the killer reconnects (at once), no `KILL` for a raw `BEGIN` (an idle transaction at the checkin) and for a failed reset, a `Kill_off` during a try before the membership check (the seam `Pool.Private.before_try`: no `KILL`), a cancel after the shutdown, the age limit with a full queue and a `KILL` after the age limit, a waiter that gets its connect at once after a free by `kill_off`, by `Kill_off` of the control connection and by a duplicate, the control connection over TLS, and `kill=false`. `Fake_db.config` has `kill=false` by default, thus the other tests have no killer. `check_end` also requires no control connection, no `Killing` slot and no entry of the queue.
  - The storms with `KILL` (phase 1.5c2): the pool storm and the storm of `Db.transaction` (also on TLS) with `kill=true`. Each statement of the pool storm sleeps up to 30 ms, each reply of the `Tx` storm comes 2 ms late, half of the calls get a cancel, and `fibers / 50` statements of 60 ms between the waves are cancelled while they run, thus each seed has kill requests. A fiber of faults changes the faults of `KILL` every 10 to 30 ms: for 5 ms a late reply (300 ms), a reply past the limit of a try (2.5 s), or the end of the control connection at its next `KILL`; for 5 s replies past the limit (two failed tries: `Kill_dropped Failed`); for 50 ms the end of the control connection and the refusal of its reopen, only of the control connection (the seam `Pool.Private.connect_control`); for 30 ms another server identity for the new connections (`Kill_dropped Other_server`). An idle control connection gets 4031 after 25 ms (the killer opens it again), and in the seeds with `seed mod 4 = 3` each `KILL` gives 1095 (`Kill_off`). After the storm, the test waits until the killer ended its work. Each seed requires the checks of its storm (the bounds of the statements and of the commits are lower: about half of the record), one `Kill` or `Kill_dropped` event for each kill request (`Pool.Private` counter `kill_requests`), at most one control connection (a sample), at most one `Kill_off` event, `Kill_off` only in the seeds of 1095, at least 2 kills in the other seeds, and `check_end` (no socket, no slot, no `Killing` slot, no entry of the queue, no control connection). Each run (the gate: 200 fibers × 8 seeds, TLS × 4; `make sim-slow`: 1000 fibers × 20 seeds, TLS × 8) requires, as sums over its seeds, `kill_retry` and `control_reopened` above 0, and `Kill_dropped Failed` and `Kill_dropped Other_server` events above 0 (not the `Off` drops of the seeds of 1095). A full queue and a duplicate stay with the directed tests.
  - The typed API (phase 2d1, `test/sim/test_model.ml`): the script of `Fake_db` answers the exact SQL of each statement of the hand-written models. The rows: `get` (a row; no row: `Not_found`), `find`, `get_by_*` and `find_by_*` (also of a nullable unique field), `create` (the last insert id; 0: `Decode`; a key in the draft; `Auto_only`), `update` (1 matched row; 0: `Not_found`; no field: no command and no event), `delete` (1; 0: `Not_found`), a row that does not decode (the connection stays), a value with no literal (no byte, no connect), a lost reply (`Connection_lost` for a read, `Outcome_unknown` for a write), no session reset, the redacted events, and the same in a `Tx` (`Not_found` keeps the `Tx`; an update with no field in a failed `Tx` gives its first error). The unit rows of each model: each field is the field of the schema with the same column, each column goes to its field, the draft columns are columns of the fields, each setter, the cut of a `Ptime.t`, and a `Decode` message with no text of a column.
  - The queries (phase 2d2, `test/sim/test_typed_query.ml`): `to_list` with a filter, two orders, a limit and an offset (the key appended), two filters, `first` (`LIMIT 1`), `count` with and with no limit (a derived table) and a reply with no row (`Decode`), `exists`, `filter_by_*` of an index and of the key of a `Belongs_to`, the bulk forms (the matched rows) and their refusals, `for_update` in `Db.exec` (`Invalid_argument`, no event, also after `Stmt.map`), `count` and `exists` with `for_update` in `Db.exec`, `for_share` in `Db.exec`, a lost reply (`Connection_lost` for `count`, `Outcome_unknown` for a bulk form), and `for_update`, `first`, `count` and the bulk forms in a `Tx`.
  - The schema (phase 2e, `test/sim/test_schema_ops.ml`): `push_schema` sends `SET` and each `CREATE TABLE` in order on its own connection (one connect, no round trip 3 also with `kill=true`, `COM_QUIT` at the end) with their events; the value of `lock_wait_timeout` for several timeouts; a refused schema (no connect); an empty list; a failed connect, a failed statement (the abort, no more statement), 1205, the deadline in a statement and in the connect, the margin (no statement after the deadline), a cancel in a statement (the event, the abort), a failed `SET` (the abort, no `CREATE TABLE`), `lock_wait_timeout` from the time left after a slow connect; the fiber of a `Tx` and a use after `with_db` (also with a refused schema: the use after comes first). `check_schema`: the exact SQL and the redacted event, equal, the differences, a MariaDB fake (its collation gives `Ok ()`, the collation of MySQL gives the differences), no session reset, a refused schema (no statement), and a server error (`Cistern_error`). Unit rows: the bounds of `lock_wait_timeout`.
  - Round trip 3 and `KILL` (phase 1.5b, `test/sim/test_identity.ml`): the exact SQL of round trip 3 on MySQL and on MariaDB, each check (a server error, an OK, two rows, five values, a start time that does not decode, another `CONNECTION_ID()`, another thread id in the greeting, a 64-bit id whose low 32 bits are or are not the thread id, the low 32 bits of round trip 1 with another id, no grantee row, the admin rows, an enabled role, the order of the checks, the end of the stream), `kill=false` with no round trip 3, `Connection.kill` (an OK, 1094, 1095, another error, the end of the stream, and the whole 64-bit id), and on `Fake_db` a `KILL` that ends another connection, one `Kill_off` for each refused identity, one `Kill_off` for 4 connections on 4 domains, and none with `kill=false`. The test configurations of the scripts of `Fake_server` have `kill=false`. `Fake_db` answers round trip 3 after round trip 2, outside its commands and its log, and gives each connection its number as the thread id and `CONNECTION_ID()`. The integration suite checks the identity of round trip 3 on the socket, TCP and TLS against a separate query, and `Privileged` for root.
- Integration tests (`Weft_os`): `suite : (module S : SERVER) -> Windtrap.test` runs the tests through the typed API on each server. The `Tx.exec` runner runs on MySQL 8.4 and MariaDB 11.8.
  - Phase 1.2 runs `Db` (`test/integration/db_tests.ml`), first in the deterministic group, on the three servers, on the socket:
    - A pool of 4 and 64 fibers on 4 domains: each fiber gets its row, each `exec` gives one `Query` event, and root sees at most 5 sessions of the test user (V3): the 4 slots and the control connection of the killer (phase 1.5c1a).
    - A raw `SET sql_mode`, `SET time_zone`, `SET NAMES`, a user variable and `USE`: the next `exec` has the same connection id and the session of Cistern again (V5).
    - A raw `START TRANSACTION WITH CONSISTENT SNAPSHOT` (a transaction that root sees at once): the checkin aborts the connection, root sees no session and no transaction of it, the next `exec` has a new connection id, and an `INSERT` after it commits at once. A raw `BEGIN` through `Db.exec` thus has no effect on the next `exec`. A transaction of more than one statement needs `Db.transaction` (phase 1.3).
    - An idle connection that root kills: the stale check finds it at the checkout (V1), and the `exec` takes a new connection with no error.
    - `SELECT SLEEP(2)` with a deadline of 200 ms gives `Outcome_unknown` at the deadline, and the pool stays usable.
    - A wrong password gives `Connect` with 1045 at the deadline, and `Connect` events with the error.
    - The end of `with_db` with execs in flight: the statements end, the waiter gets `Connection_lost`, a later `exec` raises `Invalid_argument`, and the sessions of the test user end.
    - The timing group (nightly): a global `wait_timeout` of 6 s gives an idle limit of 1 s, and after 2 s the next `exec` takes a new connection with no error (V2). A `max_lifetime` of 1 s gives a new connection after 1.5 s.
  - Phase 1.3 runs `Db.transaction` (`test/integration/tx_tests.ml`), after the tests of `Db`, on the three servers, on the socket. Root reads the rows of the table at the end of each test:
    - `COMMIT` of two writes. A `Cistern_error` from the callback gives `ROLLBACK`, and no write of it stays.
    - A deadlock of two `Tx` in two fibers: one victim gets `Retryable deadlock`, its later `Tx.exec` gives the same error with no event, no write of it stays, and the other `Tx` commits. With the second `UPDATE` in a savepoint, the savepoint of the victim gives the deadlock, and no `ROLLBACK TO SAVEPOINT` goes.
    - `TRUNCATE` and `CREATE TABLE` in a `Tx` give `Implicit_commit`, and the writes before them are committed (V4). A `CREATE TABLE` of a table that exists also gives `Implicit_commit`, and the write before it is committed (V10). A `Tx.savepoint` after it gives `Implicit_commit` with no event, and its scope does not run.
    - 1062 and 1064 leave the `Tx` usable, and `COMMIT` commits the other writes.
    - Savepoints: `RELEASE` after a value, and `ROLLBACK TO SAVEPOINT` after a `Cistern_error`, which removes only the writes of the savepoint. A caught cancel in a savepoint (phase 5a0): the write of the scope is not in the table after the end.
    - The first-word check (`START TRANSACTION`, `SET autocommit = 0`, `/* */ COMMIT`, `EXECUTE s`) and `Db.exec` in the callback raise `Invalid_argument` with no event, and the `Tx` stays usable.
    - A read deadline of 200 ms on `SELECT SLEEP(2)`: `Tx.exec` gives `Outcome_unknown statement`, and `Db.transaction` gives `Timeout`, because each statement was DML (D5). No write stays.
    - A `Tx` past its deadline of 300 ms gives `Timeout`, one `ROLLBACK`, and no write: the timer cancels a wait in the callback at the deadline, and the time check catches a callback that returns later (a wait in a shield). The test takes the time at the end of the callback, because on MySQL 8.4 the `ROLLBACK` of an `INSERT` into a new table can take up to about 1 s on the server (0.75 s to 0.94 s in the records, and not each time).
    - Root holds `FLUSH TABLES WITH READ LOCK`, and polls `PROCESSLIST` until the `COMMIT` of a `Tx` waits. `KILL QUERY` of the `COMMIT` gives `Outcome_unknown commit`, and after `UNLOCK TABLES` no write stays (V8). A cancel of the caller while the `COMMIT` waits gives the result of the `COMMIT` (the shield), and the write is committed.
    - With a global `completion_type = 'CHAIN'` (set back to `DEFAULT` at the end), two `Tx` commit on the same connection. Thus the OK of `COMMIT` had `IN_TRANS` clear, because else the checkin aborts the connection (V11, claim C26).
    - The timing group (nightly): in a `Tx` with a deadline of 500 ms, an `UPDATE` with a statement deadline of 300 ms waits for a row lock. It gives `Outcome_unknown statement`, the `Tx` gives `Timeout`, and the holder of the lock commits. The two deadlines are not at the same instant, because then the timer of the callback can fire first (it did on macOS), and the test would depend on the order of two timers. A `COMMIT` that waits 2.5 s on `FLUSH TABLES WITH READ LOCK`, after the deadline of 1 s of its `Tx`, commits: the flow deadline of `COMMIT` is at least 5 s.
  - Phase 1.4 runs TLS (`test/integration/tls_tests.ml`) on the three servers. The TCP tests of phase 1.1 (the column types, the connect, a wrong password, an unknown database, `KILL CONNECTION`) use TLS (`VERIFY_IDENTITY` with the test CA), and the tests of the clear-text rule keep `DISABLED`:
    - Each row of the table of the verified configurations connects (with a warm cache on MySQL): `REQUIRED`, the pin under each mode, `VERIFY_CA` with the CA, `VERIFY_IDENTITY` by `127.0.0.1` and by `localhost`, and an authenticator of the program.
    - The full path with an empty cache on MySQL: the verified rows connect, and `REQUIRED`, `VERIFY_CA` and an authenticator alone give the message of the clear-text rule. MariaDB connects in each row (`mysql_native_password`). A wrong password over verified TLS gives 1045.
    - A CA of another PKI, and an authenticator of the program from another PKI, give `Connect "TLS: … invalid certificate chain"`, and another pin gives a text with the two fingerprints at its end. The authenticator `none` connects with a warm cache, and gives the message of the clear-text rule on MySQL with an empty cache. A wrong identity: the IPv4-mapped address `::ffff:127.0.0.1` reaches the server through an IPv6 socket and is not in the certificate. `REQUIRED` and `VERIFY_CA` reach the server, and `VERIFY_IDENTITY` (with the CA or the pin) fails with `does not contain the IP ::ffff:127.0.0.1` at the end. The test runs only where a plain TCP connect to that address works (Linux), else it is skipped with the reason.
    - The pool on TLS: `KILL CONNECTION` of an idle TLS connection. The stale check finds the end (no `close_notify`), and the next exec has a new connection with no error. A `Tx` then commits.
    - The timing group (nightly): after the idle timeout over TLS, a ping gives `Connection_lost` (MySQL sends 4031, MariaDB ends the session), and a new connect works.
  - Phase 1.5 runs `KILL` (`test/integration/kill_tests.ml`) on the three servers, with the default `kill=true`, and root reads `PROCESSLIST` with the client of the server: a statement that a read deadline of 300 ms cuts, on the socket and over TLS, leaves `PROCESSLIST`, with one `Kill` event (on MySQL, the control connection of a configuration of TLS runs TLS: `Ssl_cipher` in `performance_schema.status_by_thread`); no `KILL` for a wait of the checkout past its limit, a cancel in that wait, the margin, and a server error; root as the pool user gives `Kill_off privileged`, and its cut statement runs to its end, as with `kill=false`; a `Tx` gets its `KILL` at its end (D13); a cut write of a `Tx` that waits for a row lock leaves no row, and the lock is free; the end of `with_db` right after a cut statement (the `KILL` in the join or in the drain); and the privilege check with users that root makes and drops: a direct grant of the admin privilege (`Kill_off privileged`), the admin privilege through a default role (`Kill_off role`), and a user name with `@` (`KILL` stays on). The claims C32, C33 and C35 run in the deterministic group, and C34 in the timing group.
  - Phase 2e runs `push_schema` and `check_schema` (`test/integration/schema_tests.ml`) on the three servers, with the hand-written models: `push_schema` two times (the second changes nothing), a row through `create` and `get_by_id`, `check_schema` equal, then a raw `ALTER TABLE` to a shorter `VARCHAR` and one that removes `AUTO_INCREMENT`, which `check_schema` finds. The test of `kinds` finds `DATETIME` with no fraction, a case-insensitive collation and an unsigned column (phase 2f).
  - Phase 2f runs the typed API (`test/integration/typed_tests.ml`) on the three servers, with the hand-written models. One suite runs through the two runners `(fun s -> Db.exec db s)` and `(fun s -> Tx.exec tx s)` (a polymorphic parameter; the `Tx` commits at the end): `create`, `get`, `find`, `get_by_*` and `find_by_*`; 34 filters (each row of the NULL rules, `in_`, the orders, the case-sensitive equality, `starts_with`, `contains`, `starts_with_opt` and `contains_opt` with `%`, `_` and `!`, the nesting, `and_ []` and `or_ []`) on rows with `NULL`, each against its OCaml rule (the rule of the property test of `Expr`), with `not p` (the complement), `count` and `exists`; the order with two keys (`NULL` first in `ASC`, last in `DESC`, as `None` in OCaml); stable pages over equal values; `count` and `exists` with a limit or an offset; `first`; `update` and the bulk forms; `filter_by_*`; the errors of a unique key, a foreign key, `NOT NULL` and a value too long (each fails only its statement); `delete`; the round trip of each scalar of `kinds` (a `Ptime.t` cut to 1 µs; `-0.0` keeps its sign only on MySQL); `Auto_only`. Then: the event of a 1062 holds no value; a second `Tx` waits for the `for_update` of the first and reads its commit; `for_share` in `Db.exec` and in a `Tx`.
  - Phase 4e runs the relations (`test/integration/relation_tests.ml`) on the three servers, with the hand-written `user` and `todo` and a pool of one connection: the nested create in `Db.exec` (the events `START TRANSACTION`, the parent `INSERT`, the children, `COMMIT`; the children have the key of the parent); a child that fails with 1406 (the error of the child, `ROLLBACK`, no parent row, and the next statement on the same connection finds `IN_TRANS` clear after a ping, through the seam `Pool.Private.on_hold`, because the test user of MySQL 8.4 cannot read `performance_schema.events_transactions_current`); the nested create in a `Tx` (the savepoint commands, `ROLLBACK TO SAVEPOINT` after a child 1406, and the earlier and later rows commit); a child `INSERT` after a plain read of the parent and a concurrent update of the parent (1020 and a failed `Tx` on MariaDB 11.8, no error on the other servers); `include_` with 2500 parents (1 + 3 queries, with 1000, 1000 and 500 keys, and the grouping; then the belongs_to direction over 2500 parents, in 3 chunks, each todo with its own parent) and with 2500 children of one parent in each direction; `include_` after `for_update` and `for_share`, with a plain read first and a concurrent insert of a child of the locked parent (the newest committed child on MySQL 8.4 and MariaDB 11.4, 1020 on MariaDB 11.8), and the same for the belongs_to direction with a concurrent update of the parent (the new value, or 1020 on 11.8); `Todo.update ~user_id` with a new key (`Unloaded`, and the new key on the server) and with the same key (the loaded `user` stays); `User.todos` with a filter and an order against its OCaml rule; the delete of a parent with children (`Foreign_key`).
  - Phase 1.1 runs Cistern's own client: the connect on the socket and on TCP (`ssl-mode=DISABLED`), the session values, the admin route (root on the socket with no password), a wrong password and an unknown database (1044 for the test user, which has no rights on other databases), and a socket path that does not exist (a network error, not a DNS error). After `KILL CONNECTION` from root, three pings give `Connection_lost`. The idle timeout is a timing test, thus it comes with the timing claims (phase 1.1e).
  - On MySQL, a TCP connection with `DISABLED` uses only the fast path of `caching_sha2_password`. Thus each TCP test first warms the cache with a connect on the socket with `allow-cleartext=socket`, and the tests do not depend on their order. A wrong password over TCP then gives the message of the gate, not 1045.
  - The MySQL sequence: `FLUSH PRIVILEGES` as root; TCP gives the message of the gate; the socket without the opt-in gives it too; the socket with the opt-in passes (the full path); then TCP passes (the fast path).
  - The queries: each column type of the table of "Commands and packets" (the same table as the sim fixture), a round trip of `Float`, `Datetime`, `Decimal` and `Int` literals through the server (an `INSERT` then a `SELECT`, and a `SELECT` of the literals), `last_insert_id`, the found rows of an `UPDATE` with no change, `status`, `init_db` and `reset`, and an unknown table. The size check: root (with Cistern's client on the socket) sets `max_allowed_packet` to 1 MiB, then a payload of 1 MiB is refused before it goes, and one byte less runs. At the end, root sets the default of the server again (64 MiB on MySQL, 16 MiB on MariaDB).
- Verification of claims (phase 1.1): `test/integration/claims.ml`, with Cistern's own client, on the pinned servers (2026-11-07). Each test asserts the error number and the SQLSTATE of the server (`Connection.Private.last_error`) and the meaning that Cistern gives them. The group `deterministic` runs with `make integration` (in each integration job). The timing claims run with `make integration-timing`, and C8c (it stops the server) with `make integration-shutdown`, only in the nightly `db.yml`. The table of the results follows the list.

### Verification of claims (phase 1.1)

"8.4" is MySQL 8.4.11, "11.4" and "11.8" are MariaDB 11.4.13 and 11.8.8. A test of a claim fails if a server gives another result. From phase 1.4, each claim that ran on 8.4 and 11.8 also runs on 11.4 (the probe T10 of the plan of phase 1.4 showed that each one holds there), except C25, which is a claim of MySQL.

| Claim | Servers | Result |
|---|---|---|
| C1, C7: the default plugins and `CLIENT_MYSQL` | 8.4, 11.4, 11.8 | The greeting and `CREATE USER ... IDENTIFIED BY` give `caching_sha2_password` on MySQL and `mysql_native_password` on MariaDB. `CLIENT_MYSQL` is set only on MySQL, as `Dialect.of_version` tells |
| C2: `START TRANSACTION` in a transaction | 8.4, 11.4, 11.8 | It commits: after `ROLLBACK` the row exists |
| C3: `COM_RESET_CONNECTION` | 8.4, 11.4, 11.8 | The database of `USE` stays. The user variables are gone, and `sql_mode` is the global value. The collation is the global value on MySQL (`utf8mb4_0900_ai_ci`), and the collation of the character set of the handshake on MariaDB (`utf8mb4_general_ci` on 11.4, `utf8mb4_uca1400_ai_ci` on 11.8). It rolls back an open transaction, and its OK has `IN_TRANS` clear |
| C4: `IN_TRANS` in the OK of `COM_PING` | 8.4, 11.4, 11.8 | Set after `START TRANSACTION` and after a write in it, clear after `COMMIT` |
| C5, C23: the start time | 8.4, 11.4, 11.8 | 1000 identity `SELECT`s on one connection and one on a second give one value. The test user reads `performance_schema.global_status` on MySQL |
| C6: the thread id | 8.4, 11.4, 11.8 | The thread id of the greeting is `CONNECTION_ID()`, on the socket and on TCP |
| C8a: `KILL CONNECTION` of an autocommit write (timing) | 8.4, 11.4, 11.8 | The end of the stream with no ERR (no 1927): `Outcome_unknown`. No row is written |
| C8b: `KILL QUERY` (timing) | 8.4, 11.4, 11.8 | 1317 (SQLSTATE 70100): `Driver`. The connection stays usable. No row is written |
| C8c: `SHUTDOWN` during a write (shutdown) | 8.4, 11.4, 11.8 | MySQL sends 1053 (SQLSTATE 08S01), then the end of the stream. MariaDB sends only the end. The write gives `Outcome_unknown`. An idle connection gets the end: `Connection_lost` |
| C9: the idle timeout (timing) | 8.4, 11.4, 11.8 | After `wait_timeout` (1 s and a wait of 3 s), `ping` and an `INSERT` give `Connection_lost`, on the socket and on TCP. No row is written |
| C10: the text of `DOUBLE` | 8.4, 11.4, 11.8 | 2713 values (the edges, 1 to 17 digits at each exponent from -20 to 20, and 2000 seeded random values) decode to the same bits, except −0 on MariaDB (C11). The text is the model of `test/prop/server_text.ml`: the shortest digits, the positional form for a decimal exponent from -15 to 14 (and 15 with 17 digits), else `d.ddde<exp>` |
| C11: the literals | 8.4, 11.4, 11.8 | `5e-324` and `-9223372036854775808` round-trip. MySQL keeps −0 (`ATAN2(d, -1)` is −π). MariaDB stores and reads +0, also for `SELECT -0e0` (the text `0`) |
| C12: an encoded surrogate (`ED A0 80`) | 8.4, 11.4, 11.8 | MySQL refuses it: 1366 for `X'EDA080'` and `_binary`, 1300 for `_utf8mb4`. MariaDB stores it, and the read gives `Decode`. The connection stays usable |
| C13: strict mode | 8.4, 11.4, 11.8 | 1265 (SQLSTATE 01000) and 1365 (22012): `Invalid_value` |
| C14: read-only, with no restart | 8.4, 11.4, 11.8 | `read_only` (and `super_read_only` on MySQL) gives 1290 for the test user: `Read_only`. `START TRANSACTION READ ONLY` then a write gives 1792 (25006): `Read_only`. 1836 and 1874 need `--innodb-read-only` (a restart), thus the suite does not make them. The probe of the plan review made them on MySQL 8.4 with `--innodb-read-only`: 1836 for `CREATE TABLE` and `CREATE DATABASE`, 1874 for `CREATE TEMPORARY TABLE` |
| C15: `NOWAIT` | 8.4, 11.4, 11.8 | MariaDB 1205: `Retryable `Lock_timeout`. MySQL 3572: `Driver`. Then `IN_TRANS` is clear |
| C16: 3101 | — | One server with no replication cannot make it: `perror` is the proof |
| C37: 1020 (phase 4a0) | 8.4, 11.4, 11.8 | With `innodb_snapshot_isolation = ON` (the default of 11.8; on 11.4 the claim sets it in the session of a raw connection, because the API of the pool cannot set a session variable), a transaction that inserts a row, makes a plain read (the snapshot), and then reads a row with `FOR UPDATE` after another session changed it and committed gets 1020 (`HY000`): `Retryable `Rolled_back`. A ping then shows `IN_TRANS` clear (an ERR packet leaves the flag of the last OK), and the own row is gone: the server rolled back the whole transaction. Through `Db.transaction` on 11.8 (the default, no change of a variable), the `Tx` gives `Retryable `Rolled_back`, not `Implicit_commit`, and the same connection then has no transaction. With `OFF` (11.4), and on MySQL 8.4, the same steps give no error. The claim makes no global change |
| C38: the last insert id in a transaction (phase 4e) | 8.4, 11.4, 11.8 | In a transaction, the last insert id of the OK of an `INSERT` is the id of the new row, also when another session inserts a row between two `INSERT`s of the transaction. The nested create takes the key of the parent from it (R3 of the plan of phase 4) |
| C17: 1052, 1761, 1762, 1169, 1859, 3673 | 8.4, 11.4, 11.8 | 1052 (23000): `Driver`. 1761 (23000): `Unique`. The try of 1762 (a user with rights only on the parent) gives 1761. The tries of 1169 (a unique key on a `BLOB`, a `MEMORY` unique key) and of 1859 (`ADD UNIQUE` with duplicates) give 1062. `INSERT IGNORE` and `UPDATE IGNORE` of `NULL` give no error (3673 is not made), and a plain `INSERT` gives 1048. 1762, 1169, 1859 and 3673 keep `perror` as the proof |
| C18: names | 8.4, 11.4, 11.8 | A table name of 65 characters: 1059 (MySQL), 1103 (MariaDB). A column `a😀`: MariaDB 1300; MySQL stores `a?`, and a second such column gives 1060. A table of 64 × `€`: 1030 (MySQL), 1005 (MariaDB). A column of 64 × `€` is valid |
| C19, C24: no padding, `COM_PING` | 8.4, 11.4, 11.8 | `'a ' = 'a'` gives 0 after the session setup. `COM_PING` gives OK |
| C20: `PAD_ATTRIBUTE` | 8.4, 11.4, 11.8 | MariaDB 1054. MySQL gives a value |
| C21: `FOR SHARE` | 8.4, 11.4, 11.8 | MariaDB 1064 for `FOR SHARE`, OK for `LOCK IN SHARE MODE`. MySQL OK for the two |
| C22: `CHECK` and the statement timeout | 8.4, 11.4, 11.8 | MySQL 3819, MariaDB 4025: `Check`. A recursive CTE with a limit of 100 ms: MySQL 3024, MariaDB 1969 (70100): `Statement_timeout`. The connection stays usable |
| C25: `caching_sha2_password` | 8.4 | The integration suite (phase 1.1c2) |
| C26: `completion_type` | 8.4, 11.4, 11.8 | With a global `CHAIN`, the session of Cistern reads `NO_CHAIN` (also after `Connection.reset`), and the OK of `COMMIT` has `IN_TRANS` clear. With `CHAIN` in the session (the control), the OK of `COMMIT` has `IN_TRANS` set, and the write is committed. Then `Connection.reset` of the same connection gives `NO_CHAIN` again |
| C27: `KILL QUERY` of a `COMMIT` that waits | 8.4, 11.4, 11.8 | A `COMMIT` that waits on `FLUSH TABLES WITH READ LOCK` gives 1317 (70100) after `KILL QUERY`. The connection stays open, the ping gives `IN_TRANS` clear, and the write is gone. `Db.transaction` gives `Outcome_unknown commit` (the `Tx` tests), because no fact shows a rollback for each 1317 of a `COMMIT` |
| C28: a failed DDL commits first | 8.4, 11.4, 11.8 | In a transaction, `CREATE TABLE` of a table that exists gives 1050 (42S01), the ping after it gives `IN_TRANS` clear, and the write before it is committed. The OK of `TRUNCATE` in a transaction has `IN_TRANS` clear, and the write before it is committed. `Db.transaction` gives `Implicit_commit` for the two (the `Tx` tests) |
| C29: no byte after the SSL request | 8.4, 11.4, 11.8 | A raw check: the greeting, the SSL request with the sequence id 1, then 300 ms. The reader and `read_now` have no byte. The server waits for the ClientHello, thus a byte there is a sign of an attack (the buffer check of "Handshake") |
| C30: no `close_notify` | 8.4, 11.4, 11.8 | A raw TLS session (the greeting gives the thread id, C6), and `KILL CONNECTION` before the authentication. A read gives `Flow.Closed` (a truncation), not the end of the stream. Cistern reads the two as the end. The probe T8 showed the same after `COM_QUIT` and at the idle timeout |
| C31: the tickets after the TLS handshake | 8.4, 11.4, 11.8 | The two session tickets of each server (probe T7) come before its first data: the authentication and the queries read past them, and the connection is not stale after a query. A record with no data at an idle time is tested in the simulator |
| C32: `KILL` of the test user | 8.4, 11.4, 11.8 | `Connection.kill` of the test user ends its own other session (a `SELECT SLEEP` gives `Outcome_unknown`) with no privilege, and gives 1095 (`Not_owner`) for a session of root; its connection stays usable (K1, K3) |
| C33: `KILL` of an id that does not exist | 8.4, 11.4, 11.8 | 1094: `Gone`, and the connection stays usable (K2) |
| C34: the end of a killed session (timing) | 8.4, 11.4, 11.8 | The session leaves `PROCESSLIST` within 1 s after `KILL CONNECTION` (the probes: 1 to 64 ms) |
| C35: `KILL CONNECTION` of a `COMMIT` that waits | 8.4, 11.4, 11.8 | Root holds `FLUSH TABLES WITH READ LOCK`, and the `COMMIT` of the test user waits. `Connection.kill` from another session of the test user ends it while the lock is held: the `COMMIT` gives `Outcome_unknown`, and after `UNLOCK TABLES` the write is gone (V7, the probe P1 of the plan of 1.5) |
| C36: `<=>` uses an index (phase 2f) | 8.4, 11.4, 11.8 | On a nullable `BIGINT` with an index, `EXPLAIN` of `n <=> 5` gives the type `ref` and the key of the index, as `n = 5`. Thus `==.` on an `option` field is not a scan (V7 of the plan of phase 2). The `FOR SHARE` of `for_share` is C21 |

The killer of the timing claims polls `information_schema.PROCESSLIST` and `INNODB_TRX` every 250 ms until the write waits on the lock. A faster poll sees the same old rows each time: InnoDB fills `INNODB_TRX` again only when nobody read it for 100 ms.

Other facts of the reviews: 1557 (`ER_FOREIGN_DUPLICATE_KEY_OLD_UNUSED` on MySQL, `ER_UNUSED_4` on MariaDB) has no row in the error mapping, because the servers do not send it. MySQL `CHECK` constraint names are unique in the schema (version 1 has no `CHECK`). The grammar of MariaDB 11.4.13 and 11.8.8 (`sql/sql_yacc.yy`) has no `FOR SHARE`, thus Cistern uses `LOCK IN SHARE MODE` on MariaDB (C21).

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
   - 1.2: `Pool_state`, the weft shell, the opener, the deadlines, the session reset at checkout, `Db.with_db`, `Db.exec`, the events. Cancel with the `kill=false` behavior. Simulator tests, with a fake server of many connections (`test/sim/fake_db.ml`).
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

Phase 0 is done (2026-11-07): ocaml-caravanserai/cistern#1 (the skeleton), #2, #3 and #4 (`cistern.core`) and #5 (the test servers). The CI of #5 passed on all jobs, also the three macOS legs of `db.yml`. Phase 1.1 starts with its plan of six pull requests.

Phases 1.1, 1.2 and 1.3 are done (2026-11-07): ocaml-caravanserai/cistern#6 to #20. Phase 1.4 (TLS): 1.4a is cistern#21, 1.4b1 (TLS on `Connection`) is cistern#22, and 1.4b2 (the pool and the storm on TLS) is cistern#23 (merged 2026-11-08). 1.4c (the integration tests, the claims C29 to C31, and MariaDB 11.4 on each PR) is cistern#24 (merged 2026-11-08), and it completes phase 1.4. Phase 1.5 (cancel with `KILL`): 1.5a (the `Killing` slots of `Pool_state`) is cistern#25, 1.5b (round trip 3 with the identity and the privilege check, `Connection.kill` and the events of `KILL`) is cistern#26, 1.5c1a (the kill queue, the killer with its control connection, and the kill requests of `Pool.exec` and `Tx`) is cistern#27, 1.5c1b (the drain at the end of `with_db`) is cistern#28, and 1.5c2 (the storms with `KILL`) is cistern#29 (all merged 2026-11-08). 1.5d (the integration tests of `KILL` and the claims C32 to C35) completes phase 1.5 and phase 1.

Phase 2 (the typed API with hand-written models): 2a0 (the events with no message of a server error) is cistern#31, 2a (`Field`, the model AST, `Expr` with the two-valued filters, `Update`, `module type MODEL` and the rule of lowercase table names) is cistern#32, 2b (the SQL AST, the compiler and the serializer of `cistern.sql`, with the suite `test/expect`) is cistern#33, 2c (the DDL text and the comparison of `check_schema`) is cistern#34, 2d1 (`Model`, the one-row operations, the typed statements and the hand-written models of `test/model`) is cistern#35, 2d2 (`Query`, `all`, `where`, `filter_by_*`, the finishers, the bulk forms and the check of `for_update`) is cistern#36, and 2e (`Db.push_schema` and `Db.check_schema`) is cistern#37 (all merged 2026-11-08). 2f (the integration tests of the typed API and the claim C36) completes phase 2.

Phase 3 (`ppx_cistern` for models without relations) is complete. 3a0 (cistern#40) moves `Schema` into the library `cistern.schema`, with the structured faults of `Schema.faults`. 3a (cistern#41) adds the deriver `cistern`, the parser of a group, all the errors of the ppx with their locations, and the suite `test/ppx`. 3b (cistern#42) generates stage 1 (`Cistern.Private.Encode`, the lift of the one `Schema.model` value), and the hand-written stage 1 of `kinds`, `auto_only` and the new `person` has the form of the ppx. 3c (cistern#43) generates stages 2 and 3 (labels for `update` and `create`), and the hand-written files have the whole form of the ppx (3a0 to 3c merged 2026-11-08). 3d completes phase 3: it moves the tests of the groups with no relation to the derived models of `test/model_ppx`, and adds the hostile group with its SQL names to the integration suite. The group of `user` and `todo` stays hand-written, with the tests of its relation, until phase 4.

Phase 4 (relations): 4a0 (cistern#45) fixes the merged code of phases 1.3 and 2: error 1020 of MariaDB (`innodb_snapshot_isolation`, the default of 11.8) is `Retryable `Rolled_back`, so a `Tx` fails with no ping and no `Implicit_commit` (claim C37); cistern#48 fixed a flaky property of 4a0. 4a (cistern#46) adds `Cistern.Deferred`, the abstract `Relation.t` with the constructors of `Cistern.Private.Relation`, and the relation of a fault of `Schema.faults`. 4b1 (cistern#47) runs an atomic statement of `Db.exec` through the code of `Db.transaction`, with one event for each command, and the pool storm runs atomic statements with the invariant that no holder gets a session in a transaction. 4b2 (cistern#49) runs an atomic statement of `Tx.exec` through the savepoint code of `Tx.savepoint` (E6), and the Tx storm runs atomic statements in 1 of 10 steps, with one event for each command. 4c1 (cistern#50) adds the nested create (`Model.child`, `Model.create ~children`), the reset of a `belongs_to` relation for a new key (`Encode.reset`), and the relation fields of the hand-written `user` and `todo` with their setters. 4c2 (cistern#51) adds the relation scopes and `Rel` of the hand-written `user` and `todo`, `Model.get_parent`, and the hand-written tree `node` (a self relation with a nullable key, `test/model/tree.ml`). 4d (cistern#52) adds `Query.include_`, and the flag `multi` of a statement of several reads (one event for each command). 4e (cistern#54) adds the integration rows of relations on the three servers and the claim C38. 4f (cistern#55) adds the parser of the relation fields to the ppx (E12, E15). 4g (cistern#56) adds the generated code of relations, and writes the hand-written `user`/`todo` and `node` again from the generator. (4a0 to 4g merged 2026-11-08.) 4h runs the run-time tests of relations on the derived `user`/`todo` and `node`, and the hostile relation group with SQL names on the three servers, and it completes phase 4.

Phase 5 (hardening and documentation): 5a1 (cistern#58) gives the odoc gate (`make doc`, no warning). 5a0 fixes a defect of phase 1.3: a cancel in the scope of `Tx.savepoint` that the callback caught left the `Tx` usable, and a later `COMMIT` committed the part of the scope that ran. Now the `Tx` fails with `Connection_lost`. 5a2 gives the pages `doc/index.mld`, `doc/limits.mld` and `doc/security.mld`, `Cistern_core.Limits` with its unit rows, and the doc of each error. 5b gives the README of the user, `CONTRIBUTING.md`, the examples of `examples/` with the README rule, and their integration tests. 5c gives `make soak` (`tools/soak.sh`) and its nightly step on mysql84 for the flaky TLS test of cistern#39.

Round 5 found 3 critical defects. Two of them were in fixes of round 4 (the session reset and the killer drain), and one was a gap from an earlier draft (`Decimal`). Most of the critical and major findings are in the cancel path with `KILL`. Thus phases 1.1 to 1.4 ship without `KILL` (`kill=false`), and phase 1.5 adds it. The remaining risk is in server behavior that only a real server can show (the "not verified" claims). Phase 1.1 tests these claims first.
