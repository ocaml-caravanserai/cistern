# Cistern

Cistern is an ORM and database library for OCaml 5.5, modeled on [Toasty](https://github.com/tokio-rs/toasty). It runs on [weft](https://github.com/ocaml-caravanserai/weft), a structured concurrency library with effects.

Status: in development. No release exists yet. Do not use it for production data.

Version 1 has these features:

- One pure OCaml client for MySQL 8.4 and MariaDB 11.4 and 11.8, with the text protocol and TLS.
- Models from one recursive record type with `[@@deriving cistern]`, with Toasty-style relations (`has_many`, `belongs_to`, `Deferred.t`, `include_`, nested create).
- Typed filters, a pool with deadlines, the cancel of a statement on the server with `KILL`, transactions with savepoints, and raw SQL from fragments.

The odoc of the package `cistern` has the API and the pages [index](doc/index.mld) (the order of reading, the servers, the session and the operations), [limits](doc/limits.mld) and [security](doc/security.mld). [DESIGN.md](DESIGN.md) gives the design. [CONTRIBUTING.md](CONTRIBUTING.md) is for the developers of Cistern.

## Contents

- [Install](#install)
- [Quick start](#quick-start)
- [Models](#models)
- [Queries and filters](#queries-and-filters)
- [Writes](#writes)
- [Transactions](#transactions)
- [Relations](#relations)
- [Raw SQL](#raw-sql)
- [Errors and events](#errors-and-events)
- [TLS](#tls)
- [Servers and session](#servers-and-session)
- [Operations](#operations)
- [License](#license)

## Install

Cistern needs OCaml 5.5, and weft and weft-tls from their git repository (they are not in opam-repository). Use the commit of weft that `flake.lock` of Cistern gives.

With Nix, add the flake `github:ocaml-caravanserai/cistern` as an input. Its overlay gives the packages `cistern` and `ppx_cistern`.

With opam, pin `cistern` and `ppx_cistern`. The `pin-depends` of `cistern.opam` pin weft and weft-tls (and windtrap for the tests) at the commits of the `flake.lock` of the same Cistern commit, and opam asks you to accept these pins. Pin a commit or a tag of Cistern, not a branch that moves:

```sh
opam pin add cistern git+https://github.com/ocaml-caravanserai/cistern#<commit>
opam pin add ppx_cistern git+https://github.com/ocaml-caravanserai/cistern#<commit>
```

In dune, use the libraries `cistern`, `weft.os` and `mirage-crypto-rng.unix`, and `(preprocess (pps ppx_cistern))`.

## Quick start

The program calls `main` with the URL of the database, for example `mysql://app:secret@db.example.com/app?ssl-ca=/etc/ssl/db-ca.pem`. The doc of `Cistern.Config.of_url` gives the keys of the URL.

<!-- example: quickstart -->
```ocaml
open Cistern

type note =
  { id: int64 [@key] [@auto]
  ; title: string
  ; body: string option
  ; pinned: bool [@default false] }
[@@deriving cistern]

let main url =
  (* TLS takes its randomness from the default generator. *)
  Mirage_crypto_rng_unix.use_default () ;
  Weft.run (module Weft_os)
  @@ fun () ->
  let config =
    match Config.of_url url with Ok c -> c | Error m -> failwith m
  in
  Db.with_db config
  @@ fun db ->
  Db.push_schema db [Note.model] ;
  let n = Note.create ~title:"First note" ~pinned:true () |> Db.exec_exn db in
  Printf.printf "created the note %Ld\n" n.id ;
  Note.where Note.Q.(pinned ==. true)
  |> Query.order_by Note.Q.id `Asc
  |> Query.to_list |> Db.exec_exn db
  |> List.iter (fun (n : Note.t) -> print_endline n.title)
```

`Mirage_crypto_rng_unix.use_default ()` is necessary for TLS: without it, `Db.with_db` raises `Invalid_argument`. `Db.with_db` owns the pool. Each statement is a value of type `'a Stmt.t`, and `Db.exec_exn` (or `Db.exec`, which gives a result) runs it.

The examples of this page are in [examples/](examples/). They compile in each build, and the integration suite runs them on the three servers.

## Models

One recursive record type gives the models of a group. Each record has one `[@key]`. `[@auto]` makes the server give the value. The deriver makes one module for each type (`Author`, `Post`) with `create`, `draft`, `get_by_id`, `find_by_id`, `get_by_<field>` and `find_by_<field>` for each `[@unique]` field, `filter_by_<field>` for each `[@index]` field, `where`, `all`, `update`, `delete`, the fields of the filters (`Q`), and the relations (`Rel`).

<!-- example: models -->
```ocaml
type author =
  { id: int64 [@key] [@auto]
  ; name: string
  ; email: string [@unique]
  ; posts: post list Cistern.Deferred.t [@has_many] }

and post =
  { id: int64 [@key] [@auto]
  ; author_id: int64
  ; author: author Cistern.Deferred.t
        [@belongs_to {key= author_id; references= id}]
  ; title: string [@index]
  ; views: int [@default 0]
  ; published: bool [@default false] }
[@@deriving cistern]

let models = [Author.model; Post.model]
```

`Db.push_schema db Blog.models` makes the tables that do not exist, and `Db.check_schema` compares them with the models. Give all the models of a program together.

These rules of models are not checked by the ppx:

- An attribute after the `;` of a field belongs to that field, also on a line of its own.
- With another deriver that reads a bare name (`[@key]`, `[@default]`), use the long forms (`[@cistern.key]`).
- The ppx does not know a type alias of the program. Use the type itself.
- Two names that differ only in the case of a non-ASCII letter pass the ppx, and the server refuses the DDL.

## Queries and filters

The operators of `Q` make a typed filter of one model: `==.`, `<>.`, `<.`, `>.`, `<=.`, `>=.`, `is_null`, `in_`, `starts_with`, `contains`, `starts_with_opt`, `contains_opt`, `&&`, `||` and `not`. The doc of `Cistern.Expr.OPS` gives the SQL of each.

<!-- example: queries -->
```ocaml
(* The field names of [Post.Q] hide the values of the same names in
   [Post.Q.( ... )]. Thus the author id has another name here. *)
let published_titles db aid =
  Post.where Post.Q.(author_id ==. aid && published ==. true)
  |> Query.order_by Post.Q.views `Desc
  |> Query.limit 10 |> Query.to_list |> Db.exec_exn db
  |> List.map (fun (p : Post.t) -> p.title)

let popular db =
  Post.where Post.Q.(views >=. 100) |> Query.count |> Db.exec_exn db

let by_email db email = Author.find_by_email email |> Db.exec_exn db
```

## Writes

<!-- example: writes -->
```ocaml
let writes db =
  let ada =
    Author.create ~name:"Ada" ~email:"ada@example.com" () |> Db.exec_exn db
  in
  let ada = Author.update ~name:"Ada L." ada |> Db.exec_exn db in
  let post =
    Post.create ~author_id:ada.id ~title:"Notes" () |> Db.exec_exn db
  in
  let _post = Post.update ~views:1 post |> Db.exec_exn db in
  (* An update of each row that the query finds: the number of rows. *)
  let changed =
    Post.where Post.Q.(author_id ==. ada.id)
    |> Query.update (Update.set Post.Q.published true)
    |> Db.exec_exn db
  in
  Printf.printf "published %d posts\n" changed ;
  Post.where Post.Q.(author_id ==. ada.id)
  |> Query.delete |> Db.exec_exn db |> ignore ;
  Author.delete ada |> Db.exec_exn db
```

`update` sets only the fields that it gets, and gives the new record. `Query.update` and `Query.delete` give the number of rows.

## Transactions

<!-- example: transactions -->
```ocaml
(* The posts of the author are locked until the end of the transaction. A
   [Cistern_error] from [Tx.exec_exn] fails the transaction: [Db.transaction]
   rolls back and gives the error. *)
let publish_all db aid =
  Db.transaction db (fun tx ->
      let posts =
        Post.where Post.Q.(author_id ==. aid && published ==. false)
        |> Query.for_update |> Query.to_list |> Tx.exec_exn tx
      in
      List.iter
        (fun p -> ignore (Post.update ~published:true p |> Tx.exec_exn tx))
        posts ;
      List.length posts )
```

`Db.transaction` has a deadline for the whole transaction (`?timeout`, 30 s by default). `Tx.savepoint` runs a part of a transaction in a savepoint. Do not use DDL in a transaction: the server commits before a DDL statement, and the transaction fails with `Implicit_commit`.

A transaction that fails with `Retryable` can run again:

<!-- example: retry -->
```ocaml
(* [Retryable] (a deadlock, for example): the transaction was rolled back, by
   the server or by Db.transaction when the error left the callback. Thus a new
   run of the whole transaction is safe. Cistern has no helper for this. Do not
   retry [Outcome_unknown] or [Implicit_commit]: a part of the transaction can
   be committed. *)
let rec with_retry ?(tries = 3) db f =
  match Db.transaction db f with
  | Error (Error.Retryable _) when tries > 1 ->
      with_retry ~tries:(tries - 1) db f
  | result ->
      result
```

On MariaDB 11.8, a transaction that reads a row, then writes or locks it after another session changed it, fails with `Retryable` (error 1020). Thus use a retry loop on MariaDB 11.8.

## Relations

<!-- example: relations -->
```ocaml
(* A nested create: the author and the posts in one transaction. *)
let create_with_posts db =
  Author.create ~name:"Mo" ~email:"mo@example.com"
    ~posts:[Post.draft ~title:"One" (); Post.draft ~title:"Two" ()]
    ()
  |> Db.exec_exn db

(* The query of a relation: the posts of an author, and the author of a post. *)
let posts_of db (a : Author.t) =
  Author.posts a |> Query.to_list |> Db.exec_exn db

let author_of db (p : Post.t) = Post.author p |> Db.exec_exn db

(* include_ loads the posts of each author with one more query, into the field
   [posts]. All the rows stay in memory: put a limit on the main query. *)
let authors_with_posts db =
  Author.all
  |> Query.include_ Author.Rel.posts
  |> Query.order_by Author.Q.id `Asc
  |> Query.limit 100 |> Query.to_list |> Db.exec_exn db
  |> List.map (fun (a : Author.t) ->
      (a.name, List.length (Deferred.get_exn a.posts)) )
```

A field of type `Deferred.t` is `Unloaded` in a record from a query. `Query.include_` and a nested create make it `Loaded`.

## Raw SQL

<!-- example: raw -->
```ocaml
(* Raw.v gives a literal of the client for a value. Put each value of a
   user in Raw.v, never in the text of Raw.sql. *)
let titles_with_views db min =
  let open Raw in
  let r =
    Raw.query
      ( sql "SELECT `title` FROM `post` WHERE `views` >= "
      ++ v (Value.Int (Int64.of_int min))
      ++ sql " ORDER BY `title`" )
    |> Db.exec_exn db
  in
  List.map
    (function [|Value.Text t|] -> t | _ -> failwith "a row of one text")
    r.rows
```

`Raw` has no quoting of identifiers: never build a table or column name from the input of a user.

## Errors and events

<!-- example: errors -->
```ocaml
(* Db.exec gives a result. The doc of Cistern.Error tells what each error
   means and what to do. *)
let add_author db ~name ~email =
  match Author.create ~name ~email () |> Db.exec db with
  | Ok a ->
      Ok a
  | Error (Error.Constraint_violation {kind= Unique; _}) ->
      Error "this email has an author"
  | Error e ->
      (* Error.pp_summary prints no value of a row: use it for logs. *)
      Error (Format.asprintf "%a" Error.pp_summary e)
```

The doc of `Cistern.Error` has a list of each error, what it tells, and what to do. The most important rules:

- `Outcome_unknown`: a write was sent, and its reply was lost. It can be committed. Do not retry it blindly.
- `Retryable`: the transaction was rolled back, by the server or by `Db.transaction` when the error left the callback. A new run of the whole transaction is safe. With `innodb_rollback_on_timeout` off (the default), a `Retryable `Lock_timeout` rolls back only the statement, and the `Tx` stays usable.
- `Error.pp` and `Error.to_string` print the message of the server, which can hold a value of a row. Use `Error.pp_summary` for logs.

<!-- example: events -->
```ocaml
(* The hook gets an event for each statement, connect and kill. The SQL of
   an event has [?] in place of each value. *)
let with_logged_db config f =
  let on_query e = Format.eprintf "cistern: %a@." Event.pp e in
  Db.with_db ~on_query config f
```

## TLS

- The `ssl-mode` of the URL has the names of MySQL: `DISABLED`, `REQUIRED`, `VERIFY_CA` and `VERIFY_IDENTITY`. `ssl-ca=<path>` with no `ssl-mode` gives `VERIFY_IDENTITY`.
- `REQUIRED` with no `ssl-fingerprint` does not check the server. Use `VERIFY_IDENTITY` or a pinned certificate.
- On a Unix socket (`socket=<path>`), version 1 has no TLS. The full path of `caching_sha2_password` on a socket needs the opt-in `allow-cleartext=socket`.

The page [security](doc/security.mld) has the other facts.

## Servers and session

- The servers: MySQL 8.4, MariaDB 11.4 and MariaDB 11.8. The authenticators: `caching_sha2_password` and `mysql_native_password`.
- Each connection sets `utf8mb4` with a binary collation (text compares case-sensitive), `time_zone = '+00:00'` (the times are UTC), and a strict `sql_mode`. A zero `DATETIME` gives `Decode`.
- An indexed `string` is `VARCHAR(255)`, and a `Ptime.t` is `DATETIME(6)` from the year 1000.

## Operations

When a deadline or a cancel stops a statement, a control connection of the pool sends `KILL CONNECTION` for its session (`kill=true`, the default). These rules apply:

- On MySQL, `performance_schema` must be on. Each MariaDB server needs a unique `@@server_id` or `@@hostname`. A cloned MySQL data directory needs a new `auto.cnf`.
- The control connection counts toward `max_connections`: one pool has at most `pool_size` + 1 sessions. Each `Db.push_schema` that runs adds one more session for its time.
- Behind a proxy (ProxySQL, MaxScale, MySQL Router), use `kill=false`.
- The event `Kill_off` tells that `KILL` went off for the pool.

The page [index](doc/index.mld) has all the rules, and [limits](doc/limits.mld) has the numbers.

## License

MPL-2.0. See [LICENSE](LICENSE).
