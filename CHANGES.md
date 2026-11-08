# Changes

## 0.1.0 (not released)

The first version. It is prepared, but it has no tag and no opam publication.

### Features

- One pure OCaml client for MySQL and MariaDB on weft, with the text protocol, TLS (`ssl-mode`, a CA file or a pinned certificate) and the authenticators `caching_sha2_password` and `mysql_native_password`.
- A pool with deadlines, and the cancel of a statement on the server with `KILL CONNECTION`, after an exact check of the identity of the server.
- Transactions (`Db.transaction`, `Tx`) with savepoints, and the rules of each error of the server in a transaction.
- Models from one recursive record type with `[@@deriving cistern]` (the package `ppx_cistern`): `has_many`, `belongs_to`, `Deferred.t`, `Query.include_`, and a nested create of one level.
- Typed filters, `order_by`, `limit`, `offset`, `to_list`, `first`, `count`, `exists`, `for_update`, `for_share`, and bulk `update` and `delete`.
- Raw SQL from fragments (`Raw`), with the values as literals of the client.
- `Db.push_schema` and a reduced `Db.check_schema`.
- Events of each statement, connect and kill (`?on_query`), with redacted SQL and errors.
- The odoc of the packages with the pages index, limits and security, and a README whose examples compile and run on the servers.

### Supported servers

- MySQL 8.4, MariaDB 11.4 and MariaDB 11.8. MySQL 5.7 refuses the session setup (error 1273).

### Known limits

- `check_schema` compares the columns (the type, the length, the precision, the collation, unsigned, `NULL` and `AUTO_INCREMENT`), but not the indexes and the foreign keys.
- The deriver does not work in a `.mli` file.
- No prepared statements, no binary protocol, no packets of 16 MiB or more.
- No `has_one`, no composite keys, no nested create of more than one level, no `DECIMAL`, `JSON` or `ENUM` fields.
- `Query.include_` holds all the rows in memory: put a limit on the main query.
- Behind a proxy (ProxySQL, MaxScale, MySQL Router), use `kill=false`.
- DESIGN.md ("Later versions") has the full list.

### Phases

- Phase 0: the repository, the flake, the CI and the test servers.
- Phase 1: the client, the pool, transactions, TLS and the cancel with `KILL`.
- Phase 2: the typed API with hand-written models.
- Phase 3: the deriver for models with no relation.
- Phase 4: relations.
- Phase 5: hardening and documentation: the odoc gate, the doc pages, the README with checked examples, the soak of a flaky test, the release metadata, and a fix of a caught cancel in `Tx.savepoint`.
