# Cistern

Cistern is an ORM and database library for OCaml 5.5, modeled on [Toasty](https://github.com/tokio-rs/toasty). It runs on [weft](https://github.com/ocaml-caravanserai/weft), a structured concurrency library with effects.

Status: in development. No release exists yet. Do not use it for production data.

Version 1 has these features:

- One pure OCaml client for MySQL 8.4 and MariaDB 11.4 and 11.8, with the text protocol.
- Models from one recursive record type with `[@@deriving cistern]`, with Toasty-style relations (`has_many`, `belongs_to`, `Deferred.t`, `include_`, nested create).
- Typed filters, a pool with deadlines, transactions with savepoints, and raw SQL from fragments.

[DESIGN.md](DESIGN.md) gives the design and the phases of the implementation.

## Development

All the dependencies come from `flake.nix`. Use `nix develop`, then `dune build` and `dune runtest`.

`make help` lists the mutation targets and the test servers. `make mutate` is the gate of each pull request.

## Test servers

The integration tests of phase 1.1 will use local servers from the flake: MySQL 8.4, MariaDB 11.4 and MariaDB 11.8.

```sh
make db-up SERVER=mysql84      # or mariadb114, mariadb118
make db-check SERVER=mysql84   # the smoke check
make db-down                   # stop each server that runs
```

- Each `make db-up` makes a new data directory in `.db/<server>`. `DB_DIR` changes `.db`. The path of the Unix socket can have at most 107 bytes (103 on macOS).
- The ports are 13384 (`mysql84`), 13414 (`mariadb114`) and 13418 (`mariadb118`). `DB_PORT` changes the port. `DB_SERVER_ARGS` adds server options.
- The servers listen only on 127.0.0.1, with TLS from a test CA in `.db/certs`.
- The test user is `cistern_sha2` (MySQL, `caching_sha2_password`) or `cistern_native` (MariaDB, `mysql_native_password`). It has the password `cistern` and the database `cistern_test`.
- root has no password, and can log in only on the Unix socket. The directory `.db/<server>` has the mode 700.
- `.db/<server>/env` has these values for the tests: `DB_SERVER`, `DB_STORE`, `DB_VERSION`, `DB_PID`, `DB_PORT`, `DB_SOCKET`, `DB_USER`, `DB_PASSWORD`, `DB_DATABASE`, `DB_CA`, `DB_PLUGIN` and `DB_ADMIN_USER`.
- `make db-errors` compares the error names of the error mapping with `perror` of the three servers.

The workflow `db.yml` runs these targets each night, on request, and on a pull request that changes the files of the test servers. The server targets run on Linux and macOS. `make db-errors` runs only on Linux.

## License

MPL-2.0. See [LICENSE](LICENSE).
