# Contributing to Cistern

[DESIGN.md](DESIGN.md) gives the design, the rules of the tests and the phases of the implementation.

## Development

All the dependencies come from `flake.nix`. Use `nix develop`, then `dune build` and `dune runtest`.

`make help` lists the targets. These are the gates of each pull request:

- `make mutate`: the mutation gate.
- `make check-scope` and `make check-ocaml55`.
- `make doc`: the odoc of the packages, with each warning of odoc as an error.
- The integration suite on the three servers, for a change of the library (the next section).

`dune runtest` also compares README.md with the regions of `examples/` (`test/readme`). After a change of an example, run `dune promote` to update README.md. Each OCaml block of README.md (a fence with the language `ocaml` or `ml`, in any case) has a marker `<!-- example: <name> -->` that names its region, or the marker `<!-- no-check -->`. The rule refuses a fence in another language than `sh`, `nix`, `dune` and `text`, a fence that is not at column 0 (in a list item or a blockquote), an indented code block, a `<pre>` block, and CRLF line ends. These rules are stricter than Markdown: indent a nested list with 2 spaces, put no paragraph with 4 spaces of indent in a list item, and do not write the text `<pre` in README.md.

## Test servers

The integration tests use local servers from the flake: MySQL 8.4, MariaDB 11.4 and MariaDB 11.8.

```sh
make db-up SERVER=mysql84      # or mariadb114, mariadb118
make db-check SERVER=mysql84   # the smoke check
make integration SERVER=mysql84
make db-down                   # stop each server that runs
```

- Each `make db-up` makes a new data directory in `.db/<server>`. `DB_DIR` changes `.db`. The path of the Unix socket can have at most 107 bytes (103 on macOS).
- The ports are 13384 (`mysql84`), 13414 (`mariadb114`) and 13418 (`mariadb118`). `DB_PORT` changes the port. `DB_SERVER_ARGS` adds server options.
- The servers listen only on 127.0.0.1, with TLS from a test CA in `.db/certs`.
- The test user is `cistern_sha2` (MySQL, `caching_sha2_password`) or `cistern_native` (MariaDB, `mysql_native_password`). It has the password `cistern` and the database `cistern_test`.
- The test user of MySQL cannot read `performance_schema.events_transactions_current`.
- root has no password, and can log in only on the Unix socket. The directory `.db/<server>` has the mode 700.
- `.db/<server>/env` has these values for the tests: `DB_SERVER`, `DB_STORE`, `DB_VERSION`, `DB_PID`, `DB_PORT`, `DB_SOCKET`, `DB_USER`, `DB_PASSWORD`, `DB_DATABASE`, `DB_CA`, `DB_FINGERPRINT`, `DB_OTHER_CA`, `DB_PLUGIN` and `DB_ADMIN_USER`.
- `make integration-timing` and `make integration-shutdown` run the groups `timing` and `shutdown`. The group `shutdown` stops the server.
- `make db-errors` compares the error names of the error mapping with `perror` of the three servers.

The workflow `db.yml` runs these targets each night, on request, and on a pull request that changes the files of the test servers. The server targets run on Linux and macOS. `make db-errors` runs only on Linux.

## Commits

Each commit is atomic and passes the gates. The text of the repository (comments, documentation, commit messages) follows ASD-STE100 Simplified Technical English, as DESIGN.md does. Each new source file starts with the MPL-2.0 header.
