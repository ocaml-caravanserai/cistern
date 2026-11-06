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

`make help` lists the mutation targets.

## License

MPL-2.0. See [LICENSE](LICENSE).
