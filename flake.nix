{
  description = "Cistern: an ORM for OCaml 5.5 on weft, for MySQL and MariaDB";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";

    # The test library and its ppx (expect tests, coverage, mutation). It has
    # no release. flake.lock pins a commit of its main branch.
    windtrap = {
      url = "github:ocaml-caravanserai/windtrap";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    # flake.lock pins one commit of weft main.
    weft = {
      url = "github:ocaml-caravanserai/weft";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.windtrap.follows = "windtrap";
    };
  };

  # Another flake can use Cistern in three ways:
  #
  # - In its own OCaml scope. This uses the compiler of that scope. The scope
  #   must also have weft (weft.lib.package):
  #
  #     ocamlPackages = pkgs.ocaml-ng.ocamlPackages_5_5.overrideScope
  #       (final: prev: {
  #         weft = final.callPackage weft.lib.package { };
  #         cistern = final.callPackage cistern.lib.package { };
  #         ppx_cistern = final.callPackage cistern.lib.ppxPackage { };
  #       });
  #
  # - With the overlay, which adds weft, cistern and ppx_cistern to
  #   ocaml-ng.ocamlPackages_5_5.
  #
  # - As cistern.packages.${system}.cistern and
  #   cistern.packages.${system}.ppx_cistern, with
  #   inputs.cistern.inputs.nixpkgs.follows = "nixpkgs".
  outputs =
    {
      self,
      nixpkgs,
      windtrap,
      weft,
    }:
    let
      # nixpkgs dropped x86_64-darwin.
      systems = [
        "x86_64-linux"
        "aarch64-linux"
        "aarch64-darwin"
      ];

      version = "0.1.0-dev";

      meta = lib: {
        homepage = "https://github.com/ocaml-caravanserai/cistern";
        license = lib.licenses.mpl20;
      };

      # The directories of the four libraries of the package cistern.
      libraryDirs = [
        ./core
        ./sql
        ./mysql
        ./cistern
      ];

      # The source of a package has only the files of its build. Thus the
      # package does not run the tests.
      source =
        lib: dirs:
        lib.fileset.toSource {
          root = ./.;
          fileset = lib.fileset.unions (
            [
              ./dune-project
              ./LICENSE
              ./README.md
            ]
            ++ dirs
          );
        };

      # The four libraries cistern.core, cistern.sql, cistern.mysql and
      # cistern.
      package =
        {
          lib,
          buildDunePackage,
          weft,
          ptime,
        }:
        buildDunePackage {
          pname = "cistern";
          inherit version;
          minimalOCamlVersion = "5.5";
          src = source lib ([ ./cistern.opam ] ++ libraryDirs);
          propagatedBuildInputs = [
            weft
            ptime
          ];
          doCheck = false;
          meta = meta lib // {
            description = "An ORM for OCaml 5.5 on weft, for MySQL and MariaDB";
          };
        };

      # The deriver [@@deriving cistern].
      ppxPackage =
        {
          lib,
          buildDunePackage,
          ppxlib,
        }:
        buildDunePackage {
          pname = "ppx_cistern";
          inherit version;
          minimalOCamlVersion = "5.5";
          src = source lib [
            ./ppx_cistern.opam
            ./ppx
          ];
          propagatedBuildInputs = [ ppxlib ];
          doCheck = false;
          meta = meta lib // {
            description = "The deriver of Cistern models";
          };
        };

      # The OCaml 5.5 scope of nixpkgs, with weft and windtrap from the
      # inputs, and the two packages of this flake. nixpkgs has an older
      # windtrap, thus the scope overrides it.
      scope =
        pkgs:
        pkgs.ocaml-ng.ocamlPackages_5_5.overrideScope (
          final: _: {
            weft = final.callPackage weft.lib.package { };
            windtrap = final.callPackage windtrap.lib.windtrap { };
            ppx_windtrap = final.callPackage windtrap.lib.ppx_windtrap { };
            cistern = final.callPackage package { };
            ppx_cistern = final.callPackage ppxPackage { };
          }
        );

      forAllSystems =
        f:
        nixpkgs.lib.genAttrs systems (
          system:
          let
            pkgs = nixpkgs.legacyPackages.${system};
          in
          f pkgs (scope pkgs)
        );
    in
    {
      lib = { inherit package ppxPackage; };

      # weft is not in nixpkgs. Thus the overlay adds weft first, with the
      # overlay of the weft input.
      overlays.default = nixpkgs.lib.composeManyExtensions [
        weft.overlays.default
        (_: prev: {
          ocaml-ng = prev.ocaml-ng // {
            ocamlPackages_5_5 = prev.ocaml-ng.ocamlPackages_5_5.overrideScope (
              ofinal: _: {
                cistern = ofinal.callPackage package { };
                ppx_cistern = ofinal.callPackage ppxPackage { };
              }
            );
          };
        })
      ];

      packages = forAllSystems (
        _: ocamlPackages: {
          inherit (ocamlPackages) cistern ppx_cistern;
          default = ocamlPackages.cistern;
        }
      );

      # The build of all the code, and the tests that need no database
      # server. The build uses the dev profile, thus a warning is an error.
      # The integration suites use their own dune alias, not @runtest, thus
      # this check never needs a server.
      checks = forAllSystems (
        pkgs: ocamlPackages: {
          inherit (ocamlPackages) cistern ppx_cistern;

          # The two packages from a nixpkgs with overlays.default.
          overlay =
            let
              o =
                (import nixpkgs {
                  inherit (pkgs.stdenv.hostPlatform) system;
                  overlays = [ self.overlays.default ];
                }).ocaml-ng.ocamlPackages_5_5;
            in
            pkgs.symlinkJoin {
              name = "cistern-overlay";
              paths = [
                o.cistern
                o.ppx_cistern
              ];
            };

          tests = pkgs.stdenv.mkDerivation {
            name = "cistern-tests";
            src = source pkgs.lib (
              [
                ./cistern.opam
                ./ppx_cistern.opam
                ./ppx
                ./test
              ]
              ++ libraryDirs
            );
            # Not `with ocamlPackages`: the names weft and windtrap are also
            # the inputs of this flake, and a `with` does not hide them.
            nativeBuildInputs = [
              ocamlPackages.ocaml
              ocamlPackages.dune_3
              ocamlPackages.findlib
            ];
            # The libraries of the two packages, and the test libraries. Thus
            # a new dependency of a package needs no change here.
            buildInputs =
              ocamlPackages.cistern.propagatedBuildInputs
              ++ ocamlPackages.ppx_cistern.propagatedBuildInputs
              ++ [
                ocamlPackages.windtrap
                ocamlPackages.ppx_windtrap
              ];
            buildPhase = ''
              runHook preBuild
              export DUNE_CACHE=disabled
              dune build @install @runtest --display short
              runHook postBuild
            '';
            installPhase = "touch $out";
          };
        }
      );

      devShells = forAllSystems (
        pkgs: ocamlPackages: {
          default = pkgs.mkShell {
            # The compiler, dune, findlib and each library dependency.
            inputsFrom = [
              ocamlPackages.cistern
              ocamlPackages.ppx_cistern
            ];
            packages = [
              # The tests.
              ocamlPackages.windtrap
              ocamlPackages.ppx_windtrap
              # The version of .ocamlformat, as in weft.
              pkgs.ocamlformat_0_29_0
              # Dev tools.
              ocamlPackages.ocaml-lsp
              ocamlPackages.odoc
            ];
          };
        }
      );

      formatter = forAllSystems (pkgs: _: pkgs.nixfmt);
    };
}
