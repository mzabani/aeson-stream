{
  # This is a template created by `hix init`
  inputs.haskellNix.url =
    "github:input-output-hk/haskell.nix/c2f14344f119f68c10be2ea84fd372d8d8d16cd7";
  inputs.nixpkgs.follows = "haskellNix/nixpkgs-unstable";
  inputs.flake-utils.url = "github:numtide/flake-utils";
  outputs = { self, nixpkgs, flake-utils, haskellNix }:
    let
      supportedSystems =
        [ "x86_64-linux" "x86_64-darwin" "aarch64-linux" "aarch64-darwin" ];
    in flake-utils.lib.eachSystem supportedSystems (system:
      let
        pkgs = import nixpkgs {
          inherit system overlays;
          inherit (haskellNix) config;
        };

        overlays = [
          haskellNix.overlay
          (final: prev:
            let
              finalIohkPkgs = final.haskell-nix.haskellPackages;
              hixProject = final.haskell-nix.project' {
                src = ./.;
                # evalSystem = "x86_64-linux";
                compiler-nix-name = "ghc8107";

                modules = [{
                  # Musl builds fail because postgresql-libpq requires pg_config in the path for its configure phase.
                  # See https://github.com/haskellari/postgresql-libpq/blob/master/Setup.hs#L65-L66
                  # packages.postgresql-libpq.components.library.build-tools =
                  #   [ postgres ];

                  # Work around https://github.com/input-output-hk/haskell.nix/issues/231. More info
                  # in codd.cabal
                  packages.aeson-stream.components.tests.aeson-stream.build-tools =
                    [
                      finalIohkPkgs.hspec-discover.components.exes.hspec-discover
                    ];
                }];

                # This is used by `nix develop .` to open a shell for use with
                # `cabal`, `hlint` and `haskell-language-server`
                shell.tools = {
                  cabal = "latest";
                  hlint = "latest";
                  haskell-language-server = "latest";
                };

                shell.buildInputs = with pkgs; [
                  ghcid
                  haskellPackages.brittany # Brittany from the LTS is older than this
                  # finalIohkPkgs.brittany.components.exes.brittany
                  glibcLocales
                ];
              };
            in { inherit hixProject; })
        ];
        flake = pkgs.hixProject.flake { };
      in flake // { legacyPackages = pkgs; });
}
