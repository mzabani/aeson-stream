{
  description = "aeson-stream's flake";
  inputs.haskellNix.url =
    "github:input-output-hk/haskell.nix/c2f14344f119f68c10be2ea84fd372d8d8d16cd7";
  inputs.nixpkgs.follows = "haskellNix/nixpkgs-unstable";
  inputs.flake-utils.url = "github:numtide/flake-utils";

  # We only have flake-compat here while we support nix-shell and
  # nix-build, i.e. while we support non-flakes Nix usage.
  inputs.flake-compat = {
    url = "github:edolstra/flake-compat";
    flake = false;
  };
  outputs = { self, nixpkgs, flake-utils, haskellNix, flake-compat }:
    flake-utils.lib.eachSystem [
      "x86_64-linux"
      "aarch64-linux"
      "x86_64-darwin"
      "aarch64-darwin"
    ] (system:
      let
        pkgs = import nixpkgs {
          inherit system overlays;
          inherit (haskellNix) config;
        };

        overlays = [
          haskellNix.overlay
          (final: prev:
            let finalIohkPkgs = final.haskell-nix.haskellPackages;
            in {
              # This overlay adds our project to pkgs
              aesonStreamProject = final.haskell-nix.project' {
                src = ./.;
                compiler-nix-name = "ghc8107";

                modules = [{
                  # Work around https://github.com/input-output-hk/haskell.nix/issues/231. More info
                  # in aeson-stream.cabal
                  packages.aeson-stream.components.tests.aeson-stream-tests.build-tools =
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
                # Non-Haskell shell tools go here
                shell.buildInputs = with pkgs; [
                  ghcid
                  haskellPackages.brittany # Brittany from the LTS is older than this
                  # finalIohkPkgs.brittany.components.exes.brittany
                  glibcLocales
                ];
                shell.shellHook = ''
                  # source scripts/source-env.sh .env
                '';
              };
            })
        ];

        flake = pkgs.aesonStreamProject.flake { };
      in flake);
}
