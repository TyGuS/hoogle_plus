{
  description = "Nix derivation for HooglePlus";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-21.05";
    nixpkgs-2003.url = "github:NixOS/nixpkgs/nixos-20.03";
  };

  outputs = {
    self,
    nixpkgs,
    nixpkgs-2003,
  }: let
    systems = [
      "x86_64-linux"
      "aarch64-linux"
      "x86_64-darwin"
      "aarch64-darwin"
    ];

    forAllSystems = f: nixpkgs.lib.genAttrs systems (system: f system);
  in {
    packages = forAllSystems (system: let
      pkgs-2003 = import nixpkgs-2003 {
        inherit system;

        # packdeps is marked as broken in 20.03 but seems to work fine
        config.allowBroken = true;
      };

      # compile ghc844's base module with the `-fno-omit-yields` flag
      # https://downloads.haskell.org/~ghc/8.4.4/docs/html/users_guide/bugs.html#bugs-in-ghc
      ghc844-no-omit-yields = pkgs-2003.haskell.compiler.ghc844.overrideAttrs (old: {
        patches = old.patches ++ [./base-no-omit-yields.patch];
      });

      hPkgs844 = pkgs-2003.haskell.packages.ghc844.extend (self: super: {
        ghc = ghc844-no-omit-yields;

        # package version consistent with lts-12.26
        haskell-src-exts = self.callHackage "haskell-src-exts" "1.20.3" {};
        hoogle = self.callHackage "hoogle" "5.0.17.3" {};
      });

      pkgs = import nixpkgs {
        inherit system;
        overlays = [
          (final: prev: {
            inherit (pkgs-2003) z3;
            ghc = ghc844-no-omit-yields;
          })
        ];
      };
    in {
      inherit ghc844-no-omit-yields;

      hoogle = hPkgs844.hoogle;
      hplus = hPkgs844.mkDerivation {
        pname = "HooglePlus";
        version = "0-unstable-20250713";
        src = ./.;
        doCheck = false;
        doHaddock = false;
        license = "MIT";

        isLibrary = true;
        isExecutable = true;
        librarySystemDepends = with pkgs;
        with hPkgs844; [
          ncurses
          gmp
          glibcLocales
          libffi
          zlib
          pkgs-2003.z3
        ];

        libraryHaskellDepends = with hPkgs844; [
          haskell-src-exts
          ChasingBottoms
          MissingH
          packdeps
          smallcheck
          yaml
          vector
          uuid
          hoogle
          aeson
          ansi-terminal
          ansi-wl-pprint
          bimap
          ghc-paths
          heap
          hint
          html
          indents
          leancheck
          lens
          pqueue
          pretty-simple
          pretty-tree
          safe
          silently
          sort
          z3

          pkgs-2003.makeWrapper
        ];

        # hplus uses hint to dynamically evaluate Haskell code using ChasingBottoms
        postInstall = let
          ghc-with-cb = hPkgs844.ghcWithPackages (pkgs: with pkgs; [ChasingBottoms]);
        in ''
          wrapProgram $out/bin/hplus \
            --set GHC_PACKAGE_PATH "${ghc-with-cb}/lib/ghc-${ghc-with-cb.version}/package.conf.d:"
        '';
      };
    });

    devShells = forAllSystems (system: let
      pkgs = nixpkgs.legacyPackages.${system};
    in {
      default = pkgs.mkShell {
        packages = with pkgs; [
          self.outputs.packages.${system}.hoogle
          self.outputs.packages.${system}.hplus

          (python3.withPackages (
            ps:
              with ps; [
                pyyaml
                numpy
                tabulate
                matplotlib
                colorama
                pandas
              ]
          ))
        ];
      };
    });
  };
}
