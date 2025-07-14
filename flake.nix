{
  description = "hoogle_plus";

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
        config.allowBroken = true;
      };
      ghc844-no-omit-yields = pkgs-2003.haskell.compiler.ghc844.overrideAttrs (old: {
        patches = old.patches ++ [./base-no-omit-yields.patch];
      });

      hPkgs844 = pkgs-2003.haskell.packages.ghc844.extend (self: super: {
        haskell-src-exts = self.callHackage "haskell-src-exts" "1.20.3" {};
        hoogle = self.callHackage "hoogle" "5.0.17.3" {};
      });

      pkgs = import nixpkgs {
        inherit system;
        config.allowUnfree = true;
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
      hplus = hPkgs844.mkDerivation rec {
        pname = "HooglePlus";
        version = "0-unstable-20250713";
        src = ./.;
        sha256 = "4b1a5c8d3f0e2f6c7b8c9e2d3f4e5f6a7b8c9e2d3f4e5f6a7b8c9e2d3f4e5f6";
        doCheck = false;
        doHaddock = false;
        license = "MIT";

        isLibrary = true;
        isExecutable = true;
        librarySystemDepends = with pkgs; [
          ncurses
          gmp
          glibcLocales
          libffi
          zlib
          pkgs-2003.z3
        ];

        buildDepends = with hPkgs844; [
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
        ];
      };
    });

    devShells = forAllSystems (system: let
      pkgs = nixpkgs.legacyPackages.${system};
      pkgs-2003 = nixpkgs-2003.legacyPackages.${system};
    in {
      default = pkgs.mkShell {
        shellHook = ''
          export LANG=en_US.UTF-8
          export LC_ALL=en_US.UTF-8
          export LC_CTYPE=en_US.UTF-8
          export LD_LIBRARY_PATH="${pkgs.zlib}/lib:$LD_LIBRARY_PATH"
        '';

        packages = with pkgs; [
          stack
          ncurses
          gmp
          glibcLocales
          libffi
          zlib
          pkgs-2003.z3
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
