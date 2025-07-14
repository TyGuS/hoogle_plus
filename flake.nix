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
      pkgs-2003 = nixpkgs-2003.legacyPackages.${system};
      ghc844-no-omit-yields = pkgs-2003.haskell.compiler.ghc844.overrideAttrs (old: {
        patches = old.patches ++ [./base-no-omit-yields.patch];
      });
      hPkgs844 = pkgs-2003.haskell.packages.ghc844;

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
      hPkgs884 = pkgs.haskell.packages.ghc884;
    in {
      hplus = hPkgs844.mkDerivation rec {
        pname = "HooglePlus";
        version = "0-unstable-20250713";
        src = ./.;
        sha256 = "4b1a5c8d3f0e2f6c7b8c9e2d3f4e5f6a7b8c9e2d3f4e5f6a7b8c9e2d3f4e5f6";
        doCheck = false;
        license = "MIT";

        buildDepends = with hPkgs844;
          [
            ChasingBottoms
          ]
          ++ (
            with hPkgs884; [
              z3
              packdeps
              smallcheck
              yaml
              vector
              uuid
              hoogle
              aeson
              MissingH
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
            ]
          );
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
          self.outputs.packages.${system}.ghc844-no-omit-yields

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
