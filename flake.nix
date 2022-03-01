{
  description = "xrwm";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs";
    flake-utils.url = "github:numtide/flake-utils";
    flake-compat = { url = "github:edolstra/flake-compat"; flake = false; };
  };

  outputs = { self, nixpkgs, flake-utils, ... }:
    flake-utils.lib.eachSystem
      (with flake-utils.lib.system; [
        x86_64-linux

      ])
      (system:
        let
          pkgs = import nixpkgs {
            inherit system;
          };

          systemLibraries = with pkgs; [
            xorg.libX11
            xorg.libXinerama
            xorg.libXcursor
            xorg.libXrandr
            xorg.libXi
            xorg.libXxf86vm
            xorg.libXext
            libGL
            zlib
            mesa
            glslang

            vulkan-headers
          ];



          dynamicLibraries = with pkgs; [
            vulkan-loader
            libGL
          ];

          packageName = "xrwm";

        in
        {

          packages.${packageName} = pkgs.haskell.lib.overrideCabal
            (pkgs.haskellPackages.callCabal2nix packageName self {
              # Dependency overrides go here
            })
            (drv: {
              /* buildInputs = systemLibraries ++ dynamicLibraries; */

              postFixup = ''
                patchelf --add-needed libvulkan.so "$out/bin/xrwm"
                patchelf --add-rpath "${pkgs.lib.strings.makeLibraryPath dynamicLibraries}" "$out/bin/xrwm"
              '';
            });

          devShells.init = pkgs.mkShell {
            packages = with pkgs;
              [
                cabal-install
                ghc
              ];
          };

          devShells.dev = pkgs.mkShell {
            buildInputs = with pkgs; [
              haskellPackages.haskell-language-server
              ghcid
            ];

            inputsFrom = [
              self.devShells.${system}.init
              self.packages.${system}.${packageName}
            ];

            shellHook = ''
              [ $STARSHIP_SHELL ] && exec $STARSHIP_SHELL
            '';

            LD_LIBRARY_PATH = pkgs.lib.strings.makeLibraryPath dynamicLibraries;

            CURRENT_PROJECT = packageName;
          };

          defaultPackage = self.packages.${system}.${packageName};
          devShell = self.devShells.${system}.dev;

        });

}


