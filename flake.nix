{
  description = "xrwm";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs";
    flake-utils.url = "github:numtide/flake-utils";
    flake-compat = { url = "github:edolstra/flake-compat"; flake = false; };

    nixpkgs-wayland = { url = "github:nix-community/nixpkgs-wayland"; };
  };

  outputs = { self, nixpkgs, flake-utils, ... }@inputs:
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
            vulkan-validation-layers

            libxkbcommon
            pixman
            udev
            wayland
            wayland-protocols
            inputs.nixpkgs-wayland.packages.${system}.wlroots
          ];

          dynamicLibraries = with pkgs; [
            vulkan-loader
          ];

          packageName = "xrwm";

        in
        {

          packages.${packageName} = pkgs.haskell.lib.overrideCabal
            (pkgs.haskellPackages.callCabal2nix packageName self {
              # Dependency overrides go here
            })
            (drv: {
              extraLibraries = with pkgs; [ udev ];
              postFixup = ''
                patchelf --add-needed libvulkan.so "$out/bin/xrwm"
                patchelf --add-rpath "${pkgs.lib.strings.makeLibraryPath dynamicLibraries}" "$out/bin/xrwm"
              '';
            });

          devShells.init = pkgs.mkShell {
            packages = with pkgs; [
              cabal-install
              ghc
            ];
          };

          devShells.dev = pkgs.mkShell {
            packages = with pkgs; [
              rnix-lsp
              haskell-language-server
              haskellPackages.fourmolu

              ghcid
              vulkan-tools
            ];

            buildInputs = systemLibraries ++ dynamicLibraries;

            inputsFrom = [
              self.devShells.${system}.init
              self.packages.${system}.${packageName}
            ];

            shellHook = ''
              [ $STARSHIP_SHELL ] && exec $STARSHIP_SHELL
            '';

            LD_LIBRARY_PATH = pkgs.lib.strings.makeLibraryPath dynamicLibraries;
            VK_LAYER_PATH = "${pkgs.vulkan-validation-layers}/share/vulkan/explicit_layer.d";

            CURRENT_PROJECT = packageName;
          };

          defaultPackage = self.packages.${system}.${packageName};
          devShell = self.devShells.${system}.dev;

        });

}


