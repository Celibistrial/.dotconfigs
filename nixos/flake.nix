{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    nixctl = {
      url = "github:Celibistrial/nixctl";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    home-manager = {
      url = "github:nix-community/home-manager/master";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    catppuccin.url = "github:Celibistrial/catpuccin-nix";
    lanzaboote = {
      url = "github:nix-community/lanzaboote/v0.4.2";

      # Optional but recommended to limit the size of your system closure.
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = {
    self,
    nixpkgs,
    home-manager,
    ...
  } @ inputs: {
    nixosConfigurations.iris = nixpkgs.lib.nixosSystem {
      system = "x86_64-linux";
      specialArgs = {inherit inputs;};
      modules = [
        ./hosts/iris/configuration.nix
        inputs.lanzaboote.nixosModules.lanzaboote
        ({
          pkgs,
          lib,
          ...
        }: {
          environment.systemPackages = [
            # For debugging and troubleshooting Secure Boot.
            pkgs.sbctl
          ];

          # Lanzaboote currently replaces the systemd-boot module.
          # This setting is usually set to true in configuration.nix
          # generated at installation time. So we force it to false
          # for now.
          boot.loader.systemd-boot.enable = lib.mkForce false;

          boot.lanzaboote = {
            enable = true;
            pkiBundle = "/var/lib/sbctl";
          };
        })

        inputs.catppuccin.nixosModules.catppuccin

        home-manager.nixosModules.home-manager
        {
          home-manager.useGlobalPkgs = true;
          home-manager.useUserPackages = true;
          home-manager.backupFileExtension = "bkp";
          # home-manager.users.gaurav = import ./hosts/hp-pav/home.nix;
          home-manager.extraSpecialArgs = {inherit inputs;};
          home-manager.users.gaurav = {
            imports = [
              ./hosts/iris/home.nix
              inputs.catppuccin.homeManagerModules.catppuccin
            ];
          };
        }
      ];
    };

    nixosConfigurations.hp-pav = nixpkgs.lib.nixosSystem {
      system = "x86_64-linux";
      specialArgs = {inherit inputs;};
      modules = [
        ./hosts/hp-pav/configuration.nix

        inputs.lanzaboote.nixosModules.lanzaboote
        ({
          pkgs,
          lib,
          ...
        }: {
          environment.systemPackages = [
            # For debugging and troubleshooting Secure Boot.
            pkgs.sbctl
          ];

          # Lanzaboote currently replaces the systemd-boot module.
          # This setting is usually set to true in configuration.nix
          # generated at installation time. So we force it to false
          # for now.
          boot.loader.systemd-boot.enable = lib.mkForce false;

          boot.lanzaboote = {
            enable = true;
            pkiBundle = "/var/lib/sbctl";
          };
        })

        inputs.catppuccin.nixosModules.catppuccin

        home-manager.nixosModules.home-manager
        {
          home-manager.useGlobalPkgs = true;
          home-manager.useUserPackages = true;
          home-manager.backupFileExtension = "bkp";
          # home-manager.users.gaurav = import ./hosts/hp-pav/home.nix;
          home-manager.extraSpecialArgs = {inherit inputs;};
          home-manager.users.gaurav = {
            imports = [
              ./hosts/hp-pav/home.nix
              inputs.catppuccin.homeManagerModules.catppuccin
            ];
          };
        }
      ];
    };
    nixosConfigurations.deb1 = nixpkgs.lib.nixosSystem {
      system = "x86_64-linux";
      specialArgs = {inherit inputs;};
      modules = [
        ./hosts/deb1/configuration.nix
      ];
    };
  };
}
