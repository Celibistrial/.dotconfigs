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
    anyrun = {
      url = "github:anyrun-org/anyrun";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = {
    self,
    nixpkgs,
    home-manager,
    ...
  } @ inputs: {
    nixosConfigurations.hp-pav = nixpkgs.lib.nixosSystem {
      system = "x86_64-linux";
      specialArgs = {inherit inputs;};
      modules = [
        ./hosts/hp-pav/configuration.nix

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
              inputs.anyrun.homeManagerModules.default
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
