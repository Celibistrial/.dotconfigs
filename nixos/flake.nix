{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    nbfc-linux = {
      url = "github:Celibistrial/nbfc-linux";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    nixctl = {
      url = "github:Celibistrial/nixctl";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    home-manager = {
      url = "github:nix-community/home-manager/master";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    catppuccin.url = "github:catppuccin/nix";
  };

  outputs = {
    self,
    nixpkgs,
    home-manager,
    catppuccin,
    ...
  } @ inputs: {
    nixosConfigurations.hp-pav = nixpkgs.lib.nixosSystem {
      system = "x86_64-linux";
      specialArgs = {inherit inputs;};
      modules = [
        ./hosts/hp-pav/configuration.nix

        catppuccin.nixosModules.catppuccin

        home-manager.nixosModules.home-manager
        {
          home-manager.useGlobalPkgs = true;
          home-manager.useUserPackages = true;
          home-manager.backupFileExtension = "bkp";
          # home-manager.users.gaurav = import ./hosts/hp-pav/home.nix;
          home-manager.users.gaurav.imports = [
            ./hosts/hp-pav/home.nix
            catppuccin.homeManagerModules.catppuccin
          ];
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
