# Edit this configuration file to define what should be installed on
# your system. Help is available in the configuration.nix(5) man page, on
# https://search.nixos.org/options and in the NixOS manual (`nixos-help`).
{
  config,
  lib,
  pkgs,
  inputs,
  ...
}: {
  imports = [
    # Include the results of the hardware scan.
    ./hardware-configuration.nix
  ];

  boot.loader = {
    efi.canTouchEfiVariables = true;
    systemd-boot.enable = true;
  };

  nix.settings.experimental-features = ["nix-command" "flakes"];
  nix.gc = {
    automatic = true;
    dates = "weekly";
    options = "--delete-older-than 30d";
  };
  networking.hostName = "deb1"; # Define your hostname.

  # Pick only one of the below networking options.
  # networking.wireless.enable = true;  # Enables wireless support via wpa_supplicant.
  networking.networkmanager.enable = true; # Easiest to use and most distros use this by default.
  networking.nftables.enable = true;
  networking.firewall.enable = true;
  # Set your time zone.
  time.timeZone = "Asia/Kolkata";
  i18n.defaultLocale = "en_IN";
  i18n.extraLocaleSettings = {
    LC_ADDRESS = "en_IN";
    LC_IDENTIFICATION = "en_IN";
    LC_MEASUREMENT = "en_IN";
    LC_MONETARY = "en_IN";
    LC_NAME = "en_IN";
    LC_NUMERIC = "en_IN";
    LC_PAPER = "en_IN";
    LC_TELEPHONE = "en_IN";
    LC_TIME = "en_IN";
  };
  environment.sessionVariables = {
    EDITOR = "vim";
  };
  security.rtkit.enable = true;
  # Define a user account. Don't forget to set a password with ‘passwd’.
  security.sudo.extraConfig = ''
    Defaults timestamp_type = global
  '';

  programs.fish.enable = true;
  users.users.gaurav = {
    isNormalUser = true;
    description = "Gaurav Choudhury";
    extraGroups = ["networkmanager" "wheel"];
    shell = pkgs.fish;

    packages = with pkgs; [
      fd
      ranger
      xdg-user-dirs
      btop
      zoxide
      neofetch
      fzf
    ];
  };
  zramSwap.enable = true;
  nixpkgs.config.allowUnfree = true;
  environment.systemPackages = with pkgs; [
    libnotify
    python3
    wget
    curl
    ripgrep
    git
    gnupg
    pciutils
    jellyfin
    jellyfin-web
    jellyfin-ffmpeg
  ];
  services = {
    adguardhome = {
      enable = true;
      mutableSettings = true;
      openFirewall = true;
      settings.bind_port = 8000;
    };
    jellyfin = {
      enable = true;
      openFirewall = true;
    };
    # btrbk = {
    #   instances.data = {
    #     onCalendar = "hourly";
    #     settings = {
    #       # 48h means 48 hourly snapshots are preversed , 7d means 7 daily snapshots are preserved
    #       snapshot_preserve = "7d";
    #       snapshot_preserve_min = "2d";

    #       volume = {
    #         "/data" = {
    #           snapshot_dir = "/dataSnaps";
    #           subvolume = {
    #             "." = {
    #             };
    #           };
    #         };
    #       };
    #     };
    #   };
    # };

    syncthing = {
      enable = true;
      user = "gaurav";
      configDir = "/home/gaurav/.config/syncthing"; # Folder for Syncthing's settings and keys
    };

    openssh = {
      enable = true;
      settings.PasswordAuthentication = true;
    };
  };
  # This option defines the first version of NixOS you have installed on this particular machine,
  # and is used to maintain compatibility with application data (e.g. databases) created on older NixOS versions.
  #
  # Most users should NEVER change this value after the initial install, for any reason,
  # even if you've upgraded your system to a new NixOS release.
  #
  # This value does NOT affect the Nixpkgs version your packages and OS are pulled from,
  # so changing it will NOT upgrade your system.
  #
  # This value being lower than the current NixOS release does NOT mean your system is
  # out of date, out of support, or vulnerable.
  #
  # Do NOT change this value unless you have manually inspected all the changes it would make to your configuration,
  # and migrated your data accordingly.
  #
  # For more information, see `man configuration.nix` or https://nixos.org/manual/nixos/stable/options#opt-system.stateVersion .
  system.stateVersion = "23.11"; # Did you read the comment?
}
