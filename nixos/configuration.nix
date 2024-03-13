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
    ./nvidia.nix
    #./nbfc.nix
  ];
  # Use the systemd-boot EFI boot loader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;
  nix.settings.experimental-features = ["nix-command" "flakes"];

  networking.hostName = "hp-pav"; # Define your hostname.
  networking.extraHosts = ''
    192.168.29.85 deb1
  '';

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
  environment.sessionVariables = rec {
    EDITOR = "vim";
  };

  sound.enable = true;
  hardware.pulseaudio.enable = false;
  security.rtkit.enable = true;
  services.pipewire = {
    enable = true;
    alsa.enable = true;
    alsa.support32Bit = true;
    pulse.enable = true;
    # If you want to use JACK applications, uncomment this
    #jack.enable = true;

    # use the example session manager (no others are packaged yet so this is enabled by default,
    # no need to redefine it in your config for now)
    #media-session.enable = true;
  };

  fonts.packages = with pkgs; [
    noto-fonts
    noto-fonts-cjk
    noto-fonts-emoji
    liberation_ttf
    fira-code
    fira-code-symbols
    mplus-outline-fonts.githubRelease
    dina-font
    proggyfonts
    dejavu_fonts
    nerdfonts
  ];
  services.xserver = {
    enable = true;
    displayManager.gdm.enable = true;
    windowManager.i3 = {
      enable = true;
      extraPackages = with pkgs; [
        betterlockscreen
        arandr
        lxappearance
        xidlehook
        i3status-rust
        autotiling
      ];
    };
  };
  # Define a user account. Don't forget to set a password with ‘passwd’.
  security.sudo.extraConfig = ''
    Defaults timestamp_type = global
  '';
  programs.steam = {
    enable = true;
  };
  programs.zsh.enable = true;
  users.users.gaurav = {
    isNormalUser = true;
    description = "Gaurav Choudhury";
    extraGroups = ["networkmanager" "wheel"];
    shell = pkgs.zsh;
    packages = with pkgs; [
      eza
      feh
      networkmanagerapplet
      i3-swallow
      bat
      catppuccin-gtk
      papirus-icon-theme
      fd
      cinnamon.nemo
      firefox
      chromium
      htop
      zoxide
      tree
      zathura
      anki
      hplip
      neofetch
    ];
  };
  zramSwap.enable = true;
  nixpkgs.config.allowUnfree = true;
  environment.systemPackages = with pkgs; [
    alejandra
    emacs
    libnotify
    python3
    dunst
    wget
    curl
    ripgrep
    kitty
    vim
    git
    picom
    gnupg
    pavucontrol
    polkit-kde-agent
    pciutils
    # inputs.envycontrol.packages.x86_64-linux.default
  ];
  # List services that you want to enable:
  services.btrbk = {
    instances.data.settings = {
      snapshot_preserve = "7d";
      snapshot_preserve_min = "2d";

      volume = {
        "/data" = {
          snapshot_dir = "/dataSnaps";
          subvolume = {
            "." = {
            };
          };
        };
      };
    };
  };

  location.provider = "geoclue2";
  services.geoclue2 = {
    enable = true;
  };
  services.redshift = {
    enable = true;
  };
  services = {
    syncthing = {
      enable = true;
      user = "gaurav";
      configDir = "/home/gaurav/.config/syncthing"; # Folder for Syncthing's settings and keys
    };
  };

  # Enable CUPS to print documents.
  services.printing.enable = true;

  # Enable touchpad support (enabled default in most desktopManager).
  services.xserver.libinput.enable = true;

  #services.openssh.enable = true;
  services.gvfs.enable = true;
  services.gnome.core-os-services.enable = true;
  services.greenclip.enable = true;

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
