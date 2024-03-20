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
    ./nbfc.nix
  ];
  # Use the systemd-boot EFI boot loader.
  nixpkgs.overlays = [
    (final: prev: {
      # see https://github.com/svenstaro/rofi-calc/issues/117
      libqalculate = prev.libqalculate.overrideAttrs (_: rec {
        pname = "libqalculate";
        version = "4.8.1";

        src = pkgs.fetchFromGitHub {
          owner = "qalculate";
          repo = "libqalculate";
          rev = "v${version}";
          sha256 = "sha256-4WqKlwVf4/ixVr98lPFVfNL6EOIfHHfL55xLsYqxkhY=";
        };
      });
    })
  ];

  boot = {
    tmp.useTmpfs = true;
    consoleLogLevel = 0;
    initrd.verbose = false;
    plymouth.enable = true;
    kernelParams = [
      "quiet"
      "splash"
      "loglevel=3"
      "rd.systemd.show_status=false"
      "rd.udev.log_level=3"
      "udev.log_priority=3"
    ];

    loader = {
      # holding SPACE will make it appear
      timeout = lib.mkDefault 0;
      efi.canTouchEfiVariables = true;
      systemd-boot.enable = true;
    };
  };
  nix.settings.experimental-features = ["nix-command" "flakes"];
  nix.settings.auto-optimise-store = true;

  nix.gc = {
    automatic = true;
    dates = "weekly";
    options = "--delete-older-than 30d";
  };
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
    EDITOR = "nvim";
  };

  sound.enable = true;
  hardware.pulseaudio.enable = false;
  security.rtkit.enable = true;
  fonts.packages = with pkgs; [
    noto-fonts
    font-awesome
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
      bat
      catppuccin-gtk
      papirus-icon-theme
      fd
      cinnamon.nemo
      firefox
      xdg-user-dirs
      chromium
      btop
      zoxide
      zathura
      anki
      neofetch
      fzf
      nil
      alejandra
      obs-studio
      flameshot
      pandoc
      jupyter
      gnome.eog
    ];
  };
  zramSwap.enable = true;
  nixpkgs.config.allowUnfree = true;
  environment.systemPackages = with pkgs; [
    libnotify
    python3
    dunst
    wget
    curl
    ripgrep
    kitty
    neovim
    git
    picom
    gnupg
    pavucontrol
    polkit-kde-agent
    pciutils
  ];
  # List services that you want to enable:
  location.provider = "geoclue2";
  services = {
    # for printers
    avahi = {
      enable = true;
      nssmdns4 = true;
      openFirewall = true;
    };
    emacs.enable = true;
    xserver = {
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
          i3-swallow
          eww
          autotiling
        ];
      };
    };

    pipewire = {
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

    btrbk = {
      instances.data = {
        onCalendar = "hourly";
        settings = {
          # 48h means 48 hourly snapshots are preversed , 7d means 7 daily snapshots are preserved
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
    };

    geoclue2 = {
      enable = true;
    };
    redshift = {
      enable = true;
    };
    syncthing = {
      enable = true;
      user = "gaurav";
      configDir = "/home/gaurav/.config/syncthing"; # Folder for Syncthing's settings and keys
    };

    openssh = {
      enable = true;
      settings.PasswordAuthentication = true;
    };
    # Enable CUPS to print documents.
    printing.enable = true;

    # Enable touchpad support (enabled default in most desktopManager).
    xserver.libinput.enable = true;

    gvfs.enable = true;
    greenclip.enable = true;
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
