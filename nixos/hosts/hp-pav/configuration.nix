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
    # ./../../containers/ollama-webui.nix
  ];
  # nix.settings.trusted-substituters = ["https://nix-ai-stuff.cachix.org" "https://ai.cachix.org" "https://cuda-maintainers.cachix.org"];
  # nix.settings.trusted-public-keys = ["nix-ai-stuff.cachix.org-1:WlUGeVCs26w9xF0/rjyg32PujDqbVMlSHufpj1fqix8=" "ai.cachix.org-1:N9dzRK+alWwoKXQlnn0H6aUx0lU/mspIoz8hMvGvbbc=" "cuda-maintainers.cachix.org-1:0dq3bujKpuEPMCX6U4WylrUDZ9JyUG0VpVZa7CNfq5E="];
  boot = {
    binfmt.registrations.appimage = {
      wrapInterpreterInShell = false;
      interpreter = "${pkgs.appimage-run}/bin/appimage-run";
      recognitionType = "magic";
      offset = 0;
      mask = ''\xff\xff\xff\xff\x00\x00\x00\x00\xff\xff\xff'';
      magicOrExtension = ''\x7fELF....AI\x02'';
    };

    tmp = {
      useTmpfs = true;
      tmpfsSize = "100%";
    };
    kernelPackages = pkgs.linuxPackages_latest;
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
  hardware.bluetooth = {
    enable = true;
    powerOnBoot = true;
  };

  nix.settings.experimental-features = ["nix-command" "flakes"];
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
  networking.networkmanager.enable = true; # Easiest to use and most distros use this by default.
  networking.firewall.enable = false;
  # Set your time zone.
  time.timeZone = "Asia/Kolkata";
  i18n.defaultLocale = "en_GB.UTF-8";
  i18n.extraLocaleSettings = {
    LC_ADDRESS = "en_GB.UTF-8";
    LC_IDENTIFICATION = "en_GB.UTF-8";
    LC_MEASUREMENT = "en_GB.UTF-8";
    LC_MONETARY = "en_GB.UTF-8";
    LC_NAME = "en_GB.UTF-8";
    LC_NUMERIC = "en_GB.UTF-8";
    LC_PAPER = "en_GB.UTF-8";
    LC_TELEPHONE = "en_GB.UTF-8";
    LC_TIME = "en_GB.UTF-8";
  };
  environment.sessionVariables = {
    EDITOR = "vim";
    FLAKE = "/home/gaurav/.dotconfigs/nixos/";
    # Tell xdg-open to chill and just use the following default applications.
    # Without this setting, xdg-open will try to defer to exo-open despite not
    # using XFCE.  exo-open will use whatever XFCE4 defaults you may have left
    # over in your homedir.  Cursed.
    XDG_CURRENT_DESKTOP = "X-Generic";
  };

  xdg.mime.defaultApplications = let
    browser = "firefox.desktop";
    documentViewer = "org.pwmt.zathura.desktop";
    imageViewer = "org.gnome.eog.desktop";
    emailClient = "thunderbird.desktop";
  in {
    "application/pdf" = documentViewer;
    "x-scheme-handler/http" = browser;
    "x-scheme-handler/https" = browser;
    "x-scheme-handler/mailto" = emailClient;
    "message/rfc822" = emailClient;
    "text/calendar" = emailClient;
    "text/x-vcard" = emailClient;
    "text/html" = browser;
    "image/jpeg" = imageViewer;
    "image/png" = imageViewer;
    "image/gif" = imageViewer;
  };

  sound.enable = true;
  hardware.pulseaudio.enable = false;
  security.rtkit.enable = true;
  security.polkit.enable = true;
  fonts.packages = with pkgs; [
    noto-fonts
    font-awesome
    noto-fonts-cjk
    noto-fonts-emoji
    (nerdfonts.override {fonts = ["FiraCode" "JetBrainsMono"];})
  ];
  security.sudo.extraConfig = ''
    Defaults timestamp_type = global
  '';
  programs = {
    zsh.enable = true;
    nix-ld.enable = true;
    gnupg.agent.enable = true;
    noisetorch.enable = true;
  };
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
      fd
      ranger
      cinnamon.nemo
      sonixd
      firefox
      chromium
      inputs.nixctl.packages.x86_64-linux.default
      upscayl
      emacs
      #(callPackage ../../pkgs/lrcget.nix {})
      texliveMedium
      sqlite
      graphviz
      vim
      gnome.eog
      anki
      xdg-user-dirs
      encfs
      btop
      zoxide
      zathura
      neofetch
      fzf
      alejandra
      obs-studio
      audacity
      mpv
      flameshot
      pandoc
      nil
      jupyter
      pinta
      (aspellWithDicts (dicts: with dicts; [en en-computers en-science]))
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
    # ollama = {
    #   enable = true;
    #   acceleration = "cuda";
    # };
    auto-cpufreq.settings = {
      enable = true;
      charger = {
        governor = "performance";
        turbo = "auto";
      };

      battery = {
        governor = "powersave";
        turbo = "auto";
      };
    };
    undervolt = {
      enable = true;
      coreOffset = -100;
      gpuOffset = -100;
    };
    blueman.enable = true;

    xserver = {
      excludePackages = [pkgs.xterm];
      enable = true;
      displayManager.gdm.enable = true;
      # displayManager.sddm.enable = true;
      # desktopManager.plasma6.enable = true;
      # displayManager.defaultSession = "plasma";
      windowManager.i3 = {
        enable = true;
        extraPackages = with pkgs; [
          betterlockscreen
          arandr
          lxappearance
          xidlehook
          i3status-rust
          i3-swallow
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
      openDefaultPorts = true;
      enable = true;
      user = "gaurav";
      configDir = "/home/gaurav/.config/syncthing"; # Folder for Syncthing's settings and keys
    };
    # Disabled due to xy backdoor
    openssh = {
      enable = false;
      settings.PasswordAuthentication = false;
    };
    # Enable CUPS to print documents.
    printing.enable = true;
    printing.drivers = [pkgs.hplip];

    # Enable touchpad support (enabled default in most desktopManager).
    libinput.enable = true;

    gvfs.enable = true;
    greenclip.enable = true;
  };
  # virtualisation.docker = {
  #   enable = true;
  #   enableNvidia = true;
  #   daemon.settings = {
  #     data-root = "/data/docker/";
  #   };
  # };
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
