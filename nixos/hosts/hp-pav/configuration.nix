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
    ./overlays.nix
    # ./hyprland.nix
    #./../../containers/ollama-webui.nix
  ];
  nix.extraOptions = ''
    extra-substituters = https://devenv.cachix.org
    extra-trusted-public-keys = devenv.cachix.org-1:w1cLUi8dv3hnoSPGAuibQv+f9TZLr6cv/Hm9XgU50cw=
  '';
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

  nix.settings = {
    experimental-features = ["nix-command" "flakes"];
    warn-dirty = false;
  };
  # nix.gc = {
  #   automatic = true;
  #   dates = "weekly";
  #   options = "--delete-older-than 30d";
  # };
  networking = {
    hostName = "hp-pav"; # Define your hostname.
    extraHosts = ''
      192.168.29.85 deb1
      10.21.103.1 deb1

      192.168.29.22 m35
      10.21.103.3 m35
    '';

    networkmanager.enable = true;
    firewall = {
      enable = false;
    };
  };
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
    KWIN_DRM_NO_AMS = 1;
    QT_STYLE_OVERRIDE = "kvantum";
  };
  # xdg.portal.enable = true;
  # xdg.portal.extraPortals = [pkgs.xdg-desktop-portal-gtk];
  xdg.portal.config.common.default = ["gtk"];
  xdg.mime.defaultApplications = let
    browser = "firefox.desktop";
    documentViewer = "org.pwmt.zathura.desktop";
    imageViewer = "org.gnome.eog.desktop";
    # emailClient = "thunderbird.desktop";
  in {
    "application/pdf" = documentViewer;
    "x-scheme-handler/http" = browser;
    "x-scheme-handler/https" = browser;
    # "x-scheme-handler/mailto" = emailClient;
    # "message/rfc822" = emailClient;
    # "text/calendar" = emailClient;
    # "text/x-vcard" = emailClient;
    "text/html" = browser;
    "image/jpeg" = imageViewer;
    "image/png" = imageViewer;
    "image/gif" = imageViewer;
  };

  hardware.pulseaudio.enable = false;
  security.rtkit.enable = true;

  security.polkit.enable = true;
  systemd = {
    services.nix-daemon = {
      environment.TMPDIR = "/var/tmp";
    };
    timers.sync_data = {
      enable = true;
      wantedBy = ["timers.target"];
      timerConfig = {
        OnCalendar = "18:00:00";
        RandomizedDelaySec = "0min";
        Persistent = true;
        Unit = "sync_data.service";
      };
    };
    services.sync_data = {
      enable = true;
      path = [pkgs.bash pkgs.rsync pkgs.openssh pkgs.unixtools.ping];
      serviceConfig = {
        ExecStart = "/home/gaurav/.dotconfigs/scripts/sync_data.sh";
        Type = "oneshot";
        User = "gaurav";
      };
    };
    user.services.polkit-gnome-authentication-agent-1 = {
      enable = true;
      description = "polkit-gnome-authentication-agent-1";
      wantedBy = ["graphical-session.target"];
      wants = ["graphical-session.target"];
      after = ["graphical-session.target"];
      serviceConfig = {
        Type = "simple";
        ExecStart = "${pkgs.polkit_gnome}/libexec/polkit-gnome-authentication-agent-1";
        Restart = "on-failure";
        RestartSec = 1;
        TimeoutStopSec = 10;
      };
    };
  };

  fonts.packages = with pkgs; [
    noto-fonts-extra
    font-awesome
    noto-fonts-cjk-sans
    noto-fonts-emoji
    nerd-fonts.fira-code
    nerd-fonts.jetbrains-mono
  ];
  security.sudo.extraConfig = ''
    Defaults timestamp_type = global
  '';
  programs = {
    nh = {
      enable = true;
      clean.enable = true;
      clean.extraArgs = "--keep-since 4d --keep 3";
      flake = "/home/gaurav/.dotconfigs/nixos/";
    };
    gamemode.enable = true;
    zsh.enable = true;
    nix-ld.enable = true;
    gnupg.agent.enable = true;
    noisetorch.enable = true;
    steam = {
      enable = true;
      remotePlay.openFirewall = true; # Open ports in the firewall for Steam Remote Play
      dedicatedServer.openFirewall = true; # Open ports in the firewall for Source Dedicated Server
      localNetworkGameTransfers.openFirewall = true; # Open ports in the firewall for Steam Local Network Game Transfers
    };
  };
  users.users = {
    gaurav = {
      isNormalUser = true;
      description = "Gaurav Choudhury";
      extraGroups = ["networkmanager" "wheel"];
      shell = pkgs.zsh;

      packages = with pkgs; [
        nemo-with-extensions
        poppler_utils
        file-roller
        cinnamon-common

        feishin
        firefox
        p7zip

        libreoffice
        hunspell
        hunspellDicts.en_GB-large

        trash-cli
        qalculate-qt
        eog
        cheese
        ungoogled-chromium
        anki
        zoxide
        neofetch
        alejandra
        obs-studio
        audacity
        flameshot
        pandoc
        gimp
        # jupyter
        zathura
        mpv
        brightnessctl
        bottles
        rquickshare
        thunderbird

        # (callPackage ../../pkgs/davinci-resolve-studio-19.nix {})
        (callPackage ../../pkgs/balena-etcher.nix {})
        # (callPackage ../../pkgs/jdownloader2.nix {})
        # lmstudio

        # For emacs
        shfmt
        shellcheck
        ((emacsPackagesFor emacs).emacsWithPackages (
          epkgs: [epkgs.vterm]
        ))
        distrobox

        # jetbrains.idea-community-bin

        xclip
        xdotool
        xorg.xprop
        xorg.xkill
        xorg.xwininfo

        ffmpeg-full
        handbrake
        gocryptfs
        wireguard-tools
        rsync
        rclone

        nicotine-plus
        qbittorrent
        gcc
        gnumake
        keepassxc
      ];
    };
  };
  zramSwap.enable = true;
  nixpkgs.config = {
    allowUnfree = true;
    zathura.useMupdf = true;
  };
  environment.systemPackages = with pkgs; [
    eza
    fzf
    feh
    gparted
    networkmanagerapplet
    fd
    vim
    file
    xdg-user-dirs
    libnotify
    python3
    devenv
    deadd-notification-center
    wget
    curl
    ripgrep
    ripgrep-all
    kitty
    picom
    gnupg
    pavucontrol
    pciutils
    nixd
    libsForQt5.qtstyleplugin-kvantum
  ];
  location = {
    provider = "manual";
    latitude = 28.0;
    longitude = 77.0;
  };
  # List services that you want to enable:
  services = {
    avahi = {
      enable = true;
      nssmdns4 = true;
      nssmdns6 = true;
      publish = {
        enable = true;
        addresses = true;
        domain = true;
        hinfo = true;
        userServices = true;
        workstation = true;
      };
    };
    fstrim.enable = true;
    envfs.enable = true;
    auto-cpufreq.settings = {
      enable = true;
      charger = {
        governor = "performance";
        turbo = "always";
      };

      battery = {
        governor = "default";
        turbo = "auto";
      };
    };

    undervolt = {
      enable = true;
      coreOffset = -80;
      gpuOffset = -80;
    };
    blueman.enable = true;
    xserver = {
      excludePackages = [pkgs.xterm];
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
      jack.enable = true;
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

    syncthing = {
      openDefaultPorts = true;
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
    printing.drivers = [pkgs.hplip];

    # Enable touchpad support (enabled default in most desktopManager).
    libinput.enable = true;

    gvfs.enable = true;
    greenclip.enable = true;
    mysql = {
      enable = true;
      package = pkgs.mariadb;
    };
  };
  virtualisation.containers.enable = true;

  virtualisation = {
    podman = {
      enable = true;

      # Create a `docker` alias for podman, to use it as a drop-in replacement
      dockerCompat = true;

      # Required for containers under podman-compose to be able to talk to each other.
      defaultNetwork.settings.dns_enabled = true;
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
