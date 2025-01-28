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
  nixpkgs.config.permittedInsecurePackages = [
    "electron-31.7.7"
  ];
  imports = [
    # Include the results of the hardware scan.
    ./hardware-configuration.nix
    ./nvidia.nix
    ./nbfc.nix
    ./overlays.nix
    ./audio.nix
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
    };
    kernelPackages = pkgs.linuxPackages_latest;
    consoleLogLevel = 0;
    initrd.verbose = false;
    plymouth.enable = true;
    kernelParams = [
      # "preempt=full"
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
    extraModulePackages = [
    ];
    extraModprobeConfig = ''
      options nvidia_modeset vblank_sem_control=0
      options snd_hda_intel power_save=0
    '';

    # extraModprobeConfig = ''
    #   options nvidia NVreg_UsePageAttributeTable=1
    #   options nvidia NVreg_RegistryDwords="OverrideMaxPerf=0x1"
    #   options nvidia NVreg_PreserveVideoMemoryAllocations=1 NVreg_TemporaryFilePath=/var/tmp
    #   options nvidia_modeset vblank_sem_control=0
    #   options snd_hda_intel power_save=0
    # '';
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
      enable = true;
      extraCommands = ''
        iptables -A nixos-fw -p tcp --source 192.168.29.0/24 --dport 80:80 -j nixos-fw-accept || true
        iptables -A nixos-fw -p tcp --source 192.168.29.0/24 --dport 22:22 -j nixos-fw-accept || true
        # iptables -A nixos-fw -p udp --source 192.168.29.0/24 --dport 80:80 -j nixos-fw-accept
      '';
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
    EDITOR = "nvim";
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

  security.rtkit.enable = true;

  security.polkit.enable = true;
  security.pam.services.i3lock.enable = true;
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
    services.betterlockscreen = {
      enable = true;
      description = "Locks screen when going to sleep/suspend";
      environment = {DISPLAY = ":0";};
      serviceConfig = {
        User = "gaurav";
        Type = "simple";
        ExecStart = ''${pkgs.xidlehook}/bin/xidlehook-client --socket /tmp/xidlehook.socket control --action trigger --timer 1 '';
        TimeoutSec = "infinity";
      };
      before = ["sleep.target" "suspend.target" "hibernate.target"];
      wantedBy = ["sleep.target" "suspend.target" "hibernate.target"];
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
    # for excalidraw_export
    fg-virgil
    cascadia-code
  ];
  security.sudo.extraConfig = ''
    Defaults timestamp_type = global
  '';
  programs = {
    obs-studio = {
      enable = true;
      plugins = with pkgs.obs-studio-plugins; [
        obs-pipewire-audio-capture
        # wlrobs
      ];
      package = pkgs.obs-studio.override {
        cudaSupport = true;
      };
    };
    firefox = {
      enable = true;
      package = pkgs.firefox;
      nativeMessagingHosts.packages = [pkgs.firefoxpwa];
    };
    dconf.enable = true;
    nh = {
      enable = true;
      clean.enable = true;
      clean.extraArgs = "--keep-since 4d --keep 3";
      flake = "/home/gaurav/.dotconfigs/nixos/";
    };
    gamemode.enable = true;
    zsh.enable = true;
    gnupg.agent.enable = true;
    noisetorch.enable = true;
    steam = {
      enable = false;
      remotePlay.openFirewall = true; # Open ports in the firewall for Steam Remote Play
      dedicatedServer.openFirewall = true; # Open ports in the firewall for Source Dedicated Server
      localNetworkGameTransfers.openFirewall = true; # Open ports in the firewall for Steam Local Network Game Transfers
    };
  };
  users.users = {
    gaurav = {
      isNormalUser = true;
      description = "Gaurav Choudhury";
      extraGroups = ["networkmanager" "wheel" "audio"];
      shell = pkgs.zsh;
      packages = with pkgs; [
        nemo-with-extensions
        poppler_utils
        file-roller
        cinnamon-common
        cinnamon-desktop

        feishin
        #firefox
        firefoxpwa
        p7zip
        unzip

        libreoffice
        hunspell
        hunspellDicts.en_GB-large

        trash-cli
        qalculate-qt
        eog
        cheese
        anki
        neofetch
        alejandra
        audacity
        flameshot
        pandoc
        gimp3
        haskellPackages.greenclip
        # jupyter
        zathura
        mpv
        brightnessctl
        bottles
        # gwe

        thunderbird

        # (callPackage ../../pkgs/davinci-resolve-studio-19.nix {})
        # (callPackage ../../pkgs/balena-etcher.nix {})
        # (callPackage ../../pkgs/jdownloader2.nix {})
        # lmstudio
        alsa-utils
        # (openai-whisper-cpp.override {cudaSupport = true;})

        # For emacs
        shfmt
        shellcheck
        ((emacsPackagesFor emacs).emacsWithPackages (
          epkgs: with epkgs; [vterm tree-sitter tree-sitter-langs treesit-grammars.with-all-grammars]
        ))

        # ((emacsPackagesFor (emacs-gtk.override {withXwidgets = true;})).emacsWithPackages (
        #   epkgs: with epkgs; [vterm tree-sitter tree-sitter-langs treesit-grammars.with-all-grammars org-xlatex]
        # ))
        # distrobox
        # texliveMedium
        (texlive.combine {
          inherit
            (texlive)
            scheme-medium
            latexmk
            wrapfig
            ulem
            capt-of
            collection-fontsrecommended
            ;
        })

        # excalidraw_export

        # jetbrains.idea-community-bin

        xclip
        xdotool
        xorg.xprop
        xorg.xkill
        xorg.xwininfo

        ffmpeg-full
        carla
        # patchage
        lsp-plugins
        # calf
        # handbrake
        gocryptfs
        wireguard-tools
        rsync
        # rclone

        nicotine-plus
        qbittorrent
        gcc
        gnumake
        keepassxc

        pamixer
      ];
    };
  };
  # zramSwap.enable = true;
  nixpkgs.config = {
    # cudaSupport = true;
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
    neovim
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
    recoll
    python3Packages.recoll
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
    logind = {
      powerKey = "suspend";
      lidSwitch = "suspend-then-hibernate";
      lidSwitchDocked = "ignore";
      lidSwitchExternalPower = "ignore";
    };
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
      openFirewall = true;
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
      deviceSection = ''
        Option "Coolbits" "24"
      '';
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
    btrbk = {
      instances.data = {
        onCalendar = "hourly";
        settings = {
          # 48h means 48 hourly snapshots are preversed , 7d means 7 daily snapshots are preserved
          snapshot_preserve = "7d";
          snapshot_preserve_min = "2d";

          volume = {
            "/home" = {
              snapshot_dir = "/homeSnaps";
              subvolume = {
                "." = {
                };
              };
            };

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
      openFirewall = false;
    };
    # Enable CUPS to print documents.
    printing.enable = true;
    printing.drivers = [pkgs.hplip];

    # Enable touchpad support (enabled default in most desktopManager).
    libinput.enable = true;

    gvfs.enable = true;
    mysql = {
      enable = false;
      package = pkgs.mariadb;
    };
  };
  virtualisation.containers.enable = true;

  virtualisation = {
    podman = {
      enable = false;

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
