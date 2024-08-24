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
    #./../../containers/ollama-webui.nix
  ];
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
  nix.gc = {
    automatic = true;
    dates = "weekly";
    options = "--delete-older-than 30d";
  };
  networking = {
    hostName = "hp-pav"; # Define your hostname.
    extraHosts = ''
      192.168.29.85 deb1
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
    FLAKE = "/home/gaurav/.dotconfigs/nixos/";
    # Tell xdg-open to chill and just use the following default applications.
    # Without this setting, xdg-open will try to defer to exo-open despite not
    # using XFCE.  exo-open will use whatever XFCE4 defaults you may have left
    # over in your homedir.  Cursed.
    XDG_CURRENT_DESKTOP = "X-Generic";
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
    user.services.polkit-gnome-authentication-agent-1 = {
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
    #  hyprland.enable = true;
  };
  users.users.gaurav = {
    isNormalUser = true;
    description = "Gaurav Choudhury";
    extraGroups = ["networkmanager" "wheel"];
    shell = pkgs.zsh;

    packages = with pkgs; [
      nemo-with-extensions
      file-roller
      cinnamon-common

      sonixd
      cava
      firefox
      inputs.nixctl.packages.x86_64-linux.default
      p7zip
      libreoffice
      trash-cli
      xclip
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
      jupyter
      zathura
      mpv
      recoll
      brightnessctl

      # (callPackage ../../pkgs/lrcget.nix {})

      # For emacs
      shfmt
      shellcheck
      (aspellWithDicts (dicts: with dicts; [en en-computers en-science]))

      jetbrains.idea-community-bin
    ];
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
  ];
  # List services that you want to enable:
  location.provider = "geoclue2";
  services = {
    fstrim.enable = true;
    envfs.enable = true;
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
      coreOffset = -80;
      gpuOffset = -80;
    };
    blueman.enable = true;
    xserver = {
      excludePackages = [pkgs.xterm];
      enable = true;

      displayManager.gdm.enable = true;
      # desktopManager.gnome.enable = true;
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
