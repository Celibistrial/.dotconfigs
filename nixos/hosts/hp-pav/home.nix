{
  config,
  lib,
  pkgs,
  ...
}
: {
  home = {
    username = "gaurav";
    homeDirectory = "/home/gaurav";
    stateVersion = "23.11";
    packages = with pkgs; [
      rofimoji
    ];
    pointerCursor = {
      x11.enable = true;
      name = "Bibata-Modern-Classic";
      package = pkgs.bibata-cursors;
      size = 20;
      gtk.enable = true;
    };
  };
  programs = {
    home-manager.enable = true;
    rofi = {
      enable = true;
      theme = "Arc-Dark";
      plugins = with pkgs; [
        rofi-calc
      ];
      extraConfig = {
        modi = "calc,combi,drun,run,ssh";
      };
    };

    direnv = {
      enable = true;
      enableZshIntegration = true; # see note on other shells below
      nix-direnv.enable = true;
    };

    tmux = {
      enable = false;
      plugins = with pkgs.tmuxPlugins; [
        sensible
        vim-tmux-navigator
        catppuccin
        yank
      ];
      prefix = "C-Space";
      mouse = true;
      extraConfig = ''
        set-option -sa terminal-overrides ",xterm*:Tc"
        set -g base-index 1
        set -g pane-base-index 1
        set-window-option -g pane-base-index 1
        # set vi-mode
        set-window-option -g mode-keys vi
        # keybindings
        bind-key -T copy-mode-vi v send-keys -X begin-selection
        bind-key -T copy-mode-vi C-v send-keys -X rectangle-toggle
        bind-key -T copy-mode-vi y send-keys -X copy-selection-and-cancel
        bind '"' split-window -v -c "#{pane_current_path}"
        bind % split-window -h -c "#{pane_current_path}"
      '';
    };
    zsh = {
      enable = false;
      enableCompletion = true;
      syntaxHighlighting.enable = true;

      shellAliases = {
        # ll = "ls -l";
        # update = "sudo nixos-rebuild switch";
      };
      history.size = 10000;
      shellAliases = {
        icat = "kitty +kitten icat";
        zm = "swallow zathura";
        zr = "swallow zaread";
        em = "emacsclient -t -a ''";
        ls = "exa";
        cat = "bat";
        cpfile = "xclip -sel c <";
        remacs = "pkill emacs && emacs --daemon";
        btop = "btop --utf-force";
      };
      initExtra = ''
        export TERM="xterm-256color"                      # getting proper colors
        export MANPAGER="sh -c 'col -bx | bat -l man -p'"
        export MANROFFOPT="-c"
        export PATH="$HOME/.config/emacs/bin:$PATH"
        # Path to scripts
        export PATH="$HOME/.local/bin:$PATH"
        bindkey -v
        eval "$(direnv hook zsh)"
        eval "$(zoxide init zsh --cmd cd)"
        if [ -n "$\{commands [fzf-share]}" ]; then
          source "$(fzf-share)/key-bindings.zsh"
          source "$(fzf-share)/completion.zsh"
        fi

        # use ranger to switch DIRECTORIES AND BIND IT TO CTRL-O
        rangercd () {
        tmp="$(mktemp)"
        ranger --choosedir="$tmp" "$@"
        if [ -f "$tmp" ]; then
        dir="$(cat "$tmp")"
        rm -f "$tmp"
        [ -d "$dir" ] && [ "$dir" != "$(pwd)" ] && cd "$dir"
        fi
        }
        bindkey -s '^o' 'rangercd\n'
      '';
      plugins = [
        {
          name = "powerlevel10k";
          src = pkgs.zsh-powerlevel10k;
          file = "share/zsh-powerlevel10k/powerlevel10k.zsh-theme";
        }
        {
          name = "zsh-autosuggestions";
          src = pkgs.zsh-autosuggestions;
          file = "share/zsh-autosuggestions/zsh-autosuggestions.zsh";
        }
        {
          name = "zsh-nix-shell";
          src = pkgs.zsh-nix-shell;
          file = "share/zsh-nix-shell/nix-shell.plugin.zsh";
        }
        {
          name = "zsh-syntax-highlighting";
          src = pkgs.zsh-syntax-highlighting;
          file = "share/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh";
        }
        {
          name = "zsh-nix-completions";
          src = pkgs.nix-zsh-completions;
          file = "share/nix-zsh-completions/nix-zsh-completions.zsh";
        }
      ];
    };
  };
  services.mpris-proxy.enable = true;
  gtk = {
    enable = true;

    # gtk.cursorTheme.package = pkgs.bibata-cursors;
    # gtk.cursorTheme.name = "Bibata-Modern-Classic";

    theme.package = pkgs.catppuccin-gtk;
    theme.name = "Catppuccin-Frappe-Standard-Blue-Dark";

    iconTheme.package = pkgs.papirus-icon-theme;
    iconTheme.name = "Papirus";
  };
}
