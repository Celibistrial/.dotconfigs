(setq user-full-name "Gaurav Choudhury"
      user-mail-address "gauravchoudhury80222@gmail.com")
(setq doom-theme 'doom-one)
(setq display-line-numbers-type t)
(setq org-directory "~/org/")
(setq org-agenda-files '("~/org/"))
(setq select-enable-clipboard nil)

(map!
 :nv
 "C-S-v" #'clipboard-yank)
(map!
 :nv
 "C-S-c" #'clipboard-kill-ring-save)
(map!
 :leader
 :nv
 "z" #'comint-dynamic-complete-filename)

(setq evil-want-fine-undo t)
;; Emoji: 😄, 🤦, 🏴󠁧󠁢󠁳󠁣󠁴󠁿
(set-fontset-font t 'symbol "Apple Color Emoji")
(set-fontset-font t 'symbol "Noto Color Emoji" nil 'append)
(set-fontset-font t 'symbol "Segoe UI Emoji" nil 'append)
(set-fontset-font t 'symbol "Symbola" nil 'append)
;; (set-fontset-font t 'symbol "Segoe UI Emoji" nil 'append)
;; (setq emojify-display-style "unicode")
;; (setq vterm-font "JetBrainsMono Nerd Font:size=12")
(setq projectile-indexing-method 'alien)
(map!
 :leader
 :nv
 "C-v" #'consult-yank-from-kill-ring)
(setq projectile-enable-caching t)
(after! smartparens
  (smartparens-mode 1)
  (smartparens-global-mode 1))
(after! lsp-ui
  (setq lsp-ui-sideline-show-code-actions   nil)
  (setq lsp-headerline-breadcrumb-enable nil)
  (setq lsp-enable-symbol-highlighting t)
  (setq lsp-signature-auto-activate t)
  (setq lsp-ui-sideline-enable t)
  (setq lsp-eldoc-enable-hover t)
  (setq lsp-completion-show-detail t)
 (setq lsp-signature-auto-activate nil)
  (setq lsp-completion-show-kind t))
 (setq lsp-auto-guess-root t)
(defun lsp-ui-sideline--compute-height nil '(height unspecified))

(custom-set-faces '(markdown-code-face ((t (:inherit default)))))
(setq doom-modeline-env-version t)
(setq doom-modeline-time t)
(set-formatter! 'alejandra '("alejandra" "--quiet") :modes '(nix-mode))
(use-package! lsp-nix
  :custom (lsp-nix-nil-formatter ["alejandra" "--quiet"]))
(use-package! nix-mode
  :custom (nix-nixfmt-bin "~/.dotconfigs/scripts/alejandra-the-quiet.sh" ))
(setq org-log-done 'time)
(after! org
  (set-popup-rule! "^\\*Org Src" :ignore t))
(after! org
  (setq org-src-window-setup 'split-window-right))
(after! org
(use-package! org-download))
;; put your css files there
(defvar org-theme-css-dir "~/.doom.d/css/")

(defun toggle-org-custom-inline-style ()
  (interactive)
  (let ((hook 'org-export-before-parsing-hook)
        (fun 'set-org-html-style))
    (if (memq fun (eval hook))
        (progn
          (remove-hook hook fun 'buffer-local)
          (messag       e "Removed %s from %s" (symbol-name fun) (symbol-name hook)))
      (add-hook hook fun nil 'buffer-local)
      (message "Added %s to %s" (symbol-name fun) (symbol-name hook)))))

(defun org-theme ()
  (interactive)
  (let* ((cssdir org-theme-css-dir)
         (css-choices (directory-files cssdir nil ".css$"))
         (css (completing-read "theme: " css-choices nil t)))
    (concat cssdir css)))

(defun org-export-style (&optional backend)
  (interactive)
  (when (or (null backend) (eq backend 'html))
    (let ((f (or (and (boundp 'org-theme-css) org-theme-css) (org-theme))))
      (if (file-exists-p f)
          (progn
            (set (make-local-variable 'org-theme-css) f)
            (set (make-local-variable 'org-html-head)
                 (with-temp-buffer
                   (insert "<style type=\"text/css\">\n<!--/*--><![CDATA[/*><!--*/\n")
                   (insert-file-contents f)
                   (goto-char (point-max))
                   (insert "\n/*]]>*/-->\n</style>\n")
                   (buffer-string)))
            (set (make-local-variable 'org-html-head-include-default-style)
                 nil)
            (message "Set custom style from %s" f))
        (message "Custom header file %s doesnt exist")))))
(defun org-random-choice (file)
  "Return a random line from FILE."
  (with-temp-buffer
    (insert-file-contents file)
    (let ((lines (split-string (buffer-string) "\n" t)))
      (nth (random (length lines)) lines))))

(after! org
  (setq org-capture-templates
        '(("x" "Quick note" entry (file+headline "~/org/refile.org" "TEMP") "** %? " )
          ("t" "Personal todo" entry (file+headline "~/org/refile.org" "TODOS") "** TODO  %? %i
 %a")
          ("n" "Personal notes" entry (file+headline "~/org/refile.org" "NOTES") "* %u %?
%i %a" :prepend t)
         ("j" "Journal Entry" entry
           (file+olp+datetree "~/org/journal.org.gpg")
           "* %<%H:%M> \n%?")
          ("p" "Templates for projects") ("pt" "Project-local todo" entry (file+headline +org-capture-project-todo-file "Inbox") "* TODO %?
%i
%a" :prepend t)
("J" "Journal Entry With Prompt" entry
           (file+olp+datetree "~/org/journal.org.gpg")
           "* %<%H:%M> \n** Prompt:%(org-random-choice \"~/org/journaling_prompts.org\")  \n%?")
          ("p" "Templates for projects") ("pt" "Project-local todo" entry (file+headline +org-capture-project-todo-file "Inbox") "* TODO %?
%i
%a" :prepend t) ("pn" "Project-local notes" entry (file+headline +org-capture-project-notes-file "Inbox") "* %U %?
%i
%a" :prepend t) ("pc" "Project-local changelog" entry (file+headline +org-capture-project-changelog-file "Unreleased") "* %U %?
%i
%a" :prepend t) ("o" "Centralized templates for projects") ("ot" "Project todo" entry #'+org-capture-central-project-todo-file "* TODO %?
 %i
 %a" :heading "Tasks" :prepend nil) ("on" "Project notes" entry #'+org-capture-central-project-notes-file "* %U %?
 %i
 %a" :heading "Notes" :prepend t) ("oc" "Project changelog" entry #'+org-capture-central-project-changelog-file "* %U %?
 %i
 %a" :heading "Changelog" :prepend t))
        )
  )
;;(load! "org-website" doom-user-dir)
(after! epa
  (setq epa-file-encrypt-to "ryan80222@gmail.com"))
(setq ispell-local-dictionary "en_GB")
