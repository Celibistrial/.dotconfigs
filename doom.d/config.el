(defun doom/ediff-init-and-example ()
  "ediff the current `init.el' with the example in doom-emacs-dir"
  (interactive)
  (ediff-files (concat doom-private-dir "init.el")
               (concat doom-emacs-dir "templates/init.example.el")))

(define-key! help-map
  "di"   #'doom/ediff-init-and-example
  )
(setq user-full-name "Gaurav Choudhury"
      user-mail-address "gauravchoudhury80222@gmail.com")
(setq doom-theme 'doom-one)
(setq display-line-numbers-type t)
(setq select-enable-clipboard nil)

;; (use-package-hook! evil
;;   :pre-init
;;   (setq evil-respect-visual-line-mode t) ;; sane j and k behavior
;;   t)

;; Emoji: 😄, 🤦, 🏴󠁧󠁢󠁳󠁣󠁴󠁿
(set-fontset-font t 'symbol "Apple Color Emoji")
(set-fontset-font t 'symbol "Noto Color Emoji" nil 'append)
(set-fontset-font t 'symbol "Segoe UI Emoji" nil 'append)
(set-fontset-font t 'symbol "Symbola" nil 'append)
;; (set-fontset-font t 'symbol "Segoe UI Emoji" nil 'append)
;; (setq emojify-display-style "unicode")
;; (setq vterm-font "JetBrainsMono Nerd Font:size=12")
(defun copy-current-line-to-clipboard ()
  "Copy the current line to the system clipboard."
  (interactive)
  (save-excursion
    (back-to-indentation)
    (set-mark (line-end-position))
    (copy-region-as-kill (point) (mark)))
  (clipboard-kill-ring-save (region-beginning) (region-end))
  (message "Line copied to clipboard"))

(map!
 :leader
 :nv
 :desc "Copy line to system clipboard" "Y" #'copy-current-line-to-clipboard)
(map!
 :leader
 :nv
 :desc "Copy to system clipboard" "y" #'clipboard-kill-ring-save)

(map!
 "C-S-v" #'clipboard-yank)
(map!
 "C-S-c" #'clipboard-kill-ring-save)

(map!
 :leader
 :nv
 "z" #'comint-dynamic-complete-filename)
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

(setq doom-modeline-env-version t)
(setq doom-modeline-time t)
(set-formatter! 'alejandra '("alejandra" "--quiet") :modes '(nix-mode))
;; (use-package! lsp-nix
  ;; :custom (lsp-nix-nil-formatter ["alejandra" "--quiet"]))
 (use-package! nix-mode
   :custom (nix-nixfmt-bin "~/.dotconfigs/scripts/alejandra-the-quiet.sh" ))
(setq org-log-done 'time)
(after! org
  (setq org-agenda-files '("~/org/"))
  (setq org-directory "~/org/"))
(after! org-roam
  (setq org-roam-capture-templates
        '(
          ("d" "default" plain "%?" :target
           (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n#+FILETAGS:  :%<%Y-%m-%d>:\n ")
           :unnarrowed t)
          )
        )
  (setq org-roam-dailies-capture-templates '(("d" "default" entry "* %<%r> %?"
					      :target
					      (file+head "%<%Y-%m-%d>.org" "#+title: %<%A %Y-%m-%d>\n#+FILETAGS:  :%<%Y-%m-%d>: "))))
  )
(after! org
(use-package! org-download))
;; put your css files there
(defvar org-theme-css-dir "~/.config/doom/css/")

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
          ("t" "Personal todo" entry (file+headline "~/org/refile.org" "TODOS") "** TODO %?")
          ("w" "Workout Journal" entry (file "~/org/workout journal.org") "* %?\n:PROPERTIES:\n:CREATED: %U\n:END:\n ")
          ("n" "Personal notes" entry (file+headline "~/org/refile.org" "NOTES") "* %u %?
%i %a" :prepend t)
          ("j" "Journal Entry" entry
           (file+olp+datetree "~/org/journal.org.gpg")
           "* %<%H:%M> \n%?")
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
(after! epa
  (setq epa-file-encrypt-to "82810795+Celibistrial@users.noreply.github.com"))
(after! org-crypt
  (org-crypt-use-before-save-magic)
  (setq org-tags-exclude-from-inheritance '("crypt"))

  (setq org-crypt-key "82810795+Celibistrial@users.noreply.github.com")
  ;; GPG key to use for encryption.
  ;; nil means  use symmetric encryption unconditionally.
  ;; "" means use symmetric encryption unless heading sets CRYPTKEY property.

  (setq auto-save-default nil)
  )
(map! "C-x <f12>" #'org-decrypt-entries)
(map! "C-x <f11>" #'org-decrypt-entry)
(setq ispell-local-dictionary "en_GB")
;; (use-package! gptel
;;   :config
;;   (setq!
;;    gptel-model "mistral:7b"
;;    gptel-default-mode #'org-mode
;;    gptel-backend (gptel-make-ollama "Ollama"
;;                    :host "localhost:11434"
;;                    :stream t
;;                    :models '("mistral:7b")))
;;   )
;; (after! gptel
;;   (gptel-make-ollama "Ollama"             ;Any name of your choosing
;;     :host "localhost:11434"               ;Where it's running
;;     :stream t                             ;Stream responses
;;     :models '("llama3:7b"))          ;List of models

;;   )
(map!
 :leader
 :nv
 :desc "fuzzy find files" "F" #'affe-find)
;;; cc-ediff-mode.el --- Ediff configuration for Charles Choi
;; ediff-mode

;;; Commentary:
;;

(require 'ediff)
;;; Code:
;; these defvars are here to let cc-ediff-mode.el compile clean
(defvar ediff-buffer-A)
(defvar ediff-buffer-B)
(defvar ediff-buffer-C)
(defvar ediff-merge-job)
(defvar ediff-ancestor-buffer)

;; CC: I set my Ediff variables in `custom-set-variables'
;; Use your own preference.
;; '(ediff-keep-variants nil)
;; '(ediff-split-window-function 'split-window-horizontally)
;; '(ediff-window-setup-function 'ediff-setup-windows-plain)

(defvar cc/ediff-revision-session-p nil
  "If t then `cc/ediff-revision-actual' has been called.
This state variable is used to insert added behavior to the overridden
function `ediff-janitor'.")

(defun cc/ediff-revision-from-menu (e)
  "Invoke `ediff-revision' on E with variable `buffer-file-name'."
  (interactive "e")
  (cc/ediff-revision))

(defun cc/ediff-revision ()
  "Run Ediff on the current `buffer-file-name' provided that it is `vc-registered'.
This function handles the interactive concerns found in `ediff-revision'.
This function will also test if a diff should apply to the current buffer."
  (interactive)
  (when (and (bound-and-true-p buffer-file-name)
             (vc-registered (buffer-file-name)))
    (if (and (buffer-modified-p)
             (y-or-n-p (format "Buffer %s is modified.  Save buffer? "
                               (buffer-name))))
      (save-buffer (current-buffer)))
    (message buffer-file-name)
    (cc/ediff-revision-actual))

  (cond ((not (bound-and-true-p buffer-file-name))
         (message (concat (buffer-name) " is not a file that can be diffed.")))
        ((not (vc-registered buffer-file-name))
         (message (concat buffer-file-name " is not under version control.")))))

(defun cc/ediff-revision-actual ()
  "Invoke Ediff logic to diff the modified repo file to its counterpart in the
current branch.
This function handles the actual diff behavior called by `ediff-revision'."
  (let ((rev1 "")
        (rev2 ""))
    (setq cc/ediff-revision-session-p t)
    (ediff-load-version-control)
    (funcall
     (intern (format "ediff-%S-internal" ediff-version-control-package))
     rev1 rev2 nil)))

(defun ediff-janitor (ask keep-variants)
  "Kill buffers A, B, and, possibly, C, if these buffers aren't modified.
In merge jobs, buffer C is not deleted here, but rather according to
`ediff-quit-merge-hook'.
ASK non-nil means ask the user whether to keep each unmodified buffer, unless
KEEP-VARIANTS is non-nil, in which case buffers are never killed.
A side effect of cleaning up may be that you should be careful when comparing
the same buffer in two separate Ediff sessions: quitting one of them might
delete this buffer in another session as well.

CC MODIFICATION: This method overrides the original Ediff function."
  (let ((ask (if (and (boundp 'cc/ediff-revision-session-p)
                      cc/ediff-revision-session-p)
                 nil
               ask)))
    (ediff-dispose-of-variant-according-to-user
     ediff-buffer-A 'A ask keep-variants)
    ;; !!!: CC Note: Test global state variable `cc/ediff-revision-session-p' to
    ;; determine if the modified repo file should be kept.
    ;; Guarding in place to hopefully avoid side-effects when `ediff-janitor' is
    ;; called from other Ediff functions. Informal testing has not revealed any
    ;; side-effects but YOLO.
    (if (and (boundp 'cc/ediff-revision-session-p)
             cc/ediff-revision-session-p)
        (ediff-dispose-of-variant-according-to-user
         ;; CC Note: keep-variants argument is hard-coded to t to keep
         ;; buffer holding modified repo file around.
         ediff-buffer-B 'B t t)
      (ediff-dispose-of-variant-according-to-user
       ediff-buffer-B 'B ask keep-variants))
    (if ediff-merge-job  ; don't del buf C if merging--del ancestor buf instead
        (ediff-dispose-of-variant-according-to-user
         ediff-ancestor-buffer 'Ancestor ask keep-variants)
      (ediff-dispose-of-variant-according-to-user
       ediff-buffer-C 'C ask keep-variants))
    ;; CC Note: Reset global state variable `cc/ediff-revision-session-p'.
    (if (and (boundp 'cc/ediff-revision-session-p)
             cc/ediff-revision-session-p)
        (setq cc/ediff-revision-session-p nil))))

(defun cc/stash-window-configuration-for-ediff ()
  "Store window configuration to register 🧊.
Use of emoji is to avoid potential use of keyboard character to reference
the register."
  (window-configuration-to-register ?🧊))

(defun cc/restore-window-configuration-for-ediff ()
  "Restore window configuration from register 🧊.
Use of emoji is to avoid potential use of keyboard character to reference
the register."
  (jump-to-register ?🧊))

(add-hook 'ediff-before-setup-hook #'cc/stash-window-configuration-for-ediff)
;; !!!: CC Note: Why this is not `ediff-quit-hook' I do not know. But this works
;; for cleaning up ancillary buffers on quitting an Ediff session.
(add-hook 'ediff-after-quit-hook-internal #'cc/restore-window-configuration-for-ediff)

(provide 'cc-ediff-mode)

;;; cc-ediff-mode.el ends here
(defun disable-y-or-n-p (orig-fun &rest args)
  (cl-letf (((symbol-function 'y-or-n-p) (lambda (prompt) t)))
    (apply orig-fun args)))

(advice-add 'ediff-quit :around #'disable-y-or-n-p)
(setq markdown-css-paths  `(,(expand-file-name "~/.dotconfigs/doom.d/css/simple.min.css")))
