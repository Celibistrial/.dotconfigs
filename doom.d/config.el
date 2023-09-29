(setq user-full-name "Gaurav Choudhury"
      user-mail-address "gauravchoudhury80222@gmail.com")
(setq doom-theme 'doom-gruvbox)
(setq display-line-numbers-type t)
(setq org-directory "~/org/")
(setq org-agenda-files '("~/org/"))
(setq doom-font (font-spec :family "JetBrains Mono" :size 14 :weight 'Medium)
      doom-variable-pitch-font (font-spec :family "JetBrains Mono" :size 15))
(setq! doom-unicode-font (font-spec :family "JetBrainsMono Nerd Font" :style "Regular" :size 11))
(setq select-enable-clipboard nil)
(map!
 "C-S-v" #'clipboard-yank)
(map!
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
;; (after! company
;;   (set-company-backend! 'org-mode 'company-capf 'company-files)
;; (set-company-backend! 'org-mode 'company-files 'company-capf))
(after! helm
(setq projectile-indexing-method 'alien)
(map!
 :leader
 :nv
 "C-v" #'helm-show-kill-ring)
(setq projectile-enable-caching t)
;;(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action)
;; (define-key helm-find-files-map (kbd "<C-<backspace>>")  'helm-find-files-up-one-level)
(map!
 :map helm-find-files-map
 "C-<backspace>" #'helm-find-files-up-one-level)
(after! company
  (setq company-backends '(company-files company-dabbrev-code company-dabbrev helm-company)))
)
(after! helm
  (defsubst helm-themes--delete-theme ()
    "Delete theme."
    (mapc 'disable-theme custom-enabled-themes))

  (defun helm-themes--load-theme (theme-str)
    "Load the theme by THEME-STR."
    (helm-themes--delete-theme)
    (if (string= theme-str "default")
        t
      (load-theme (intern theme-str) t)))

  (defun helm-themes--candidates ()
    "Return a list of themes."
    (cons 'default (custom-available-themes)))

  (defvar helm-themes-source
    (helm-build-sync-source "Selection Theme"
      :candidates 'helm-themes--candidates
      :action 'helm-themes--load-theme
      :persistent-action 'helm-themes--load-theme)
    "Helm source for themes selection.")
  (defun helm-themes ()
    "Theme selection with helm interface."
    (interactive)
    (let ((changed nil)
          (orig-theme (when custom-enabled-themes
                        (car custom-enabled-themes))))
      (unwind-protect
          (when (helm :prompt (format "pattern (current theme: %s): "
                                      (if (null custom-enabled-themes)
                                          'default
                                        (symbol-name orig-theme)))
                      :preselect (format "%s$" (symbol-name orig-theme))
                      :sources helm-themes-source
                      :buffer "*helm-themes*")
            (setq changed t))
        (when (not changed)
          (helm-themes--delete-theme)
          (when orig-theme
            (load-theme orig-theme t))))))

  )
(setq projectile-indexing-method 'alien)
(map!
 :leader
 :nv
 "C-v" #'consult-yank-from-kill-ring)
(setq projectile-enable-caching t)
(defun clear-kill-ring()
    (interactive)
  (progn (setq kill-ring nil) (garbage-collect)))
(after! smartparens
  (smartparens-mode 1)
  (smartparens-global-mode 1))
(after! eldoc
  (setq lsp-eldoc-hook nil))
(after! lsp-ui
  (setq lsp-ui-sideline-show-code-actions   nil)
  (setq lsp-headerline-breadcrumb-enable t)
  (setq lsp-enable-symbol-highlighting t)
  (setq lsp-signature-auto-activate nil)

  (setq lsp-headerline-breadcrumb-enable nil)
  (setq lsp-ui-sideline-enable t)
  (setq lsp-ui-sideline-enable t)
  (setq lsp-modeline-code-actions-enable t)
  (setq lsp-ui-sideline-enable t)
  (setq lsp-eldoc-enable-hover t)
  (setq lsp-completion-show-detail t)
  (setq lsp-completion-show-kind t))

  (defun lsp-ui-sideline--compute-height nil '(height unspecified))

(custom-set-faces '(markdown-code-face ((t (:inherit default)))))
(setq doom-modeline-env-version t)
(setq doom-modeline-time t)
(defun create-cpp-project ()
  "Create a new C++ project with CMake configuration files."
  (interactive)
  (let ((project-dir (read-directory-name "Project directory: ")))
    (make-directory project-dir)
    (find-file (expand-file-name "CMakeLists.txt" project-dir))
    (insert "cmake_minimum_required(VERSION 3.10)\n")
    (insert (format "project(%s)\n" (file-name-nondirectory project-dir)))
    (insert (format "add_executable(%s src/main.cpp)\n" (file-name-nondirectory project-dir)))
    (make-directory (expand-file-name "src" project-dir))
    (find-file (expand-file-name "src/main.cpp" project-dir))
    (insert "#include <iostream>\n")
    (insert "int main() {\n")
    (insert "    std::cout << \"Hello, World!\" << std::endl;\n")
    (insert "    return 0;\n")
    (insert "}\n")
    (cd project-dir)
    (shell-command "cmake -H. -Bbuild")))
(defun create-c-project ()
  "Create a new C project with CMake configuration files."
  (interactive)
  (let ((project-dir (read-directory-name "Project directory: ")))
    (make-directory project-dir)
    (make-directory (expand-file-name "src" project-dir))
    (find-file (expand-file-name "CMakeLists.txt" project-dir))
    (insert "cmake_minimum_required(VERSION 3.10)\n")
    (insert (format "project(%s)\n" (file-name-nondirectory project-dir)))
    (insert (format "add_executable(%s src/main.c)\n" (file-name-nondirectory project-dir)))
    (find-file (expand-file-name "src/main.c" project-dir))
    (insert "#include <stdio.h>\n")
    (insert "int main() {\n")
    (insert "    printf(\"Hello, World!\\n\");\n")
    (insert "    return 0;\n")
    (insert "}\n")
    (cd project-dir)
    (shell-command "cmake -H. -Bbuild")))
(defun my-save-word ()
  (interactive)
  (let ((current-location (point))
        (word (flyspell-get-word)))
    (when (consp word)
      (flyspell-do-correct 'save nil (car word) current-location (cadr word) (caddr word) current-location))))
(after! org
  (map!      :prefix "C-x"
             :map org-mode-map
             :nv "w" #'my-save-word)
  )
(defun org-format ()
  "A messed up way to auto-format org docs"
  (interactive)
  (let* ((current-file (buffer-file-name))
         (exported-file (concat current-file ".org")))
    (org-org-export-to-org)
    (delete-file current-file)
    (rename-file exported-file current-file)
    (revert-buffer)
    ))
(map!
 :leader
 :after org
 :map org-mode-map
 :nv "c F" #'org-format)
(after! langtool
(setq langtool-java-classpath
      "/usr/share/languagetool:/usr/share/java/languagetool/*")
(require 'langtool)
(defun correct-buffer ()
  (interactive)
  (langtool-check-buffer)
  (langtool-correct-buffer))
(setq langtool-default-language "en-GB")
(global-set-key "\C-x4w" 'langtool-check)
(global-set-key "\C-x4W" 'langtool-check-done)
(global-set-key "\C-x4l" 'langtool-switch-default-language)
(global-set-key "\C-x44" 'langtool-show-message-at-point)
(global-set-key "\C-x4c" 'correct-buffer))
;; (setq org-src-window-setup 'current-window)
(after! org
(setq org-src-window-setup 'reorganize-frame))
(after! org
  (set-popup-rule! "^\\*Org Src" :ignore t))
(map!
 :leader
 :after org
 :map org-mode-map
 :nv
 "p l" #'org-latex-preview)
(after! org
    (setq org-hide-emphasis-markers t)
    )
;; (use-package! anki-editor
;;   :commands anki-editor-mode
;;   :custom (anki-editor-latex-style 'mathjax))

;; (use-package! ankiorg
;;   :commands
;;   ankiorg-pull-notes
;;   ankiorg-buffer-get-media-files
;;   ankiorg-pull-tags
;;   :custom
;;   (ankiorg-sql-database
;;    "/home/gaurav/.local/share/Anki2/User 1/collection.anki2")
;;   (ankiorg-media-directory
;;    "/home/gaurav/.local/share/Anki2/User 1/collection.media/"))
(after! org
  (require 'org-download)
  (add-hook 'dired-mode-hook 'org-download-enable)
  )
(setq org-file-apps '((auto-mode . emacs)
                      ("\\.pdf\\'" . "firefox %s")))
(after! org
(require 'org-protocol))
(setq org-log-done 'time)
(after! org
  (setq org-roam-directory "~/org/org-roam"))
(setq org-journal-encrypt-journal nil)
(setq org-journal-encrypt-on nil)
(defun org-journal-find-location ()
  ;; Open today's journal, but specify a non-nil prefix argument in order to
  ;; inhibit inserting the heading; org-capture will insert the heading.
  (org-journal-new-entry t)
  (unless (eq org-journal-file-type 'daily)
    (org-narrow-to-subtree))
  (goto-char (point-max)))

(defun org-journal-find-location ()
  ;; Open today's journal, but specify a non-nil prefix argument in order to
  ;; inhibit inserting the heading; org-capture will insert the heading.
  (org-journal-new-entry t)
  (unless (eq org-journal-file-type 'daily)
    (org-narrow-to-subtree))
  (goto-char (point-max)))
(after! org
  (setq org-capture-templates
        '(("x" "Quick note" entry (file+headline "~/org/refile.org" "TEMP") "** %? " )
          ("t" "Personal todo" entry (file+headline "~/org/refile.org" "TODOS") "** TODO  %? %i
 %a")
          ("n" "Personal notes" entry (file+headline "~/org/refile.org" "NOTES") "* %u %?
%i %a" :prepend t) ("j" "Journal" entry (file+olp+datetree +org-capture-journal-file) "



* %U %?
** TODO Have-to-do-list [0%]
+ [ ] Revise class notes [0%]
  + [ ] Physics
  + [ ] Chemistry
  + [ ] Maths
%i
%a" :prepend t) ("p" "Templates for projects") ("pt" "Project-local todo" entry (file+headline +org-capture-project-todo-file "Inbox") "* TODO %?
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
(set-language-environment "UTF-8")
(defun my/org-html-src-block (html)
  "Modify the output of org-html-src-block for highlight.js"
  (replace-regexp-in-string
   "</pre>" "</code></pre>"
   (replace-regexp-in-string
    "<pre class=\"src src-\\(.*\\)\">"
    "<pre><code class=\"\\1\">"
    html)))

(advice-add 'org-html-src-block :filter-return #'my/org-html-src-block)
                                        ; Customize the HTML output
(setq org-html-validation-link nil            ;; Don't show validation link
      org-html-head-include-scripts nil       ;; Use our own scripts
      org-html-head-include-default-style nil ;; Use our own styles
      org-html-head "<link rel=\"stylesheet\" href=\"simple.min.css\" />
<meta name=\"google-site-verification\" content=\"y7aQP8bFOYT2JGYy4gLKMZt2AtHrFMFIMMWPFYlzP-I\" />
 ")

;; Define the publishing project
(setq org-publish-project-alist
      (list
       (list "org-main"
             :recursive t
             :base-directory "~/org/celibistrial-website/content"
             :publishing-function 'org-html-publish-to-html
             :publishing-directory "~/org/celibistrial-website/public"
             :with-author nil           ;; Don't include author name
             :footnote-section-p t
             :html-footnotes-section t
             :html-doctype "<!doctype html>"
             :html-preamble "<script type=\"text/javascript\"> function goBack() {window.history.back();}</script>
<link rel=\"stylesheet\" href=\"https://unpkg.com/highlightjs@9.16.2/styles/obsidian.css\">
<script src=\"https://cdnjs.cloudflare.com/ajax/libs/highlight.js/11.7.0/highlight.min.js\"></script>
<script src=\"https://cdnjs.cloudflare.com/ajax/libs/highlight.js/11.7.0/languages/lisp.min.js\"></script>
<script src=\"particles.min.js\"></script>
<div id=\"particles-js\"></div>
<!--<script>particlesJS.load(\'particles-js\',\"particlesjs-config.json\");</script> -->
"
             :html-postamble "
<div class=\"navigation\">
<font size=\"-1\">
	    <div class=\"footer\"></div>
            <center>
<a href=\"index.html\">Go to home page</a>
<script>hljs.highlightAll();</script>
            </center>
	    </div>
    </font>
</div>
<footer class=\"blog-footer\"><div class=\"container\"><div class=\"row\"><div class=\"col-sm col-md text-sm-left text-md-right text-lg-right text-xl-right\"><p>Made with Emacs (Org mode)</p></div></div></div></footer>
"
             :with-creator nil            ;; Include Emacs and Org versions in footer
             :with-toc nil                ;; Include a table of contents
             :header t
             :section-numbers nil       ;; Don't include section numbers
             :time-stamp-file nil)

       )
      )
(add-to-list 'org-publish-project-alist
             '( "org-static"
                :base-directory "~/org/celibistrial-website/content"
                :base-extension "css\\|js\\|png\\|jpg\\|gif\\|pdf\\|mp3\\|ogg\\|swf\\|svg"
                :publishing-directory "~/org/celibistrial-website/public"
                :recursive t
                :publishing-function org-publish-attachment
                ))
(defun git-commit-and-push-celibistrial ()
  "Commit changes to Git repository in ~/org/celibistrial and push them to the remote origin with commit message 'e'."

  (interactive)
  (let ((commit-msg (read-string "Commit message: ")))
    (let ((default-directory "~/org/celibistrial-website"))
      (unless (file-directory-p default-directory)
        (error "Directory not found: %s" default-directory))

      (shell-command (format "git add --all"))
      (shell-command (format "git commit -m '%s'" commit-msg))
      (shell-command "git push origin HEAD"))
    (let ((default-directory "~/org/celibistrial-website/public"))
      (unless (file-directory-p default-directory)
        (error "Directory not found: %s" default-directory))
      (shell-command (format "git add --all"))
      (shell-command (format "git commit -m '%s'" commit-msg))
      (shell-command "git push origin HEAD"))))
(map!
 "C-x 6 p"
 #'git-commit-and-push-celibistrial)
(setq org-crypt-key "Celibistrial")
(setenv "GPG_AGENT_INFO" nil)

(org-crypt-use-before-save-magic)
(setq org-tags-exclude-from-inheritance (quote ("crypt")))

(map! "C-x <f12>" #'org-decrypt-entry)
(map! "C-x <f11>" #'org-encrypt-entry)
(setq org-clock-sound "~/Music/service-bell-ring-14610.wav")
(beacon-mode 1)
(use-package emojify
  :hook (after-init . global-emojify-mode))
