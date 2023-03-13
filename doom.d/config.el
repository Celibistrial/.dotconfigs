;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-
;;; 9603064939

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets. It is optional.
(setq user-full-name "celibistrial"
      user-mail-address "celibistrial@gmail.com")
(setq select-enable-clipboard nil)
(map!
 "C-S-v" #'clipboard-yank)
(map!
 "C-S-c" #'clipboard-kill-ring-save)
(map!
 :leader
 :nv
 "z" #'comint-dynamic-complete-filename)
;; Doom exposes five (optional) variables for controlling fonts in Doom:
;;
;; - `doom-font' -- the primary font to use
;; - `doom-variable-pitch-font' -- a non-monospace font (where applicable)
;; - `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;; - `doom-unicode-font' -- for unicode glyphs
;; - `doom-serif-font' -- for the `fixed-pitch-serif' face
;;
;; See 'C-h v doom-font' for documentation and more examples of what they
;; accept. For example:
;;
;;(setq doom-font (font-spec :family "Fira Code" :size 12 :weight 'semi-light)
;;      doom-variable-pitch-font (font-spec :family "Fira Sans" :size 13))
;;
;; If you or Emacs can't find your font, use 'M-x describe-font' to look them
;; up, `M-x eval-region' to execute elisp code, and 'M-x doom/reload-font' to
;; refresh your font settings. If Emacs still can't find your font, it likely
;; wasn't installed correctly. Font issues are rarely Doom issues!

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-one)
(setq display-line-numbers-type t)
;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")
(setq doom-font (font-spec :family "JetBrains Mono" :size 14 :weight 'Medium)
      doom-variable-pitch-font (font-spec :family "JetBrains Mono" :size 15))
                                        ;
;;(setq doom-font (font-spec :family "JetBrains Mono" :size 13))
;; Whenever you reconfigure a package, make sure to wrap your config in an
;; `after!' block, otherwise Doom's defaults may override your settings. E.g.
;;
;;   (after! PACKAGE
;;     (setq x y))
;;
;; The exceptions to this rule:
;;
;;   - Setting file/directory variables (like `org-directory')
;;   - Setting variables which explicitly tell you to set them before their
;;     package is loaded (see 'C-h v VARIABLE' to look up their documentation).
;;   - Setting doom variables (which start with 'doom-' or '+').
;;
;; Here are some additional functions/macros that will help you configure Doom.
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;; Alternatively, use `C-h o' to look up a symbol (functions, variables, faces,
;; etc).
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.
;;;; Emoji: 😄, 🤦, 🏴󠁧󠁢󠁳󠁣󠁴󠁿
;;(set-fontset-font t 'symbol "Apple Color Emoji")
;;(set-fontset-font t 'symbol "Noto Color Emoji" nil 'append)
;;(set-fontset-font t 'symbol "Segoe UI Emoji" nil 'append)
;;(set-fontset-font t 'symbol "Symbola" nil 'append)

(after! lsp-ui
  (setq lsp-ui-sideline-show-code-actions t)
  (setq lsp-headerline-breadcrumb-enable t)
  (setq lsp-enable-symbol-highlighting t)
  (setq lsp-ui-doc-enable t)
  (setq lsp-headerline-breadcrumb-enable nil)
  (setq lsp-ui-sideline-enable t)
  (setq lsp-ui-sideline-enable t)
  (setq lsp-modeline-code-actions-enable t)
  (setq lsp-ui-sideline-enable t)
  (setq lsp-eldoc-enable-hover t)
  (setq lsp-completion-show-detail t)
  (setq lsp-completion-show-kind t))
(setq doom-modeline-env-version t)
(setq doom-modeline-time t)

(setq org-crypt-key "Celibistrial")
(setenv "GPG_AGENT_INFO" nil)

(setq org-journal-encrypt-journal nil)
(setq org-journal-encrypt-on nil)
(org-crypt-use-before-save-magic)
(setq org-tags-exclude-from-inheritance (quote ("crypt")))


(map! "C-x <f12>" #'org-decrypt-entry)
(map! "C-x <f11>" #'org-encrypt-entry)
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
;; (setq org-capture-templates '(("t" "Personal todo" entry (file+headline +org-capture-todo-file "Inbox") "* [ ] %?
;; %i
;; %a" :prepend t) ("n" "Personal notes" entry (file+headline +org-capture-notes-file "Inbox") "* %u %?
;; %i
;; %a" :prepend t) ("p" "Templates for projects") ("pt" "Project-local todo" entry (file+headline +org-capture-project-todo-file "Inbox") "* TODO %?
;; %i
;; %a" :prepend t) ("pn" "Project-local notes" entry (file+headline +org-capture-project-notes-file "Inbox") "* %U %?
;; %i
;; %a" :prepend t) ("pc" "Project-local changelog" entry (file+headline +org-capture-project-changelog-file "Unreleased") "* %U %?
;; %i
;; %a" :prepend t) ("o" "Centralized templates for projects") ("ot" "Project todo" entry #'+org-capture-central-project-todo-file "* TODO %?
;;  %i
;;  %a" :heading "Tasks" :prepend nil) ("on" "Project notes" entry #'+org-capture-central-project-notes-file "* %U %?
;;  %i
;;  %a" :heading "Notes" :prepend t) ("oc" "Project changelog" entry #'+org-capture-central-project-changelog-file "* %U %?
;;  %i
;;  %a" :heading "Changelog" :prepend t) ("j" "Journal entry" plain (function org-journal-find-location)
;;                                "** %(format-time-string org-journal-time-format)%^{Title}\n%i%?"
;;                                :jump-to-captured t :immediate-finish t) ))
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
(setq langtool-java-classpath
      "/usr/share/languagetool:/usr/share/java/languagetool/*")
;; (setq langtool-http-server-host "localhost"
;;       langtool-http-server-port 8081)
;; (setq langtool-server-user-arguments '("-p" "8081"))
(require 'langtool)
;; (use-package! lsp-grammarly)
;; (add-hook! text-mode
;;            (lambda ()
;;                        (require 'lsp-grammarly)
;;                        (lsp)))
(defun correct-buffer ()
  (interactive)
  (langtool-check-buffer)
  (langtool-correct-buffer))
(setq langtool-default-language "en-GB")
(global-set-key "\C-x4w" 'langtool-check)
(global-set-key "\C-x4W" 'langtool-check-done)
(global-set-key "\C-x4l" 'langtool-switch-default-language)
(global-set-key "\C-x44" 'langtool-show-message-at-point)
(global-set-key "\C-x4c" 'langtool-correct-buffer)
(after! org
  (add-to-list 'org-capture-templates
               '("x" "Temp"
                 entry (file "~/org/temp.org")
                 "* %?"))
  )
;; ORG-PUBLISH
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
             :auto-sitemap t
             :sitemap-filename "sitemap.org"
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
(defun publish-sitemap (plist filename pub-dir)
  "Publish an Org-mode file as a sitemap XML file."
  (let* ((outfile (concat pub-dir (file-name-sans-extension filename) ".xml"))
         (sitemap-table (with-current-buffer (find-file-noselect filename)
                           (org-table-to-lisp (org-element-parse-buffer)))))
    (with-temp-buffer
      (insert "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
              "<urlset xmlns=\"http://www.sitemaps.org/schemas/sitemap/0.9\">\n")
      (dolist (row sitemap-table)
        (let ((url (nth 0 row))
              (lastmod (nth 1 row)))
          (when (stringp url)
            (insert "  <url>\n"
                    "    <loc>" url "</loc>\n"
                    "    <lastmod>" (format-time-string "%Y-%m-%dT%H:%M:%S+00:00" lastmod) "</lastmod>\n"
                    "  </url>\n"))))
      (insert "</urlset>\n")
      (write-file outfile))
    (message "Wrote sitemap to %s" outfile)))
(add-to-list 'org-publish-project-alist
             '( "org-sitemap"
                :base-directory "~/org/celibistrial-website/content"
                :base-extension "org"
                :publishing-directory "~/org/celibistrial-website/public"
                :recursive t
                :publishing-function publish-sitemap
                :sitemap-filename "sitemap.xml"
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
(setq evil-want-fine-undo t)
