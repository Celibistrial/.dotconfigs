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

(provide 'org-website)
