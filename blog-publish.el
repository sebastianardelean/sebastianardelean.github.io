;; Set the package installation directory so that packages aren't stored in the
;; ~/.emacs.d/elpa path.
(require 'package)
(setq package-user-dir (expand-file-name "./.packages"))
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))

;; Initialize the package system
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

;; Install dependencies
(package-install 'htmlize)

;; Load the publishing system
(require 'ox-publish)


;; Customize the HTML output
(setq org-html-validation-link nil            ;; Don't show validation link
      org-html-head-include-scripts nil       ;; Use our own scripts
      org-html-head-include-default-style nil ;; Use our own styles
      org-html-head "<link rel=\"stylesheet\" href=\"https://cdn.simplecss.org/simple.min.css\" />")




          

(defun my-org-sitemap-format-entry (entry style project)
  "Format ENTRY for the sitemap."
  (let ((filename (file-name-sans-extension entry))
        (title (org-publish-find-title entry project))
        (date (org-publish-find-date entry project)))
;;    (message "Debug: filename=%s, title=%s, date=%s" filename title date)
    (cond (
           (and (not (directory-name-p entry))
                (not (string= filename "index"))
                (not (string= filename "archive"))
                )
           (format "[[file:%s.html][%s]] (%s)"
                   filename
                   (or title "No Title")
                   (format-time-string "%Y-%m-%d" (or date (current-time)))))
          (t ""))))
;;          (t "[[./][Blog]]"))))




;; Define the publishing projetc
(setq org-publish-project-alist
      (list
       (list "org-site:main"
             :recursive t
             :base-directory "./blog"
             :publishing-function 'org-html-publish-to-html
             :publishing-directory "./docs/blog"
             :with-author nil           ;; Don't include author name
             :with-creator nil            ;; Include Emacs and Org versions in footer
             :with-toc t                ;; Include a table of contents
             :section-numbers t       ;; Don't include section numbers
             :time-stamp-file nil
             :html-head-extra "<header><a href=\"./\">Home</a>
</header>"
             :auto-sitemap t
             :sitemap-filename "index.org"
             :sitemap-title "Blog... or in fact, some short notes"
             :sitemap-style 'list
             :sitemap-sort-files 'anti-chronologically
             :sitemap-file-entry-format "%t (%d)" ;; Display title and date
             :sitemap-format-entry 'my-org-sitemap-format-entry
             )
      
       ))   
;; Generate the site output
(org-publish-all t)

(message "Build complete!")
