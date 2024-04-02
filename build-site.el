;;Done after tutorial presented https://github.com/SystemCrafters/org-website-example/tree/main

;; Run server simple-httpd by M-x httpd-server-directory

;; Set the package installation directory so that packages aren't stored in the
;; ~/.emacs.d/

(require 'package)
(setq package-user-dir (expand-file-name "./.packages"))
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/"))
(add-to-list 'package-archives '("elpa" . "https://elpa.gnu.org/packages/"))



;; Initialize the package system
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))


(unless (package-installed-p 'use-package)
  (package-install 'use-package))
(require 'use-package)


;; Require built-in dependencies
(require 'vc-git)
(require 'ox-publish)
(require 'subr-x)
(require 'cl-lib)


;; Install dependencies
(use-package esxml
  :pin "melpa-stable"
  :ensure t)

(use-package htmlize
  :ensure t)

(use-package webfeeder
  :ensure t)



;; Config Values

(setq user-full-name "Sebastian Mihai Ardelean")
(setq user-mail-address "ardeleanasm@gmail.com")


;; The site!

(defvar dw/site-url (if (string-equal (getenv "CI") "true")
                        "https://sebastianardelean.github.io"
                      "http://localhost:8080")
  "The URL for the site being generated.")



(defun dw/site-header ()
  (list `(nav (@ (class "navbar navbar-default navbar-fixed-top") (role "navigation"))
               (div (@ (class "container"))
                    (div (@ (class "navbar-header page-scroll"))
                         (button (@ (type "button") (class "navbar-toggle") (data-toggle "collapse") (data-target "#bs-example-navbar-collapse-1"))
                                 (span (@ (class "sr-only")) "Toggle navigation")
                                 (span (@ (class "icon-bar"))) (span (@ (class "icon-bar"))) (span (@ (class "icon-bar")))))
                    (a (@ (class "navbar-brand page-scroll") (href "#home")) "Sebastian Mihai Ardelean"))
               (div (@ (class "collapse navbar-collapse") (id "bs-example-navbar-collapse-1"))
                    (ul (@ (class "nav navbar-nav navbar-right"))
                        (li (a (@ (class "page-scroll") (href "#about")) "About"))
                        (li (a (@ (class "page-scroll") (href "#services")) "Projects"))
                        (li (a (@ (class "page-scroll") (href "#contact")) "Contact"))))))
  (list `(section (@ (id "home"))
                  (div (@ (class "intro-header"))
                       (div (@ (class "container"))
                            (div (@ (class "row"))
                                 (div (@ (class "col-lg-12"))
                                      (div (@ (class "intro-message"))
                                           (h1 "{{ Bending bits... }}")
                                           (h3 "{{ Bytes and Words... }}")
                                           (hr (@ (class "intro-divider")))
                                           (ul (@ (class "list-inline intro-social-buttons"))
                                               (li (a (@ (href "https://twitter.com/mihaiseba" (class "btn btn-default btn-lg"))
                                                         (i (@ (class ,(concat "fa fa-twitter fa-fw")))) 
                                                         (span (@ (class "network-name")) "Twitter"))))
                                               (li (a (@ (href "https://github.com/sebastianardelean " (class "btn btn-default btn-lg"))
                                                         (i (@ (class ,(concat "fa fa-github fa-fw")))) 
                                                         (span (@ (class "network-name")) "Github"))))
                                               (li (a (@ (href "https://linkedin.com/in/ardelean-sebastian-mihai " (class "btn btn-default btn-lg"))
                                                         (i (@ (class ,(concat "fa fa-Linkedin fa-fw")))) 
                                                         (span (@ (class "network-name")) "Linkedin"))))
                                                  )))))))))





(defun dw/site-footer ()
  (list `(footer ()
                 (div (@ (class "container"))
                      (div (@ (class "row"))
                           (div (@ (class "col-md-8"))
                                (ul (@ (class "list-inline"))
                                    (li (a (@ (href "#home")) "Home"))
                                    (li (@ (class "footer-menu-divider")) "&sdot;")
                                    (li (a (@ (href "#about")) "About"))
                                    (li (@ (class "footer-menu-divider")) "&sdot;")
                                    (li (a (@ (href "#services")) "Services"))
                                    (li (@ (class "footer-menu-divider")) "&sdot;")
                                    (li (a (@ (href "#contact")) "Contact")))
                                (p (@ (class "copyright text-muted small")) "2024")))
                           (div (@ (class "col-md-4")) "{{ site.credits }}")))))

(defun dw/about-section ()
  (list `(section (@ (id "about") (class "container content-section text-center"))
                   (div (@ (class "row"))
                        (div (@ (class "col-lg-8 col-lg-offset-2"))
                             (br) (br)
                             (h2 "About")
                             (br)
                             (p "M.Sc. Eng. Sebastian Mihai, is a Computer Engineer and software developer interested in Computer Architecture, Embedded Programming, Operating Systems and Functional Programming")
                             (br))))))

(defun dw/services-section ()
  (list `(section (@ (id "services"))
                   (div (@ (class "content-section-a"))
                        (div (@ (class "container"))
                             (div (@ (class "row"))
                                  (div (@ (class "col-lg-5 col-sm-6"))
                                       (hr (@ (class "section-heading-spacer")))
                                       (div (@ (class "clearfix")))
                                       (h2 (@ (class "section-heading")) "Articles")
                                       (div (@ (class "lead"))
                                            (ul)
                                                (li "ARDELEAN, Sebastian Mihai; UDRESCU, Mihai. QC| pp>: A Behavioral Quantum Computing Simulation Library. In: 2018 IEEE 12th International Symposium on Applied Computational Intelligence and Informatics (SACI). IEEE, 2018. p. 000437-000442.")
                                                (li "ARDELEAN, Sebastian Mihai; UDRESCU, Mihai. Graph coloring using the reduced quantum genetic algorithm. PeerJ Computer Science, 2022, 7: e836.")))
                                  (div (@ (class "col-lg-5 col-lg-offset-2 col-sm-6"))
                                       (img (@ (class "img-responsive") (src "img/services/blog.png") (alt "")))))))
                   (div (@ (class "content-section-b"))
                        (div (@ (class "container"))
                             (div (@ (class "row"))
                                  (div (@ (class "col-lg-5 col-lg-offset-1 col-sm-push-6 col-sm-6"))
                                       (hr (@ (class "section-heading-spacer")))
                                       (div (@ (class "clearfix")))
                                       (h2 (@ (class "section-heading")) "Projects")
                                       (div (@ (class "lead"))
                                            (ul)
                                                (li (a (@ (href "https://hackage.haskell.org/package/qchas") (target "_blank")) "QCHas"))
                                                (li (a (@ (href "https://github.com/ardeleanasm/input_capture_driver") (target "_blank")) "Linux Input Capture Driver"))))
                                  (div (@ (class "col-lg-5 col-sm-pull-6 col-sm-6"))
                                       (img (@ (class "img-responsive") (src "img/services/blog.png") (alt ""))))))))))



(cl-defun dw/generate-page (title
                            content
                            info
                            &key
                            (publish-date)
                            (head-extra)
                            (pre-content)
                            (exclude-header)
                            (exclude-footer))
  (concat  "<!DOCTYPE html>"
           (sxml-to-xml
            `(html (@ (lang "en"))
                   (head
                    (meta (@ (charset "utf-8")))
                    (meta (@ (http-equiv "X-UA-Compatible") (content "IE=edge")))
                    
                  
                    (meta (@ (author "Sebastian Mihai Ardelean")))
                    (meta (@ (name "viewport")
                     (content "width=device-width, initial-scale=1, shrink-to-fit=no")))
                    (link (@ (rel "icon") (type "image/png") (href "/img/favicon.png")))
                  
                    (link (@ (rel "stylesheet") (href ,(concat dw/site-url "/css/bootstrap.min.css"))))
                    (link (@ (rel "stylesheet") (href ,(concat dw/site-url "/css/landing-page.css"))))
                    (link (@ (rel "stylesheet") (href ,(concat dw/site-url "/font-awesome-4.1.0/css/font-awesome.min.css"))))
                    (link (@ (rel "stylesheet") (href ,"http://fonts.googleapis.com/css?family=Lato:300,400,700,300italic,400italic,700italic"))))              
            ,(when head-extra head-extra)
            (title "Sebastian Mihai Ardelean"))
           (body ,@(unless exclude-header
                     (dw/site-header))
                 (div (@ (class "container"))
                      (div (@ (class "site-post"))
                           (h1 (@ (class "site-post-title"))
                               ,title)
                           ,(when publish-date
                              `(p (@ (class "site-post-meta")) ,publish-date))
                           ,(if-let ((video-id (plist-get info :video)))
                                (dw/embed-video video-id))
                           ,(when pre-content pre-content)
                           (div (@ (id "content"))
                                ,content))
                      ,(dw/embed-list-form))
                 ,@(unless exclude-footer
                     (dw/site-footer))))))


(defun dw/org-html-template (contents info)
  (dw/generate-page (org-export-data (plist-get info :title) info)
                    contents
                    info
                    :publish-date (org-export-data (org-export-get-date info "%B %e, %Y") info)))

(defun dw/org-html-link (link contents info)
  "Removes file extension and changes the path into lowercase file:// links."
  (when (and (string= 'file (org-element-property :type link))
             (string= "org" (file-name-extension (org-element-property :path link))))
    (org-element-put-property link :path
                              (downcase
                               (file-name-sans-extension
                                (org-element-property :path link)))))

  (let ((exported-link (org-export-custom-protocol-maybe link contents 'html info)))
    (cond
     (exported-link exported-link)
     ((equal contents nil)
      (format "<a href=\"%s\">%s</a>"
              (org-element-property :raw-link link)
              (org-element-property :raw-link link)))
     ((string-prefix-p "/" (org-element-property :raw-link link))
      (format "<a href=\"%s\">%s</a>"
              (org-element-property :raw-link link)
              contents))
     (t (org-export-with-backend 'html link contents info)))))

(defun dw/make-heading-anchor-name (headline-text)
  (thread-last headline-text
    (downcase)
    (replace-regexp-in-string " " "-")
    (replace-regexp-in-string "[^[:alnum:]_-]" "")))

(defun dw/org-html-headline (headline contents info)
  (let* ((text (org-export-data (org-element-property :title headline) info))
         (level (org-export-get-relative-level headline info))
         (level (min 7 (when level (1+ level))))
         (anchor-name (dw/make-heading-anchor-name text))
         (attributes (org-element-property :ATTR_HTML headline))
         (container (org-element-property :HTML_CONTAINER headline))
         (container-class (and container (org-element-property :HTML_CONTAINER_CLASS headline))))
    (when attributes
      (setq attributes
            (format " %s" (org-html--make-attribute-string
                           (org-export-read-attribute 'attr_html `(nil
                                                                   (attr_html ,(split-string attributes))))))))
    (concat
     (when (and container (not (string= "" container)))
       (format "<%s%s>" container (if container-class (format " class=\"%s\"" container-class) "")))
     (if (not (org-export-low-level-p headline info))
         (format "<h%d%s><a id=\"%s\" class=\"anchor\" href=\"#%s\">¶</a>%s</h%d>%s"
                 level
                 (or attributes "")
                 anchor-name
                 anchor-name
                 text
                 level
                 (or contents ""))
       (concat
        (when (org-export-first-sibling-p headline info) "<ul>")
        (format "<li>%s%s</li>" text (or contents ""))
        (when (org-export-last-sibling-p headline info) "</ul>")))
     (when (and container (not (string= "" container)))
       (format "</%s>" (cl-subseq container 0 (cl-search " " container)))))))

(defun dw/org-html-src-block (src-block _contents info)
  (let* ((lang (org-element-property :language src-block))
	       (code (org-html-format-code src-block info)))
    (format "<pre>%s</pre>" (string-trim code))))

(defun dw/org-html-special-block (special-block contents info)
  "Transcode a SPECIAL-BLOCK element from Org to HTML.
CONTENTS holds the contents of the block.  INFO is a plist
holding contextual information."
  (let* ((block-type (org-element-property :type special-block))
         (attributes (org-export-read-attribute :attr_html special-block)))
	  (format "<div class=\"%s center\">\n%s\n</div>"
            block-type
            (or contents
                (if (string= block-type "cta")
                    "If you find this guide helpful, please consider supporting System Crafters via the links on the <a href=\"/how-to-help/#support-my-work\">How to Help</a> page!"
                  "")))))


(org-export-define-derived-backend 'site-html 'html
  :translate-alist
  '((template . dw/org-html-template)
    (link . dw/org-html-link)
    (src-block . dw/org-html-src-block)
    (special-block . dw/org-html-special-block)
    (headline . dw/org-html-headline))
  :options-alist
  '((:video "VIDEO" nil nil)))


(defun org-html-publish-to-html (plist filename pub-dir)
  "Publish an org file to HTML, using the FILENAME as the output directory."
  (let ((article-path (get-article-output-path filename pub-dir)))
    (cl-letf (((symbol-function 'org-export-output-file-name)
               (lambda (extension &optional subtreep pub-dir)
                 ;; The 404 page is a special case, it must be named "404.html"
                 (concat article-path
                         (if (string= (file-name-nondirectory filename) "404.org") "404" "index")
                         extension))))
      (org-publish-org-to 'site-html
                          filename
                          (concat "." (or (plist-get plist :html-extension)
                                          "html"))
                          plist
                          article-path))))

;; Customize HTML output
(setq org-publish-use-timestamps-flag t
      org-publish-timestamp-directory "./.org-cache/"
      org-export-with-section-numbers nil
      org-export-use-babel nil
      org-export-with-smart-quotes t
      org-export-with-sub-superscripts nil
      org-export-with-tags 'not-in-toc
      org-html-htmlize-output-type 'css
      org-html-prefer-user-labels t
      org-html-link-home dw/site-url
      org-html-link-use-abs-url t
      org-html-link-org-files-as-html t
      org-html-html5-fancy t
      org-html-self-link-headlines t
      org-export-with-toc nil
      make-backup-files nil)




(setq org-publish-project-alist
      (list '("sebastianardelean:main"
              :base-directory "./content"
              :base-extension "org"
              :publishing-directory "./public"
              :publishing-function org-html-publish-to-html
              :with-title nil
              :with-timestamps nil)
            '("sebastianardelean:assets"
              :base-directory "./assets"
              :base-extension "css\\|js\\|png\\|jpg\\|gif\\|pdf\\|mp3\\|ogg\\|woff2\\|ttf"
              :publishing-directory "./public"
              :recursive t
              :publishing-function org-publish-attachment)))

(defun dw/generate-redirects (redirects)
  (dolist (redirect redirects)
    (let ((output-path (concat "./public/" (car redirect) "/index.html"))
          (redirect-url (concat dw/site-url "/" (cdr redirect) "/")))
      (make-directory (file-name-directory output-path) t)
      (with-temp-file output-path
        (insert
         (dw/generate-page "Redirecting..."
                           (concat "You are being redirected to "
                                   "<a href=\"" redirect-url "\">" redirect-url "</a>")
                           '()
                           :head-extra
                           (concat "<meta http-equiv=\"refresh\" content=\"0; url='" redirect-url "'\"/>")))))))

(defun dw/publish ()
  "Publish the entire site."
  (interactive)
  (org-publish-all (string-equal (or (getenv "FORCE")
                                     (getenv "CI"))
                                 "true"))

  
  ;; Copy the domains file to ensure the custom domain resolves
  (copy-file ".domains" "public/.domains" t)

  ;; Copy the .well-known folder for Matrix
  (unless (file-exists-p "public/.well-known")
    (copy-directory ".well-known" "public/" t)))

(provide 'publish)
;;; publish.el ends here
