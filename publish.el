;;Done after tutorial presented https://github.com/SystemCrafters/org-website-example/tree/main

;; Run server simple-httpd by M-x httpd-serve-directory

;; Set the package installation directory so that packages aren't stored in the
;; ~/.emacs.d/

(require 'package)
(setq package-user-dir (expand-file-name "./.packages"))
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/"))



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





;; Config Values

(setq user-full-name "Sebastian Mihai Ardelean")
(setq user-mail-address "ardeleanasm@gmail.com")



;; Site definitions

(defun dw/site-header ()
  (list `(header 
          (nav (@ (class "navbar navbar-default navbar-fixed-top") (role "navigation"))
               (div (@ (class "container"))
                    (div (@ (class "navbar-header page-scroll"))
                         (button (@ (type "button") (class "navbar-toggle") (data-toggle "collapse") (data-target "#bs-example-navbar-collapse-1"))
                                 (span (@ (class "sr-only")) "Toggle Navigation")
                                 (span (@ (class "icon-bar")) "")
                                 (span (@ (class "icon-bar")) "")
                                 (span (@ (class "icon-bar")) "")
                                 (span (@ (class "icon-bar")) "")
                                 )
                         (a (@ (class "navbar-brand page-scroll") (href "#home")) "Sebastian Mihai Ardelean"))
                    (div (@ (class "collapse navbar-collapse") (id "bs-example-navbar-collapse-1"))
                         (ul (@ (class "nav navbar-nav navbar-right"))
                             (li (a (@ (class "page-scroll") (href "#about")) "About"))
                             (li (a (@ (class "page-scroll") (href "#publications")) "Publications"))
                             (li (a (@ (class "page-scroll") (href "#projects")) "Projects"))
                             (li (a (@ (class "page-scroll") (href "#contact")) "Contact"))))))
          (section (@ (id "home"))
                   (div (@ (class "intro-header"))
                        (div (@ (class "container"))
                             (div (@ (class "row"))
                                  (div (@ (class "col-lg-12"))
                                       (div (@ (class "intro-message"))
                                            (h1 "Bending bits...")
                                            (h3 "Bytes and Words...")
                                            (hr (@ (class "intro-divider")))
                                            (ul (@ (class "list-inline intro-social-buttons"))
                                                (li
                                                 (a (@ (class "btn btn-default btn-lg") (href "https://twitter.com/mihaiseba"))
                                                    (i (@ (class "fa fa-twitter fa-fw")) "")
                                                    (span (@ (class "network-name")) "Twitter")))
                                                (li
                                                 (a (@ (class "btn btn-default btn-lg") (href "https://github.com/sebastianardelean"))
                                                    (i (@ (class "fa fa-github fa-fw")) "")
                                                    (span (@ (class "network-name")) "Github")))
                                                (li
                                                 (a (@ (class "btn btn-default btn-lg") (href "https://linkedin.com/in/ardelean-sebastian-mihai"))
                                                    (i (@ (class "fa fa-linkedin fa-fw")) "")
                                                    (span (@ (class "network-name")) "Linkedin"))))))))))
                                                    
       )))

(defun dw/site-about ()
  (list `(section (@ (id "about") (class "container content-section text-center"))
                  (div (@ (class "row"))
                       (div (@ (class "col-lg-8 col-lg-offset-2"))
                            (br)
                            (br)
                            (h2 "About")
                            (br)
                            (p "M.Sc. Eng. Sebastian Mihai, is a Computer Engineer and software developer interested in Computer Architecture, Embedded Programming, Operating Systems and Functional Programming")
                            (br))))))

(defun dw/site-publications ()
  (list `(section (@ (id "publications"))
                  (div (@ (class "content-section-a"))
                       (div (@ (class "container"))
                            (div (@ (class "row"))
                                 (div (@ (class "col-lg-5 col-sm-6"))
                                      (hr (@ (class "section-heading-spacer")))
                                      (div (@ (class "clearfix")) "")
                                      (h2 (@ (class "section-heading")) "Publications")
                                      (div (@ (class "lead"))
                                           (ul


                                            ;;Here add new articles!
                                            
                                            (li "SUCIU, Liana, et al. Categorical Analysis of Database Consistency in Reporting Drug–Drug Interactions for Cardiovascular Diseases. Pharmaceutics, 2024, 16.3: 339.")
                                            (li "UDRESCU, Mihai; ARDELEAN, Sebastian Mihai; UDRESCU, Lucreţia. The curse and blessing of abundance—the evolution of drug interaction databases and their impact on drug network analysis. GigaScience, 2023, 12: giad011.")
                                            (li "ARDELEAN, Sebastian Mihai; UDRESCU, Mihai. Circuit level implementation of the Reduced Quantum Genetic Algorithm using Qiskit. In: 2022 IEEE 16th International Symposium on Applied Computational Intelligence and Informatics (SACI). IEEE, 2022. p. 000155-000160.")
                                            (li "ARDELEAN, Sebastian Mihai; UDRESCU, Mihai. Graph coloring using the reduced quantum genetic algorithm. PeerJ Computer Science, 2022, 7: e836.")
                                            (li "ARDELEAN, Sebastian Mihai; UDRESCU, Mihai. QC| pp>: A Behavioral Quantum Computing Simulation Library. In: 2018 IEEE 12th International Symposium on Applied Computational Intelligence and Informatics (SACI). IEEE, 2018. p. 000437-000442.")
                                            )))
                                 (div (@ (class "col-lg-5 col-lg-offset-2 col-sm-6"))
                                      (img (@ (class "img-responsive") (src "img/blog.png") (alt "")))))))
                  )))
                 
                 

(defun dw/site-projects ()
  (list `(section (@ (id "projects"))
                   (div (@ (class "content-section-b"))
                       (div (@ (class "container"))
                            (div (@ (class "row"))
                                 (div (@ (class "col-lg-5 col-lg-offset-1 col-sm-push-6 col-sm-6"))
                                      (hr (@ (class "section-heading-spacer")))
                                      (div (@ (class "clearfix")) "")
                                      (h2 (@ (class "section-heading")) "Projects")
                                      (div (@ (class "lead"))
                                           (ul
                                            (li (a (@ (href "https://hackage.haskell.org/package/qchas") (target "_blank")) "QCHas"))
                                            (li (a (@ (href "https://github.com/sebastianardelean/cell-automata") (target "_blank")) "Cellular automata"))
                                            (li (a (@ (href "https://github.com/sebastianardelean/libqrng") (target "_blank")) "LibQRNG"))
                                            (li (a (@ (href "https://github.com/sebastianardelean/cl-quantum") (target "_blank")) "cl-quantum"))
                                            (li (a (@ (href "https://github.com/sebastianardelean/hasher") (target "_blank")) "Hasher"))
                                            (li (a (@ (href "https://github.com/sebastianardelean/input_capture_driver") (target "_blank")) "Linux Input Capture Driver")))))
                                      (div (@ (class "col-lg-5 col-sm-pull-6 col-sm-6"))
                                           (img (@ (class "img-responsive") (src "img/blog.png") (alt "")))))))
                   )))

(defun dw/site-contact ()
  (list `(section (@ (id "contact"))
                  (div (@ (class "banner"))
                       (div (@ (class "container"))
                            (div (@ (class "row"))
                                 (div (@ (class "col-lg-6"))
                                      (h2 "Keep in Touch:"))
                                 (div (@ (class "col-lg-6"))
                                      (ul (@ (class "list-inline banner-social-buttons"))
                                          (li
                                           (a (@ (class "btn btn-default btn-lg") (href "https://twitter.com/mihaiseba"))
                                              (i (@ (class "fa fa-twitter fa-fw")) "")
                                              (span (@ (class "network-name")) "Twitter")))
                                          (li
                                           (a (@ (class "btn btn-default btn-lg") (href "https://github.com/sebastianardelean"))
                                              (i (@ (class "fa fa-github fa-fw")) "")
                                              (span (@ (class "network-name")) "Github")))
                                          (li
                                           (a (@ (class "btn btn-default btn-lg") (href "https://linkedin.com/in/ardelean-sebastian-mihai"))
                                              (i (@ (class "fa fa-linkedin fa-fw")) "")
                                              (span (@ (class "network-name")) "Linkedin")))))))))))

(defun dw/site-footer ()
  (list `(footer
          (div (@ (class "container"))
               (div (@ (class "row"))
                    (div (@ (class "col-md-8"))
                         (ul (@ (class "list-inline"))
                             (li
                              (a (@ (href "#home")) "Home"))
                             (li (@ (class "footer-menu-divider")) "&sdot;")
                             (li
                              (a (@ (href "#about")) "About"))
                             (li (@ (class "footer-menu-divider")) "&sdot;")
                             (li
                              (a (@ (href "#publications")) "Publications"))
                             (li (@ (class "footer-menu-divider")) "&sdot;")
                             (li
                              (a (@ (href "#projects")) "Projects"))
                             (li (@ (class "footer-menu-divider")) "&sdot;")
                             (li
                              (a (@ (href "#contact")) "Contact")))
                         (p (@ (class "copyright text-muted small")) "© 2024 · Sebastian Mihai Ardelean"))
                    (div (@ (class "col-md-4")) ""))))))

        

(defun get-article-output-path (org-file pub-dir)
  (let ((article-dir (concat pub-dir
                             (downcase
                              (file-name-as-directory
                               (file-name-sans-extension
                                (file-name-nondirectory org-file)))))))

    (if (string-match "\\/index.org\\|\\/404.org$" org-file)
        pub-dir
        (progn
          (unless (file-directory-p article-dir)
            (make-directory article-dir t))
          article-dir))))

(cl-defun dw/generate-page (title
                            content
                            info
                            &key
                            (publish-date)
                            (head-extra)
                            (pre-content)
                            (exclude-header)
                            (exclude-footer))
  (concat
   "<!-- Generated on " (format-time-string "%Y-%m-%d @ %H:%M") " with " org-export-creator-string "-->\n"
   "<!DOCTYPE html>"
   (sxml-to-xml
    `(html (@ (lang "en"))
           (head
            (meta (@ (charset "utf-8")))
            (meta (@ (http-equiv "X-UA-Compatible") (content "IE=edge")))
            (meta (@ (name "viewport") (content "width=device-width, initial-scale=1")))
            (meta (@ (name "description") (content  "Bytes and Words...")))
            (meta (@ (name "author") (content "Sebastian Mihai Ardelean")))
            (title "Bending bits...")
            (link (@ (rel "icon") (type "image/png") (href "img/favicon.png")))
            (link (@ (href "css/bootstrap.min.css") (rel "stylesheet")))
            (link (@ (href "css/landing-page.css") (rel "stylesheet")))
            (link (@ (href "font-awesome-4.1.0/css/font-awesome.min.css") (rel "stylesheet") (type "text/css")))
            (link (@ (href "http://fonts.googleapis.com/css?family=Lato:300,400,700,300italic,400italic,700italic") (rel "stylesheet") (type "text/css")))
           
;;            (script (@ (src "js/jquery.easing.min.js")))
            , (when head-extra head-extra))
           (body
            ,@(dw/site-header)
            ,@(dw/site-about)
            ,@(dw/site-publications)
            ,@(dw/site-projects)
            ,@(dw/site-contact)
            ,@(dw/site-footer)
            (script (@ (src "js/jquery-1.11.0.js")) "")
            (script (@ (src "js/jquery.easing.min.js")) "")
            (script (@ (src "js/bootstrap.min.js")) "")
            (script (@ (src "js/landing-page.js")) "")
            )))))


                 
                 

(defun dw/org-html-template (contents info)
  (dw/generate-page (org-export-data (plist-get info :title) info)
                    contents
                    info
                    :publish-date (org-export-data (org-export-get-date info "%B %e, %Y") info)))





(org-export-define-derived-backend 'site-html 'html
  :translate-alist
  '((template . dw/org-html-template)))
                                   
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
;      org-html-link-home dw/site-url
      org-html-link-use-abs-url t
      org-html-link-org-files-as-html t
      org-html-html5-fancy t
      org-html-self-link-headlines t
      org-export-with-toc nil
      make-backup-files nil)



;; Define the publishing project
(setq org-publish-project-alist
      (list
       '("website:main"
             :base-directory "./content"
             :base-extension "org"
             :publishing-directory "./docs"
             :publishing-function org-html-publish-to-html
             :with-title nil
             :time-stamp-file nil ;; no time stamp
             )
       '("website:assets"
             :base-directory "./assets"
             :base-extension "css\\|js\\|png\\|jpg\\|gif\\|pdf\\|mp3\\|ogg\\|woff2\\|ttf\\|woff"
             :publishing-directory "./docs"
             :recursive t
             :publishing-function org-publish-attachment)))



(defun dw/publish ()
  "Publish website"
  (interactive)
  (org-publish-all (string-equal (or (getenv "FORCE")
                                     (getenv "CI"))
                                 "true"))


  (message "Build complete!")
  )

;; Generate the site output
;;(org-publish-all t)

(provide 'publish)
