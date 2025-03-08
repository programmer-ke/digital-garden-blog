;; install packages
(require 'package)
(setq package-user-dir "~/projects/digital-garden-blog/pkgs") ;; Installation path
(package-initialize)  ;; Auto-adds package dirs to load-path
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)

(setq package-list
      '(htmlize ox-rss))

;; install packages
(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))


(require 'ox-publish)
(require 'ox-rss)

;;; Tag files functionality
(defun extract-all-tags ()
  "Return a list of all tags from #+FILETAGS and headline inline-tags."
  (let (tags)
    ;; Extract #+FILETAGS (includes colon-delimited tags)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "^#\\+FILETAGS:\\s-*:\\([^:\n]+\\)" nil t)
        (dolist (tag (split-string (match-string 1) ":" t))
          (push tag tags))))

    ;; Extract headline inline tags (also colon-delimited)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "^\\(\\*+\\)\\s-.*\\s-:\\([[:alnum:]_:]+\\):\\(?:\\s-\\|$\\)" nil t)
        (dolist (tag (split-string (match-string 2) ":" t))
          (push tag tags))))

    (delete-dups tags)))


(defun build-tag-file-mapping (directory)
  "Return hash table mapping tags to lists of files containing them."
  (interactive "DDirectory: ")
  (let ((tag-map (make-hash-table :test 'equal :size 200))
        (org-files (directory-files-recursively directory "\\.org$")))
    (dolist (file org-files)
      (when (and (file-regular-p file)
                 (file-readable-p file))
        (with-temp-buffer
          (insert-file-contents file)
          (org-mode)
          (let ((tags (extract-all-tags))
                (clean-file (file-relative-name file directory)))
            (dolist (tag tags)
              (puthash tag 
                       (cons clean-file (gethash tag tag-map))
                       tag-map))))))
    tag-map))

(defun generate-tag-files (tag-map target-dir prefix)
  "Generate Org files with linked entries for all tags in TAG-MAP.
TARGET-DIR: Directory to create tag files in
PREFIX: String to prepend to file paths in links"
  (unless (file-directory-p target-dir)
    (make-directory target-dir t))
  
  (maphash
   (lambda (original-tag files)
     (let* ((clean-tag (replace-regexp-in-string "[^[:alnum:]-_]" "-" original-tag))
            (tag-file (expand-file-name (concat clean-tag ".org") target-dir))
            (link-lines
             (mapcar
              (lambda (f)
                (concat "[[file:" prefix f "][" f "]]"))
              files)))
       
       (with-temp-buffer
         (insert "#+TITLE: Tag: " clean-tag "\n\n")
         (dolist (link link-lines)
           (insert link "\n"))
         (when (buffer-modified-p)
           (write-file tag-file)))))
   tag-map))

;; test driving:

;; (let ((tag-map (build-tag-file-mapping "~/digital-garden")))
;;   (generate-tag-files 
;;    tag-map
;;    "~/projects/digital-garden-blog/tags"
;;    "~/digital-garden/"))  ;; Prefix for link paths

;;(let ((tag-map (build-tag-file-mapping "~/digital-garden/")))
;;  (maphash (lambda (tag files)
;;             (message "Tag: %-20s | Files: %s" tag files))
;;           tag-map))


(defun blog/website-html-head ()
  (with-temp-buffer
    (insert-file-contents "~/projects/digital-garden-blog/html-templates/head.html")
    (buffer-string)))
    

(defvar this-date-format "%b %d, %Y %H:%M")

(setq org-html-metadata-timestamp-format this-date-format)

(defun blog/website-html-preamble (plist)
  "PLIST: An entry."
  (if (org-export-get-date plist this-date-format)
      (plist-put plist
		 :subtitle (format "Published on %s by %s."
				   (org-export-get-date plist this-date-format)
				   (car (plist-get plist :author)))))
  ;; Preamble
  (with-temp-buffer
    (insert-file-contents "~/projects/digital-garden-blog/html-templates/preamble.html") (buffer-string)))

(defun blog/website-html-postamble (plist)
  "PLIST."
  (concat (format
	   (with-temp-buffer
	     (insert-file-contents "~/projects/digital-garden-blog/html-templates/postamble.html") (buffer-string))
	   (format-time-string this-date-format (plist-get plist :time)) (plist-get plist :creator))))


(defun title-case-transform (str)
  "Convert STR with underscores/hyphens to title case words separated by spaces.
Example: 'legal-docs' -> 'Legal Docs', 'collapsing_dominoes' -> 'Collapsing Dominoes'"
  (let ((words (split-string str "[_-]" t)))
    (mapconcat 'identity
               words
               " ")))

(defun blog/org-sitemap-format-entry (entry style project)
  "Format posts with author and published data in the index page.

ENTRY: file-name
STYLE:
PROJECT: `posts in this case."
  (cond ((not (directory-name-p entry))
         (format "*[[file:%s][%s]]*
                 #+HTML: <p class='pubdate'>by %s on %s.</p>"
                 entry
                 (title-case-transform (org-publish-find-title entry project))
                 (car (org-publish-find-property entry :author project))
                 (format-time-string this-date-format
                                     (org-publish-find-date entry project))))
        ((eq style 'tree) (file-name-nondirectory (directory-file-name entry)))
        (t entry)))


(defun prepare-tag-files (plist)
  (let ((tag-map (build-tag-file-mapping "~/digital-garden")))
    (generate-tag-files 
     tag-map
     "~/projects/digital-garden-blog/tags"
     "../")))

(setq org-publish-project-alist
      `(("posts"
         :base-directory "~/digital-garden/"
         :base-extension "org"
         :publishing-directory "public/"
         :recursive t
         :publishing-function org-html-publish-to-html

         :auto-sitemap t
         :sitemap-title "Blog Index"
         :sitemap-filename "index.org"
         :sitemap-style list
	 :sitemap-sort-files anti-chronologically
	 :sitemap-format-entry blog/org-sitemap-format-entry

	 :html-head ,(blog/website-html-head)
	 :html-preamble blog/website-html-preamble
	 :html-postamble blog/website-html-postamble
	 :html-html5-fancy nil
	 :html-doctype "html5"
         :author "krm"
         :email "krm@vger"
         :with-creator t)
	("pages"
         :base-directory "pages/"
         :base-extension "org"
         :publishing-directory "public/pages/"
         :publishing-function org-html-publish-to-html

	 :html-head ,(blog/website-html-head)
	 :html-preamble blog/website-html-preamble
	 :html-postamble blog/website-html-postamble
	 :html-html5-fancy nil
	 :html-doctype "html5"
	 
         :email "krm@vger"
         :with-creator t)
	("tags"
         :base-directory "~/projects/digital-garden-blog/tags"
         :base-extension "org"
         :publishing-directory "public/tags/"
         :recursive t
	 :preparation-function prepare-tag-files
         :publishing-function org-html-publish-to-html

         :auto-sitemap t
         :sitemap-title "Tag Index"
         :sitemap-filename "index.org"
         :sitemap-style list
	 :html-html5-fancy nil
	 :html-doctype "html5"

	 :html-head ,(blog/website-html-head)
	 :html-preamble blog/website-html-preamble
	 :html-postamble blog/website-html-postamble)
	("rss"
	  :base-directory "~/digital-garden/"
	  :base-extension "org"
	  :html-link-home "https://digitalgarden.ken.ke/"
	  :html-link-use-abs-url t
	  :rss-extension "xml"
	  :publishing-directory "public/"
	  :publishing-function org-rss-publish-to-rss
	  :section-numbers nil
	  :exclude ".*"            ;; To exclude all files...
	  :include ("index.org")   ;; ... except index.org.
	  :table-of-contents nil)
        ("css"
         :base-directory "css/"
         :base-extension "css"
         :publishing-directory "public/css"
         :publishing-function org-publish-attachment
         :recursive t)
	("img"
         :base-directory "img/"
         :base-extension "png"
         :publishing-directory "public/img"
         :publishing-function org-publish-attachment
         :recursive t)
	("icons"
         :base-directory "icons/"
         :base-extension "\\(png\\|ico\\|webmanifest\\)"
         :publishing-directory "public/"
         :publishing-function org-publish-attachment
         :recursive t)
        ("all" :components ("posts" "pages" "css" "img" "icons" "tags" "rss"))))
