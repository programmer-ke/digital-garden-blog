(add-to-list 'load-path "~/.emacs.d/elpa/htmlize-1.58")
(add-to-list 'load-path (expand-file-name "~/.emacs.d/elpa/"))
(mapc (lambda (pkg-dir)
        (when (file-directory-p pkg-dir)
          (add-to-list 'load-path pkg-dir)))
      (directory-files (expand-file-name "~/.emacs.d/elpa/") t "\\w+"))

(require 'ox-publish)


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


;; possible approach for hooking in the tags functionality
;;   ‘:preparation-function’
;; 
;;     Function to be called before publishing this project.  This
;;     may also be a list of functions.  Preparation functions are
;;     called with the project properties list as their sole
;;     argument.


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

	 :makeindex t

	 :html-head ,(blog/website-html-head)
	 :html-preamble blog/website-html-preamble
	 :html-postamble blog/website-html-postamble
	 
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
	 
         :email "krm@vger"
         :with-creator t)
        ("css"
         :base-directory "css/"
         :base-extension "css"
         :publishing-directory "public/css"
         :publishing-function org-publish-attachment
         :recursive t)
        ("all" :components ("posts" "css"))))
