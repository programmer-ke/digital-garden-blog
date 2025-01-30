(require 'ox-publish)

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
