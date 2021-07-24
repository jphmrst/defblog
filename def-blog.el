;;; def-blog --- A wrapper for org-publish, for producing
;;; blogs from local Org-mode files.

;;; Commentary:

;;; Code:

(defvar def-blog/all-org-defaults-plist
    '(:publishing-function org-html-publish-to-html
      :section-numbers nil
      :table-of-contents nil
      :with-toc nil))

(cl-defmacro def-blog (&key name base-directory
			    (src-subdir "src") (pub-subdir "pub")
			    (gen-subdir "gen")
			    css-style-subpath
			    (frontpage-css-style-subpath css-style-subpath)
			    (page-css-style-subpath css-style-subpath)
			    (post-css-style-subpath css-style-subpath)
			    (category-index-css-style-subpath
			     css-style-subpath))
  "Declare a simple-structured blog to be published with ORG-PUBLISH.

Required parameters:
- NAME, used to name generated storage locations so that they do not conflict
with the names used for other blogs.
- BASE-DIRECTORY, a string giving the absolute pathname of the directory
containing the source directory, scratch work space, and HTML output directory
for this blog.

Optional parameters:
- SRC-SUBDIR, PUB-SUBDIR and GEN-SUBDIR are the local paths from BASE-DIRECTORY
to the respective subdirectories for the blog source, the HTML-output
\(\"published\") area, and the temporary scratch workspace.
- CSS-STYLE-SUBPATH, the local path from the BASE-DIRECTORY to default CSS
stylesheet for the blog.
- FRONTPAGE-CSS-STYLE-SUBPATH, PAGE-CSS-STYLE-SUBPATH, POST-CSS-STYLE-SUBPATH
and CATEGORY-INDEX-CSS-STYLE-SUBPATH are local paths from the BASE-DIRECTORY to
the CSS stylesheets for those groups of pages.  If not given, these arguments
take the value of CSS-STYLE-SUBPATH.  For any of these, a NIL value means there
should be no CSS style sheet."
  (unless (file-directory-p base-directory)
    (error "Expected a directory for :base-directory %s" base-directory))
  (unless (string-match "/$" base-directory)
    (setf base-directory (concatenate 'string base-directory "/")))
  
  (let ((file-plists-hash (intern (concatenate 'string
				    "+def-blog/" name "/file-plists-hash+")))
	(category-tags (intern (concatenate 'string
				 "*def-blog/" name "/category-tags*")))
	(category-plists-hash (intern (concatenate 'string
					"+def-blog/" name
					"/category-plists-hash+")))
	(basedir (intern (concatenate 'string
			   "+def-blog/" name "/basedir+")))
	(src-basedir (intern (concatenate 'string
			       "+def-blog/" name "/suc-basedir+")))
	(pub-basedir (intern (concatenate 'string
			       "+def-blog/" name "/pub-basedir+")))
	(tmp-basedir (intern (concatenate 'string
			       "+def-blog/" name "/tmp-basedir+")))
	(pages-basedir (intern (concatenate 'string
				 "+def-blog/" name "/pages-basedir+")))
	(posts-basedir (intern (concatenate 'string
				 "+def-blog/" name "/posts-basedir+")))
	(lv1-preamble-plist (intern (concatenate 'string
				      "+def-blog/" name
				      "/lv1-preamble-plist+")))
	(lv2-preamble-plist (intern (concatenate 'string
				      "+def-blog/" name
				      "/lv2-preamble-plist+")))
	(page-defaults-plist (intern (concatenate 'string
				       "+def-blog/" name
				       "/page-defaults-plist+")))

	(pages-prep-fn (intern (concatenate 'string
			       "def-blog/" name "/pages-prep")))
	(reset-hash-fn (intern (concatenate 'string
				 "def-blog/" name "/reset-hash"))))
    
    `(progn

       (defvar ,file-plists-hash (make-hash-table :test 'eq)
	 ,(concatenate 'string
	    "Hashtable for holding properties of the posts and pages of the "
	    name " blog."))
       
       (defvar ,category-tags nil
	 ,(concatenate 'string
	    "Storage for the list of categories in the " name " blog."))

       (defvar ,category-plists-hash (make-hash-table :test 'eq)
	 ,(concatenate 'string
	    "Hashtable for holding properties of the categories of the "
	    name " blog."))

       (defvar ,basedir ,base-directory
	 ,(concatenate 'string "Base work directory for the " name " blog."))
       (defvar ,src-basedir ,(concatenate 'string base-directory src-subdir)
	 ,(concatenate 'string
	    "Hashtable for holding properties of the categories of the "
	    name " blog."))
       (defvar ,pub-basedir ,(concatenate 'string base-directory pub-subdir)
	 ,(concatenate 'string
	    "Hashtable for holding properties of the categories of the "
	    name " blog."))
       (defvar ,tmp-basedir ,(concatenate 'string base-directory gen-subdir)
	 ,(concatenate 'string
	    "Hashtable for holding properties of the categories of the "
	    name " blog."))

       (defvar ,lv1-preamble-plist
	   '(:html-preamble
	     "<link rel=stylesheet type=\"text/css\" href=\"./style.css\"/>"))

       (defvar ,lv2-preamble-plist
	   '(:html-preamble
	     "<link rel=stylesheet type=\"text/css\" href=\"../style.css\"/>"))

       (defvar ,page-defaults-plist
	   `(:base-directory ,+def-blog/pages-basedir+
			     :publishing-directory ,+def-blog/publish-dir+
			     ,@def-blog/all-org-defaults-plist))

       (defun ,pages-prep-fn (properties)
	 (def-blog/pages-prep properties ,tmp-basedir ,category-tags))
       (defun ,reset-hash-fn (properties)
	 (def-blog/reset-hash ,tmp-basedir))
       
       ;; Register this blog with org-project.
       (setf org-publish-project-alist
	     `(;; Convert the top-level front page from the source
	       ;; directory to the pub directory --- this file only,
	       ;; no need to copy it anywhere.
	       (,,(concatenate 'string name "-top-page")
		  :exclude ".*" :include ("index.org")
		  :html-postamble nil
		  :recursive nil
		  :preparation-function ,reset-hash-fn
		  ,@,page-defaults-plist
		  ,@,lv1-preamble-plist)
	
	       ;; Other (top-level) non-index pages: copy them from
	       ;; the source area to a tmp/pages area, then convert
	       ;; into the pub area.
	       (,,(concatenate 'string name "-pages")
		  :exclude "index.org"
		  ;; TODO This preparation-function needs reworking.
		  ;; It must only copy the pages into the tmp/pages
		  ;; space. Or maybe, no preparation here (mostly this
		  ;; needs to move to the indices and copy-verbatim
		  ;; tasks), and use non-recursive exploration of just
		  ;; the top-level org files?
		  :preparation-function ,pages-prep-fn
		  :html-postamble "<a href=\"./\">Back to the top</a>."
		  :recursive t
		  ,@,page-defaults-plist
		  ,@,lv1-preamble-plist)

	       ;; Category indices generated into the pages subdir. ;;
	       ;; TODO Generate these into a new tmp/category-indices
	       ;; area, and convert from there.
	       (,,(concatenate 'string name "-cat-indices")
		  :exclude "\\.*"
		  :include ("compute/index.org"
			    "cook/index.org"
			    "cool/index.org"
			    "meta/index.org"
			    "politic/index.org")
		  :html-postamble
		  "<a href=\"../\">Back to the top</a>."
		  :recursive t
		  ,@,page-defaults-plist
		  ,@,lv2-preamble-plist)
	
	       ;; Static files in the pages directory without translation.
	       ;; TODO Should be the source directory
	       (,,(concatenate 'string name "-statics")
		  :base-directory ,+def-blog/pages-basedir+ ;; Nope,
							    ;; the
							    ;; whole
							    ;; SRC
							    ;; dir..
		  :base-extension "html\\|css\\|jpg\\|gif\\|png\\|xml"
		  :publishing-directory ,,pub-basedir
		  :section-numbers nil
		  :table-of-contents nil
		  :with-toc nil
		  :publishing-function org-publish-attachment
		  :recursive t)

	       ;; TODO There will also be generated static files.  We
	       ;; need to generate them from the preparation-functiom,
	       ;; and then convert them over.
	       
	       ;; TODO Individual posts should be copied into
	       ;; tmp/posts (its subdirectories created as well), and
	       ;; converted from there.
	       (,,(concatenate 'string name "-posts")
		  :base-directory ,+def-blog/posts-basedir+
		  :publishing-directory ,pub-basedir
		  :html-preamble "<style type=\"text/css\"> .title { text-align: left; } </style> <link rel=stylesheet type=\"text/css\" href=\"../style.css\"/>"
		  :html-postamble "<a href=\"../\">Back to the top</a>, or <a href=\"./\">more like this</a>."
		  :recursive t
		  ,@def-blog/all-org-defaults-plist)
	       
	       ;; Everything for the overall blog target
	       (,,name
		:components (;; Do *-top-page first; it has the side
			     ;; effect of updating the properties
			     ;; hashtable.
			     ,,(concatenate 'string name "-top-page")
			       ,,(concatenate 'string name "-pages")
			       ,,(concatenate 'string name "-posts")
			       ,,(concatenate 'string name "-cat-indices")
			       ,,(concatenate 'string name "-statics")))
	       
	       ;; Everything else in org-publish-project-alist
	       ,,@(assq-delete-all
		   (concatenate 'string name "-top-page")
		   (assq-delete-all
		    (concatenate 'string name "-pages")
		    (assq-delete-all
		     (concatenate 'string name "-cat-indices")
		     (assq-delete-all
		      (concatenate 'string name "-statics")
		      (assq-delete-all
		       (concatenate 'string name "-posts")
		       (assq-delete-all
			(concatenate 'string name)
			org-publish-project-alist)))))))))))


(defun def-blog/pages-prep (properties tmp-basedir category-tags)
  "Writes the automatically-generated files in the pages directory.
- PROPERTIES is as specified in org-publish.
- TMP-BASEDIR is the pathname we can use to locate the temporary space.
- CATEGORY-TAGS is the list of directory names holding post categories, which
we use as tags of the categories."
  ;; (message "Start def-blog/pages-prep")

  ;; Since this function is called here, make sure that this function
  ;; DEF-BLOG/PAGES-PREP is called from the first component in the
  ;; ORG-PUBLISH config.
  (def-blog/write-post-indices properties tmp-basedir)
  (def-blog/write-rss properties tmp-basedir category-tags)
  ;; (message "\nEnd def-blog/pages-prep")
  )

;;; =================================================================
;;; Writing RSS feeds

(defun def-blog/write-rss (properties tmp-basedir category-tags)
  "Write RSS files for the overall site and for each post category.  PROPERTIES are from org-publish."
  (let* ((pages-basedir (concatenate 'string tmp-basedir "/pages"))
	 (all-buf (find-file-noselect (concatenate 'string
					pages-basedir "rss.xml")))
	 (posts-basedir (concatenate 'string tmp-basedir "/posts")))
    (with-current-buffer all-buf
      (erase-buffer)
      (def-blog/write-rss-opening "JM&#8217;s website"
				"Occasional notes and observations"
				"http://maraist.org/atom.xml"
				"http://maraist.org/"
				;; TODO --- Calculate this date
				"Sat, 02 May 2020 22:07:21 +0000"))
    
    (dolist (category-tag category-tags)
      (let* ((posts-subdir (concatenate 'string posts-basedir category-tag))
	     (post-fullpaths (file-expand-wildcards (concatenate 'string
						      posts-subdir "/*.org")))
	     (plists (mapcar #'def-blog/fetch-file-plist post-fullpaths))
	     (rss-buf (find-file-noselect (concatenate 'string
					    pages-basedir category-tag
					    "/rss.xml")))

	     ;; TODO --- fetch the proper category name
	     (cat-rss-title (concatenate 'string
			      "JM&#8217;s website: " category-tag))
	     
	     (cat-html-url (concatenate 'string
			     "\"http://maraist.org/" category-tag "/"))
	     (cat-atom-url (concatenate 'string cat-html-url "atom.xml")))
	(with-current-buffer rss-buf
	  (erase-buffer)
	  (def-blog/write-rss-opening cat-rss-title
				    nil ;; TODO Description here
				    cat-atom-url cat-html-url
				    ;; TODO --- Calculate this date
				    "Sat, 02 May 2020 22:07:21 +0000"))
	
	(dolist (plist plists)
	  ;; TODO Only add things from the last (let's say) five years.
	  (with-current-buffer all-buf
	    (def-blog/write-rss-for-plist plist category-tag))
	  (with-current-buffer rss-buf
	    (def-blog/write-rss-for-plist plist category-tag)))

	(with-current-buffer rss-buf
	  (def-blog/write-rss-closing)
	  (save-buffer 0)
	  (kill-buffer rss-buf))))
    
    (with-current-buffer all-buf
      (def-blog/write-rss-closing)
      (save-buffer 0)
      (kill-buffer all-buf))))

(defun def-blog/write-rss-opening (title description
				 atom-link html-link lastBuiltDate)
  (insert "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n")
  (insert "<rss version=\"2.0\"\n")
  (insert "     xmlns:content=\"http://purl.org/rss/1.0/modules/content/\"\n")
  (insert "     xmlns:wfw=\"http://wellformedweb.org/CommentAPI/\"\n")
  (insert "     xmlns:dc=\"http://purl.org/dc/elements/1.1/\"\n")
  (insert "     xmlns:atom=\"http://www.w3.org/2005/Atom\"\n")
  (insert "     xmlns:sy=\"http://purl.org/rss/1.0/modules/syndication/\"\n")
  (insert "     xmlns:slash=\"http://purl.org/rss/1.0/modules/slash/\">\n")
  (insert "  <channel>\n")
  (insert "    <title>" title "</title>\n")
  (insert "    <atom:link href=" atom-link "\"\n")
  (insert "               rel=\"self\"\n")
  (insert "               type=\"application/rss+xml\" />\n")
  (insert "    <link>" html-link "</link>\n")
  (when description
    (insert "    <description>" description "</description>\n"))
  (insert "    <lastBuildDate>" lastBuiltDate "</lastBuildDate>\n")
  (insert "    <language>en-US</language>\n")
  (insert "    <sy:updatePeriod>hourly</sy:updatePeriod>\n")
  (insert "    <sy:updateFrequency>1</sy:updateFrequency>\n"))

(defun def-blog/write-rss-closing ()
  (insert "  </channel>\n</rss>\n"))

(defun def-blog/write-rss-for-plist (plist category-tag)
  (let ((title (plist-get plist :title))
	(bare  (plist-get plist :bare))
	(date  (plist-get plist :date))
	(desc  (plist-get plist :desc)))
    (insert "\n    <item>\n")
    (insert "      <title>" (cond (title title) (t "(untitled)")) "</title>\n")
    (insert "      <link>https://maraist.org/"
	    category-tag "/"
	    (replace-regexp-in-string "\\.org$" ".html" bare)
	    "</link>\n")
    (insert "      <dc:creator><![CDATA[jm]]></dc:creator>\n")
    (insert "      <pubDate>"
	    (cond
	      (date (format-time-string "%a, %d %B %Y %H:%M:%S" date))
	      (t "Fri, 08 Jan 2005 12:00:00"))
	    " +0000</pubDate>\n")
    (insert "      <category><![CDATA["
	    category-tag ;; TODO Look up and print the proper name.
	    "]]></category>\n")
    ;; (insert "      <guid isPermaLink=\"false\">http://maraist.org/?p=425</guid>\n")
    (when desc
      (insert "      <description><![CDATA[" desc "]]></description>\n"))
    (insert "      <slash:comments>0</slash:comments>\n")
    (insert "    </item>\n")))

;;; =================================================================
;;; Writing index pages

(defun def-blog/write-post-indices (properties tmp-basedir)
  "For DEF-BLOG/PAGES-PREP, writes the automatically-generated files in the pages directory.  PROPERTIES is as specified in org-publish."
  ;; (message "Start def-blog/write-post-indices")
  (let* ((pages-basedir (concatenate 'string tmp-basedir "/pages"))
	 (posts-basedir (concatenate 'string tmp-basedir "/posts"))
	 (post-dir-files (directory-files posts-basedir))
	 (post-subdirs (filter #'(lambda (x)
				   (let ((xx (concatenate 'string
					       posts-basedir "/" x)))
				     (and (file-directory-p xx)
					  (not (string-match "^\\." x)))))
			       post-dir-files)))
    ;; (message "post-dir-files: %s" post-dir-files)
    ;; (message "post-subdirs: %s" post-subdirs)
    (dolist (post-subdir post-subdirs)
      ;; (message "\npost-subdir: %s" post-subdir)
      (let ((dir (concatenate 'string posts-basedir post-subdir)))
	;; (message "- dir: %s" dir)
	(when (file-directory-p dir)
	  (let* ((out-dir (concatenate 'string pages-basedir post-subdir))
		 ;; (m1 (message "- out-dir: %s" out-dir))
		 (all-files (directory-files dir))
		 (org-files
		  (filter #'(lambda (x)
			      (and (string-match "\\.org$" x)
				   (not (string-match "/index.org$" x))))
			  all-files))
		 ;; (m2 (message "- org-files: %s" org-files))
		 (prop-lists
		  (mapcar #'(lambda (x)
			      (let ((full (concatenate 'string
					    dir "/" x)))
				(def-blog/fetch-file-plist full)))
			  org-files)))
	    ;; (message "- prop-lists: %s" prop-lists)
	    (make-directory out-dir t)
	    (let ((index-buffer (find-file-noselect (concat out-dir
							    "/index.org"))))
	      (with-current-buffer index-buffer
		(erase-buffer)
		(let* ((title-file (concatenate 'string dir "/title.txt"))
		       (title-buffer (find-file-noselect title-file))
		       (title (with-current-buffer title-buffer
				(buffer-string))))
		  ;; (message "Inserting: %s" title-file)
		  (insert "#+TITLE: [JM's website] ")
		  (insert title)
		  (kill-buffer title-buffer))
		(insert "#+html_head:  <link rel=stylesheet type=\"text/css\" href=\"../style.css\"/>\n\n")
		(dolist (prop-list
			 (sort prop-lists
			       #'(lambda (y x)
				   (time-less-p (plist-get x :date)
						(plist-get y :date)))))
		  (message "Destructuring %s" prop-list)
		  (let ((bare  (plist-get prop-list :bare))
			(path  (plist-get prop-list :path))
			(title (plist-get prop-list :title))
			(desc  (plist-get prop-list :desc))
			(date  (plist-get prop-list :date))
			(updated  (plist-get prop-list :updated)))
		    (message "- bare %s title \"%s\" desc \"%s\"" bare title desc)
		    (when bare
		      (insert "- @@html:<a href=\""
			      (replace-regexp-in-string "\\.org$" ".html" bare)
			      "\">"))
		    (insert (cond (title title) (t "(untitled)")))
		    (when bare (insert "</a>@@."))
		    (when desc (insert " " desc))
		    (when date
		      (insert (format-time-string " /%B %d, %Y/" date)))
		    (when updated
		      (cond
			(date (insert ", /updated "))
			(t (insert "/Last updated ")))
		      (insert (format-time-string "%B %d, %Y/" updated)))
		    (when (or date updated) (insert "."))
		    (insert "\n")))
		(save-buffer 0))
	      (kill-buffer index-buffer)))))))
  ;; (message "\nEnd def-blog/write-post-indices")
  )

(defun filter (f xs)
  "The classical filter function: only the elements of XS which satisfy F are retained in the result."
  (cond
    ((null xs) nil)
    ((funcall f (car xs)) (cons (car xs) (filter f (cdr xs))))
    (t (filter f (cdr xs)))))

;;; =================================================================
;;; Cached properties.

;; TODO These three all need to be defined per-blog inside the macro.

;;; (defun def-blog/cache-file-properties (tmp-basedir)
;;;   "Set up the various cached hashtables."
;;;   (def-blog/reset-hash tmp-basedir))
;;; 
;;; (defvar +def-blog/file-properties+
;;;     (make-hash-table :test 'eq)
;;;   "Hashtable for holding properties of the posts and pages of the maraist.org files.")
;;; 
;;; (defun def-blog/fetch-file-plist (path)
;;;   (let ((result (gethash (intern path) +def-blog/file-properties+)))
;;;     ;; (message "Cached %s --> %s" path result)
;;;     result))

;;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

(defun def-blog/reset-hash (tmp-basedir file-properties)
  "Set up the properties hash"
  (let* ((pages-basedir (concatenate 'string tmp-basedir "/pages"))
	 (posts-basedir (concatenate 'string tmp-basedir "/posts")))
    (clrhash file-properties)
    (dolist (base-dir (list posts-basedir pages-basedir))
      (let ((base-dir-contents (directory-files base-dir)))
	(dolist (base-dir-item base-dir-contents)
	  (let ((base-dir-item-fullpath (concatenate 'string
					  base-dir base-dir-item)))
	    (def-blog/process-file-for-hash base-dir-item
		base-dir-item-fullpath)))))
    ;; (message "Finished property hash reset")
    ))

(defun def-blog/process-file-for-hash (bare-name full-path)
  "Recursive function for populating the org properties hash from a given 
file."
  ;; (message "Processing %s" full-path)
  (cond
    
    ;; If it's directory, recursively traverse that directory.
    ((and (file-directory-p full-path) (not (string-match "^\\." bare-name)))
     ;; (message "- Recurring for directory contents")
     (let ((dir-contents (directory-files full-path)))
       (dolist (dir-item dir-contents)
	 (let ((dir-item-fullpath (concatenate 'string
				    full-path "/" dir-item)))
	   (def-blog/process-file-for-hash dir-item
					 dir-item-fullpath)))))

    ;; If it's an ORGMODE file, pull and cache its properties.
    ((string-match "\\.org$" bare-name)
     (let ((plist (def-blog/build-file-plist bare-name full-path)))
       ;; (message "- Caching %s --> %s" full-path plist)
       (puthash (intern full-path) plist +def-blog/file-properties+)))

    ;; When debugging we may want to know about this fall-through.
    ;; (t (message "- No action for %s" full-path))
    ))

(defun def-blog/build-file-plist (bare-file path)
  "Extract a list of the properties we need from the file at the given PATH.  BARE-FILE and PATH should refer to the same file; the former excludes all surrounding directories."
  ;; (message "* Start def-blog/build-file-plist %s" path)
  (let ((buf (find-file-noselect path)))
    (with-current-buffer buf
      (let ((parsed-buffer
	     (org-element-parse-buffer 'greater-element)))
	;; (message "  parsed-buffer %s" parsed-buffer)
	(let ((keyvals (org-element-map parsed-buffer '(keyword)
			 #'def-blog/kwdpair)))
	  ;; (message "  keyvals %s" keyvals)
	  (kill-buffer buf)
	  (let ((result (def-blog/format-orgprops-plist bare-file
						      path keyvals)))
	    
	    ;; (message "  result %s" result)
	    result))))))

(defun def-blog/format-orgprops-plist (bare-file path keyvals)
  "Given a key-values list, set up a plist for a file path."
  (let ((bare-date (assoc "DATE" keyvals))
	(bare-updated (assoc "UPDATED" keyvals)))
    (list :bare bare-file :path path
	  :title (nth 1 (assoc "TITLE" keyvals))
	  :desc (nth 1 (assoc "DESCRIPTION" keyvals))
	  :date (cond
		  (bare-date (date-to-time (nth 1 bare-date)))
		  (t nil))
	  :updated (cond
		     (bare-updated (date-to-time (nth 1 bare-updated)))
		     (t nil)))))

(defun def-blog/kwdpair (kwd)
  (let ((data (cadr kwd)))
    (list (plist-get data :key) (plist-get data :value))))

(provide 'def-blog)
;;; def-blog ends here
