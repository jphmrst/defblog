;;; defblog --- A wrapper for org-publish, for producing blogs from
;;; local Org-mode files.

;;; Commentary:

;;; Code:

(cl-defmacro defblog (name base-directory
			    blog-title blog-desc blog-url
			    &key
			    (src-subdir "src/") (pub-subdir "pub/")
			    (gen-subdir "gen/")
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
  
  (let (;; The stateful structures associated with this blog, to be
	;; updated before each time the blog HTML is built.  Each of
	;; these names is associated with a DEFVAR in the macro
	;; expansion.
	(file-plists-hash (intern (concatenate 'string
				    "+defblog/" name "/file-plists-hash+")))
	(category-tags (intern (concatenate 'string
				 "*defblog/" name "/category-tags*")))
	(category-plists-hash (intern (concatenate 'string
					"+defblog/" name
					"/category-plists-hash+")))

	;; Names of global constants associated with this blog.  Each
	;; of these names is also associated with a DEFVAR in the
	;; macro expansion.
	(basedir (intern (concatenate 'string
			   "+defblog/" name "/basedir+")))
	(src-basedir (intern (concatenate 'string
			       "+defblog/" name "/src-basedir+")))
	(pub-basedir (intern (concatenate 'string
			       "+defblog/" name "/pub-basedir+")))
	(tmp-basedir (intern (concatenate 'string
			       "+defblog/" name "/tmp-basedir+")))
	(posts-basedir (intern (concatenate 'string
				 "+defblog/" name "/posts-basedir+")))
	(cat-indices-basedir (intern (concatenate 'string
				       "+defblog/" name "/cat-indices+")))
	(derived-xml-basedir (intern (concatenate 'string
				       "+defblog/" name "/derived-xml+")))
	(lv1-preamble-plist (intern (concatenate 'string
				      "+defblog/" name
				      "/lv1-preamble-plist+")))
	(lv2-preamble-plist (intern (concatenate 'string
				      "+defblog/" name
				      "/lv2-preamble-plist+")))

	;; Names of functions associated with this blog.  Each of
	;; these names is associated with a DEFUN in the macro
	;; expansion.

	(cat-indices-prep-fn (intern (concatenate 'string
				       "defblog/" name "/cat-indices-prep")))
	(derived-xml-prep-fn (intern (concatenate 'string
				       "defblog/" name "/derived-xml-prep")))
	(posts-prep-fn (intern (concatenate 'string
				 "defblog/" name "/posts-prep")))
	(overall-setup-fn (intern (concatenate 'string
				    "defblog/" name "/overall-setup")))
	(overall-cleanup-fn (intern (concatenate 'string
				      "defblog/" name "/overall-cleanup"))))
    
    `(progn

       ;; DEFVARs corresponding to the stateful components of this
       ;; blog.
       
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

       ;; DEFVARs corresponding to the constants defined for this
       ;; blog.

       (defvar ,basedir ,base-directory
	 ,(concatenate 'string "Base work directory for the " name " blog."))
       
       (defvar ,src-basedir ,(concatenate 'string base-directory src-subdir)
	 ,(concatenate 'string
	    "Directory with the source ORG files of the " name " blog."))
       
       (defvar ,pub-basedir ,(concatenate 'string base-directory pub-subdir)
	 ,(concatenate 'string
	    "Target directory for publishable files of the " name " blog."))
       
       (defvar ,tmp-basedir ,(concatenate 'string base-directory gen-subdir)
	 ,(concatenate 'string
	    "Scratch space directory for the " name " blog."))
       
       (defvar ,posts-basedir
	   ,(concatenate 'string base-directory gen-subdir "posts/")
	 ,(concatenate 'string
	    "Scratch space directory for copying over posts for the " name " blog."))
       
       (defvar ,cat-indices-basedir 
	   ,(concatenate 'string base-directory gen-subdir "cat-indices/")
	 ,(concatenate 'string
	    "Scratch space area for generating category index files for the "
	    name " blog."))
       
       (defvar ,derived-xml-basedir 
	   ,(concatenate 'string base-directory gen-subdir "derived-xml/")
	 ,(concatenate 'string
	    "Scratch space area for generating XML files for the "
	    name " blog."))

       (defvar ,lv1-preamble-plist
	   '(:html-preamble
	     "<link rel=stylesheet type=\"text/css\" href=\"./style.css\"/>"))

       (defvar ,lv2-preamble-plist
	   '(:html-preamble
	     "<link rel=stylesheet type=\"text/css\" href=\"../style.css\"/>"))

       ;; DEFUNs used in the ORG-PUBLISH-PROJECT-ALIST clauses for
       ;; this blog.  Each of these will add additional blog-specific
       ;; parameters to a call to a related function defined after
       ;; this macro expansion.

       (defun ,overall-setup-fn (properties)
	 (defblog/table-setup-fn properties ,tmp-basedir ,src-basedir
				  ,file-plists-hash ,category-plists-hash
				  #'(lambda (x) (setf ,category-tags x))
				  #'(lambda (x) ,category-tags)))
       
       (defun ,cat-indices-prep-fn (properties)
	 (defblog/cat-indices-prep properties))
       
       (defun ,derived-xml-prep-fn (properties)
	 (defblog/derived-xml-prep properties))
       
       (defun ,posts-prep-fn (properties)
	 (defblog/posts-prep-fn properties))

       (defun ,overall-setup-fn (properties)
	 (defblog/table-setup-fn properties))
       
       (defun ,overall-cleanup-fn (properties)
	 (defblog/overall-cleanup-fn properties))
       
       (defun ,overall-cleanup-fn (properties)
	 (defblog/overall-cleanup-fn properties))
       
       ;; Register this blog with org-project.
       (let ((cleaned-alist (alist-remove-string-key
			     ,(concatenate 'string name "-top-page")
			     (alist-remove-string-key
			      ,(concatenate 'string name "-pages")
			      (alist-remove-string-key
			       ,(concatenate 'string name "-cat-indices")
			       (alist-remove-string-key
				,(concatenate 'string name "-derived-xml")
				(alist-remove-string-key
				 ,(concatenate 'string name "-statics")
				 (alist-remove-string-key
				  ,(concatenate 'string name "-posts")
				  (alist-remove-string-key
				   ,(concatenate 'string name)
				   org-publish-project-alist))))))))
	     
	     ;; Convert the top-level front page from the source
	     ;; directory to the pub directory --- this file only, no
	     ;; need to copy it anywhere.
	     (top-page-entry
	      (list* :base-directory ,src-basedir
		     :publishing-directory ,pub-basedir
		     :publishing-function 'org-html-publish-to-html
		     :section-numbers nil
		     :table-of-contents nil
		     :with-toc nil
		     :exclude ".*" :include '("index.org")
		     :recursive nil
		     :html-postamble nil
		     
		     ,lv1-preamble-plist))

	     ;; Other (top-level) non-index pages: right now, convert
	     ;; straight from the source directory to the pub area.
	     ;;
	     ;; But maybe these should really be copied with
	     ;; header/footer org-text into scratch area?
	     (pages-entry
	      (list* :base-directory ,src-basedir
		     :publishing-directory ,pub-basedir
		     :exclude "index.org"
		     :html-postamble
		     "<a href=\"./\">Back to the top</a>."
		     :recursive nil
			     
		     ,lv1-preamble-plist))

	     ;; Category indices: generate ORG files into tmp space,
	     ;; and then convert.
	     (cat-indices-entry
	      ;; TODO Make sure ,cat-indices-prep-fn only sets up
	      ;; files in the tmp space, no caching.
	      (list* :preparation-function ',cat-indices-prep-fn
		     :base-directory ,cat-indices-basedir
		     :publishing-directory ,pub-basedir
		     :html-postamble
		     "<a href=\"../\">Back to the top</a>."
		     :recursive t
		     
		     ,lv2-preamble-plist))

	     ;; XML files: generate XML files into tmp space, and then
	     ;; publishing copies over to pub space.
	     (derived-xml-entry
	      ;; TODO Make sure ,derived-xml-prep-fn only sets up
	      ;; files in the tmp space, no caching.
	      (list :preparation-function ',derived-xml-prep-fn
		    :base-directory ,derived-xml-basedir
		    :base-extension "xml"
		    :publishing-directory ,pub-basedir
		    :recursive t))

	     ;; Static files in the source directory that can be
	     ;; copied over to pub space without translation.
	     (statics-entry
	      (list :base-directory ,src-basedir
		    :base-extension "html\\|css\\|jpg\\|gif\\|png\\|xml"
		    :publishing-directory ,pub-basedir
		    :section-numbers nil
		    :table-of-contents nil
		    :with-toc nil
		    :publishing-function 'org-publish-attachment
		    :recursive t))

	     ;; Individual posts are copied into tmp/posts (its
	     ;; subdirectories created as well), and converted from
	     ;; there.
	     (posts-entry
	      ;; TODO Make sure ,posts-prep-fn only sets up files in
	      ;; the tmp space, no caching.
	      (list* :preparation-function ',posts-prep-fn
		     :base-directory ,posts-basedir
		     :publishing-directory ,pub-basedir
		     :html-preamble "<style type=\"text/css\"> .title { text-align: left; } </style> <link rel=stylesheet type=\"text/css\" href=\"../style.css\"/>"
		     :html-postamble "<a href=\"../\">Back to the top</a>, or <a href=\"./\">more like this</a>."
		     :recursive t
		     :publishing-function 'org-html-publish-to-html
		     :section-numbers nil
		     :table-of-contents nil
		     :with-toc nil
		     
		    ,lv2-preamble-plist))

	     (overall-target
	      '(:preparation-function ',overall-setup-fn
		:completion-function ',overall-cleanup-fn
		:components (;; Do *-top-page first; it has the side
			     ;; effect of updating the properties
			     ;; hashtable.
			     ,(concatenate 'string name "-top-page")
			     ,(concatenate 'string name "-pages")
			     ,(concatenate 'string name "-posts")
			     ,(concatenate 'string name "-cat-indices")
			     ,(concatenate 'string name "-statics")))))
	 
	 (setf org-publish-project-alist
	       (acons ,(concatenate 'string name "-top-page")
		      top-page-entry
		      (acons ,(concatenate 'string name "-pages")
			     pages-entry
			     (acons ,(concatenate 'string name "-cat-indices")
				    cat-indices-entry
				    (acons ,(concatenate 'string name "-derived-xml")
					   derived-xml-entry
					   (acons ,(concatenate 'string name "-statics")
						  statics-entry
						  (acons ,(concatenate 'string name "-posts")
							 posts-entry
							 (acons ,name overall-target
	       							cleaned-alist)))))))))
       (message "Defined blog %s; use org-publish to generate" ',name))))

;;; =================================================================
;;; Preparing the hash tables and reference lists at the start of a
;;; blog build.
(defun defblog/table-setup-fn (blog-plist tmp-basedir src-basedir
				file-plist-hash category-plist-hash
				cat-list-setter cat-list-getter)
  "Reset the global structures associated with a blog.
- BLOG-PLIST is the property list provided from ORG-PUBLISH.
- TMP-BASEDIR is the root directory of the temporary files area
- FILE-PLIST-HASH is the hashtable from paths to ORG files, to the plist of
information extracted from that file.
- CAT-LIST-SETTER and CAT-LIST-GETTER are thunks which set (respectively, get) 
the category list global variable for this blog."
  (defblog/reset-file-plist-hash tmp-basedir file-plist-hash)
  (defblog/reset-categories-list src-basedir cat-list-setter)
  (defblog/reset-categories-plist-hash src-basedir
      (funcall cat-list-getter) category-plist-hash))

;;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
;;; Managing the file-plist hashtable.

(defun defblog/fetch-file-plist (path file-plist-hash)
  (let ((result (gethash (intern path) file-plist-hash)))
    ;; (message "Cached %s --> %s" path result)
    result))

(defun defblog/reset-file-plist-hash (tmp-basedir file-plist-hash)
  "Set up the properties hash"
  (let* ((pages-basedir (concatenate 'string tmp-basedir "/pages"))
	 (posts-basedir (concatenate 'string tmp-basedir "/posts")))
    (clrhash file-plist-hash)
    (dolist (base-dir (list posts-basedir pages-basedir))
      (let ((base-dir-contents (directory-files base-dir)))
	(dolist (base-dir-item base-dir-contents)
	  (let ((base-dir-item-fullpath (concatenate 'string
					  base-dir base-dir-item)))
	    (defblog/process-file-for-hash base-dir-item
		base-dir-item-fullpath file-plist-hash)))))
    ;; (message "Finished property hash reset")
    ))

(defun defblog/process-file-for-hash (bare-name full-path file-plist-hash)
  "Recursive function for populating the org properties hash from a given file."
  ;; (message "Processing %s" full-path)
  (cond
    
    ;; If it's directory, recursively traverse that directory.
    ((and (file-directory-p full-path) (not (string-match "^\\." bare-name)))
     ;; (message "- Recurring for directory contents")
     (let ((dir-contents (directory-files full-path)))
       (dolist (dir-item dir-contents)
	 (let ((dir-item-fullpath (concatenate 'string
				    full-path "/" dir-item)))
	   (defblog/process-file-for-hash dir-item
	       dir-item-fullpath file-plist-hash)))))

    ;; If it's an ORGMODE file, pull and cache its properties.
    ((string-match "\\.org$" bare-name)
     (let ((plist (defblog/build-file-plist bare-name full-path)))
       ;; (message "- Caching %s --> %s" full-path plist)
       (puthash (intern full-path) plist file-plist-hash)))

    ;; When debugging we may want to know about this fall-through.
    ;; (t (message "- No action for %s" full-path))
    ))

(defun defblog/build-file-plist (bare-file path)
  "Extract a list of the properties we need from the file at the given PATH.  BARE-FILE and PATH should refer to the same file; the former excludes all surrounding directories."
  ;; (message "* Start defblog/build-file-plist %s" path)
  (let ((buf (find-file-noselect path)))
    (with-current-buffer buf
      (let ((parsed-buffer
	     (org-element-parse-buffer 'greater-element)))
	;; (message "  parsed-buffer %s" parsed-buffer)
	(let ((keyvals (org-element-map parsed-buffer '(keyword)
			 #'defblog/kwdpair)))
	  ;; (message "  keyvals %s" keyvals)
	  (kill-buffer buf)
	  (let ((result (defblog/format-orgprops-plist bare-file
						      path keyvals)))
	    
	    ;; (message "  result %s" result)
	    result))))))

(defun defblog/format-orgprops-plist (bare-file path keyvals)
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

(defun defblog/kwdpair (kwd)
  (let ((data (cadr kwd)))
    (list (plist-get data :key) (plist-get data :value))))

(defun defblog/reset-categories-list (src-basedir cat-list-setter)
  (let ((category-tag-list nil))
    ;; Look at each file in the source directory.
    (dolist (item (directory-files src-basedir))

      ;; We are skipping any dotfiles
      (unless (string-match "^\\." item)
	(let ((cat-dir-path (concatenate 'string src-basedir item "/")))

	  ;; We are also only looking at directories
	  (when (file-directory-p cat-dir-path)
	  
	    ;; Make sure there is a category.txt file in this
	    ;; directory.
	    (let ((cat-path (concatenate 'string cat-dir-path "category.txt")))
	      (when (file-regular-p cat-path)

	      ;; Add the tag to the result list
	      (push item category-tag-list)))))))
    
    (funcall cat-list-setter category-tag-list)))

(defun defblog/reset-categories-plist-hash (src-basedir category-tag-list
					    category-plist-hash)
  "Given the categories list, rebuild the cateogories plist hashtable."
  
  ;; Clear anything previously in the hashtable.
  (clrhash category-plist-hash)

  ;; For each category tag
  (dolist (cat-tag category-tag-list)
  
    ;; Extract the ORG properties of the category.txt file.
    (let ((buf (find-file-noselect (concatenate 'string
				     src-basedir "category.txt"))))
      (with-current-buffer buf
	(let ((parsed-buffer (org-element-parse-buffer 'greater-element)))
	
	  ;; Form a plist for the category.
	  (let ((plist `(:tag ,cat-tag
			      :title (nth 1 (assoc "TITLE" keyvals))
			      :description (nth 1 (assoc "DESC" keyvals)))))

	    ;; Store the plist in the hash.
	    (puthash (intern cat-tag) plist category-plist-hash)))))))

;;; =================================================================
;;; Writing RSS feeds

(defun defblog/write-rss (properties tmp-basedir
			   category-tags file-plist-hash
			   blog-name blog-desc blog-url)
  "Write RSS files for the overall site and for each post category.
- PROPERTIES are from org-publish.
- CATEGORY-TAGS and FILE-PLIST-HASH are the internal data structures of the
- BLOG-NAME, BLOG-DESC and BLOG-URL are strings describing the blog itself."
  (let* ((pages-basedir (concatenate 'string tmp-basedir "/pages"))
	 (all-buf (find-file-noselect (concatenate 'string
					pages-basedir "rss.xml")))
	 (posts-basedir (concatenate 'string tmp-basedir "/posts")))
    (with-current-buffer all-buf
      (erase-buffer)
      (defblog/write-rss-opening blog-name blog-desc
	(concatenate 'string blog-url "atom.xml")
	blog-url 
				;; TODO --- Calculate this date
				"Sat, 02 May 2020 22:07:21 +0000"))
    
    (dolist (category-tag category-tags)
      (let* ((posts-subdir (concatenate 'string posts-basedir category-tag))
	     (post-fullpaths (file-expand-wildcards (concatenate 'string
						      posts-subdir "/*.org")))
	     (plists (mapcar #'(lambda (p)
				 (defblog/fetch-file-plist p file-plist-hash))
			     post-fullpaths))
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
	  (defblog/write-rss-opening cat-rss-title
				    nil ;; TODO Description here
				    cat-atom-url cat-html-url
				    ;; TODO --- Calculate this date
				    "Sat, 02 May 2020 22:07:21 +0000"))
	
	(dolist (plist plists)
	  ;; TODO Only add things from the last (let's say) five years.
	  (with-current-buffer all-buf
	    (defblog/write-rss-for-plist plist category-tag))
	  (with-current-buffer rss-buf
	    (defblog/write-rss-for-plist plist category-tag)))

	(with-current-buffer rss-buf
	  (defblog/write-rss-closing)
	  (save-buffer 0)
	  (kill-buffer rss-buf))))
    
    (with-current-buffer all-buf
      (defblog/write-rss-closing)
      (save-buffer 0)
      (kill-buffer all-buf))))

(defun defblog/write-rss-opening (title description
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

(defun defblog/write-rss-closing ()
  (insert "  </channel>\n</rss>\n"))

(defun defblog/write-rss-for-plist (plist category-tag)
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

(defun defblog/write-post-indices (properties tmp-basedir file-plist-hash)
  "For DEFBLOG/PAGES-PREP, writes the automatically-generated files in the pages directory.  PROPERTIES is as specified in org-publish."
  ;; (message "Start defblog/write-post-indices")
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
				(defblog/fetch-file-plist full
				    file-plist-hash)))
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
  ;; (message "\nEnd defblog/write-post-indices")
  )

       
(defun defblog/overall-cleanup-fn (plist)
  ;; TODO
)

(defun defblog/posts-prep-fn (properties)
  ;; TODO
  )

;; TODO --- is this used anymore?  But calls in body might be useful.
(defun defblog/pages-prep (properties tmp-basedir category-tags
			    file-plist-hash blog-name blog-desc blog-url)
  "Writes the automatically-generated files in the pages directory.
- PROPERTIES is as specified in org-publish.
- TMP-BASEDIR is the pathname we can use to locate the temporary space.
- CATEGORY-TAGS is the list of directory names holding post categories, which
we use as tags of the categories."
  ;; (message "Start defblog/pages-prep")

  ;; Since this function is called here, make sure that this function
  ;; DEFBLOG/PAGES-PREP is called from the first component in the
  ;; ORG-PUBLISH config.
  (defblog/write-post-indices properties tmp-basedir file-plist-hash)
  (defblog/write-rss properties tmp-basedir category-tags
		      file-plist-hash blog-name blog-desc blog-url)
  ;; (message "\nEnd defblog/pages-prep")
  )

(defun defblog/cat-indices-prep (properties)
  "For the \"-cat-indices\" publish targets, generate category index ORG files.
These files should be written to the cat-indices subdirectory of the
temporary files workspace.
- PROPERTIES is as specified in org-publish."
  ;; TODO
  )

(defun defblog/derived-xml-prep (properties)
  "For the \"-derived-xml\" publish targets, generate XML files.
These files should be written to the derived-xml subdirectory of the
temporary files workspace.
- PROPERTIES is as specified in org-publish."
  ;; TODO
  )

(defun defblog/posts-prep (properties)
  "For the \"-posts\" publish targets, copy post ORG files into the workspace.
- PROPERTIES is as specified in org-publish."
  ;; TODO
  )

;;; =================================================================
;;; Miscellaneous utilities

(defun alist-remove-string-key (key alist)
  "Remove all pairs matching KEY from ALIST, where KEY is a string."
  (cond
    ((null alist) nil)
    (t (let* ((head-pair (car alist))
	      (head-key (car head-pair))
	      (recur-cdr (alist-remove-string-key key (cdr alist))))
	 (cond
	   ((string= key head-key) recur-cdr)
	   (t (cons head-pair recur-cdr)))))))

(defun filter (f xs)
  "The classical filter function: only the elements of XS which satisfy F are retained in the result."
  (cond
    ((null xs) nil)
    ((funcall f (car xs)) (cons (car xs) (filter f (cdr xs))))
    (t (filter f (cdr xs)))))

(provide 'defblog)
;;; defblog ends here
