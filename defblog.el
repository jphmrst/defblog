;;; defblog --- A wrapper for org-publish, for producing blogs from
;;; local Org-mode files.

;;; Commentary:

;;; Code:
;;
;; TODO XML sitemap entry for root page.
;;
;; TODO Global blog defaults for XML sitemap change-freq, priority:
;; parameters/defaults, and into calls to
;; defblog/write-xml-sitemap-entry from defblog/write-xml-sitemap.
;;
;; TODO Not base-directory, but source-directory.  Then optional pub-
;; and gen-areas, but no *-subdir arguments.  If not defined, then
;; these should create a /tmp space.
;;
;; TODO Centralize creating/emptying the published and temporary
;; spaces
;;
;; TODO Add a way to create the ORG for the front page.
;;
;; TODO Add a way to create the ORG for an arbitrary page.
;;
;; TODO Add a sunset duration for posts in RSS/Atom feeds.
;;
;; TODO Add a sunset duration for posts on front page.
;;
;; TODO Generalize the COPY-FILE in DEFBLOG/POSTS-PREP.
;;
;; TODO Generalize how the title is formed for category index pages in
;; DEFBLOG/CAT-INDICES-PREP.
;;
;; TODO Ignore index.org source files in the category directories.
;;
;; TODO Add blog-default page sitemap change frequency, priority.

(cl-defmacro defblog (name base-directory blog-title 
			   &key
			   blog-url blog-desc
			   (src-subdir "src/") ;; To go away
			   (pub-subdir "pub/") ;; To go away
			   (gen-subdir "gen/") ;; To go away
			   published-directory generated-directory
			   ;;
			   css-style-rel-path
			   frontpage-css-style-rel-path
			   page-css-style-rel-path
			   post-css-style-rel-path
			   category-index-css-style-rel-path
			   ;;
			   frontpage-section-numbers
			   page-section-numbers
			   post-section-numbers
			   category-index-section-numbers
			   ;;
			   frontpage-table-of-contents
			   page-table-of-contents
			   post-table-of-contents
			   category-index-table-of-contents
			   ;;
			   upload
			   ;;
			   rsync-dest rsync-shell
			   (rsync-options '("-rLptgoD"))
			   (rsync-delete-excluded t)
			   ;;
			   (generate-xml-sitemap t)
			   (generate-rss t)
			   (generate-atom t)
			   default-author-name
			   ;;
			   feed-entry-sunset)

  "Declare a simple structured blog to be published with ORG-PUBLISH.

Information about file layout, Org property use, and other
details is in the README.md file at https://github.com/jphmrst/defblog .

Required parameters to this macro:
- NAME, a string used to identify this blog.  This NAME is used by 
ORG-PUBLISH to publish this particular blog, and it is also used to
name generated storage locations so that they do not conflict with
the names used for other blogs.
- BASE-DIRECTORY, a string giving the absolute pathname of the 
directory containing the source directory, scratch work space,
and HTML output directory for this blog.
- BLOG-TITLE, a string with the human-oriented name for this web 
site.

Optional parameters:
- BLOG-URL (respectively BLOG-DESC) gives the URL for the top of 
this blog (human-facing description of the web site).  The URL
is required when generating most of the XML artifacts.
- SRC-SUBDIR, PUB-SUBDIR and GEN-SUBDIR are the local paths from
BASE-DIRECTORY to the respective subdirectories for the blog
source, the HTML-output (\"published\") area, and the temporary
scratch workspace.
- CSS-STYLE-SUBPATH, the local path from the BASE-DIRECTORY to 
default CSS stylesheet for the blog.
- FRONTPAGE-CSS-STYLE-REL-PATH, PAGE-CSS-STYLE-REL-PATH,
POST-CSS-STYLE-REL-PATH and CATEGORY-INDEX-CSS-STYLE-REL-PATH are
local paths from the BASE-DIRECTORY to the CSS stylesheets for
those groups of pages.  If not given, these arguments take the
value of CSS-STYLE-REL-PATH.  For any of these, a NIL value means
there should be no CSS style sheet.
- GENERATE-XML-SITEMAP, GENERATE-RSS and GENERATE-ATOM indicate
whether the published blog should include these XML artifacts.
The RSS and Atom feeds are validated via
https://validator.w3.org/feed/ .  The XML sitemap is validated via
https://www.xml-sitemaps.com/validate-xml-sitemap.html .
- FEED-ENTRY-SUNSET gives the length of time that a post should be 
included in any XML feed (RSS or Atom).  The value may be 
 1. An Emacs Lisp time value (used as-is: the age of a post 
    calculated via TIME-SUBTRACT, and compared to this upper
    bound).
 2. A Lisp list (passed as arguments to MAKE-DECODED-TIME,
    whose result is used as a sunset bound as above).
 3. A string (passed to PARSE-TIME-STRING, and used as an
    absolute limit of the earliest date included."
  
  ;; Check fatal combinations of present/missing arguments.
  (unless (file-directory-p base-directory)
    (error "Expected a directory for :base-directory %s" base-directory))
  (when (or generate-rss generate-atom)
    (unless blog-url
      (error "Generating RSS/Atom feed requires BLOG-URL"))) 
  (unless (or (null upload) (eq upload :rsync))
    (error "Unrecognized value for upload: %s" upload))
 
  ;; Refinements to the given arguments.
  (unless (string-match "/$" base-directory)
    (setf base-directory (concatenate 'string base-directory "/")))

  ;; CSS-STYLE-REL-PATH as the default for various page groups.
  (unless frontpage-css-style-rel-path
    (setf frontpage-css-style-rel-path css-style-rel-path))
  (unless page-css-style-rel-path
    (setf page-css-style-rel-path css-style-rel-path))
  (unless post-css-style-rel-path
    (setf post-css-style-rel-path (concatenate 'string
				    "../" css-style-rel-path)))
  (unless category-index-css-style-rel-path
    (setf category-index-css-style-rel-path (concatenate 'string
					      "../" css-style-rel-path)))
  
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
	(use-system-tmpspace-var (intern (concatenate 'string
					   "+defblog/" name
					   "/use-system-tmpspace+")))
	(system-tmpspace-var (intern (concatenate 'string
				       "+defblog/" name "/system-tmpspace+")))
	(source-directory-var (intern (concatenate 'string
			       "+defblog/" name "/src-basedir+")))
	(publish-directory-var (intern (concatenate 'string
			       "+defblog/" name "/pub-basedir+")))
	(gen-directory-var (intern (concatenate 'string
			       "+defblog/" name "/tmp-basedir+")))
	(get-posts-directory-var (intern (concatenate 'string
				 "+defblog/" name "/posts-basedir+")))
	(gen-cat-indices-directory-var (intern (concatenate 'string
				       "+defblog/" name "/cat-indices+")))
	(gen-statics-directory (intern (concatenate 'string
				       "+defblog/" name "/gen-statics+")))
	(last-blog-update (intern (concatenate 'string
				      "+defblog/" name "/last-blog-update+")))

	;; Names of functions associated with this blog.  Each of
	;; these names is associated with a DEFUN in the macro
	;; expansion.

	(cat-indices-prep-fn (intern (concatenate 'string
				       "defblog/" name "/cat-indices-prep")))
	(gen-statics-prep-fn (intern (concatenate 'string
				       "defblog/" name "/gen-statics-prep")))
	(posts-prep-fn (intern (concatenate 'string
				 "defblog/" name "/posts-prep")))
	(overall-setup-fn (intern (concatenate 'string
				    "defblog/" name "/overall-setup")))
	(overall-cleanup-fn (intern (concatenate 'string
				      "defblog/" name "/overall-cleanup")))
	(state-dump-fn (intern (concatenate 'string
				 "defblog/" name "/state-dump")))

	;; Sunset time for feed entries.
	(feed-entry-sunset-pred
	 (cond
	   ((null feed-entry-sunset) #'(lambda (x) t))
	   ((or (integerp feed-entry-sunset)
		(and (consp feed-entry-sunset)
		     (integerp (car feed-entry-sunset))
		     (integerp (cdr feed-entry-sunset)))
		(and (listp feed-entry-sunset)
		     (eql 4 (length feed-entry-sunset))
		     (integerp (car feed-entry-sunset))
		     (integerp (cadr feed-entry-sunset))
		     (integerp (caddr feed-entry-sunset))
		     (integerp (cadddr feed-entry-sunset))))
	    #'(lambda (x) (time-less-p (time-subtract nil x)
				       feed-entry-sunset)))
	   ((listp feed-entry-sunset)
	    (let ((bound (apply #'make-decoded-time feed-entry-sunset)))
	      #'(lambda (x) (time-less-p (time-subtract nil x) bound))))
	   ((stringp feed-entry-sunset)
	    (let ((min (parse-time-string feed-entry-sunset)))
	      #'(lambda (x) (time-less-p min x))))
	   (t (error "Unrecognized value for FEED-ENTRY-SUNSET: %s"
		     feed-entry-sunset)))))
    
    `(progn

       ;; DEFVARs corresponding to the stateful components of this
       ;; blog.

       (when (boundp ',file-plists-hash) (makunbound ',file-plists-hash))
       (defvar ,file-plists-hash (make-hash-table :test 'eq)
	 ,(concatenate 'string
	    "Hashtable for holding properties of the posts and pages of the "
	    name " blog."))
       
       (when (boundp ',category-tags) (makunbound ',category-tags))
       (defvar ,category-tags nil
	 ,(concatenate 'string
	    "Storage for the list of categories in the " name " blog."))

       (when (boundp ',category-plists-hash)
	 (makunbound ',category-plists-hash))
       (defvar ,category-plists-hash (make-hash-table :test 'eq)
	 ,(concatenate 'string
	    "Hashtable for holding properties of the categories of the "
	    name " blog."))

       ;; DEFVARs corresponding to the constants defined for this
       ;; blog.

       (when (boundp ',basedir) (makunbound ',basedir))
       (defvar ,basedir ,base-directory
	 ,(concatenate 'string "Base work directory for the " name " blog."))

       (when (boundp ',use-system-tmpspace-var)
	 (makunbound ',use-system-tmpspace-var))
       (defvar ,use-system-tmpspace-var ,(or (null published-directory)
					     (null generated-directory))
	 ,(concatenate 'string
	    "Flag specifying whether we need a system tmp directory for the "
	    name " blog."))

       (when (boundp ',system-tmpspace-var)
	 (makunbound ',system-tmpspace-var))
       (defvar ,system-tmpspace-var nil
	 ,(concatenate 'string
	    "Set to the in-use system tmp directory for the " name " blog."))

       (when (boundp ',source-directory-var) (makunbound ',source-directory-var))
       (defvar ,source-directory-var
	   ,(concatenate 'string base-directory src-subdir)
	 ,(concatenate 'string
	    "Directory with the source ORG files of the " name " blog."))
       
       (when (boundp ',publish-directory-var)
	 (makunbound ',publish-directory-var))
       (defvar ,publish-directory-var
	   ,(concatenate 'string base-directory pub-subdir)
	 ,(concatenate 'string
	    "Target directory for publishable files of the " name " blog."))
       
       (when (boundp ',gen-directory-var) (makunbound ',gen-directory-var))
       (defvar ,gen-directory-var
	   ,(concatenate 'string base-directory gen-subdir)
	 ,(concatenate 'string
	    "Scratch space directory for the " name " blog."))
       
       (when (boundp ',get-posts-directory-var)
	 (makunbound ',get-posts-directory-var))
       (defvar ,get-posts-directory-var
	   ,(concatenate 'string base-directory gen-subdir "posts/")
	 ,(concatenate 'string
	    "Scratch space directory for copying over posts for the " name " blog."))
       
       (when (boundp ',gen-cat-indices-directory-var) (makunbound ',gen-cat-indices-directory-var))
       (defvar ,gen-cat-indices-directory-var 
	   ,(concatenate 'string base-directory gen-subdir "cat-indices/")
	 ,(concatenate 'string
	    "Scratch space area for generating category index files for the "
	    name " blog."))
       
       (when (boundp ',gen-statics-directory) (makunbound ',gen-statics-directory))
       (defvar ,gen-statics-directory 
	   ,(concatenate 'string base-directory gen-subdir "gen-statics/")
	 ,(concatenate 'string
	    "Scratch space area for generating XML files for the "
	    name " blog."))

       (when (boundp ',last-blog-update) (makunbound ',last-blog-update))
       (defvar ,last-blog-update nil
	 "Last post or update to this blog")

       ;; DEFUNs used in the ORG-PUBLISH-PROJECT-ALIST clauses for
       ;; this blog.  Each of these will add additional blog-specific
       ;; parameters to a call to a related function defined after
       ;; this macro expansion.

       (defun ,overall-setup-fn (properties)
	 (message "Setting up defblog temp structures")

	 (when ,use-system-tmpspace-var
	   (setf ,system-tmpspace-var (make-temp-file "defblog" t))
	   )
	 
	 (defblog/table-setup-fn properties ,gen-directory-var ,source-directory-var
				  ,file-plists-hash ,category-plists-hash
				  #'(lambda (x) (setf ,category-tags x))
				  #'(lambda () ,category-tags)
				  #'(lambda (x) (setf ,last-blog-update x)))
	 (,state-dump-fn)
	 )

       (defun ,overall-cleanup-fn (properties)
	 (cond
	   ((eq ,upload :rsync)
	    (message "Uploading...")
	    (call-process "rsync" nil "*org-publish-rsync*" nil
			  ,@rsync-options
			  ,@(when rsync-shell
			      (list (concatenate 'string
				      "--rsh=" rsync-shell)))
			  ,@(when rsync-delete-excluded `("--delete-excluded"))
			  ,(concatenate 'string base-directory pub-subdir)
			  ,rsync-dest)))
	 (message "Cleaning up defblog temp structures")
	 (clrhash ,file-plists-hash)
	 (clrhash ,category-plists-hash)
	 (setf ,category-tags nil)
	 (when ,use-system-tmpspace-var
	   (delete-directory ,system-tmpspace-var t)))

       (defun ,state-dump-fn ()
	 (defblog/state-dump ,file-plists-hash
	     ,category-tags ,category-plists-hash))

       (defun ,cat-indices-prep-fn (properties)
	 (defblog/cat-indices-prep #'(lambda () ,category-tags)
	     ,category-plists-hash ,file-plists-hash
	     ,gen-directory-var ,source-directory-var
	     ,category-index-css-style-rel-path))

       (defun ,gen-statics-prep-fn (properties)
	 (defblog/gen-statics-prep properties ,source-directory-var
	   ,gen-directory-var 
	   ,file-plists-hash ,category-plists-hash
	   ,category-tags ,blog-title ,blog-desc ,blog-url ,last-blog-update
	   ,generate-xml-sitemap ,generate-rss ,generate-atom
	   ,default-author-name ,feed-entry-sunset-pred))

       (defun ,posts-prep-fn (properties)
	 (defblog/posts-prep ,category-tags ,category-plists-hash
	   ,gen-directory-var ,source-directory-var))

       ;; Register this blog with org-project.
       (let ((cleaned-alist (alist-remove-string-key
			     ,(concatenate 'string name "-top-page")
			     (alist-remove-string-key
			      ,(concatenate 'string name "-pages")
			      (alist-remove-string-key
			       ,(concatenate 'string name "-cat-indices")
			       (alist-remove-string-key
				,(concatenate 'string name "-gen-statics")
				(alist-remove-string-key
				 ,(concatenate 'string name "-src-statics")
				 (alist-remove-string-key
				  ,(concatenate 'string name "-posts")
				  (alist-remove-string-key
				   ,(concatenate 'string name)
				   org-publish-project-alist))))))))
	     
	     ;; Convert the top-level front page from the source
	     ;; directory to the pub directory --- this file only, no
	     ;; need to copy it anywhere.
	     (top-page-entry
	      (list :preparation-function ',overall-setup-fn
		    :base-directory ,source-directory-var
		    :publishing-directory ,publish-directory-var
		    :publishing-function 'org-html-publish-to-html
		    :section-numbers ,frontpage-section-numbers
		    :table-of-contents ,frontpage-table-of-contents
		    :with-toc ,frontpage-table-of-contents
		    :exclude ".*" :include '("index.org")
		    :recursive nil
		    :html-postamble nil
		    
		    ,@(when frontpage-css-style-rel-path
			`(:html-preamble
			  ,(concatenate 'string
			     "<link rel=stylesheet type=\"text/css\" href=\""
			     frontpage-css-style-rel-path
			     "\" />")))))

	     ;; Other (top-level) non-index pages: right now, convert
	     ;; straight from the source directory to the pub area.
	     ;;
	     ;; But maybe these should really be copied with
	     ;; header/footer org-text into scratch area?
	     (pages-entry
	      (list :publishing-function 'org-html-publish-to-html
		    :base-directory ,source-directory-var
		    :publishing-directory ,publish-directory-var
		    :exclude "index.org"
		    :html-postamble
		    "<a href=\"./\">Back to the top</a>."
		    :recursive nil
		    :section-numbers ,page-section-numbers
		    :table-of-contents ,page-table-of-contents
		    :with-toc ,page-table-of-contents
		    
		    ,@(when page-css-style-rel-path
			`(:html-preamble
			  ,(concatenate 'string
			     "<link rel=stylesheet type=\"text/css\" href=\""
			     page-css-style-rel-path
			     "\" />")))))

	     ;; Category indices: generate ORG files into tmp space,
	     ;; and then convert.
	     (cat-indices-entry
	      (list :preparation-function ',cat-indices-prep-fn
		    :publishing-function 'org-html-publish-to-html
		    :base-directory ,gen-cat-indices-directory-var
		    :publishing-directory ,publish-directory-var
		    :html-postamble
		    "<a href=\"../\">Back to the top</a>."
		    :recursive t
		    :section-numbers ,category-index-section-numbers
		    :table-of-contents ,category-index-table-of-contents
		    :with-toc ,category-index-table-of-contents
		    
		    ,@(when category-index-css-style-rel-path
			`(:html-preamble
			  ,(concatenate 'string
			     "<link rel=stylesheet type=\"text/css\" href=\""
			     category-index-css-style-rel-path
			     "\" />")))))

	     ;; XML files: generate XML files into tmp space, and then
	     ;; publishing copies over to pub space.  Note that this
	     ;; target contains the OVERALL-CLEANUP-FN, and so it
	     ;; should be invoked last.
	     (gen-statics-entry
	      (list :publishing-function 'org-publish-attachment
		    :preparation-function ',gen-statics-prep-fn
		    :completion-function ',overall-cleanup-fn
		    :base-directory ,gen-statics-directory
		    :base-extension "xml"
		    :publishing-directory ,publish-directory-var
		    :recursive t))

	     ;; Static files in the source directory that can be
	     ;; copied over to pub space without translation.
	     (src-statics-entry
	      (list :publishing-function 'org-publish-attachment
		    :base-directory ,source-directory-var
		    :base-extension "html\\|css\\|jpg\\|gif\\|png\\|xml"
		    :publishing-directory ,publish-directory-var
		    :recursive t))

	     ;; Individual posts are copied into tmp/posts (its
	     ;; subdirectories created as well), and converted from
	     ;; there.
	     (posts-entry
	      (list :preparation-function ',posts-prep-fn
		    :base-directory ,get-posts-directory-var
		    :publishing-directory ,publish-directory-var
		    :html-postamble "<a href=\"../\">Back to the top</a>, or <a href=\"./\">more like this</a>."
		    :recursive t
		    :publishing-function 'org-html-publish-to-html
		    :section-numbers ,post-section-numbers
		    :table-of-contents ,post-table-of-contents
		    :with-toc ,post-table-of-contents
		    
		    ,@(when post-css-style-rel-path
			`(:html-preamble
			  ,(concatenate 'string
			     "<link rel=stylesheet type=\"text/css\" href=\""
			     post-css-style-rel-path
			     "\" />")))))

	     (overall-target
	      '(:components (;; Do *-top-page first; it has the side
			     ;; effect of updating the properties
			     ;; hashtable.
			     ,(concatenate 'string name "-top-page")
			     ,(concatenate 'string name "-pages")
			     ,(concatenate 'string name "-cat-indices")
			     ,(concatenate 'string name "-posts")
			     ,(concatenate 'string name "-src-statics")
			     ;; Do *-gen-statics last; it has the side
			     ;; effect of calling the OVERALL-CLEANUP
			     ;; function (to discard temporary
			     ;; structures and directories) after it
			     ;; publishes.
			     ,(concatenate 'string name "-gen-statics")
			     ))))

	 (setf org-publish-project-alist cleaned-alist)
	 (push (cons ,(concatenate 'string name "-top-page") top-page-entry)
	       org-publish-project-alist)
	 (push (cons ,(concatenate 'string name "-pages") pages-entry)
	       org-publish-project-alist)
	 (push (cons ,(concatenate 'string name "-cat-indices")
		     cat-indices-entry)
	       org-publish-project-alist)
	 (push (cons ,(concatenate 'string name "-gen-statics")
		     gen-statics-entry)
	       org-publish-project-alist)
	 (push (cons ,(concatenate 'string name "-src-statics")
		     src-statics-entry)
	       org-publish-project-alist)
	 (push (cons ,(concatenate 'string name "-posts") posts-entry)
	       org-publish-project-alist)
	 (push (cons ,name overall-target) org-publish-project-alist))
       (message "Defined blog %s; use org-publish to generate" ',name))))

;;; =================================================================
;;; Preparing the hash tables and reference lists at the start of a
;;; blog build.
(defun defblog/table-setup-fn (blog-plist gen-directory source-directory
			       file-plist-hash category-plist-hash
			       cat-list-setter cat-list-getter
			       last-post-setter)
  "Reset the global structures associated with a blog.
- BLOG-PLIST is the property list provided from ORG-PUBLISH.
- GEN-DIRECTORY is the root directory of the temporary files area
- FILE-PLIST-HASH is the hashtable from paths to ORG files, to the plist of
information extracted from that file.
- CAT-LIST-SETTER and CAT-LIST-GETTER are thunks which set (respectively, get) 
the category list global variable for this blog."
  (defblog/reset-categories-list source-directory cat-list-setter)
  (defblog/reset-categories-plist-hash source-directory
      (funcall cat-list-getter) category-plist-hash)
  (defblog/reset-file-plist-hash source-directory file-plist-hash
    category-plist-hash)
  (defblog/add-table-summary-data file-plist-hash category-plist-hash
    last-post-setter)
  ;; TODO --- clear out the published and temporary spaces
  )

;;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
;;; Managing the file-plist hashtable.

(defun defblog/fetch-file-plist (path file-plist-hash)
  (let ((result (gethash (intern path) file-plist-hash)))
    ;; (message "Cached %s --> %s" path result)
    result))

(defun defblog/reset-file-plist-hash (source-directory file-plist-hash
				      category-plist-hash)
  "Set up the properties hash."
  (clrhash file-plist-hash)
  (let ((top-contents (directory-files source-directory)))
    (dolist (item top-contents)
      (let ((item-fullpath (concatenate 'string source-directory item)))
	(defblog/process-file-for-hash item 0 item-fullpath file-plist-hash
				       category-plist-hash))))
  ;; (message "Finished property hash reset")
  )

(defun defblog/process-file-for-hash (bare-name depth full-path
				      file-plist-hash category-plist-hash)
  "Recursive function for populating the properties hash from a given file."
  ;; (message "Processing %s" full-path)
  (when (file-directory-p full-path)
      (setf full-path (concatenate 'string full-path "/")))
  (cond
    
    ;; If it's directory, recursively traverse that directory.
    ((and (file-directory-p full-path) (not (string-match "^\\." bare-name)))
     ;; (message "- Recurring for directory contents")
     (let ((dir-contents (directory-files full-path)))
       (dolist (dir-item dir-contents)
	 (let ((dir-item-fullpath (concatenate 'string full-path dir-item)))
	   (defblog/process-file-for-hash dir-item (+ 1 depth)
	       dir-item-fullpath file-plist-hash category-plist-hash)))))

    ;; If it's an ORGMODE file, pull and cache its properties.
    ((string-match "\\.org$" bare-name)
     (multiple-value-bind (plist posted-date update-date)
	 (defblog/build-file-plist bare-name full-path depth)
       
       ;; (message "- Caching %s --> %s" full-path plist)
       (puthash (intern full-path) plist file-plist-hash)

    ;; When debugging we may want to know about this fall-through.
    ;; (t (message "- No action for %s" full-path))
    ))))

(defun defblog/build-file-plist (bare-file path depth)
  "Extract a list of the properties we need from the file at the given PATH.
BARE-FILE and PATH should refer to the same file; the former excludes all 
surrounding directories."
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
	  (multiple-value-bind (result posted-date update-date)
	      (defblog/format-orgprops-plist bare-file path depth keyvals)
	    
	    ;; (message "  result %s" result)
	    (values result posted-date update-date)))))))

(defun defblog/format-orgprops-plist (bare-file path depth keyvals)
  "Given a key-values list, set up a plist for a file path."
  (let* ((bare-date (assoc "DATE" keyvals))
	 (bare-updated (assoc "UPDATED" keyvals))
	 (priority (nth 1 (assoc "SITEMAP_PRIORITY" keyvals)))
	 (change-freq (nth 1 (assoc "CHANGE_FREQ" keyvals)))
	 (post-date (cond
		      (bare-date (date-to-time (nth 1 bare-date)))
		      (t nil)))
	 (post-updated (cond
			 (bare-updated (date-to-time (nth 1 bare-updated)))
			 (t nil))))
    (values (list :bare bare-file :path path :depth depth
		  :title (nth 1 (assoc "TITLE" keyvals))
		  :desc (nth 1 (assoc "DESCRIPTION" keyvals))
		  :author-name (nth 1 (assoc "AUTHOR_NAME" keyvals))
		  :date post-date :updated post-updated
		  :sitemap-priority priority :change-freq change-freq)
	    post-date post-updated)))

(defun defblog/kwdpair (kwd)
  (let ((data (cadr kwd)))
    (list (plist-get data :key) (plist-get data :value))))

(defun defblog/reset-categories-list (source-directory cat-list-setter)
  (let ((category-tag-list nil))
    ;; (message "srcdir %s" source-directory)
    ;; (message "items %s" (directory-files source-directory))

    ;; Look at each file in the source directory.
    (dolist (item (directory-files source-directory))
      ;; (message "Checking %s" item)
      
      ;; We are skipping any dotfiles
      (unless (string-match "^\\." item)
	;; (message "- not a dotfile")
	(let ((cat-dir-path (concatenate 'string source-directory item "/")))
	  
	  ;; We are also only looking at directories
	  (when (file-directory-p cat-dir-path)
	    ;; (message "- is a directory")
	  
	    ;; Make sure there is a category.txt file in this
	    ;; directory.
	    (let ((cat-path (concatenate 'string cat-dir-path "category.txt")))
	      (when (file-regular-p cat-path)

		;; Add the tag to the result list
		;; (message "- include %s in category list" item)
		(push item category-tag-list)))))))
    
    (funcall cat-list-setter category-tag-list)))

(defun defblog/reset-categories-plist-hash (source-directory category-tag-list
					    category-plist-hash)
  "Given the categories list, rebuild the cateogories plist hashtable."
  
  ;; Clear anything previously in the hashtable.
  (clrhash category-plist-hash)

  ;; For each category tag
  (dolist (cat-tag category-tag-list)
  
    ;; Extract the ORG properties of the category.txt file.
    (let* ((cat-src-dir (concatenate 'string source-directory cat-tag "/"))
	   (full-path (concatenate 'string cat-src-dir "category.txt"))
	   (buf (find-file-noselect full-path))
	   (posts-list (filter #'(lambda (n) (string-match "\\.org$" n))
			       (directory-files cat-src-dir))))
      (with-current-buffer buf
	(org-mode)
	(let* ((parsed-buffer (org-element-parse-buffer 'greater-element))
	       (keyvals (org-element-map parsed-buffer '(keyword)
			  #'defblog/kwdpair)))
	  (kill-buffer buf)
	
	  ;; Form a plist for the category.
	  (let ((plist
		 `(:tag ,cat-tag
			:src-dir ,cat-src-dir
			:title ,(nth 1 (assoc "TITLE" keyvals))
			:description ,(nth 1 (assoc "DESCRIPTION" keyvals))
			:sitemap-priority ,(nth 1 (assoc "SITEMAP_PRIORITY"
							 keyvals))
			:change-freq ,(nth 1 (assoc "CHANGE_FREQ" keyvals))
			:post-files ,posts-list)))

	    ;; Store the plist in the hash.
	    ;; (message "%s\n  %s %s\n  %s %s" full-path cat-tag keyvals (intern cat-tag) plist)
	    (puthash (intern cat-tag) plist category-plist-hash)
	    ;; (message "  %s" (gethash (intern cat-tag) +defblog/maraist/category-plists-hash+))
	    ))))))

;;; =================================================================
;;; Crossreferencing information built into the hashtables.

(defun defblog/add-table-summary-data (file-plist-hash category-plist-hash
				       last-post-setter)
  "Calculate additional summary information for the plist tables.
- FILE-PLIST-HASH (respectively CATEGORY-PLIST-HASH) maps absolute pathnames
\(category names) to their property list."
  (let ((last-blog-update +web-announcement-date+))
    (dolist (cat (hash-table-keys category-plist-hash))
      ;; (message "Crossreferencing for category %s" cat)

      (let* ((cat-plist (gethash cat category-plist-hash))
	     (cat-src-dir (plist-get cat-plist :src-dir))
	     (cat-post-files (plist-get cat-plist :post-files))

	     (latest-post +web-announcement-date+)
	     (latest-update +web-announcement-date+))
	;; (message "- Has plist %s" cat-plist)

	(dolist (post-file cat-post-files)
 	  ;; (message "  - For file %s" post-file)
	  (let* ((file-fullpath (concatenate 'string cat-src-dir post-file))
		 (file-plist (gethash (intern file-fullpath) file-plist-hash))
		 
		 (file-posted (plist-get file-plist :date))
		 (file-updated (plist-get file-plist :updated)))
	    ;; (message "    %s" file-fullpath)
	    ;; (message "    %s" file-plist)
	    ;; (message "    %s %s" file-posted file-updated)
	    (when (and (time-less-p latest-post file-posted) file-posted)
	      ;; (message "    Updating latest post time")
	      (setf latest-post file-posted))
	    (when (and file-updated (time-less-p latest-update file-updated))
	      ;; (message "    Updating latest update time")
	      (setf latest-update file-updated))))

	(puthash cat
		 (plist-put (plist-put (plist-put cat-plist
						  :latest-post latest-post)
				       :latest-update latest-update)
			    :latest-mod (cond
					  ((null latest-post) latest-update)
					  ((null latest-update) nil)
					  ((time-less-p latest-post
							latest-update)
					   latest-update)
					  (t latest-post)))
		 category-plist-hash)
	(when (time-less-p last-blog-update latest-post)
	  (setf last-blog-update latest-post))
	(when (time-less-p last-blog-update latest-update)
	  (setf last-blog-update latest-update))))
    (funcall last-post-setter last-blog-update)))

;;; =================================================================
;;; Generating non-ORG/HTML files.

(defun defblog/gen-statics-prep (properties source-directory gen-directory
				 file-plist-hash cat-plist-hash
				 category-tags blog-name blog-desc blog-url
				 last-update generate-xml-sitemap
				 generate-rss generate-atom
				 default-author-name feed-entry-sunset-pred)
  "Generate XML and other non-ORG/HTML files.

These files should be written to the gen-statics subdirectory of 
the temporary files workspace.
- PROPERTIES is as specified in org-publish."

  (let ((gen-basedir (concatenate 'string gen-directory "gen-statics/")))
    (when (file-directory-p gen-basedir) (delete-directory gen-basedir t))
    (make-directory gen-basedir t))
  
  (when generate-rss
    (defblog/write-rss properties source-directory gen-directory category-tags
		       file-plist-hash cat-plist-hash
		       blog-name blog-desc blog-url last-update
		       feed-entry-sunset-pred))

  (when generate-atom
    (defblog/write-atom properties source-directory gen-directory category-tags
			file-plist-hash cat-plist-hash
			blog-name blog-desc blog-url
			last-update default-author-name
			feed-entry-sunset-pred))

  (when generate-xml-sitemap
    (defblog/write-xml-sitemap properties gen-directory blog-url
      file-plist-hash cat-plist-hash)))

;;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
;;; Generating the XML sitemap

(defun defblog/write-xml-sitemap (properties gen-directory site-url
				  file-plist-hash cat-plist-hash)
  "Generate an XML sitemap for a blog.
- PROPERTIES are from org-publish.
- GEN-DIRECTORY is the absolute path to the scratch space directory.
- CATEGORY-TAGS, FILE-PLIST-HASH and CAT-PLIST-HASH are the internal data
structures of the blog artifacts."

  (message "Generating XML sitemap")
  (let ((gen-basedir (concatenate 'string gen-directory "gen-statics/")))    
    (let ((sitemap-buf (find-file-noselect (concatenate 'string 
					     gen-basedir "sitemap.xml"))))
      (with-current-buffer sitemap-buf
	(erase-buffer)
	(insert
	 "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
	 "<urlset xmlns=\"http://www.sitemaps.org/schemas/sitemap/0.9\">\n")

	(dolist (file-plist (hash-table-values file-plist-hash))
	  (when (zerop (plist-get file-plist :depth))
	    (let* ((page-url
		    (replace-regexp-in-string "\\.org$" ".html"
					      (concatenate 'string
						site-url
						(plist-get file-plist
							   :bare)))))
	      (defblog/write-xml-sitemap-entry page-url
		  (plist-get file-plist :date) (plist-get file-plist :updated)
		  (plist-get file-plist :change-freq) nil nil
		  (plist-get file-plist :priority) nil nil))))
	(dolist (cat-plist (hash-table-values cat-plist-hash))
	  (let ((cat-tag (plist-get cat-plist :tag)))
	    (defblog/write-xml-sitemap-entry
		(concatenate 'string site-url cat-tag "/")
		(plist-get cat-plist :latest-post) nil
		nil (plist-get cat-plist :change-freq) nil
		nil (plist-get cat-plist :sitemap-priority) nil)
	  
	    (dolist (file (plist-get cat-plist :post-files))
	      (message "File %s" file)
	      (let* ((file-fullpath (concatenate 'string
				      (plist-get cat-plist :src-dir) file))
		     (file-plist (gethash (intern file-fullpath)
					  file-plist-hash)))
		(message "     plist %s" file-plist)
		(let* ((base-file-src (plist-get file-plist :bare))
		       (page-url
			(concatenate 'string
			  site-url cat-tag "/"
			  (replace-regexp-in-string "\\.org$" ".html"
						    base-file-src))))
		  (defblog/write-xml-sitemap-entry page-url
		      (plist-get file-plist :date)
		    (plist-get file-plist :updated)
		    (plist-get file-plist :change-freq)
		    (plist-get cat-plist :change-freq)
		    nil
		    (plist-get file-plist :sitemap-priority)
		    (plist-get cat-plist :sitemap-priority)
		    nil))))))
	(insert "</urlset>\n")
	(save-buffer 0))
      (kill-buffer sitemap-buf))))

(defun defblog/write-xml-sitemap-entry (page-url post-time update-time
					page-change-freq
					group-default-change-freq
					blog-default-change-freq
					page-priority
					group-default-priority
					blog-default-priority)
  (let* ((mod-time (cond
		     ((null post-time) update-time)
		     ((null update-time) post-time)
		     ((time-less-p post-time update-time) update-time)
		     (t post-time))))
    (insert "  <url>\n"
	    "    <loc>" page-url "</loc>\n"
	    "    <lastmod>" (format-time-string +rfc-3339-time-format+ mod-time)
	    "</lastmod>\n"
	    "    <changefreq>"
	    (cond
	      (page-change-freq page-change-freq)
	      (group-default-change-freq group-default-change-freq)
	      (blog-default-change-freq blog-default-change-freq)
	      (t "yearly"))
	    "</changefreq>\n"
	    "    <priority>"
	    (cond
	      (page-priority page-priority)
	      (group-default-priority group-default-priority)
	      (blog-default-priority blog-default-priority)
	      (t "0.5"))
	    "</priority>\n"
	    "  </url>\n")))


;;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
;;; Writing RSS feeds

(defun defblog/write-rss (properties source-directory gen-directory
			  category-tags file-plist-hash cat-plist-hash
			  blog-name blog-desc blog-url blog-last-mod
			  feed-entry-sunset-pred)
  "Write RSS files for the overall site and for each post category.
- PROPERTIES are from org-publish.
- SOURCE-DIRECTORY (respectively GEN-DIRECTORY) is the absolute path 
to the blog source (scratch space) directory.
- CATEGORY-TAGS, FILE-PLIST-HASH and CAT-PLIST-HASH are the internal data
structures of the blog artifacts.
- BLOG-NAME, BLOG-DESC and BLOG-URL are strings describing the blog itself."
  (let ((gen-basedir (concatenate 'string gen-directory "gen-statics/")))    
    (let ((all-buf (find-file-noselect (concatenate 'string 
					 gen-basedir "rss.xml"))))
    (with-current-buffer all-buf
      (erase-buffer)
      (defblog/write-rss-opening blog-name blog-desc
	(concatenate 'string blog-url "rss.xml") blog-url blog-last-mod))

    (dolist (category-tag category-tags)
      (let* ((cat-src-dir (concatenate 'string
			    source-directory category-tag "/"))
	     (post-fullpaths (file-expand-wildcards (concatenate 'string
						      cat-src-dir "*.org")))
	     (file-plists (mapcar #'(lambda (p) (defblog/fetch-file-plist p
						    file-plist-hash))
				  post-fullpaths))
	     (cat-properties (gethash (intern category-tag) cat-plist-hash))

	     (cat-rss-title (concatenate 'string blog-name ": "
					 (plist-get cat-properties :title)))
	     (cat-desc (plist-get cat-properties :description))	      
	     (cat-last-mod-date (plist-get cat-properties :latest-mod))
	     (cat-html-url (concatenate 'string blog-url category-tag "/"))
	     (cat-atom-url (concatenate 'string cat-html-url "rss.xml"))

	     (cat-out-dir (concatenate 'string gen-basedir category-tag "/")))

	(unless (file-directory-p cat-out-dir) (make-directory cat-out-dir))

	(let ((rss-buf (find-file-noselect (concatenate 'string 
					     cat-out-dir "rss.xml"))))

	  (with-current-buffer rss-buf
	    (erase-buffer)
	    (defblog/write-rss-opening cat-rss-title cat-desc
	      cat-atom-url cat-html-url cat-last-mod-date))

	  (dolist (plist file-plists)
	    ;; TODO Only add things from the last (let's say) five years.
	    (with-current-buffer all-buf
	      (defblog/write-rss-for-plist plist cat-properties))
	    (with-current-buffer rss-buf
	      (defblog/write-rss-for-plist plist cat-properties)))

	  (with-current-buffer rss-buf
	    (defblog/write-rss-closing)
	    (save-buffer 0))
	  (kill-buffer rss-buf))))
    
    (with-current-buffer all-buf
      (defblog/write-rss-closing)
      (save-buffer 0))
    (kill-buffer all-buf))))

(defconst +rfc-822-time-format+ "%a, %d %b %Y %H:%M:%S %z"
  "Format string for RFC822 Date and Time Specification, used in RSS.")

(defun defblog/write-rss-opening (title description
				 rss-link html-link last-built-date)
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
  (insert "    <atom:link href=\"" rss-link "\"\n")
  (insert "               rel=\"self\"\n")
  (insert "               type=\"application/rss+xml\" />\n")
  (insert "    <link>" html-link "</link>\n")
  (when description
    (insert "    <description>" description "</description>\n"))
  (insert "    <lastBuildDate>"
	  (format-time-string +rfc-822-time-format+ last-built-date)
	  "</lastBuildDate>\n")
  (insert "    <language>en-US</language>\n")
  (insert "    <sy:updatePeriod>hourly</sy:updatePeriod>\n")
  (insert "    <sy:updateFrequency>1</sy:updateFrequency>\n"))

(defun defblog/write-rss-closing ()
  (insert "  </channel>\n</rss>\n"))

(defun defblog/write-rss-for-plist (plist category-properties)
  (let* ((title (plist-get plist :title))
	 (bare  (plist-get plist :bare))
	 (date  (plist-get plist :date))
	 (desc  (plist-get plist :desc))
	 (this-link (concatenate 'string
		      "https://maraist.org/"
		      (plist-get category-properties :tag) "/"
		      (replace-regexp-in-string "\\.org$" ".html" bare))))
    (insert "\n    <item>\n")
    (insert "      <title>" (cond (title title) (t "(untitled)")) "</title>\n")
    (insert "      <link>" this-link "</link>\n")
    (insert "      <guid>" this-link "</guid>\n")
    (insert "      <dc:creator><![CDATA[jm]]></dc:creator>\n")
    (insert "      <pubDate>"
	    (cond
	      (date (format-time-string +rfc-822-time-format+ date))
	      (t "Fri, 08 Jan 2005 12:00:00"))
	    " +0000</pubDate>\n")
    (insert "      <category><![CDATA["
	    (plist-get category-properties :title)
	    "]]></category>\n")
    (when desc
      (insert "      <description><![CDATA[" desc "]]></description>\n"))
    (insert "      <slash:comments>0</slash:comments>\n")
    (insert "    </item>\n")))

;;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
;;; Writing Atom feeds

(defun defblog/write-atom (properties source-directory gen-directory
			   category-tags file-plist-hash cat-plist-hash
			   blog-name blog-desc blog-url blog-last-mod
			   default-author-name feed-entry-sunset-pred)
  "Write Atom files for the overall site and for each post category.
- PROPERTIES are from org-publish.
- SOURCE-DIRECTORY (respectively GEN-DIRECTORY) is the absolute path to 
the blog source (scratch space) directory.
- CATEGORY-TAGS, FILE-PLIST-HASH and CAT-PLIST-HASH are the internal 
data structures of the blog artifacts.
- BLOG-NAME, BLOG-DESC and BLOG-URL are strings describing the blog
itself."
  (let ((gen-basedir (concatenate 'string gen-directory "gen-statics/")))
    
    (let ((all-buf (find-file-noselect (concatenate 'string 
					 gen-basedir "atom.xml"))))
    (with-current-buffer all-buf
      (erase-buffer)
      (defblog/write-atom-opening blog-name blog-desc
	(concatenate 'string blog-url "atom.xml") blog-url blog-last-mod))

    (dolist (category-tag category-tags)
      (let* ((cat-src-dir (concatenate 'string
			    source-directory category-tag "/"))
	     (post-fullpaths (file-expand-wildcards (concatenate 'string
						      cat-src-dir "*.org")))
	     (file-plists (mapcar #'(lambda (p) (defblog/fetch-file-plist p
						    file-plist-hash))
				  post-fullpaths))
	     (cat-properties (gethash (intern category-tag) cat-plist-hash))

	     (cat-atom-title (concatenate 'string blog-name ": "
					 (plist-get cat-properties :title)))
	     (cat-desc (plist-get cat-properties :description))	      
	     (cat-last-mod-date (plist-get cat-properties :latest-mod))
	     (cat-html-url (concatenate 'string blog-url category-tag "/"))
	     (cat-atom-url (concatenate 'string cat-html-url "atom.xml"))

	     (cat-out-dir (concatenate 'string gen-basedir category-tag "/")))

	(unless (file-directory-p cat-out-dir) (make-directory cat-out-dir))
	
	(let ((atom-buf (find-file-noselect (concatenate 'string 
					     cat-out-dir "atom.xml"))))

	  (with-current-buffer atom-buf
	    (erase-buffer)
	    (defblog/write-atom-opening cat-atom-title cat-desc
	      cat-atom-url cat-html-url cat-last-mod-date))

	  (dolist (plist file-plists)
	    ;; TODO Only add things from the last (let's say) five years.
	    (with-current-buffer all-buf
	      (defblog/write-atom-for-plist plist cat-properties
		default-author-name))
	    (with-current-buffer atom-buf
	      (defblog/write-atom-for-plist plist cat-properties
		default-author-name)))

	  (with-current-buffer atom-buf
	    (defblog/write-atom-closing)
	    (save-buffer 0))
	  (kill-buffer atom-buf))))
    
    (with-current-buffer all-buf
      (defblog/write-atom-closing)
      (save-buffer 0))
    (kill-buffer all-buf))))

(defconst +rfc-3339-time-format+ "%Y-%m-%dT%H:%M:%S%:z"
  "Format string for RFC3339 Date and Time Specification, used in Atom.")

(defun defblog/write-atom-opening (title description
				   atom-link html-link last-built-date)
  (insert "<?xml version=\"1.0\" encoding=\"utf-8\"?>")
  (insert "<feed xmlns=\"http://www.w3.org/2005/Atom\">\n")
  (insert "  <title>" title "</title>\n")
  (insert "  <link href=\"" html-link "\" />\n")
  (insert "  <link rel=\"self\" href=\"" atom-link "\" />\n")
  (insert "  <id>" html-link "</id>\n")
  ;; (when description (insert "  <summary>" description "</summary>\n"))
  (insert "  <updated>"
	  (format-time-string +rfc-3339-time-format+ last-built-date)
	  "</updated>\n"))

(defun defblog/write-atom-closing ()
  (insert "</feed>\n"))

(defun defblog/write-atom-for-plist (file-properties category-properties
				     default-author-name)
  (let* ((title (plist-get file-properties :title))
	 (bare (plist-get file-properties :bare))
	 (date (plist-get file-properties :date))
	 (desc (plist-get file-properties :desc))
	 (author (plist-get file-properties :author-name))
	 (this-link (concatenate 'string
		      "https://maraist.org/"
		      (plist-get category-properties :tag) "/"
		      (replace-regexp-in-string "\\.org$" ".html" bare))))
    (insert "\n  <entry>\n")
    (insert "      <title>" (cond (title title) (t "(untitled)")) "</title>\n")
    (insert "      <link href=\"" this-link "\" />\n")
    (insert "      <id>" this-link "</id>\n")
    (insert "      <author><name>"
	    (cond
	      (author author)
	      (default-author-name default-author-name)
	      (t (error "No author (or default) for file %s" bare)))
	    "</name></author>\n")
    (insert "      <updated>"
	    (cond
	      (date (format-time-string +rfc-3339-time-format+ date))
	      (t (format-time-string +rfc-3339-time-format+
				     +web-announcement-date+)))
	    "</updated>\n")
    (when desc
      (insert "      <summary><![CDATA[" desc "]]></summary>\n"))
    (insert "    </entry>\n")))

;;; =================================================================
;;; Copying posts into the tmp space

(defun defblog/posts-prep (cat-list cat-plist-hash gen-directory source-directory)
  (dolist (cat cat-list)
    (let ((cat-src-dir (concatenate 'string source-directory cat "/"))
	  (cat-tmp-dir (concatenate 'string gen-directory "posts/" cat "/")))
      (when (file-directory-p cat-tmp-dir)
	(delete-directory cat-tmp-dir t))
      (make-directory cat-tmp-dir t)
      (dolist (file (plist-get (gethash (intern cat) cat-plist-hash)
			       :post-files))
	(let ((cat-src-file (concatenate 'string cat-src-dir file))
	      (cat-tmp-file (concatenate 'string cat-tmp-dir file)))
	  ;; (message "%s %s" cat-src-file cat-tmp-file)
	  (copy-file cat-src-file cat-tmp-file)
	  ;; TODO This function into configuration
	  )))))

;;; =================================================================
;;; Building indices of posts in the tmp space

(defun defblog/cat-indices-prep (cat-list-getter cat-plist-hash
				 file-plist-hash gen-directory source-directory
				 cat-indices-style-link)
  "For the \"-cat-indices\" publish targets, generate category index ORG files.
These files should be written to the cat-indices subdirectory of the
temporary files workspace."
  ;; (message "Called defblog/cat-indices-prep %s" (funcall cat-list-getter))
  
  ;; For each category, and for its source and scratch directories,
  (dolist (cat (funcall cat-list-getter))
    (let* ((cat-src-dir (concatenate 'string source-directory cat "/"))
	   (dest-dir (concatenate 'string gen-directory "cat-indices/" cat "/"))
	   (dest-org (concatenate 'string dest-dir "index.org"))

	   (cat-plist (gethash (intern cat) cat-plist-hash))
	   (cat-title (plist-get cat-plist :title)))
      ;; (message "Indexing category %s %s \"%s\"" cat cat-plist cat-title)

      ;; Make sure the destination directory exists, and is empty.
      ;; (message "Creating directory %s for %s" dest-dir cat-title)
      (when (file-directory-p dest-dir) (delete-directory dest-dir t))
      (make-directory dest-dir t)

      ;; Identify the ORG files in the source directory, and retrieve
      ;; their property lists.
      (let* ((all-files (directory-files cat-src-dir))
	     (org-files
	      (filter #'(lambda (x)
			  (and (string-match "\\.org$" x)
			       (not (string-match "/index.org$" x))))
			  all-files))
	     ;; (m2 (message "- org-files: %s" org-files))
      
	     ;; Create the category's index.org file.
	     (index-buffer (find-file-noselect dest-org)))
	;; (message "L1 %s %s" org-files index-buffer)
	
	(with-current-buffer index-buffer
	  (erase-buffer)

	  (insert "#+TITLE: " ; TODO Generalize the title.
		  cat-title
		  " [JM's website]\n"
		  "#+html_head:  "
		  "<link rel=stylesheet type=\"text/css\" href=\""
		  cat-indices-style-link
		  "\"/>"
		  "\n\n")

	  (let* ((full-files (mapcar #'(lambda (x)
					 (concatenate 'string
					   cat-src-dir x))
				     org-files))
		 (plists (mapcar #'(lambda (x)
				     (defblog/fetch-file-plist x
					 file-plist-hash))
				 full-files))
		 (sorter #'(lambda (y x) (time-less-p (plist-get x :date)
						      (plist-get y :date)))))
	    ;; (message "Iterating through full-files %s\n  plists %s"
	    ;;	     full-files plists)
	    (dolist (prop-list (sort plists sorter))
	      ;; (message "Destructuring %s" prop-list)
	      (let ((bare  (plist-get prop-list :bare))
		    (path  (plist-get prop-list :path))
		    (title (plist-get prop-list :title))
		    (desc  (plist-get prop-list :desc))
		    (date  (plist-get prop-list :date))
		    (updated  (plist-get prop-list :updated)))
		;; (message "- bare %s title \"%s\" desc \"%s\"" bare title desc)
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
		(insert "\n"))))
	  (save-buffer 0))))))

;;; =================================================================
;;; Debugging utilities

(defun defblog/state-dump (file-plists-hash cat-list cat-plists-hash)
  (message "--------------------")
  (dolist (file (hash-table-keys file-plists-hash))
    (message "%s\n ==> %s\n" file (gethash file file-plists-hash)))
  (message "\nCategories: %s\n" cat-list)
  (message "\nCat hash: %s\n" cat-plists-hash)
  (dolist (cat cat-list)
    (message "%s\n ==> %s\n" cat (gethash (intern cat) cat-plists-hash)))
  (message "--------------------"))

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

(defconst +web-announcement-date+
  (encode-time (list 20 56 14 6 8 1991 nil nil nil)) ;; August 6, 1991, 14:56:20 GMT
  "The date when Sir Tim announced the invention of the World-Wide Web.
Used as an earliest-possible post- or updated-date for pages and posts.")

(provide 'defblog)
;;; defblog ends here
