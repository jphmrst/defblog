;;; defblog --- A wrapper for org-publish, for producing blogs from
;;; local Org-mode files.

;; Copyright (C) 2021 John Maraist

;; Author: John Maraist <defblog-author@maraist.org>
;; Maintainer: John Maraist <defblog-author@maraist.org>
;; Keywords: org-publish, web, blog
;; Version: 0.1.1
;; X-URL: https://github.com/jphmrst/defblog

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file LICENSE.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; DEFBLOG is a wrapper around ORG-PUBLISH which allows you to declare
;; a simple structured blog.  This package offers an all-Emacs
;; solution to maintaining a web site (except for any uploading via
;; rsync, which DEFBLOG can triggered through ORG-PUBLISH).  For more
;; information, see the README.md file.

;;; NOTES AND CURRENT TODOs

;;; Things to have before calling it Version 1.0

;; TODO Format string for each page list entry in category.txt file
;; property.
;;
;; TODO Add options for CATEGORY-LINKS pragma in
;; DEFBLOG/PAGE-COPY-WITH-SUBSTITUTIONS
;;
;; Develop a scheme for tags, essentially as a virtual superset of
;; categories:
;;
;; - TODO Receive TAGS property in posts.
;;
;; - TODO Write TAG-LINKS pragma for DEFBLOG/PAGE-COPY-WITH-SUBSTITUTIONS.
;;
;; Refine HTACCESS file generation:
;;
;; - TODO Use the remote-htaccess argument for the remote destination
;; - of the htaccess file.  If defined:
;;
;; -- Generate to a gen directory file
;;
;; -- Exclude .htaccess from rsync file list
;;
;; -- Second call to rsync for the htaccess target
;;
;; -- Remove this file in cleanup
;;
;; - TODO Add a forwards-list argument to defdoc for other forwards.
;;
;; TODO Ignore/warn about index.org source files in the category
;; directories.

;;; Other ideas and tasks

;; TODO Add a way to create the ORG for a page/post from scratch.

;;; Code:
(require 'anaphora-anywhere)

(defmacro if-show-state-dump (&rest forms)
  "Debugging flag: execute FORMS when debugging."
  (declare (indent 0))
  `(progn ,@forms) ;; nil
  )

(defconst +defblog/debug-level+ 0)
(defconst +defblog/debug-topics+ '(:control))
(cl-defmacro debug-msg ((level topic-or-topics) &rest args)
  (declare (indent 2))
  (when (and (<= level +defblog/debug-level+)
             (or (eq topic-or-topics t)
                 (cl-intersection (cond
                                    ((symbolp topic-or-topics)
                                     (list topic-or-topics))
                                    (t topic-or-topics))
                                  +defblog/debug-topics+ :test 'eq)))
    `(message ,@args)))

(defmacro with-plist-properties (prop-specs plist-expr &rest forms)
  "Macro akin to WITH-ACCESSORS for property lists.
- PLIST-EXPR is a form which should evaluate to a property list.  The macro
  returns a let-binding which first evaluates PLIST-EXPR.
- PROP-SPECS is a list of forms (VAR PROP), which translates to a let-binding
  (VAR (plist-get plist PROP)), where plist is the result of evaluating the
  PLIST-EXPR.
- The FORMS become the body of this let-expression."
  (declare (indent 2))
  (let ((plist (gensym)))
    `(let ((,plist ,plist-expr))
       (let ,(mapcar #'(lambda (spec)
                         (cond
                           ((symbolp spec)
                            `(,spec (plist-get ,plist ,spec)))
                           ((and (listp spec) (symbolp (car spec))
                                 (null (cdr spec)))
                            `(,(car spec) (plist-get ,plist ,(car spec))))
                           ((and (listp spec) (symbolp (car spec))
                                 (symbolp (cadr spec)) (null (cddr spec)))
                            `(,(car spec) (plist-get ,plist ,(cadr spec))))
                           (t (error "Malformed binder %s" spec))))
                     prop-specs)
         ,@forms))))

(cl-defmacro defblog (name source-directory blog-title
                           &key
                           blog-url blog-desc
                           published-directory generated-directory
                           retain-published-directory
                           retain-generated-directories
                           (post-copy-function 'defblog/page-copy-verbatim)
                           (page-copy-function 'defblog/page-copy-verbatim)
                           (front-copy-function 'defblog/page-copy-verbatim)
                           (cat-index-title-fn
                            '(lambda (cat-plist blog-title)
                              (concatenate 'string
                                blog-title ": " (plist-get cat-plist :title))))
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
                           rsync-dest rsync-rsh
                           (rsync-options '("-rLptgoD"))
                           (rsync-delete-excluded t)
                           ;;
                           (generate-xml-sitemap t)
                           (sitemap-default-change-freq 'monthly)
                           (sitemap-default-priority 0.5)
                           ;;
                           (generate-rss t)
                           (generate-atom t)
                           default-author-name
                           ;;
                           (generate-htaccess t)
                           remote-htaccess
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
- SOURCE-DIRECTORY, a string giving the absolute pathname of the
directory containing the source directory, scratch work space,
and HTML output directory for this blog.
- BLOG-TITLE, a string with the human-oriented name for this web
site.

Optional parameters:
- BLOG-URL (respectively BLOG-DESC) gives the URL for the top of
this blog (human-facing description of the web site).  The URL
is required when generating most of the XML artifacts.
- PUBLISHED-DIRECTORY and GENERATED-DIRECTORY, if given, are used
as temporary directories in generating the final website.
GENERATED-DIRECTORY is used to hold various partial images.
PUBLISHED-DIRECTORY is used to hold the final web site image before
uploading it to a remote server.  If omitted, space in /tmp/ is
used for these directories.  Normally the contents of these
directories is erased after generation; however
RETAIN-PUBLISHED-DIRECTORY and RETAIN-GENERATED-DIRECTORIES may
be set to non-null values to keep the directories' contents
instead.
- CSS-STYLE-SUBPATH, the local path from the SOURCE-DIRECTORY to
default CSS stylesheet for the blog.
- FRONTPAGE-CSS-STYLE-REL-PATH, PAGE-CSS-STYLE-REL-PATH,
POST-CSS-STYLE-REL-PATH and CATEGORY-INDEX-CSS-STYLE-REL-PATH are
local paths from the SOURCE-DIRECTORY to the CSS stylesheets for
those groups of pages.  If not given, these arguments take the
value of CSS-STYLE-REL-PATH.  For any of these, a NIL value means
there should be no CSS style sheet.
- GENERATE-XML-SITEMAP, if non-nil, indicates that an XML sitemap
should be generated.  The SITEMAP-DEFAULT-CHANGE-FREQ (respectively
SITEMAP-DEFAULT-PRIORITY) argument gives the default value for
pages/posts which do not otherwise have a setting for the announced
change frequency (sitemap priority).
- GENERATE-RSS and GENERATE-ATOM indicate
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
    absolute limit of the earliest date included.
- Most Org files in the source directory are copied into the
temporary space before being generated.  By default, they are
copied with the function DEFBLOG/PAGE-COPY-VERBATIM, which copies
a file verbatim.  However any function may be used.  Defblog
also provides the function DEFBLOG/PAGE-COPY-WITH-SUBSTITUTIONS,
which expands certain pragmas in Org-mode comments to additional
content (see that function's documentation for additional
information).  These copier functions are provided via two
arguments:
 1. POST-COPY-FUNCTION, applied to posts.
 2. FRONT-COPY-FUNCTION, applied to the top-level page.
"
  (declare (indent 3))

  ;; Check fatal combinations of present/missing arguments.
  (unless (file-directory-p source-directory)
    (error "Expected a directory for SOURCE-DIRECTORY %s" source-directory))

  (when (or generate-rss generate-atom)
    (unless blog-url
      (error "Generating an RSS/Atom feed requires BLOG-URL")))

  (unless (or (null upload) (eq upload :rsync))
    (error "Unrecognized value for upload: %s" upload))

  (when (and (null upload) (null published-directory))
    (warn
     "No upload method specified, but no local :PUBLISHED-DIRECTORY given"))

  ;; Refinements to the given arguments.
  (unless (string-match "/$" source-directory)
    (setf source-directory (concatenate 'string source-directory "/")))

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
        (site-plist-var (intern (concatenate 'string
                                  "+defblog/" name "/site-plist+")))
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
        (source-directory-var (intern (concatenate 'string
                               "+defblog/" name "/src-basedir+")))
        (publish-directory-var (intern (concatenate 'string
                               "+defblog/" name "/pub-basedir+")))
        (gen-directory-var (intern (concatenate 'string
                               "+defblog/" name "/tmp-basedir+")))
        (get-posts-directory-var (intern (concatenate 'string
                                 "+defblog/" name "/posts-basedir+")))
        (get-pages-directory-var (intern (concatenate 'string
                                 "+defblog/" name "/pages-basedir+")))
        (gen-cat-indices-directory-var (intern (concatenate 'string
                                       "+defblog/" name "/cat-indices+")))
        (gen-statics-directory (intern (concatenate 'string
                                       "+defblog/" name "/gen-statics+")))
        (last-blog-update (intern (concatenate 'string
                                      "+defblog/" name "/last-blog-update+")))
        (system-tmp-dir-var (intern (concatenate 'string
                                      "+defblog/" name "/system-tmp-dir+")))

        ;; Names of functions associated with this blog.  Each of
        ;; these names is associated with a DEFUN in the macro
        ;; expansion.

        (cat-indices-prep-fn (intern (concatenate 'string
                                       "defblog/" name "/cat-indices-prep")))
        (gen-statics-prep-fn (intern (concatenate 'string
                                       "defblog/" name "/gen-statics-prep")))
        (posts-prep-fn (intern (concatenate 'string
                                 "defblog/" name "/posts-prep")))
        (pages-prep-fn (intern (concatenate 'string
                                 "defblog/" name "/pages-prep")))
        (overall-setup-fn (intern (concatenate 'string
                                    "defblog/" name "/overall-setup")))
        (overall-cleanup-fn (intern (concatenate 'string
                                      "defblog/" name "/overall-cleanup")))
        (state-dump-fn (intern (concatenate 'string
                                 "defblog/" name "/state-dump")))
        (system-tmp-dir-rm-fn (intern (concatenate 'string
                                        "defblog/" name "/rm-system-tmp-dir")))

        ;; Sunset time for feed entries.
        (feed-entry-sunset-pred
         (cond
           ((null feed-entry-sunset)
            (debug-msg (3 :internal) "feed-entry-sunset-pred (1)")
            `#'(lambda (x) t))
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
            (debug-msg (3 :internal) "feed-entry-sunset-pred (2)")
            `#'(lambda (x)
                 (let ((min (time-subtract (current-time)
                                           ',feed-entry-sunset)))
                   (debug-msg (3 :internal) " - Comparing to minimum %s"
                              (format-time-string "%d %b %Y" min))
                   (time-less-p min x))))
           ((listp feed-entry-sunset)
            (debug-msg (3 :internal) "feed-entry-sunset-pred (3)")
            (let ((bound (mapcar
                          #'(lambda (x) (cond ((null x) 0) (t (- x))))
                          (apply #'make-decoded-time feed-entry-sunset))))
              `#'(lambda (x)
                   (let ((min (encode-time (decoded-time-add
                                            (decode-time (current-time))
                                            ',bound))))
                     (debug-msg (3 :internal) " - Comparing to minimum %s"
                                (format-time-string "%d %b %Y" min))
                     (time-less-p min x)))))
           ((stringp feed-entry-sunset)
            (debug-msg (3 :internal) "feed-entry-sunset-pred (4)")
            (let ((min (parse-time-string feed-entry-sunset)))
              `#'(lambda (x) (time-less-p ',min x))))
           (t (error "Unrecognized value for FEED-ENTRY-SUNSET: %s"
                     feed-entry-sunset)))))

    `(progn

       ;; DEFVARs corresponding to the stateful components of this
       ;; blog.

       (when (boundp ',site-plist-var) (makunbound ',site-plist-var))
       (defvar ,site-plist-var nil
         ,(concatenate 'string "General property list for the " name " blog."))

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

       ;; Managing temporary directories.  Since the Org source
       ;; directories in ORG-PUBLISH-PROJECT-ALIST are hardcoded, we
       ;; need to fix a directory in /tmp when calling defblog.
       (when (boundp ',system-tmp-dir-var)
         (when ,system-tmp-dir-var
           (delete-directory ,system-tmp-dir-var t))
         (makunbound ',system-tmp-dir-var))
       (defvar ,system-tmp-dir-var
           ,(when (or (null published-directory) (null generated-directory))
              `(make-temp-file "defblog-" t)))
       (unless (string-match "/$" ,system-tmp-dir-var)
         (setf ,system-tmp-dir-var
               (concatenate 'string ,system-tmp-dir-var "/")))
       (defun ,system-tmp-dir-rm-fn ()
         (when ,system-tmp-dir-var
           (delete-directory ,system-tmp-dir-var t)))
       (add-hook 'kill-emacs-hook ',system-tmp-dir-rm-fn)

       ;; DEFVARs corresponding to the constants defined for this
       ;; blog.

       (when (boundp ',source-directory-var)
         (makunbound ',source-directory-var))
       (defvar ,source-directory-var ,source-directory
         ,(concatenate 'string
            "Directory with the source ORG files of the " name " blog."))

       (when (boundp ',publish-directory-var)
         (makunbound ',publish-directory-var))
       (defvar ,publish-directory-var
           ,(cond
              (generated-directory published-directory)
              (t `(concatenate 'string ,system-tmp-dir-var "pub/")))
         ,(concatenate 'string
            "Target directory for publishable files of the " name " blog."))

       (when (boundp ',gen-directory-var)
         (makunbound ',gen-directory-var))
       (defvar ,gen-directory-var
           ,(cond
              (generated-directory generated-directory)
              (t system-tmp-dir-var))
         ,(concatenate 'string
            "Scratch space directory for the " name " blog."))

       (when (boundp ',get-posts-directory-var)
         (makunbound ',get-posts-directory-var))
       (defvar ,get-posts-directory-var
           (concatenate 'string ,gen-directory-var "posts/")
         ,(concatenate 'string
            "Scratch space directory for copying over posts for the "
            name " blog."))

       (when (boundp ',get-pages-directory-var)
         (makunbound ',get-pages-directory-var))
       (defvar ,get-pages-directory-var
           (concatenate 'string ,gen-directory-var "pages/")
         ,(concatenate 'string
            "Scratch space directory for copying non-index top-level pages for the " name " blog."))

       (when (boundp ',gen-cat-indices-directory-var) (makunbound ',gen-cat-indices-directory-var))
       (defvar ,gen-cat-indices-directory-var
           (concatenate 'string ,gen-directory-var "cat-indices/")
         ,(concatenate 'string
            "Scratch space area for generating category index files for the "
            name " blog."))

       (when (boundp ',gen-statics-directory) (makunbound ',gen-statics-directory))
       (defvar ,gen-statics-directory
           (concatenate 'string ,gen-directory-var "gen-statics/")
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

         (debug-msg (0 t) "Ensuring clean temporary directories...")
         (ensure-ready-work-dir ,publish-directory-var)
         (dolist (subdir +defblog/scratch-subdirectories+)
           (ensure-ready-work-dir (concatenate 'string
                                    ,gen-directory-var subdir "/")))
         (debug-msg (0 t) "Ensuring clean temporary directories...done")

         (debug-msg (0 t) "Setting up defblog temp structures...")
         (defblog/table-setup-fn properties
             ,gen-directory-var ,source-directory-var
             ,file-plists-hash ,category-plists-hash
             #'(lambda (x) (setf ,category-tags x))
             #'(lambda () ,category-tags)
             #'(lambda (x) (setf ,last-blog-update x))
             #'(lambda (x) (setf ,site-plist-var x))
             ,blog-title ,blog-desc ,blog-url)
         (debug-msg (0 t) "Setting up defblog temp structures...done")
         (,state-dump-fn)

         ;; The setup for the front page is just to copy it in to its
         ;; scratch area.
         (let ((source-org (concatenate 'string
                             ,source-directory-var "index.org")))
           (,front-copy-function source-org
                                 (concatenate 'string
                                   ,gen-directory-var "front/index.org")
                                 ,site-plist-var
                                 (gethash (intern source-org)
                                          ,file-plists-hash))))

       (defun ,overall-cleanup-fn (properties)

         (unless ,retain-published-directory
           (debug-msg (0 t) "Removing pub directory...")
           (delete-directory ,publish-directory-var t)
           (debug-msg (0 t) "Removing pub directory...done"))

         (unless ,retain-generated-directories
           (debug-msg (0 t) "Removing scratch directories...")
           (dolist (subdir +defblog/scratch-subdirectories+)
             (let ((fullpath (concatenate 'string
                               ,gen-directory-var subdir "/")))
               (when (file-directory-p fullpath)
                 (delete-directory fullpath t))))
           (debug-msg (0 t) "Removing scratch directories...done"))

         (cond
           ((eq ,upload :rsync)
            (debug-msg (0 t) "Uploading...")
            (let ((args
                   (list "rsync" nil "*org-publish-rsync*" nil
                         ,@rsync-options
                         ,@(when rsync-rsh
                             (list (concatenate 'string
                                     "--rsh=" rsync-rsh)))
                         ,@(when rsync-delete-excluded `("--delete-excluded"))
                         ,publish-directory-var
                         ,rsync-dest)))
              (debug-msg (3 :internal) "Invoking rsync:\n  %s" args)
              (apply #'call-process args))
            (debug-msg (0 t) "Uploading...done"))
           ((null ,upload)
            (debug-msg (0 t) "Uploading not selected")))

         (debug-msg (0 t) "Cleaning up defblog temp structures...")
         (clrhash ,file-plists-hash)
         (clrhash ,category-plists-hash)
         (setf ,category-tags nil)
         (debug-msg (0 t) "Cleaning up defblog temp structures...done"))

       (defun ,state-dump-fn ()
         (if-show-state-dump
           (defblog/state-dump ,site-plist-var ,category-tags)))

       (defun ,cat-indices-prep-fn (properties)
         (defblog/cat-indices-prep #'(lambda () ,category-tags)
                                   ,site-plist-var ,gen-directory-var
                                   ,source-directory-var
                                   ,category-index-css-style-rel-path
                                   #',cat-index-title-fn))

       (defun ,gen-statics-prep-fn (properties)
         (defblog/gen-statics-prep properties ,source-directory-var
           ,gen-directory-var ,publish-directory-var ,site-plist-var
           ,category-tags ,last-blog-update
           ,generate-xml-sitemap ,generate-rss ,generate-atom
           ,generate-htaccess ,default-author-name ,feed-entry-sunset-pred
           (symbol-name ',sitemap-default-change-freq)
           ,sitemap-default-priority))

       (defun ,posts-prep-fn (properties)
         (defblog/posts-prep ,site-plist-var ,category-tags ,gen-directory-var
                             ,source-directory-var #',post-copy-function))

       (defun ,pages-prep-fn (properties)
         (defblog/pages-prep ,site-plist-var ,category-tags
                             ,gen-directory-var
                             ,source-directory-var #',page-copy-function))

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
                    :base-directory (concatenate 'string
                                      ,gen-directory-var "front/")
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
              (list :preparation-function ',pages-prep-fn
                    :publishing-function 'org-html-publish-to-html
                    :base-directory ,get-pages-directory-var
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
                    :base-extension "html\\|css\\|jpg\\|gif\\|png\\|xml\\|pdf\\|el\\|gz\\|uu"
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
       (debug-msg (0 t) "Defined blog %s; use org-publish to generate" ',name))))

;;; =================================================================
;;; Preparing and cleaning temporary directories.

(defun ensure-ready-work-dir (pathname)
  "Prepare the temporary directory PATHNAME for blog generation."

  ;; If the directory already exists, remove its contents.
  (when (file-directory-p pathname)
    (dolist (item (directory-files pathname t))
      (cond
        ((string-match "/\\.\\.?$" item) nil) ;; . or .. --- skip
        ((file-directory-p item) (delete-directory item t))
        (t (delete-file item)))))

  ;; If the directory does not exist at all, create it.
  (unless (file-directory-p pathname)
    (make-directory pathname t)))

;;; =================================================================
;;; Preparing the hash tables and reference lists at the start of a
;;; blog build.
(defun defblog/table-setup-fn (properties gen-directory source-directory
                               file-plist-hash category-plist-hash
                               cat-list-setter cat-list-getter
                               last-post-setter site-plist-setter
                               site-title site-desc site-url)
  "Reset the global structures associated with a blog.
- PROPERTIES is the property list provided from ORG-PUBLISH.
- GEN-DIRECTORY is the root directory of the temporary files area
- FILE-PLIST-HASH is the hashtable from paths to ORG files, to the plist of
information extracted from that file.
- CAT-LIST-SETTER and CAT-LIST-GETTER are thunks which set (respectively, get)
the category list global variable for this blog."
  (declare (indent nil))
  (defblog/reset-categories-list source-directory cat-list-setter)
  (defblog/reset-categories-plist-hash source-directory
      (funcall cat-list-getter) category-plist-hash)
  (defblog/reset-file-plist-hash source-directory file-plist-hash
    category-plist-hash)
  (defblog/add-table-summary-data file-plist-hash category-plist-hash
    last-post-setter)
  (funcall site-plist-setter
           (list :file-plists-hash file-plist-hash
                 :cat-plists-hash category-plist-hash
                 :sorted-file-plists
                 (sort (hash-table-values file-plist-hash)
                       #'(lambda (x y)
                           (time-less-p (plist-get y :mod)
                                        (plist-get x :mod))))
                 :title site-title :desc site-desc :url site-url)))

;;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
;;; Managing the file-plist hashtable.

(defun defblog/fetch-file-plist (path file-plist-hash)
  (let ((result (gethash (intern path) file-plist-hash)))
    (debug-msg (3 :internal) "Cached %s --> %s" path result)
    result))

(defun defblog/reset-file-plist-hash (source-directory file-plist-hash
                                      category-plist-hash)
  "Set up the properties hash."
  (clrhash file-plist-hash)
  (let ((top-contents (directory-files source-directory)))
    (dolist (item top-contents)
      (let ((item-fullpath (concatenate 'string source-directory item)))
        (defblog/process-file-for-hash item nil 0 item-fullpath file-plist-hash
                                       category-plist-hash))))
  (debug-msg (3 :internal) "Finished property hash reset")
  )

(defun defblog/process-file-for-hash (bare-name cat-tag depth full-path
                                      file-plist-hash category-plist-hash)
  "Recursive function for populating the properties hash from a given file."
  (debug-msg (3 :internal) "Processing %s" full-path)
  (when (file-directory-p full-path)
      (setf full-path (concatenate 'string full-path "/")))
  (cond

    ;; If it's directory, recursively traverse that directory.
    ((and (file-directory-p full-path) (not (string-match "^\\." bare-name)))
     (debug-msg (3 :internal) "- Recurring for directory contents")
     (let ((dir-contents (directory-files full-path)))
       (dolist (dir-item dir-contents)
         (let ((dir-item-fullpath (concatenate 'string full-path dir-item)))
           (defblog/process-file-for-hash dir-item bare-name (+ 1 depth)
               dir-item-fullpath file-plist-hash category-plist-hash)))))

    ;; If it's an ORGMODE file, pull and cache its properties.
    ((string-match "\\.org$" bare-name)
     (multiple-value-bind (plist posted-date update-date)
         (defblog/build-file-plist bare-name cat-tag full-path depth)

       (debug-msg (3 :internal) "- Caching %s --> %s" full-path plist)
       (puthash (intern full-path) plist file-plist-hash)

    ;; When debugging we may want to know about this fall-through.
    ;; (t (debug-msg (0 t) "- No action for %s" full-path))
    ))))

(defun defblog/build-file-plist (bare-file cat-tag path depth)
  "Extract a list of the properties we need from the file at the given PATH.
- BARE-FILE and PATH should refer to the same file; the former excludes all
  surrounding directories.
- CAT-TAG is either the category of a post, or NIL for pages.  DEPTH is the
  number of subdirectories from the top level where the file lives:
  currently zero for pages, one for posts."
  (debug-msg (3 :internal) "* Start defblog/build-file-plist %s" path)

  (let ((keyvals (defblog/get-orgfile-properties path)))
    (debug-msg (3 :internal) "  keyvals %s" keyvals)

    (multiple-value-bind (result posted-date update-date)
        (defblog/format-orgprops-plist bare-file cat-tag path depth keyvals)

      (debug-msg (3 :internal) "  result %s" result)
      (values result posted-date update-date))))

(defun defblog/format-orgprops-plist (bare-file cat-tag path depth keyvals)
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
                         (t nil)))
         (post-mod (cond
                     ((and (null post-date) (null post-updated))
                      +web-announcement-date+)
                     ((null post-date) post-updated)
                     ((null post-updated) post-date)
                     ((time-less-p post-date post-updated) post-updated)
                     (t post-date))))
    (values (list :bare bare-file :path path :depth depth
                  :title (nth 1 (assoc "TITLE" keyvals))
                  :desc (nth 1 (assoc "DESCRIPTION" keyvals))
                  :link (nth 1 (assoc "LINK" keyvals))
                  :cat cat-tag
                  :author-name (nth 1 (assoc "AUTHOR_NAME" keyvals))
                  :date post-date :updated post-updated
                  :old-urls (when (its (nth 1 (assoc "OLD_URL" keyvals)))
                              (split-string (it) " *, *"))
                  :mod post-mod
                  :sitemap-priority priority :change-freq change-freq)
            post-date post-updated)))

(defun defblog/reset-categories-list (source-directory cat-list-setter)
  (let ((category-tag-list nil))
    (debug-msg (3 :internal) "srcdir %s" source-directory)
    (debug-msg (3 :internal) "items %s" (directory-files source-directory))

    ;; Look at each file in the source directory.
    (dolist (item (directory-files source-directory))
      (debug-msg (3 :internal) "Checking %s" item)

      ;; We are skipping any dotfiles
      (unless (string-match "^\\." item)
        (debug-msg (3 :internal) "- not a dotfile")
        (let ((cat-dir-path (concatenate 'string source-directory item "/")))

          ;; We are also only looking at directories
          (when (file-directory-p cat-dir-path)
            (debug-msg (3 :internal) "- is a directory")

            ;; Make sure there is a category.txt file in this
            ;; directory.
            (let ((cat-path (concatenate 'string cat-dir-path "category.txt")))
              (when (file-regular-p cat-path)

                ;; Add the tag to the result list
                (debug-msg (3 :internal) "- include %s in category list" item)
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
           (posts-list (filter #'(lambda (n) (string-match "\\.org$" n))
                               (directory-files cat-src-dir)))
           (keyvals (defblog/get-orgfile-properties full-path)))

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
        (debug-msg (3 :internal) "%s\n  %s %s\n  %s %s"
                   full-path cat-tag keyvals (intern cat-tag) plist)
        (puthash (intern cat-tag) plist category-plist-hash)
        (debug-msg (3 :internal) "  %s"
                   (gethash (intern cat-tag)
                            +defblog/maraist/category-plists-hash+))))))

;;; =================================================================
;;; Crossreferencing information built into the hashtables.

(defun defblog/add-table-summary-data (file-plist-hash category-plist-hash
                                       last-post-setter)
  "Calculate additional summary information for the plist tables.
- FILE-PLIST-HASH (respectively CATEGORY-PLIST-HASH) maps absolute pathnames
\(category names) to their property list."
  (let ((last-blog-update +web-announcement-date+))
    (dolist (cat (hash-table-keys category-plist-hash))
      (debug-msg (3 :internal) "Crossreferencing for category %s" cat)

      (let ((cat-plist (gethash cat category-plist-hash))
            (latest-post +web-announcement-date+)
            (latest-update +web-announcement-date+))
        (with-plist-properties ((cat-src-dir :src-dir)
                                (cat-post-files :post-files))
            cat-plist
          (debug-msg (3 :internal) "- Has plist %s" cat-plist)

          (dolist (post-file cat-post-files)
            (debug-msg (3 :internal) "  - For file %s" post-file)
            (let* ((file-fullpath (concatenate 'string cat-src-dir post-file))
                   (file-plist (gethash (intern file-fullpath)
                                        file-plist-hash)))
              (with-plist-properties ((file-posted :date)
                                      (file-updated :updated))
                  file-plist
                (debug-msg (3 :internal) "    %s" file-fullpath)
                (debug-msg (3 :internal) "    %s" file-plist)
                (debug-msg (3 :internal) "    %s %s" file-posted file-updated)
                (when (and (time-less-p latest-post file-posted) file-posted)
                  (debug-msg (3 :internal) "    Updating latest post time")
                  (setf latest-post file-posted))
                (when (and file-updated
                           (time-less-p latest-update file-updated))
                  (debug-msg (3 :internal) "    Updating latest update time")
                  (setf latest-update file-updated)))))

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
            (setf last-blog-update latest-update)))))
    (funcall last-post-setter last-blog-update)))

;;; =================================================================
;;; Generating non-ORG/HTML files.

(defun defblog/gen-statics-prep (properties source-directory
                                 gen-directory pub-directory site-plist
                                 category-tags
                                 last-update generate-xml-sitemap
                                 generate-rss generate-atom generate-htaccess
                                 default-author-name feed-entry-sunset-pred
                                 default-change-freq default-priority)
  "Generate XML and other non-ORG/HTML files.

These files should be written to the gen-statics subdirectory of
the temporary files workspace.
- PROPERTIES is as specified in org-publish."

  (when generate-rss
    (defblog/write-rss properties site-plist source-directory gen-directory
                       category-tags last-update feed-entry-sunset-pred))

  (when generate-atom
    (defblog/write-atom properties site-plist source-directory gen-directory
                        category-tags last-update default-author-name
                        feed-entry-sunset-pred))

  (when generate-xml-sitemap
    (defblog/write-xml-sitemap properties site-plist gen-directory
                               default-change-freq default-priority))

  (when generate-htaccess
    (defblog/write-htaccess properties site-plist pub-directory)))

;;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
;;; Generating an HTACCESS file for forwards.

(defun defblog/write-htaccess (properties site-plist pub-directory)
  "Write a .htaccess file for URL forwarding."

  (with-plist-properties ((file-plist-hash :file-plists-hash)
                          (cat-plist-hash :cat-plists-hash)
                          (blog-url :url))
      site-plist

    ;; Set up buffer to be written to the target file.
    (with-temp-file (concatenate 'string pub-directory ".htaccess")

      ;; Check each file in the file plist hash.
      (dolist (file-plist (hash-table-values file-plist-hash))
        (with-plist-properties ((cat :cat)
                                (bare :bare)
                                (old-links :old-urls))
            file-plist

          ;; One Redirect per old link to this file.
          (dolist (old-link old-links)
            (insert "Redirect "
                    (replace-regexp-in-string "https?://[^/]+" "" old-link)
                    " "
                    blog-url
                    (cond (cat (concatenate 'string cat "/")) (t ""))
                    (replace-regexp-in-string
                     "/index.html$" "/"
                     (replace-regexp-in-string "\\.org$" ".html" bare))
                    "\n")))))))

;;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
;;; Generating the XML sitemap

(defun defblog/write-xml-sitemap (properties site-plist gen-directory
                                  default-change-freq default-priority)
  "Generate an XML sitemap for a blog.
- PROPERTIES are from org-publish.
- GEN-DIRECTORY is the absolute path to the scratch space directory.
- CATEGORY-TAGS, FILE-PLIST-HASH and CAT-PLIST-HASH are the internal data
structures of the blog artifacts."

  (debug-msg (3 :internal) "Generating XML sitemap")
  (with-plist-properties ((file-plist-hash :file-plists-hash)
                          (cat-plist-hash :cat-plists-hash)
                          (site-url :url))
      site-plist
    (let ((gen-basedir (concatenate 'string gen-directory "gen-statics/")))
      (with-temp-file (concatenate 'string gen-basedir "sitemap.xml")
        (insert
         "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
         "<urlset xmlns=\"http://www.sitemaps.org/schemas/sitemap/0.9\">\n")

        ;; First process the zero-depth files.
        (dolist (file-plist (hash-table-values file-plist-hash))
          (when (zerop (plist-get file-plist :depth))
            (debug-msg (3 :internal) "Processing zero-depth %s"
              (plist-get file-plist :path))
            (let* ((page-url (replace-regexp-in-string
                              "/index.html$" "/"
                              (replace-regexp-in-string
                               "\\.org$" ".html"
                               (concatenate 'string
                                 site-url (plist-get file-plist :bare))))))
              (defblog/write-xml-sitemap-entry page-url
                  (plist-get file-plist :date) (plist-get file-plist :updated)
                  (plist-get file-plist :change-freq) nil default-change-freq
                  (plist-get file-plist :priority) nil default-priority))))
        (dolist (cat-plist (hash-table-values cat-plist-hash))
          (let ((cat-tag (plist-get cat-plist :tag)))
            (defblog/write-xml-sitemap-entry
                (concatenate 'string site-url cat-tag "/")
                (plist-get cat-plist :latest-post) nil
                nil (plist-get cat-plist :change-freq) default-change-freq
                nil (plist-get cat-plist :sitemap-priority) default-priority)

            (dolist (file (plist-get cat-plist :post-files))
              (debug-msg (3 :internal) "File %s" file)
              (let* ((file-fullpath (concatenate 'string
                                      (plist-get cat-plist :src-dir) file))
                     (file-plist (gethash (intern file-fullpath)
                                          file-plist-hash)))
                (debug-msg (3 :internal) "     plist %s" file-plist)
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
                    default-change-freq
                    (plist-get file-plist :sitemap-priority)
                    (plist-get cat-plist :sitemap-priority)
                    default-priority))))))
        (insert "</urlset>\n")))))

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
              (blog-default-priority (format "%f" blog-default-priority))
              (t "0.5"))
            "</priority>\n"
            "  </url>\n")))

;;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
;;; Writing RSS feeds

(defun defblog/write-rss (properties site-plist source-directory gen-directory
                          category-tags blog-last-mod feed-entry-sunset-pred)
  "Write RSS files for the overall site and for each post category.
- PROPERTIES are from org-publish.
- SOURCE-DIRECTORY (respectively GEN-DIRECTORY) is the absolute path
to the blog source (scratch space) directory.
- CATEGORY-TAGS, FILE-PLIST-HASH and CAT-PLIST-HASH are the internal data
structures of the blog artifacts.
- BLOG-NAME, BLOG-DESC and BLOG-URL are strings describing the blog itself."
  (with-plist-properties ((file-plist-hash :file-plists-hash)
                          (cat-plist-hash :cat-plists-hash)
                          (blog-name :title)
                          (blog-desc :desc)
                          (blog-url :url))
      site-plist
    (let ((gen-basedir (concatenate 'string gen-directory "gen-statics/")))

      ;; First write the RSS files for each category.
      (dolist (category-tag category-tags)
        (let* ((cat-out-dir (concatenate 'string gen-basedir category-tag "/"))
               (cat-properties (gethash (intern category-tag) cat-plist-hash))
               (cat-html-url (concatenate 'string blog-url category-tag "/")))
          (unless (file-directory-p cat-out-dir)
            (make-directory cat-out-dir))
          (with-temp-file (concatenate 'string cat-out-dir "rss.xml")
            (defblog/write-rss-opening (concatenate 'string
                                         blog-name ": "
                                         (plist-get cat-properties :title))
                (plist-get cat-properties :description)
              (concatenate 'string cat-html-url "rss.xml")
              cat-html-url
              (plist-get cat-properties :latest-mod))
            (defblog/write-category-rss-entries category-tag cat-properties
              file-plist-hash blog-name blog-url
              gen-basedir feed-entry-sunset-pred)
            (defblog/write-rss-closing))))

      ;; Now write the main RSS file, with the entries from each category.
      (with-temp-file (concatenate 'string gen-basedir "rss.xml")
        (defblog/write-rss-opening blog-name blog-desc
          (concatenate 'string blog-url "rss.xml") blog-url blog-last-mod)
        (dolist (category-tag category-tags)
          (let ((cat-properties (gethash (intern category-tag) cat-plist-hash)))
            (defblog/write-category-rss-entries category-tag cat-properties
              file-plist-hash blog-name blog-url
              gen-basedir feed-entry-sunset-pred)))
        (defblog/write-rss-closing)))))

(defun defblog/write-category-rss-entries (category-tag cat-properties
                                           file-plist-hash blog-name blog-url
                                           gen-basedir feed-entry-sunset-pred)
  (let* ((cat-src-dir (concatenate 'string source-directory category-tag "/"))
         (post-fullpaths (file-expand-wildcards (concatenate 'string
                                                  cat-src-dir "*.org")))
         (file-plists (mapcar #'(lambda (p) (defblog/fetch-file-plist p
                                                file-plist-hash))
                              post-fullpaths)))

    (dolist (plist file-plists)
      (debug-msg (3 :internal) "- Considering post for RSS feed: %s (%s)"
                 (plist-get plist :bare)
                 (format-time-string "%d %b %Y" (plist-get plist :mod)))
      (when (funcall feed-entry-sunset-pred (plist-get plist :mod))
        (debug-msg (3 :internal) "  Added")
        (defblog/write-rss-for-plist plist cat-properties)))))

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
  (with-plist-properties ((title :title)
                          (bare :bare)
                          (date :date)
                          (desc :desc)
                          (quick-link :link))
      plist
    (let* ((this-link (cond
                        (quick-link quick-link)
                        (t
                         (concatenate 'string
                           "https://maraist.org/"
                           (plist-get category-properties :tag) "/"
                           (replace-regexp-in-string "\\.org$" ".html" bare))))))
      (insert "\n    <item>\n")
      (insert "      <title>"
              (cond (title title) (t "(untitled)"))
              "</title>\n")
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
      (insert "    </item>\n"))))


;;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
;;; Writing Atom feeds

(defun defblog/write-atom (properties site-plist source-directory gen-directory
                           category-tags blog-last-mod
                           default-author-name feed-entry-sunset-pred)
  "Write Atom files for the overall site and for each post category.
- PROPERTIES are from org-publish.
- SOURCE-DIRECTORY (respectively GEN-DIRECTORY) is the absolute path to
the blog source (scratch space) directory.
- CATEGORY-TAGS are internal data structures of the blog artifacts.
- BLOG-LAST-MOD is the date of its last change.
- DEFAULT-AUTHOR-NAME is required for the author tags in Atom.
- FEED-ENTRY-SUNSET-PRED describes when a post has aged out of inclusion in
the feed."

  (with-plist-properties ((file-plist-hash :file-plists-hash)
                          (cat-plist-hash :cat-plists-hash)
                          (blog-name :title)
                          (blog-desc :desc)
                          (blog-url :url))
      site-plist
    (let ((gen-basedir (concatenate 'string gen-directory "gen-statics/")))

      (with-temp-file (concatenate 'string gen-basedir "atom.xml")
        (defblog/write-atom-opening blog-name blog-desc
          (concatenate 'string blog-url "atom.xml") blog-url blog-last-mod)
        (dolist (category-tag category-tags)
          (let* ((cat-src-dir (concatenate 'string
                                source-directory category-tag "/"))
                 (post-fullpaths (file-expand-wildcards (concatenate 'string
                                                          cat-src-dir "*.org")))
                 (file-plists (mapcar #'(lambda (p) (defblog/fetch-file-plist p
                                                        file-plist-hash))
                                      post-fullpaths))
                 (cat-properties (gethash (intern category-tag) cat-plist-hash)))
            (dolist (plist file-plists)
              (defblog/write-atom-for-plist plist cat-properties
                default-author-name))))

        (defblog/write-atom-closing))

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

          (unless (file-directory-p cat-out-dir)
            (make-directory cat-out-dir))

          (with-temp-file (concatenate 'string cat-out-dir "atom.xml")

            (defblog/write-atom-opening cat-atom-title cat-desc
              cat-atom-url cat-html-url cat-last-mod-date)

            (dolist (plist file-plists)
              ;; Only add things from the last (let's say) five years.

              (debug-msg (3 :internal)
                  "- Considering post for Atom feed: %s (%s)"
                (plist-get plist :bare)
                (format-time-string "%d %b %Y"
                                    (plist-get plist :mod)))
              (when (funcall feed-entry-sunset-pred (plist-get plist :mod))
                (debug-msg (3 :internal) "  Added")
                (defblog/write-atom-for-plist plist cat-properties
                  default-author-name)))

            (defblog/write-atom-closing)))))))

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
  (with-plist-properties ((title :title)
                          (bare :bare)
                          (date :date)
                          (desc :desc)
                          (link :link)
                          (author :author-name))
      file-properties
    (let ((this-link (cond
                       (link link)
                       (t
                        (concatenate 'string
                          "https://maraist.org/"
                          (plist-get category-properties :tag) "/"
                          (replace-regexp-in-string "\\.org$" ".html"
                                                    bare))))))
      (insert "\n  <entry>\n")
      (insert "      <title>" (cond (title title) (t "(untitled)"))
              "</title>\n")
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
      (insert "    </entry>\n"))))

;;; =================================================================
;;; Copying posts into the tmp space

(defun defblog/posts-prep (site-plist cat-list gen-directory
                           source-directory post-copy-function)
  (with-plist-properties ((file-plist-hash :file-plists-hash)
                          (cat-plist-hash :cat-plists-hash))
      site-plist
    (dolist (cat cat-list)
      (let ((cat-src-dir (concatenate 'string source-directory cat "/"))
            (cat-tmp-dir (concatenate 'string
                           gen-directory "posts/" cat "/")))
        (when (file-directory-p cat-tmp-dir)
          (delete-directory cat-tmp-dir t))
        (make-directory cat-tmp-dir t)
        (dolist (file (plist-get (gethash (intern cat) cat-plist-hash)
                                 :post-files))
          (let ((cat-src-file (concatenate 'string cat-src-dir file))
                (cat-tmp-file (concatenate 'string cat-tmp-dir file)))
            (debug-msg (3 :internal) "%s %s" cat-src-file cat-tmp-file)
            (funcall post-copy-function cat-src-file cat-tmp-file
                     site-plist (gethash cat-src-file file-plist-hash))))))))

;;; =================================================================
;;; Copying posts into the tmp space


(defun defblog/pages-prep (site-plist cat-list gen-directory
                           source-directory page-copy-function)
  "Main function for installing non-index top-level pages.
- SITE-PLIST
- CAT-LIST
- GEN-DIRECTORY
- SOURCE-DIRECTORY PAGE-COPY-FUNCTION"
  (declare (indent 0))

  (with-plist-properties ((file-plist-hash :file-plists-hash)
                          (cat-plist-hash :cat-plists-hash))
      site-plist
    (debug-msg (1 t) "In defblog/pages-prep")
    (debug-msg (6 t) "- Categories: %s" cat-plist-hash)
    (debug-msg (4 t) "- gen-directory %s" gen-directory)
    (debug-msg (4 t) "- source-directory %s" source-directory)
    (let ((tmp-dir (concatenate 'string gen-directory "pages/")))
      (debug-msg (4 t) "- gen-directory %s" gen-directory)
      ;; Look at all of the file plists
      (dolist (plist (hash-table-values file-plist-hash))
        ;; Pages have depth zero
        (when (zerop (plist-get plist :depth))
          (let ((bare (plist-get plist :bare)))
            ;; Skip the index file; we treat it separately.
            (unless (string-match "index.org$" bare)
              (let ((src-file
                     (concatenate 'string source-directory bare))
                    (tmp-file (concatenate 'string tmp-dir bare)))
                (debug-msg (3 :internal) "%s %s" cat-src-file cat-tmp-file)
                (funcall page-copy-function src-file tmp-file
                         site-plist (gethash (intern src-file)
                                             file-plist-hash))))))))))

;;; =================================================================
;;; Building indices of posts in the tmp space

(defun defblog/cat-indices-prep (cat-list-getter site-plist
                                 gen-directory source-directory
                                 cat-indices-style-link
                                 cat-index-title-fn)
  "For the \"-cat-indices\" targets, generate category index ORG files.
These files should be written to the cat-indices subdirectory of the
temporary files workspace."
  (declare (indent 0))
  (debug-msg (3 :internal) "Called defblog/cat-indices-prep %s" (funcall cat-list-getter))

  (with-plist-properties ((file-plist-hash :file-plists-hash)
                          (cat-plist-hash :cat-plists-hash)
                          (blog-title :title))
      site-plist
    ;; For each category, and for its source and scratch directories,
    (dolist (cat (funcall cat-list-getter))
      (let* ((cat-src-dir (concatenate 'string source-directory cat "/"))
             (dest-dir (concatenate 'string
                         gen-directory "cat-indices/" cat "/"))
             (dest-org (concatenate 'string dest-dir "index.org"))

             (cat-plist (gethash (intern cat) cat-plist-hash))
             (cat-title (plist-get cat-plist :title)))
        (debug-msg (3 :internal) "Indexing category %s %s \"%s\"" cat cat-plist cat-title)

        ;; Make sure the destination directory exists, and is empty.
        (debug-msg (3 :internal) "Creating directory %s for %s" dest-dir cat-title)
        (when (file-directory-p dest-dir) (delete-directory dest-dir t))
        (make-directory dest-dir t)

        ;; Identify the ORG files in the source directory, and retrieve
        ;; their property lists.
        (let* ((all-files (directory-files cat-src-dir))
               (org-files
                (filter #'(lambda (x)
                            (and (string-match "\\.org$" x)
                                 (not (string-match "/index.org$" x))))
                        all-files)))
          (debug-msg (3 :internal) "L1 %s %s" org-files dest-org)

          (with-temp-file dest-org

            (insert "#+TITLE: " ; TODO Generalize the title.
                    (funcall cat-index-title-fn cat-plist blog-title)
                    "\n#+html_head:  "
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
              (debug-msg (3 :internal)
                  "Iterating through full-files %s\n  plists %s"
                full-files plists)
              (dolist (prop-list (sort plists sorter))
                (debug-msg (3 :internal) "Destructuring %s" prop-list)
                (with-plist-properties ((bare :bare)
                                        (path :path)
                                        (title :title)
                                        (desc :desc)
                                        (date :date)
                                        (link :link)
                                        (updated :updated))
                    prop-list
                  (debug-msg (3 :internal) "- bare %s title \"%s\" desc \"%s\"" bare title desc)
                  (when (or link bare)
                    (insert "- @@html:<a href=\""
                            (cond
                              (link link)
                              (t (replace-regexp-in-string "\\.org$" ".html"
                                                           bare)))
                            "\">"))
                  (insert (cond (title title) (t "(untitled)")))
                  (when (or link bare) (insert "</a>@@."))
                  (when desc (insert " " desc))
                  (when date
                    (insert (format-time-string " — /%B %d, %Y/" date)))
                  (when updated
                    (cond
                      (date (insert ", /updated "))
                      (t (insert "/Last updated ")))
                    (insert (format-time-string "%B %d, %Y/" updated)))
                  (when (or date updated) (insert "."))
                  (insert "\n"))))))))))

;;; =================================================================
;;; Debugging utilities

(defun defblog/state-dump (site-plist cat-list)
  (with-plist-properties ((file-plists-hash :file-plists-hash)
                          (cat-plists-hash :cat-plists-hash))
      site-plist
    (debug-msg (0 t) "--------------------")
    (debug-msg (0 t) "Site properties: %s\n" site-plist)
    (dolist (file (hash-table-keys file-plists-hash))
      (debug-msg (0 t) "%s\n ==> %s\n" file (gethash file file-plists-hash)))
    (debug-msg (0 t) "\nCategories: %s\n" cat-list)
    (debug-msg (0 t) "\nCat hash: %s\n" cat-plists-hash)
    (dolist (cat cat-list)
      (debug-msg (0 t) "%s\n ==> %s\n"
        cat (gethash (intern cat) cat-plists-hash)))
    (debug-msg (0 t) "--------------------")))

;;; =================================================================
;;; Miscellaneous utilities

(defun defblog/get-orgfile-properties (path)
  (with-temp-buffer
    (insert-file-contents path)
    (org-mode)
    (let ((parsed-buffer
           (org-element-parse-buffer 'greater-element)))
      (debug-msg (3 :internal) "  parsed-buffer %s" parsed-buffer)
      (org-element-map parsed-buffer '(keyword) #'defblog/kwdpair))))

(defun defblog/kwdpair (kwd)
  (let ((data (cadr kwd)))
    (list (plist-get data :key) (plist-get data :value))))

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

(defun take (n xs)
  "Classical take N function on a list XS."
  (cond
    ((zerop n) nil)
    ((null xs) nil)
    (t (cons (car xs) (take (- n 1) (cdr xs))))))

(defconst +defblog/scratch-subdirectories+
  '("cat-indices" "gen-statics" "posts" "pages" "front")
  "Scratch space subdirectories used internally by DEFBLOG.")

(defconst +web-announcement-date+
  (encode-time (list 20 56 14 6 8 1991 nil nil nil)) ;; August 6, 1991, 14:56:20 GMT
  "The date when Sir Tim announced the invention of the World-Wide Web.
Used as an earliest-possible post- or updated-date for pages and posts.")

;;; =================================================================
;;; Various functions available as arguments to DEFBLOG.

(defun defblog/page-copy-verbatim (src-path dest-path
                                   site-properties page-properties)
  "Function which only copies in page source.
Can be used as the :FRONT-COPY-FUNCTION and :POST-COPY-FUNCTION arguments."
  (copy-file src-path dest-path))

(defun defblog/page-copy-with-substitutions (src-path dest-path
                                             site-properties page-properties)
  "Page source copying function which injects text for certain Org comments.
Can be used as the :FRONT-COPY-FUNCTION and :POST-COPY-FUNCTION arguments.

This processor copies most text verbatim, but rewrites certain expressions and
pragmas.  Currently there are two substitutions:

- If a line starts

  # RECENT-POST-LINKS

  then this line will be replaced with information from recent posts.
  The substitution is controlled by the following file properties
  which can vary from file to file:

   - PAGE_SUBST_POSTS_MAX gives the maximum number of posts which
     will be included.  By default this value is nil, signifying no
     maximum.

   - PAGE_SUBST_POSTS_MAX_AGE_DAYS,
     PAGE_SUBST_POSTS_MAX_AGE_MONTHS, and
     PAGE_SUBST_POSTS_MAX_AGE_YEARS give the maximum age of post
     which should be included.  At most one of these properties
     will be consulted: if DAYS is given, then it is used;
     otherwise MONTHS; and then YEARS.

   - PAGE_SUBST_POSTS_EARLIEST This property is checked only when
     the PAGE_SUBST_POSTS_MAX_AGE_* properties are not given, and
     gives the date of the earliest last-modified post which can
     appear.  The string given for this property is converted to
     a date with parse time string.

   - PAGE_SUBST_POSTS_INDENT describes the indentation associated
     with each post reference.

      - If this property is a number N, then N spaces will precede
        the information printed for each reference.

      - If this property is nil, then no prefix is printed.

        - Otherwise, the string given for this property is used.

   - PAGE_SUBST_POSTS_NEWLINES If this property is non-null, then a
     newline is started after the information printed for each
     reference.

   - PAGE_SUBST_POSTS_FINAL_NEWLINE If this property is non-null but
     PAGE_SUBST_POSTS_NEWLINES is null, then a newline will be
     inserted after the last reference.

   - PAGE_SUBST_POSTS_FORMAT_STRING This format string describes
     what should be printed for each reference.  In the usual manner
     for format strings, most characters are echoed verbatim, %%
     translates to %, and

      - %L is replaced with the opening of a link to the post.
          - %Z is replaced with the closing of a link to the post.
      - %t is replaced with the title of the post.
      - %T is replaced with the title of the post, capitalizing the
        first letter if it is not already capitalized.
      - %d is replaced with the description of the post.
      - %M is replaced with the full name of the month of the last
        update to the post.
      - %D is replaced with the number of the day of month
        (space-padded) of the last update to the post.
      - %Y is replaced with the year (including century) of the last
        update to the post.

         The default is \"%L%T%Z (%M %D, %Y)\".

   - PAGE_SUBST_POSTS_NUMBERED If this property is non-null, then
     the references are inserted as a numbered list; otherwise they
     are inserted as a non-numbered list.

- If a line starts

  # CATEGORY-LINKS

  then this line will be replaced with several lines of links to
  category index pages.  Right now the substitution produces a
  non-numbered list; future work will allow controlling the list with
  file properties, as for the recent posts pragma."
  (debug-msg (3 :internal)
             "Called page-copy-with-substitutions for %s"
             (plist-get page-properties :path))

  ;; First read in the source file as a list of lines.
  (let ((source-lines (with-temp-buffer
                        (insert-file-contents src-path)
                        (split-string (buffer-string) "\n" t))))

    ;; Now prepare the output file.
    (debug-msg (3 :internal) "Opening %s for writing" dest-path)
    (with-temp-file dest-path

      ;; Line-by-line, perform substitutions in the original, write to
      ;; the destination.
      (dolist (line source-lines)
        (cond

          ;; Insert the last few new posts.
          ((string-match "^# CATEGORY-LINKS\\(.*\\)$" line)
           (insert "# Inserting category links\n")
           (let ((category-plist-hash
                  (plist-get site-properties :cat-plists-hash)))
             (dolist (cat-plist
                      (sort
                       (hash-table-values category-plist-hash)
                       #'(lambda (x y)
                           (string-lessp (plist-get x :title)
                                         (plist-get y :title)))))
               (with-plist-properties ((tag :tag)
                                       (title :title))
                    cat-plist
                 (insert "- [[./" tag "/][" title "]]\n")))))

          ;; Insert the last few new posts.
          ((string-match "^# RECENT-POST-LINKS\\(.*\\)$" line)
           (destructuring-bind (max indent newlines final-newline
                                    earliest format-string numbered)
               (defblog/page-subst-recent-posts-args src-path)

             (debug-msg (3 :internal) "Earliest: %s %s" earliest
                        (format-time-string +rfc-3339-time-format+ earliest))
             (let* ((all-posts (plist-get site-properties
                                          :sorted-file-plists))
                    (selected
                     (filter #'(lambda (pl)
                                 (time-less-p earliest (plist-get pl :mod)))
                             (cond
                               ((numberp max) (take max all-posts))
                               (t all-posts))))

                    (prefix (cond
                              ((stringp indent) indent)
                              ((numberp indent)
                               (concatenate 'string
                                 (make-string indent (char-from-name "SPACE"))
                                 (if numbered "1. " "- ")))
                              (t (if numbered "1. " "- ")))))
               (debug-msg (3 :internal) "- Selected posts: %s" selected)
               (dolist (post selected)
                 (debug-msg (3 :internal) "- Writing for post: %s" post)
                 (insert prefix)
                 (defblog/insert-formatted-page-plist format-string
                     site-properties post)
                 (when newlines (insert "\n")))

               (when (and final-newline (not newlines))
                 (insert "\n")))))

          ;; Default case: just insert the line
          (t (debug-msg (0 t) "- Regular line")
             (insert line "\n"))))
      (debug-msg (3 :internal) "Wrote lines"))))

(defun defblog/insert-formatted-page-plist (format-string site-properties
                                            page-properties)
  "Format a POST property list according to the FORMAT-STRING.
As with other instances of format strings, the percent (%) character is special:
- %L is replaced with the opening of a link to the post.
- %Z is replaced with the closing of a link to the post.
- %t is replaced with the title of the post.
- %T is replaced with the title of the post, capitalizing the first letter if
it is not already capitalized.
- %d is replaced with the description of the post.
- %M is replaced with the full name of the month of the last update to
the post.
- %D is replaced with the number of the day of month (space-padded) of
the last update to the post.
- %Y is replaced with the year (including century) of the last update to
the post.
and %% is replaced by a single %."
  (debug-msg (3 :internal) "Called defblog/insert-formatted-page-plist for %s" page-properties)
  (let ((next-char 0)
        (end-char (length format-string)))
    (while (< next-char end-char)
      (let ((c (aref format-string next-char)))
        (debug-msg (3 :internal) "Processing character %d %s" next-char (char-to-string c))
        (case c
          ((?%)
           (debug-msg (3 :internal) "- Is %%")
           (incf next-char)
           (unless (< next-char end-char)
             (error "Trailing %% in format string \"%s\"" format-string))
           (case (aref format-string next-char)
             ((?L) (debug-msg (0 t) "- Is %%L")
              (with-plist-properties ((url :url)) site-properties
                (with-plist-properties ((cat :cat) (link :link))
                    page-properties
                  (let* ((bare (replace-regexp-in-string
                                "\\.org$" ".html"
                                (plist-get page-properties :bare))))
                    (debug-msg (0 t) "LINK %s -> %s" bare link)
                    (cond
                      (link (setf url link))
                      (t
                       (unless (string-match "/$" url)
                         (setf url (concatenate 'string url "/")))
                       (when cat (setf url (concatenate 'string url cat "/")))
                       (setf url (concatenate 'string url bare))))
                    (debug-msg (0 t) "- url is %s" url)
                    (insert "[[" url "][")
                    (debug-msg (3 :internal) "- Inserting URL %s" url)))))
             ((?Z) (debug-msg (0 t) "- Is %%Z")
              (insert "]]"))
             ((?t) (debug-msg (0 t) "- Is %%t")
              (insert (plist-get page-properties :title)))
             ((?T) (debug-msg (0 t) "- Is %%T")
              (let ((title (plist-get page-properties :title)))
                (when (and title (> (length title) 0))
                  (let ((first (seq-elt title 0))
                        (rest (seq-drop title 1)))
                    (insert (char-to-string (capitalize first)) rest)))))
             ((?d) (debug-msg (0 t) "- Is %%d")
              (insert (plist-get page-properties :desc)))
             ((?M) (debug-msg (0 t) "- Is %%M")
              (insert (format-time-string "%B"
                                          (plist-get page-properties :mod))))
             ((?D)
              (debug-msg (0 t) "- Is %%D")
              (insert (format-time-string "%e"
                                          (plist-get page-properties :mod))))
             ((?Y)
              (debug-msg (0 t) "- Is %%Y")
              (insert (format-time-string "%Y"
                                          (plist-get page-properties :mod))))
             (otherwise
              (error "Unrecognized character '%s' in format string \"%s\""
                     (char-to-string c) format-string))))
          (otherwise (insert (char-to-string c)))))
      (incf next-char))))

(defun defblog/page-subst-recent-posts-args (src-path)
  (let* ((keyvals (defblog/get-orgfile-properties src-path))

         (max (when (its (nth 1 (assoc "PAGE_SUBST_POSTS_MAX" keyvals)))
                (string-to-number (it))))
         (indent (cond
                   ((its (nth 1 (assoc "PAGE_SUBST_POSTS_INDENT"
                                       keyvals)))
                    (cond
                      ((string-match "^[0-9]+$" (it))
                       (string-to-number (it)))
                      ((string= "nil" (it)) nil)
                      (t (it))))
                   (t 0)))
         (newlines (cond
                     ((its (nth 1 (assoc "PAGE_SUBST_POSTS_NEWLINES"
                                         keyvals)))
                      (not (string= (it) "nil")))
                     (t t)))
         (final-newline (cond
                          ((its (nth 1 (assoc
                                        "PAGE_SUBST_POSTS_FINAL_NEWLINE"
                                        keyvals)))
                           (not (string= (it) "nil")))
                          (t nil)))
         (format-string (cond
                          ((its
                            (nth 1 (assoc "PAGE_SUBST_POSTS_FORMAT_STRING"
                                          keyvals)))
                           (it))
                          (t "%L%T%Z (%M %D, %Y)")))
         (numbered (cond
                     ((its (nth 1 (assoc "PAGE_SUBST_POSTS_NUMBERED"
                                         keyvals)))
                      (not (string= (it) "nil")))
                     (t nil)))

         (earliest-string (nth 1 (assoc "PAGE_SUBST_POSTS_EARLIEST"
                                        keyvals)))
         (max-age-days
          (when (its (nth 1 (assoc "PAGE_SUBST_POSTS_MAX_AGE_DAYS"
                                   keyvals)))
            (string-to-number (it))))
         (max-age-months
          (when (its (nth 1 (assoc "PAGE_SUBST_POSTS_MAX_AGE_MONTHS"
                                   keyvals)))
            (string-to-number (it))))
         (max-age-years
          (when (its (nth 1 (assoc "PAGE_SUBST_POSTS_MAX_AGE_YEARS"
                                   keyvals)))
            (string-to-number (it))))

         ;; Work out the earliest last-modified date/time for
         ;; which we would actually include a post.
         (earliest
          (cond
            ;; Use a number of days before today if it is given.
            (max-age-days
             (debug-msg (3 :internal) "Using max-age-days %s" max-age-days)
             (encode-time
              (decoded-time-add
               (decode-time (current-time))
               (make-decoded-time :second 0 :minute 0 :hour 0
                                  :day (- max-age-days)
                                  :month 0 :year 0 :zone 0))))
            ;; Otherwise use a number of months.
            (max-age-months
             (debug-msg (3 :internal) "Using max-age-months %s" max-age-months)
             (its (encode-time
                   (decoded-time-add
                    (decode-time (current-time))
                    (make-decoded-time :second 0 :minute 0 :hour 0 :day 0
                                       :month (- max-age-months)
                                       :year 0 :zone 0))))
             (debug-msg (3 :internal) "- %s" (it))
             (it))
            ;; Otherwise use a number of years.
            (max-age-years
             (debug-msg (3 :internal) "Using max-age-years %s" max-age-years)
             (its
              (encode-time
               (decoded-time-add
                (decode-time (current-time))
                (make-decoded-time :second 0 :minute 0 :hour 0
                                   :day 0 :month 0
                                   :year (- max-age-years) :zone 0))))
             (debug-msg (3 :internal) "- %s" (it))
             (it))
            ;; Otherwise parse a date string with an absolute
            ;; earliest point.
            (earliest-string
             (debug-msg (3 :internal) "Using earliest-string")
             (parse-time-string earliest-string))
            ;; Otherwise use the date of the announcement of the
            ;; web.
            (t
             (debug-msg (3 :internal) "Using +web-announcement-date+")
             +web-announcement-date+)))

         (result (list max indent newlines final-newline earliest
                       format-string numbered)))
    result))

(provide 'defblog)
;;; defblog ends here

