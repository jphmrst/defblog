# defblog — Web sites as Org-mode files

Declare a simple structured blog to be published with `org-publish`.
This package offers an all-Emacs solution to maintaining a web site
(except for uploading via rsync, which `defblog` triggers through
`org-publish`).

## Version

This file describes `defblog` version 0.2.0.

## Requirements

Requires Emacs 27.1 or later, specifically for the `make-decoded-time`
function introduced with that release.

Also requires the `anaphora-anywhere` library [available
here](https://github.com/jphmrst/anaphora-anywhere).

## File structure

To use `defblog`, you must structure your Org files as follows:
- The `index.org` file, if it exists, is the front page of the blog.
- The other top level `*.org` pages correspond to individual undated 
pages (i.e., not posts).
- Each directory within the source directory which contains a file
`category.txt` corresponds to a category of posts.  The `category.txt`
files will be loaded as Org-mode files, although including first-line
Org-magic is optional.
- Category directories should not contain an index.org file!  An index
of posts will be generated, and this file will be ignored.
- Otherwise, each ORG file in a category directory corresponds to 
a blog post.

So if the source directory is /DIR/TO/SRC, then a possible
directory layout is:

    /DIR/TO/SRC/
      |
      +-- index.org    Top-level page
      |
      +-- style.css    Style sheet for the generated pages
      |
      +-- contact.org  A page of contact information
      |
      +-- jokes.org    A page of favorite jokes
      |
      +-- kittens      A subdirectory corresponding to a category of 
           |           posts.
           |
           +-- category.txt   Properties of the "kittens" category.
           |
           +-- adopted.org    A post about adopting a kitten
           |
           +-- vet-jul20.org  A post about a trip to the vet 
                              in July 2020.

## Calling `defblog`

Required parameters:

- `name`, a string used to identify this blog.  This `name` is used by
  `org-publish` to publish this particular blog, and it is also used
  to name generated storage locations so that they do not conflict
  with the names used for other blogs.

- `source-directory`, a string giving the absolute pathname of the
  source directory of Org- and other files for this site.

- `blog-title`, a string with the human-oriented name for the web
  site.

Optional parameters:

- `blog-url` gives the URL for the top of this blog.  This value is
  required for most XML artifacts.

- By default, `defblog` will use a temporary directory named by the
  system for both the intermediate file areas, and for the directory
  of final files to be published.  However it possible to name
  specific directories for these uses with the `published-directory`
  and `generated-directory` arguments.

- By default, `defblog` will delete the `published-directory` and
  `generated-directory` after it finishes.  These deletions can be
  suppressed by passing non-null values to the
  `retain-published-directory` and `retain-generated-directory`
  arguments.

- The `:postdate-policy` specifies how `defblog` will treat posts and
  pages whose date is later than the current time when the site is
  published.  The default value is `:hide`, meaning that these pages
  should not be included or indexed in the published site.  The other
  valid setting for this option is `:show`, meaning that these pages
  should be included and indexed.

- The `css-style-rel-path` argument specifies the local path from the
  `source-directory` to the default CSS stylesheet for the blog.

- The `frontpage-css-style-rel-path`, `page-css-style-rel-path`,
  `post-css-style-rel-path` and `category-index-css-style-rel-path`
  arguments specify local paths from the `source-directory` to the CSS
  stylesheets for those groups of pages.  If not given, these
  arguments take the value of `css-style-rel-path`.  For any of these,
  a NIL value means there should be no CSS style sheet.
- Org-mode can generate section numbering and a table of contents when
  converting Org files to HTML.  The `frontpage-section-numbers`,
  `page-section-numbers`, `post-section-numbers`,
  `category-index-section-numbers`, `frontpage-table-of-contents`,
  `page-table-of-contents`, `post-table-of-contents` and
  `category-index-table-of-contents` arguments control these options
  for the various classes of document.  Their values are checked for a
  `null` or non-`null` setting.

- The `post-copy-function`, `page-copy-function` and
  `front-copy-function` arguments specify how a file in the temporary
  directories are made from the various source Org files.  The default
  is `defblog/page-copy-verbatim`, which simply copies the source
  file.  Other options, and the protocol for writing a custom
  function, are discussed in the *Processing files* section below.

- The `generate-xml-sitemap`, `generate-rss` and `generate-atom`
  arguments indicate whether the published blog should include these
  XML artifacts.  
  
  For the RSS and Atom feeds, the `feed-entry-sunset` argument gives
  the length of time that a post should be included in any XML feed (RSS
  or Atom).  The value may be:

   1. An Emacs Lisp time value (used as-is: the age of a post
      calculated via `time-subtract`, and compared to this upper
      bound).
   2. A Lisp list (passed as arguments to `make-decoded-time`, whose
      result is used as a sunset bound as above).
   3. A string (passed to `parse-time-string`, and used as an absolute
      limit of the earliest date included.
  
  For the Atom feed, the `default-author-name` argument gives a value
  for the default article attribution; this can be overridden on a
  post-by-post basis with the file properties (below).
  
  For the XML sitemap, the `sitemap-default-change-freq` and
  `sitemap-default-priority` give default values for pages which do
  not set a change frequency or priority in their file properties.  By
  default their values are `monthly` and `0.5`.  Priority values
  should be between 0.0 and 1.0 (inclusively), 1.0 being the most
  important.  Other valid values for the change frequency are
  `always`, `hourly`, `daily`, `weekly`, `yearly` and `never`.
  
  The RSS and Atom feeds are validated via
  [validator.w3.org/feed](https://validator.w3.org/feed/). The XML
  sitemap is validated via
  [xml-sitemaps.com/validate-xml-sitemap.html](https://www.xml-sitemaps.com/validate-xml-sitemap.html).
  
- The `upload` argument specifies whether `defblog` should upload the
  website files it produces to a remote server.  Its two current valid
  values are

  - `nil`, indicating that no upload should occur.
  - `:rsync`, indicating an upload via the `rsync` protocol.
  
  There are four additional options for `rsync`:
  
  - The `rsync-rsh` argument specifies the protocol provided for the
    `--rsh` option to `rsync`.  If `:rsync` is selected, this value is
    required.

  - The `rsync-dest` argument specifies the remote machine and
    directory for the file transfer.  This value is provided as the
    final argument to the `rsync` call.  If `:rsync` is selected, this
    value is required.

  - The `rsync-delete-excluded` argument, if non-null, requests the
    `--delete-excluded` switch for `rsync`.  This argument is
    optional, and is set to `t` by default.

  - The `rsync-options` argument should be a list of strings passed as
    additional options to `rsync`.  This argument is optional, and is
    set to `("-rLptgoD")` by default.

## File properties

This section presents the Org file properties of pages, posts, and
category specifications that `defblog` reads and uses.

### Properties applied from any file

These properties are meaningful in both page/post files, and in
`category.txt` files.

- `TITLE` The title of the page/post, or the name of the category.
  Note that org-publish will place this title as the headline of the
  HTML it generates.
- `DESCRIPTION` A short blurb of the contents.
- `CHANGE_FREQ` A hint about how often a page/post changes, to be
  included in the XML sitemap.  Valid values are discussed at
  [[https://www.sitemaps.org/protocol.html#changefreqdef]].
- `SITEMAP_PRIORITY` A relative assessment of the importance of
  crawling a page/post, compared to other pages/posts on this site.
  Should be a floating-point value between 0.0 and 1.0, with 1.0
  having the highest priority.

### Properties applied from page/post files only

The properties in this section are not meaningful in `category.txt`
files.  Date information about categories is synthesized from the
posts in that category)

- `DATE` The publication date of the page/post.
- `UPDATED` The last change of the page/post.
- `AUTHOR_NAME` The name of the author of the given post/page.  When
  an Atom feed is generated, either this property must be given for
  every post/page, or a non-null value for the DEFAULT-AUTHOR-NAME
  parameter must be provided to DEFBLOG.
- `DRAFT` If this property is set, then the page is considered a
  draft, not to be exported for publication.

There are additional page/post file properties applicable only to the
`defblog/page-copy-with-substitutions` processing function, which are
described below.

## Processing files

`defblog` transfers Org source files into temporary directories, from
which `org-publish` converts them into HTML and other files.  By
default the files are simply copied, but it is also possible to
transform the file contents based on pragmas in the source.

In addition to the `defblog/page-copy-verbatim` function for a simple
verbatim copy, the `defblog/page-copy-with-substitutions` function can
be passed for either or both of the `post-copy-function` and
`front-copy-function` keyword arguments to `defblog`.  This processor
also copies most text verbatim, but rewrites certain expressions and
pragmas.  Currently there are two substitutions:

- If a line starts

  ```lisp
  # RECENT-POST-LINKS
  ```

  then this line will be replaced with information from recent posts.
  The substitution is controlled by the following file properties
  which can vary from file to file:

   - `PAGE_SUBST_POSTS_MAX` gives the maximum number of posts which
     will be included.  By default this value is `nil`, signifying no
     maximum.

   - `PAGE_SUBST_POSTS_MAX_AGE_DAYS`,
     `PAGE_SUBST_POSTS_MAX_AGE_MONTHS`, and
     `PAGE_SUBST_POSTS_MAX_AGE_YEARS` give the maximum age of post
     which should be included.  At most one of these properties will
     be consulted: if `DAYS` is given, then it is used; otherwise
     `MONTHS`; and then `YEARS`.

   - `PAGE_SUBST_POSTS_EARLIEST` This property is checked only when
     the `PAGE_SUBST_POSTS_MAX_AGE_*` properties are not given, and
     gives the date of the earliest last-modified post which can
     appear.  The string given for this property is converted to a
     date with `parse-time-string`.

   - `PAGE_SUBST_POSTS_INDENT` describes the indentation associated
     with each post reference.
     
      - If this property is a number `n`, then `n` spaces will precede
        the information printed for each reference.
        
      - If this property is `nil`, then no prefix is printed.

      - Otherwise, the string given for this property is used.
      
   - `PAGE_SUBST_POSTS_NEWLINES` If this property is non-null, then a
     newline is started after the information printed for each
     reference.
      
   - `PAGE_SUBST_POSTS_FINAL_NEWLINE` If this property is non-null but
     `PAGE_SUBST_POSTS_NEWLINES` is null, then a newline will be
     inserted after the last reference.
      
   - `PAGE_SUBST_POSTS_FORMAT_STRING` This format string describes
     what should be printed for each reference.  In the usual manner
     for format strings, most characters are echoed verbatim, `%%`
     translates to `%`, and
     
      - `%L` is replaced with the opening of a link to the post.
      - `%Z` is replaced with the closing of a link to the post.
      - `%t` is replaced with the title of the post.
      - `%T` is replaced with the title of the post, capitalizing the
        first letter if it is not already capitalized.
      - `%d` is replaced with the description of the post.
      - `%M` is replaced with the full name of the month of the last
        update to the post.
      - `%D` is replaced with the number of the day of month
        (space-padded) of the last update to the post.
      - `%Y` is replaced with the year (including century) of the last
        update to the post.
     
     The default is `"%L%T%Z (%M %D, %Y)"`.
      
   - `PAGE_SUBST_POSTS_NUMBERED` If this property is non-null, then
     the references are inserted as a numbered list; otherwise they
     are inserted as a non-numbered list.

   - `PAGE_SUBST_PIN_POLICY` This value describes how pinned posts
     should be handled in the list of posts here.  There are five
     possible values; an error will be raised if the given policy does
     not intern to one of these fine keyword symbols:

	  - `:ignore-pin` The `PIN` property of posts will be ignored, and
        only the post date and `PAGE_SUBST_POSTS_MAX` maximum limit of
        posts will be considered.

	  - `:pinned-only`  Only pinned posts (and moreover all of them)
        will be listed.

	  - `:raise-cap` The list will include both all pinned posts and
        up to `PAGE_SUBST_POSTS_MAX` unpinned posts.

	  - `:capped` The list will include all pinned posts, and if the
        limit of `PAGE_SUBST_POSTS_MAX` allows, some unpinned posts as
        well.

	  - `:strict-cap` The list can include pinned posts and recent
        unpinned posts, and will be sorted by descending modification
        date, but with no more than the limit of
        `PAGE_SUBST_POSTS_MAX` total posts.

- If a line starts

  ```lisp
  # CATEGORY-LINKS
  ```

  then this line will be replaced with several lines of links to
  category index pages.  Right now the substitution produces a
  non-numbered list; future work will allow controlling the list with
  file properties, as for the recent posts pragma.

### Custom file processors 

Any function can be provided for the `post-copy-function` and
`front-copy-function` keyword arguments so long as they expect four
arguments:

 1. A string giving the absolute path to the source Org file.
 2. A string giving the absolute path to the temporary Org file to be
    generated.
 3. The property list of the overall site.
 4. The property list of the post being generated.
 
More information about the property lists are presently below.

## Internal property list representation

`defblog` builds several [property
lists](https://www.gnu.org/software/emacs/manual/html_node/elisp/Property-Lists.html)
describing pages, posts, categories, and the overall site to be
generated.

### Page and post property lists and their hash table

Each page and post is associated with a property list with these
values:

 - `:bare` is the name of the Org file for this page/post, without any
   enclosing directory paths.
 - `:path` is the full absolute pathname for this file.
 - `:depth` is the number of directories enclosing this file relative
   to the top-level source directory.
 - `:title` is the `TITLE` property from the Org source file.
 - `:desc` is the `DESC` description property from the Org source
   file.
 - `:cat` is the tag for the category of a post, or `nil` for a
   top-level page.
 - `:author-name` is the `AUTHOR_NAME` property of a page/post, or the
   site default passed to `defblog`.
 - `:date` is the `DATE` creation time property of a page/post, or
   `nil` if none is given.
 - `:updated` is the `UPDATED` property for the last update time of a
   page/post, or `nil` if none is given.
 - `:mod` is the later of the creation time and last update time, or
   `nil` if neither are given.
 - `:sitemap-priority` is the relative priority of the page for the
   XML sitefeed.
 - `:change-freq` is the change frequency of the page for the XML
   sitefeed.

The file property list hash table maps *the symbol* whose name is the
absolute path for a source file, to the property list for that
page/post.

### Category property lists and their hash table

 - `:title` is the human-oriented name for the category.
 - `:description` is a short text description of the category.
 - `:tag` is the short tag name, which is also the directory holding
   the category's files.
 - `:src-dir` is the absolute path of the subdirectory of the source
   directory for that category.
 - `:sitemap-priority` and `:change-freq` are the XML sitemap
   properties for the category index page.
 - `:post-files` is a list of the bare (without surrounding
   directories) source file names of the post files in the category.

The category property list hash table maps *the symbol* whose name is
the category tag, to the property list for that category.

### The site property list

 - `:title` is the name of the web site.
 - `:desc` is a short text description of the site.
 - `:url` is the URL of the top level of the site.
 - `:file-plists-hash` is the file property list hash table.  Use
   `hash-table-values` for just the plists.
 - `:cat-plists-hash` is the category property list hash table.
 - `:sorted-file-plists` is a list of file property lists sorted in
   descending order of last-modified date.

## Roadmap for future versions

### Things to have before calling it Version 1.0

 - Format string for each page list entry in `category.txt` file
   property.

 - Add options for `CATEGORY-LINKS` pragma in
   `defblog/page-copy-with-substitutions`.
  
 - Develop a scheme for tags, essentially as a virtual superset of
   categories:
  
    - Receive `TAGS` property in posts.
  
    - Write `TAG-LINKS` pragma for
      `defblog/page-copy-with-substitutions`.
  
 - Refine `HTACCESS` file generation:
  
    - Use the `remote-htaccess` argument for the remote destination of
      the htaccess file.  If defined:
  
       - Generate to a `gen` directory file
  
       - Exclude `.htaccess` from rsync file list
  
       - Second call to rsync for the htaccess target
  
       - Remove this file in cleanup
  
 - Add a forwards-list argument to `defblog` for other forwards.
  
 - Ignore/warn about index.org source files in the category
   directories.

### Other ideas and tasks

 - Add a way to create the `Org` for a page/post from scratch/script.
