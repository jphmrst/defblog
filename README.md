# defblog - Web sites as Org-mode files

Declare a simple structured blog to be published with ORG-PUBLISH.
This package offers an all-Emacs solution to maintaining a web site
(except for any uploading via rsync, which can be triggered from with
ORG-PUBLISH).

## File structure

To use `defblog`, you must structure your Org files as follows:
- The `index.org` file, if it exists, is the front page of the blog.
However this page may be generated instead; see the ZZZZZZZZZZZZZZ
option below.
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
	  +-- kittens      A subdirectory corresponding to a category of posts.
	       |
		   +-- category.txt   Properties of the \"kittens\" category.
           |
		   +-- adopted.org    A post about adopting a kitten
           |
		   +-- vet-jul20.org  A post about a trip to the vet 
                              in July 2020.

## File properties

`defblog` will use the following properties of page and post Org
files:
- `TITLE` The title of the page/post.  Note that org-publish will
  place this title as the headline of the HTML it generates.
- `DESCRIPTION` A short blurb of the contents.
- `DATE` The publication date of the page/post.
- `UPDATED` The last change of the page/post.
- `AUTHOR_NAME` The name of the author of the given post/page. 
When an Atom feed is generated, either this property must be given
for every post/page, or a non-null value for the DEFAULT-AUTHOR-NAME
parameter must be provided to DEFBLOG.
- `CHANGE_FREQ` A hint about how often a page/post changes, to be
  included in the XML sitemap.  Valid values are discussed at
  [[https://www.sitemaps.org/protocol.html#changefreqdef]].
- `SITEMAP_PRIORITY` A relative assessment of the importance of
  crawling a page/post, compared to other pages/posts on this site.
  Should be a floating-point value between 0.0 and 1.0, with 1.0
  having the highest priority.

`defblog` will also use the `TITLE`, `DESCRIPTION`, `CHANGE_FREQ` and
`SITEMAP_PRIORITY` properties of `category.txt` files (but not the
date properties, which are instead synthesized from the posts in that
category).

## Calling `defblog`

Required parameters:
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
- BLOG-URL gives the URL for the top of this blog.  This value is
required if generating most of the XML artifacts.
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
