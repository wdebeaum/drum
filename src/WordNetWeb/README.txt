WordNetWeb
William de Beaumont
$Date: 2012/04/18 18:14:54 $

WordNetWeb is a faster alternative to the official WordNet search website,
which also includes sense tags in the glosses rendered as links. To install and
use (on Mac OS X):

1. Follow the instructions in ../WordNetSQL/README.html to make wn.db and/or
   eswn.db, and optionally sense-map.db if you did both (installing the
   prerequisites first). 
2. Run "make install" in this directory (if you get "permission denied", try
   "sudo make install" instead).
3. Edit /etc/apache2/httpd.conf so that the AllowOverride directive for the
   "/Library/WebServer/Documents" Directory includes Options, FileInfo, and
   AuthConfig, e.g. change:
     AllowOverride None
   to:
     AllowOverride Options FileInfo AuthConfig
4. Go to Sharing in System Preferences and make sure "Web Sharing" is on.
5. Go to this URL in your web browser:
   http://localhost/WordNetWeb/get-word-xml.pl
6. Log in with user "ihmc" and password "iehsmWcN".
7. Enter a word or phrase to search for.

By default, files are installed in /Library/WebServer/Documents/WordNetWeb/ so
that the URL above works. Change CGI_DIR in the Makefile to install somewhere
else. If you need to install non-script (style) files in a separate directory
from the CGI scripts, set STYLE_RELATIVE_PATH (this is useful if your webserver
wants to execute anything in the CGI_DIR as a script rather than serving the
file itself to the browser).

