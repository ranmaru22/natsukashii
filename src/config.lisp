(in-package :natsukashii)

(defparameter *cdx-url* "http://web.archive.org/cdx/search/cdx?to=2002&url="
  "URL to the Wayback Machine's CDX API.")

(defparameter *web-url* "http://web.archive.org/web/"
  "URL to the Wayback Machine Web Archive.")

(defparameter *story-url* "fanfiction.net/read.php?storyid=*"
  "Base URL for archived stories. Meant to be used with the CDX API.")
