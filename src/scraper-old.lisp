;;;; The scraper for the old version of fanfiction.net (2001 and before).
;;;;
;;;; That site did not have proper design and didn't follow standards, so
;;;; parsing it can be a pain and usually involves reading text content of nodes
;;;; and such things.

(in-package :natsukashii)

(defun old--get-current-chapter (dom)
  "Attempt to get the current chapter as an integer from a story's DOM."
  (let ((list
          (coerce
           (lquery:$ dom "a"
             (text)
             (map (lambda (txt)
                    (ppcre:register-groups-bind (prev next)
                        ("Previous Chapter \\( (\\d+) \\)|Next Chapter \\( (\\d+) \\)" txt)
                      (cond
                        (prev (1+ (parse-integer prev)))
                        (next (1- (parse-integer next)))))))
             (filter (complement #'null)))
           'list)))

    (when (every #'eql list (cdr list)) (car list))))

(defun old--get-story-title (dom)
  "Attept to get the title of a story from DOM."
  (str:pascal-case (lquery:$1 dom "tr.TableHeading>td.TextWhiteHeading>b" (render-text))))

(defun old--get-story-categories (dom)
  "Attempt to get the categories of a story from DOM."
  (format-categories (lquery:$ dom "td.TextBlack>b" (contains "Category:") (next-all "a") (render-text))))

(defun old--get-story-author (dom)
  "Attempt to get the author of a story from DOM."
  (str:pascal-case (lquery:$1 dom "td.TextBlack>b" (contains "Author") (next "a") (render-text))))

(defun old--fetch-story (memento)
  "Attempt to download a story from MEMENTO from the Web Archive."
  (let* ((timestamp (memento-timestamp memento))
         (uri (format nil "~a/~a" timestamp (memento-url memento)))
         (retry-request (dex:retry-request 5 :interval 3))
         (dom (handler-bind ((dex:http-request-failed retry-request)
                             (usocket:socket-error retry-request)
                             #+sbcl (sb-int:simple-stream-error retry-request)
                             #+sbcl (sb-sys:io-timeout retry-request))
                (dex:get (concatenate 'string *web-url* uri)))))

    (when dom
      (let* ((dom (plump:parse dom))
             (title (old--get-story-title dom))
             (categories (old--get-story-categories dom))
             (author (old--get-story-author dom))
             (chapter (old--get-current-chapter dom))
             (path (make-pathname :directory `(:relative "../out" ,@categories ,author)))
             (filename (make-pathname :name (format nil "~a~@[-Ch~A~]--~a" title chapter timestamp) :type "html")))

        (when categories
          (format t "~%Finished fetching '~a' by ~a in ~{~a~^/~}~%" title author categories)
          (format t "~a~%" (concatenate 'string *web-url* uri))

          (ensure-directories-exist path)
          (handler-case (lquery:$ dom "body"
                          (each #'strip-scripts :replace t)
                          (write-to-file (merge-pathnames path filename)))

            (plump-dom:invalid-xml-character (e)
              ;; I don't like ignoring errors. We should handle this ...
              (declare (ignore e))
              (format t "There was an invalid character in the response. :(~%")
              (unless (directory (merge-pathnames path "*.html"))
                (uiop:delete-directory-tree path :validate t)))))))))

(defun old--fetch-stories-in-category (category &key (threads 8))
  "Attempt to grab all archived stories in CATEGORY."
  (let ((target-uri (format nil "fanfiction.net/sections/~a/index.fic?action=story-read*" category)))
    (unless lparallel:*kernel* (setf lparallel:*kernel* (lparallel:make-kernel threads)))

    (with-cdx-query (target-uri :map-with #'parse-cdx-response)
      (lparallel:pmapc #'old--fetch-story (remove-if #'null response)))))

(defun old--find-archived-stories ()
  "Find a list of stories which have been archived using the CDX API.
The categories are hardcoded in because there's no nice way of getting them
programatically, also they don't change anyway."
  (mapc #'old--fetch-stories-in-category
        '("anime"
          "books"
          "cartoons"
          "comics"
          "crossovers"
          "games"
          "misc"
          "movies"
          "musicgroups"
          "originals"
          "poetries"
          "tvshows")))
