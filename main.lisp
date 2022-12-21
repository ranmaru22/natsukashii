(in-package :natsukashii)

(defun parse-category (category-array)
  "Create a proper filepath string from CATEGORY-ARRAY."
  (remove-if (lambda (str) (every #'digit-char-p str))
             (map 'list #'str:pascal-case category-array)))

(defun get-current-chapter (dom)
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

(defun strip-scripts (node)
  "Remove all script elements from NODE using lQuery."
  (lquery:$ node "script" (detach))
  node)

(defun grab-story (uri timestamp)
  "Attempt to download a story at URI from the Web Archive.
The TIMESTAMP parameter is for better archiving."
  (let ((dom
          (handler-case (dex:get (concatenate 'string *web-url* uri))
            (dex:http-request-not-found (e)
              (declare (ignore e)))
            (dex:http-request-service-unavailable (e)
              (declare (ignore e))))))

    (when dom
      (let* ((dom (plump:parse dom))
             ;; Gotta love 90s website structures. :)
             (story-title
               (str:pascal-case
                (lquery:$1 dom "tr.TableHeading>td.TextWhiteHeading>b" (render-text))))
             (story-category
               (parse-category
                (lquery:$ dom "td.TextBlack>b" (contains "Category:") (next-all "a") (render-text))))
             (author
               (str:pascal-case
                (lquery:$1 dom "td.TextBlack>b" (contains "Author") (next "a") (render-text))))
             (chapter (get-current-chapter dom))
             (path (make-pathname :directory `(:relative "out" ,@story-category ,author)))
             (filename
               (make-pathname :name (format nil "~a~@[-Ch~A~]--~a" story-title chapter timestamp) :type "html")))

        (when story-category
          ;; DEBUG: Let's print some info so we know it works
          (format t "~%Finished fetching '~a' by ~a in ~{~a~^/~}~%" story-title author story-category)
          (format t "~a~%" (concatenate 'string *web-url* uri))

          (handler-case (lquery:$ dom "body"
                          (each #'strip-scripts :replace t)
                          (write-to-file (merge-pathnames path filename) :if-exists :rename))

            (plump-dom:invalid-xml-character (e)
              ;; I don't like ignoring errors. We should handle this ...
              (declare (ignore e))
              (format t "There was an invalid character in the response. :("))))))))

(defun parse-category-cdx-response (line)
  "Format one LINE of a CDX response into a URI that works for another CDX query."
  (caddr (ppcre:split "\\s" line)))

(defun parse-story-cdx-response (line)
  "Format one LINE of a CDX response into a URI that works with the Web Archive.
Also return the timestamp of the memento for easy sorting"
  (destructuring-bind (url-key timestamp original mimetype statuscode digest length)
      (ppcre:split "\\s" line)
    (declare (ignore url-key mimetype digest length))
    (when (string= statuscode "200")
      (list
       (format nil "~a/~a" timestamp original)
       timestamp))))

(defmacro with-cdx-query ((uri &key map-with) &body body)
  "Fire a query against the CDX API with URI and then execute BODY with the result.
Optionally map the results with MAP-WITH.
The macro provides the anaphors RESPONSE and STATUS-CODE."
  (let ((return-value (gensym)))
    `(multiple-value-bind (,return-value status-code) (dex:get (concatenate 'string *cdx-url* ,uri))
       (when (and (eql status-code 200) (str:non-empty-string-p ,return-value))
         (let ((response
                 (if ,map-with
                     (mapcar ,map-with (ppcre:split "\\n" ,return-value))
                     (ppcre:split "\\n" ,return-value))))
           ,@body)))))

(defun save-story (story-uri)
  "Attempt to grab an archived story with from STORY-URI and save it."
  (let ((target-uri (quri:url-encode story-uri)))
    (with-cdx-query (target-uri :map-with #'parse-story-cdx-response)
      (loop :for (memento-uri timestamp) :in response
            :when memento-uri
              :do (grab-story memento-uri timestamp)))))

(defun get-list-of-stories-in-category (category)
  "Find a list of stories under CATEGORY in the CDX API."
  (let ((target-uri
          (quri:url-encode
           (format nil "http://fanfiction.net/sections/~a/index.fic?action=story-read*" category))))

    (with-cdx-query (target-uri :map-with #'parse-category-cdx-response)
      (loop :for story-uri :in response
            :do (save-story story-uri)))))

(defun find-archived-stories ()
  "Find a list of stories which have been archived using the CDX API.
The categories are hardcoded in because there's no nice way of getting them
programatically, also they don't change anyway."
  (mapcan #'get-list-of-stories-in-category
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
