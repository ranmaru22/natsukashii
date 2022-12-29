;;;; The scraper for the new version of fanfiction.net (after 2001).
;;;;
;;;; While nicer to work with from a web design point-of-view, we also
;;;; have to handle way more data with this one, so it has to be better
;;;; optimized.

(in-package :natsukashii)

(defun new--fetch-story (uri timestamp)
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
             (first-cat-index
               (position "subcats.php"
                         (lquery:$ dom "td a" (combine (attr :href) (render-text)))
                         :key #'car :test #'str:containsp))
             (last-cat-index
               (position "list.php"
                         (lquery:$ dom "td a" (combine (attr :href) (render-text)))
                         :start (1+ (or first-cat-index 0))
                         :key #'car :test (complement #'str:containsp)))
             (story-title
               (str:pascal-case
                (lquery:$1 dom "td a~b" (render-text))))
             (story-category
               (format-categories
                (map 'list #'cadr
                     (subseq
                      (lquery:$ dom "td a"
                        (combine (attr :href) (render-text)))
                      first-cat-index
                      last-cat-index))))
             (author
               (str:pascal-case
                (cadr (find "profile.php"
                            (lquery:$ dom "td a" (combine (attr :href) (render-text)))
                            :key #'car :test #'str:containsp))
                ))
             (chapter (old--get-current-chapter dom))
             (path (make-pathname :directory `(:relative "out" ,@story-category ,author)))
             (filename
               (make-pathname :name (format nil "~a~@[-Ch~A~]--~a" story-title chapter timestamp) :type "html")))

        (list story-title story-category author path filename)
        ))))

(defun new--fetch-all-stories ()
  "Attempt to fetch all stories that have been archived."
  (let ((target-uri (quri:url-encode "fanfiction.net/read.php?storyid=*")))
    (with-cdx-query (target-uri :map-with #'parse-category-cdx-response)
      (loop :for (memento-uri timestamp) :in response
            :do (new--fetch-story (format nil "~a/~a" timestamp memento-uri) timestamp)))))
