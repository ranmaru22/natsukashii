;;;; The scraper for the new version of fanfiction.net (after 2001).
;;;;
;;;; While nicer to work with from a web design point-of-view, we also
;;;; have to handle way more data with this one, so it has to be better
;;;; optimized.
;;;;
;;;; There is no way to conveniently group the mementos by category here
;;;; because all stories are just accessed using generic read.php URIs
;;;; and a database ID. That's kinda unfortunate because it means that
;;;; pulling all of them means processing a giant list in one go.

(in-package :natsukashii)

(defvar *all-stories-cdx-response* '()
  "Special variable to hold the initial response from the CDX API, so we
don't have to refetch it every time as it can be quite huge.")

(defun new--get-current-chapter (dom)
  "Attempt to get the current chapter as an integer from a story's DOM."
  (let ((list
          (coerce
           (lquery:$ dom "script"
             (render-text)
             (map (lambda (txt)
                    (ppcre:register-groups-bind (chapter)
                        ("var chapter = (\\d);" txt)
                      chapter)))
             (filter (complement #'null)))
           'list)))

    (when (every #'eql list (cdr list)) (car list))))

(defun new--fetch-story (memento)
  "Attempt to download a story from MEMENTO from the Web Archive."
  (let* ((timestamp (memento-timestamp memento))
         (uri (format nil "~a/~a" timestamp (memento-url memento)))
         (dom
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
               (when first-cat-index
                 (position "list.php"
                           (lquery:$ dom "td a" (combine (attr :href) (render-text)))
                           :start (1+ first-cat-index)
                           :key #'car :test (complement #'str:containsp))))
             (story-title
               (str:pascal-case
                (lquery:$1 dom "td a~b" (render-text))))
             (story-category
               (when first-cat-index
                 (format-categories
                  (map 'list #'cadr
                       (subseq
                        (lquery:$ dom "td a"
                          (combine (attr :href) (render-text)))
                        first-cat-index
                        last-cat-index)))))
             (author
               (str:pascal-case
                (cadr (find "profile.php"
                            (lquery:$ dom "td a" (combine (attr :href) (render-text)))
                            :key #'car :test #'str:containsp))))
             (chapter (new--get-current-chapter dom))
             (path (make-pathname :directory `(:relative "out" ,@story-category ,author)))
             (filename
               (make-pathname :name (format nil "~a~@[-Ch~A~]--~a" story-title chapter timestamp) :type "html")))

        (when (and (str:non-empty-string-p story-title) story-category)
          ;; DEBUG: Let's print some info so we know it works
          (format t "~%Finished fetching '~a' by ~a in ~{~a~^/~}~%" story-title author story-category)
          (format t "~a~%" (concatenate 'string *web-url* uri))

          (ensure-directories-exist path)
          (handler-case (lquery:$ dom "body"
                          (each #'strip-scripts :replace t)
                          (write-to-file (merge-pathnames path filename) :if-exists :rename))

            (plump-dom:invalid-xml-character (e)
              ;; I don't like ignoring errors. We should handle this ...
              (declare (ignore e))
              (format t "There was an invalid character in the response. :(~%")
              (unless (directory (merge-pathnames path "*.html"))
                (uiop:delete-directory-tree path :validate t)))))))))

(defun new--fetch-all-stories (&key (from 0) (process 10))
  "Attempt to fetch all stories that have been archived."
  (let ((target-uri (quri:url-encode "fanfiction.net/read.php?storyid=*")))

    (unless *all-stories-cdx-response*
      (with-cdx-query (target-uri :map-with #'parse-cdx-response)
        (setf *all-stories-cdx-response* (remove-if #'null response))))

    (loop :with mementos := (nthcdr from *all-stories-cdx-response*)
          :for memento :in mementos
          :for i :below process
          :do (format t "~%Attempting to fetch story ~a" (+ from i))
              (new--fetch-story memento))

    (+ from process)))
