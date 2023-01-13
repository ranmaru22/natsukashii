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
         (retry-request (dex:retry-request 5 :interval 3))
         (dom (handler-bind ((dex:http-request-failed retry-request))
                (dex:get (concatenate 'string *web-url* uri)))))

    (when dom
      (let* ((dom (plump:parse dom))
             (first-cat-index
               (position "subcats.php"
                         (lquery:$ dom "a" (combine (attr :href) (render-text)))
                         :key #'car :test #'str:containsp))
             (last-cat-index
               (when first-cat-index
                 (position "list.php"
                           (lquery:$ dom "a" (combine (attr :href) (render-text)))
                           :start (1+ first-cat-index)
                           :key #'car :test (complement #'str:containsp))))
             (story-title
               (str:pascal-case
                (or
                 (lquery:$1 dom "td a~b" (render-text))
                 ;; For old stories that don't bolden the title, we have to fallback to regex
                 ;; because the text leaf is not contained in ANY element. Yay.
                 (ppcre:register-groups-bind (title)
                     (".+ (?:»|>>) .+ (?:»|>>) (.+)\\n" (lquery:$1 dom (text)))
                   title))))
             (story-category
               (when first-cat-index
                 (format-categories
                  (map 'list #'cadr
                       (subseq
                        (lquery:$ dom "a"
                          (combine (attr :href) (render-text)))
                        first-cat-index
                        last-cat-index)))))
             (author
               (str:pascal-case
                (cadr (find "profile.php"
                            (lquery:$ dom "td a" (combine (attr :href) (render-text)))
                            :key #'car :test #'str:containsp))))
             (chapter (new--get-current-chapter dom))
             (path (make-pathname :directory `(:relative "../out" ,@story-category ,author)))
             (filename
               (make-pathname :name (format nil "~a~@[-Ch~A~]--~a" story-title chapter timestamp) :type "html")))

        (when story-category
          ;; DEBUG: Let's print some info so we know it works
          (format t "~%Finished fetching '~a' by ~a in ~{~a~^/~}~%" story-title author story-category)
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

(defun new--fetch-all-stories (&key (from 0) (process 10) (threads 8))
  "Attempt to fetch all stories that have been archived."
  (unless lparallel:*kernel* (setf lparallel:*kernel* (lparallel:make-kernel threads)))

  (unless *all-stories-cdx-response*
    (with-cdx-query (*story-url* :map-with #'parse-cdx-response)
        (setf *all-stories-cdx-response* (remove-if #'null response))))

  (let ((pool (subseq *all-stories-cdx-response* from (+ from process))))
    (lparallel:pmapc #'new--fetch-story pool))

  (+ from process))
