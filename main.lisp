(in-package :natsukashii)

(defun parse-category (category-array)
  "Create a proper filepath string from CATEGORY-ARRAY."
  (str:join
   "/"
   (map 'list
        (lambda (str) (str:replace-all " " "-" str))
        category-array)))

(defun write-story-to-file (path-components filename story)
  "Write STORY to FILENAME at path made from PATH-COMPONENTS. Ensure that this path exists."
  (let ((path (make-pathname :directory `(:relative "out" ,@path-components)))
        (file (make-pathname :name filename :type "txt")))

    (ensure-directories-exist path)
    (with-open-file (out
                     (merge-pathnames path file)
                     :direction :output
                     :if-exists :rename
                     :if-does-not-exist :create)
      (format out story))))

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
             (story-title (str:replace-all " " "-" (aref (lquery:$ dom "tr.TableHeading" (text)) 0)))
             (header-links (lquery:$ dom "tr.TableRow1 a" (text)))
             (story-category (parse-category (subseq header-links 0 2)))
             (author (str:replace-all " " "-" (aref header-links 3)))
             (path-components (list story-category author))
             (filename (format nil "~a--~a" story-title timestamp))
             (story (aref (lquery:$ dom "form" (text)) 1)))
        (write-story-to-file path-components filename story)))))

(defun parse-single-cdx-response (line)
  "Format one LINE of a CDX response into a URI that works with the Web Archive.
Also return the timestamp of the memento for easy sorting"
  (destructuring-bind (url-key timestamp original mimetype statuscode digest length)
      (ppcre:split "\\s" line)
    (declare (ignore url-key mimetype statuscode digest length))
    (list
     (format nil "~a/~a" timestamp original)
     timestamp)))

(defun save-story (id)
  "Attempt to grab an archived story with ID and save it."
  (let ((target-uri
          (quri:url-encode
           ;; TODO: needt to work with more generic URIs
           ;; Or better, we need to programmatically find stories that work
           (format nil "http://fanfiction.net:80/sections/anime/index.fic?action=story-read&storyid=~d" id))))

    (multiple-value-bind (return-value status-code) (dex:get (concatenate 'string *cdx-url* target-uri))
      (when (and (eql status-code 200) (str:non-empty-string-p return-value))
        (let ((mementos (mapcar #'parse-single-cdx-response (ppcre:split "\\n" return-value))))

          (loop :for (memento-uri timestamp) :in mementos
                :do (grab-story memento-uri timestamp)))))))
