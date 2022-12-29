;;;; Utility functions and macros that are useful in many places.

(in-package :natsukashii)

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

(defun format-categories (category-array)
  "Format strings in CATEGORY-ARRAY so that they work as filepaths."
  (remove-if (lambda (str) (every #'digit-char-p str))
             (map 'list #'str:pascal-case category-array)))

(defun strip-scripts (node)
  "Remove all script elements from NODE using lQuery."
  (lquery:$ node "script" (detach))
  node)

(defun parse-category-cdx-response (line)
  "Format one LINE of a CDX response into a URI that works for another CDX query."
  (destructuring-bind (url-key timestamp original mimetype statuscode digest length)
      (ppcre:split "\\s" line)
    (declare (ignore url-key mimetype digest length))
    (when (string= statuscode "200")
      (list original timestamp))))

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
