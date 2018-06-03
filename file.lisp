(defun tmp () 
  (let ((in (open "/some/file/name.txt" :if-does-not-exist nil)))
    (when in
      (format t "~a~%" (read-line in))
      (close in))))

(defun file-create-example ()
  (with-open-file (stream "~/projects/study-common-lisp/some/file/name.txt" :direction :output)
    (format stream "Some text.")))

(defun file-read-example ()
  (with-open-file (stream "~/projects/study-common-lisp/some/file/name.txt")
    (format t "~a~%" (read-line stream))))
