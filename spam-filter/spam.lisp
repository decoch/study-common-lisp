(defpackage :com.gigamonkeys.spam
  (:use :common-lisp :ppcre))

; (:use :common-lisp :com.gigamonkeys.pathnames))
; (ql:quickload :cl-ppcre)
(in-package :com.gigamonkeys.spam)

(defun classify (text)
  (classification (score (extract-features text))))

(defparameter *max-ham-score* .4)
(defparameter *min-spam-score* .6)

(defun classification (score)
  (cond
    ((<= score *max-ham-score*) 'ham)
    ((>= score *min-spam-score**) 'spam)
    (t 'unsure)))

(defclass word-feature()
  ((word
    :initarg :word
    :accessor word
    :initform (error "must supply :word")
    :documentation "この特徴を表す単語")
   (spam-count
    :initarg :spam-count
    :accessor spam-count
    :initform 0
    :documentation "この特徴が出現したスパムの数")
   (ham-count
    :initarg :ham-count
    :accessor ham-count
    :initform 0
    :documentation "この特徴が出現したハムの数")))

(defvar *feature-database* (make-hash-table :test #'equal))

(defun clear-database ()
  (setf *feature-database* (make-hash-table :test #'equal)
        *total-spams* 0
        *total-hams* 0))

(defun intern-feature (word)
  (or (gethash word *feature-database*)
      (setf (gethash word *feature-database*)
            (make-instance 'word-feature :word word))))

(defun extract-words (text)
  (delete-duplicates
   (cl-ppcre:all-matches-as-strings "[a-zA-Z]{3,}" text)
   :test #'string=))

(defun extract-features (text)
  (mapcar #'intern-feature (extract-words text)))

(defmethod print-object ((object word-feature) stream)
  (print-unreadable-object (object stream :type t)
    (with-slots (word ham-count spam-count) object
      (format stream "~s :hams ~d :spams ~d" word ham-count spam-count))))

(defun train (text type)
  (dolist (feature (extract-features text))
    (increment-count feature type))
  (increment-total-count type))

(defvar *total-spams* 0)
(defvar *total-hams* 0)

(defun increment-total-count (type)
  (ecase type
    (ham (incf *total-hams*))
    (spam (incf *total-spams*))))

(defun spam-probability (feature)
  (with-splots (spam-count ham-count) feature
               (/ spam-count(+ spam-count ham-count))))

(defun bayesian-spam-probability (feature &optional
                                          (assumed-probability 1/2)
                                          (weight 1))
  (let ((basic-probability (spam-probability feature))
        (data-points (+ (spam-count feature) (ham-count feature))))
    (/ (+ (* weight assumed-probability)
          (* data-points basic-probability))
       (+ weight data-points))))
