;; -*- mode: lisp; syntax: common-lisp; -*-

(in-package "ETSY")


;;;; Interval Timers

(defclass timer ()
  ((start-time :initform 0)
   (first-index :initform 0)
   (last-index :initform 0)
   (max :initform 100)
   (samples :initform (make-array 100))))

(defmethod clear-timer ((timer timer))
  (with-slots (first-index last-index) timer
    (setf first-index 0)
    (setf last-index 0)))

(defmethod map-over-timer-samples ((timer timer) lambda)
  (with-slots (start-time first-index last-index max samples) timer
    (cond
      ((<= first-index last-index)
       (loop for i from first-index below last-index do (funcall lambda (svref samples i))))
      (t
       (loop for i from first-index below max do (funcall lambda (svref samples i)))
       (loop for i from 0 below last-index do (funcall lambda (svref samples i)))))))

(defmacro do-timer-samples ((var timer) &body body)
  `(map-over-timer-samples ,timer #'(lambda (,var) ,@body)))

(defmethod timer-stats  ((timer timer))
  (let ((cnt 0) min max (sum 0.0) (first? t))
    (do-timer-samples (x timer)
      (when first?
        (setf first? nil)
        (setf min x)
        (setf max x))
      (incf cnt)
      (incf sum x)
      (setf min (min min x))
      (setf max (max max x)))
    (let ((d (* 1.0 INTERNAL-TIME-UNITS-PER-SECOND)))
      (values cnt (/ min d) (/ max d) (/ (/ sum d) cnt)))))
   
(defmethod begin-timer ((timer timer))
  (with-slots (start-time) timer
    (setf start-time (get-internal-real-time))))

(defmethod end-timer ((timer timer))
  (with-slots (start-time first-index last-index max samples) timer
    (macrolet ((inc-index (index)
                 `(progn
                    (incf ,index)
                    (when (<= max ,index)
                      (setf ,index 0)))))
      (inc-index last-index)
      (when (= first-index last-index)
        (inc-index first-index)))
    (setf (svref samples last-index) (- (get-internal-real-time) start-time))))



(defmacro with-interval-timer ((timer-var) &body body)
  `(let ((#1=#:timer-var ,timer-var))
     (begin-timer #1#)
     (unwind-protect
          (progn ,@body)
       (end-timer #1#))))

(defvar *etsy-api-request-timer* (make-instance 'timer))


;;;; Utilities

(defparameter *base-url* "http://beta-api.etsy.com/v1")

(defvar *api-key* "you need to set your *API-KEY*")

(defun build-symbol (&rest parts)
  (intern (format nil "~@:(~{~A~^-~}~)" parts)))

(defun copy-hash-table (hash-table &key (copy-fn #'identity) (test #'eq))
  (let ((result (make-hash-table :test test)))
    (flet ((copy (key value)
             (setf (gethash key result) (funcall copy-fn value))))
      (maphash #'copy hash-table)
      result)))

(defun camel-to-lisp (string)
  (intern (nstring-upcase
           (cl-ppcre:regex-replace-all "([A-Z])" string "-\\1"))))

(defvar *lisp-keyword-dictionary* (make-hash-table :test #'eq))

(defun estabilish-list-keyword-mapping (symbol1 symbol2)
  (setf (gethash symbol1 *lisp-keyword-dictionary*) symbol2)
  (setf (gethash symbol2 *lisp-keyword-dictionary*) symbol1))

(defun lisp-to-json-keyword (symbol)
  (or (gethash symbol *lisp-keyword-dictionary*)
      (setf (gethash symbol *lisp-keyword-dictionary*)
            (intern (substitute #\_ #\- (symbol-name symbol)) (symbol-package :keyword)))))

(defun keyword-to-lisp-symbol (keyword)
  (or (gethash keyword *lisp-keyword-dictionary*)
      (setf (gethash keyword *lisp-keyword-dictionary*)
            (intern (substitute #\- #\- (symbol-name keyword))))))

(defun underscore-to-dash (string)
  (intern (nstring-upcase
           (cl-ppcre:regex-replace-all "_" string "-"))))

(defun lisp-to-cgi (symbol)
  (nstring-downcase (substitute #\_ #\- (symbol-name symbol))))

(defmacro with-json-bindings ((&rest vars) json &body body)
  `(let* ((#1=json ,json)
          ,@(loop for var in vars
               collect `(,var (cdr (assoc ,(lisp-to-json-keyword var) #1#)))))
     ,@body))


