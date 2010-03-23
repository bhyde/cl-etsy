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

(defun api-type-to-our-type (type-string)
  (cond
    ((string= type-string "enum(low, medium, high)")
     'detail-level)
    ((string= type-string "enum(up, down)")
     'sort-order)
    ((string= type-string "enum(created, ending)")
     'sort-on-a)
    ((string= type-string "enum(created, price)")
     'sort-on-b)
    ((string= type-string "enum(true, false)")
     'boolean)
    ((string= type-string "array(string)")
     'array-of-strings)
    ((string= type-string "array(int)")
     'array-of-ints)
    (t 
     (underscore-to-dash type-string))))

(defun build-methods ()
  (flet ((url-builder (url-template parameters)
           (let* ((required-args)
                  (url-bits
                   (loop 
                     for bit in (cl-ppcre:split "[{}]" url-template)
                     as var = nil then (not var)
                     collect (cond
                               (var
                                (let* ((v (underscore-to-dash bit))
                                       (p (find v parameters :key #'car)))
                                  (push p required-args)
                                  `(marshall-type ,(second p) ,v)))
                               (t
                                bit))))
                  (optional-args 
                   (set-difference parameters required-args :test #'equal)))
             (values url-bits required-args optional-args))))
    (let ((json
           (json:decode-json-from-string
            (flexi-streams:octets-to-string
             (drakma:http-request (concatenate 'string *base-url* "/")
                                    :parameters `(("api_key" . ,*api-key*)))))))
      (json:json-bind (results) json
        (let ((*package* (symbol-package 'build-methods)))
          (with-open-file  (*standard-output* "methods.lisp"
                                              :direction :output
                                              :if-exists :rename-and-delete)
            (let ((*print-case* :downcase))
              (format t "~&;; -*- mode: lisp; syntax: common-lisp; -*-")
              (format t "~&;;; Generated by build-methods in base.lisp (so don't edit)")
              (format t "~&(in-package \"ETSY\")")
              (loop for method in results
                    do (json:json-bind (name description uri params type) method
                         (let ((parameters (loop for (key . type1) in params
                                                 as type = (api-type-to-our-type type1)
                                                 collect (list (underscore-to-dash (symbol-name key))
                                                               type))))
                           (multiple-value-bind (url-bits required-parameters optional-parameters) 
                               (url-builder uri parameters)
                             (format t "~&")
                             (pprint
                              `(defun ,(camel-to-lisp name) (,@(mapcar #'car required-parameters)
                                                             ,@(when optional-parameters '(&key))
                                                             ,@(mapcar #'car optional-parameters))
                                 ,description
                                 ,@(loop for (name type) in required-parameters
                                         collect `(parameter-type-check ,type ,name))
                                 (demarshall-results 
                                  (with-api-call ,url-bits ,@optional-parameters)
                                  ,type
                                  ',(build-symbol "DEMARSHALL" type)))))))))))))))

(defmacro with-api-call ((&rest url-bits) &rest optional-parameters)
  `(let ((cgi-args (list (cons "api_key" etsy::*api-key*))))
     ,@(loop for (name type) in optional-parameters
          collect `(when ,name
                     (parameter-type-check ,type ,name)
                     (push (cons ,(lisp-to-cgi name) (marshall-type ,type ,name))
                           cgi-args)))
      (api-call (concatenate 'string *base-url* ,@url-bits) cgi-args)))


(defun api-call (url cgi-args)
  (json:decode-json-from-string
   (flexi-streams:octets-to-string
    (multiple-value-bind (doc code)
        (with-interval-timer (*etsy-api-request-timer*)
          (drakma:http-request url :parameters cgi-args))
      (unless (eq 200 code)
        (error "Failed API call ~D ~A" code doc))
      doc))))

(defmacro parameter-type-check (type value)
  (declare (ignore type))
  `,value)

(defmacro with-result-type-check ((type) &body body)
  (declare (ignore type))
  `(progn ,@body))

(defmacro marshall-type (type value)
  `(,(build-symbol "MARSHALL" type) ,value))

(defmacro demarshall-type (type value)
  `(,(build-symbol "DEMARSHALL" type) ,value))

(defun demarshall-results (json element-type element-demarshall)
  (json:json-bind (count type results) json
    (assert (string= type element-type) () "Type ~S returned was expecting ~S" type element-type)
    (values (mapcar element-demarshall results) count)))

;;;; Meta data about the API

(defclass etsy-object ()
  ((functions-for-demarshall :allocation :class :initform (make-hash-table))))

(defmethod fill-out-etsy-object-from-json ((x etsy-object) json)
  (with-slots (functions-for-demarshall) x
    (loop
       for (key . value) in json
       as slot-name = (keyword-to-lisp-symbol key)
       as func = (gethash slot-name functions-for-demarshall)
       do
         (setf (slot-value x slot-name) (funcall func value))))
  x)

(defclass api-slot-description ()
  ((class :type api-class-description :initarg :class)
   (plist :initform nil)
   (name :type symbol :initarg :name)
   (documentation :type string :initarg :documentation)
   (detail-level :type symbol :initarg :detail-level)
   (type :type symbol :initarg :type))
  (:documentation "Holds declarative information about a slot in a result, as gleaned from the API spec."))

(defclass api-class-description ()
  ((name :type symbol)
   (plist :initform nil)
   (superclass :type symbol)
   (documentation :type string)
   (slot-descriptions :type list))
  (:documentation "Holds declarative info about each result type, as gleaned from the API spec."))

(defmacro api-class-info (class-name)
  `(get ,class-name :api-class-info))

(defmethod slot-description ((x api-class-description) slot-name)
  (with-slots (slot-descriptions) x
    (find slot-name slot-descriptions :key #'(lambda (sd) (slot-value sd 'name)))))

(defmacro def-api-class (name (&optional (superclass 'etsy-object)) doc fields)
  (loop
    with stms = ()
    with functions-for-demarshall = ()
    with build-api-slot-descriptions = ()
    with class-fields
    finally
 (return
   `(progn
      (let ((api-class-info (make-instance 'api-class-description)))
        (with-slots (name documentation superclass slot-descriptions) api-class-info
          (setf name ',name)
          (setf superclass ',superclass)
          (setf documentation ,doc)
          (setf slot-descriptions (list ,@(nreverse build-api-slot-descriptions))))
        (setf (api-class-info ',name) api-class-info))
      (defclass ,name (,superclass)
        ((functions-for-demarshall
          :initform (let ((x (copy-hash-table (slot-value (make-instance ',superclass)
                                                          'functions-for-demarshall))))
                      ,@functions-for-demarshall
                      x))
         ,@(nreverse class-fields))
        (:documentation ,doc))
      ,@stms
      (defun ,(build-symbol "DEMARSHALL" name) (x)
        (fill-out-etsy-object-from-json (make-instance ',name) x))))

    for field in fields
    do
 (destructuring-bind (name &key level type doc json-key) field
   (unless json-key (setf json-key (lisp-to-json-keyword name)))
   (push `(make-instance 'api-slot-description
                         :class api-class-info
                         :name ',name
                         :type ',type
                         :detail-level ',level) build-api-slot-descriptions)
   (push `(,name :documentation ,doc) class-fields)
   (push `(setf (gethash ',name x) ',(build-symbol "DEMARSHALL" type)) functions-for-demarshall)
   (push `(estabilish-list-keyword-mapping ',name ,json-key) stms))))
