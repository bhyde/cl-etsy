;; -*- Mode: common-lisp -*-

(in-package "ETSY")

;;; It is very unlikely that this can run to completion given the daily limit
;;; on API calls and the presumably nearly infinate size of the universe of tags

(defun get-all-tags ()
  (loop
     with result = () finally (return result)
     with q = (get-top-tags)
     while q do
       (flet ((f (x)
                (push x result)
                (loop for i in (get-child-tags x)
                     unless (or (find i q :test #'string=) (find i result :test #'string=))
                     do (push i q))
                (format t "~& ~D ~D" (length q) (length result))))
         (f (pop q)))))
