;; -*- Mode: common-lisp -*-

(in-package "ETSY")

;;; It is very unlikely that this can run to completion in a
;;; reasonable abount of time.  Given the daily limit on API calls and
;;; the presumably nearly infinite size of the universe of tags.  So any
;;; effort to download the full graph would require some persistant store
;;; coupled with a long and steady effort over many days.  Don't.

;;; Note that there are cycles in the tag-child->tag graph.  For example:
;;; men->vest->men.

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

