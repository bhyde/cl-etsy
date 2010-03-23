;; -*- Mode: common-lisp -*-

(in-package "ETSY")

;; -*- Mode: Common-Lisp -*-

(def-suite etsy-api :description "ETSY API")

(in-suite etsy-api)

(test zero-test
  "The simplest test"
  (is (eq t t)))

(test have-api-key
  "Check that we have an *API-KEY* defined."
  (is (stringp *api-key*)))

(test etsy:get-server-epoch
  "Check if we can get the server's epoch"
  (is (multiple-value-bind (n c) (etsy:get-server-epoch)
        (and (eq 1 c)
             (null (cdr n))
             (typep (car n) 'fixnum)))))

