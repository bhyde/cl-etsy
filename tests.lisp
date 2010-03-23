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
  (is (and (stringp *api-key*)
           (not (find #\space *api-key*)))))

(test ping
  (is
   (equal '(("pong") 1)
          (multiple-value-list (etsy:ping)))))

(test get-server-epoch
  "Check if we can get the server's epoch"
  (is (multiple-value-bind (n c) (etsy:get-server-epoch)
        (and (eq 1 c)
             (null (cdr n))
             (typep (car n) 'fixnum)))))

(test get-user-details
  "See if this user's details still claims to have no cats."
  (is (search "I have no cats"
              (slot-value (first (etsy:get-user-details 93 :detail-level :high))
                          'bio))))

(test get-user
  "Find the user bhyde based on his odd materials"
  (is
   (loop for user in (get-users-by-name "bhyde" :limit 100 :detail-level :high)
      when (equal '("a_warm_zephyr") (slot-value user 'materials))
      return user)))