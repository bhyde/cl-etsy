;; -*- Mode: common-lisp -*-

(in-package "ETSY")

;; -*- Mode: Common-Lisp -*-

(defun list-of-p (type list)
  (and (listp list)
       (loop for elm in list always (typep elm type))))

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
  (let ((users (get-users-by-name "bhyde" :limit 100 :detail-level :high)))
    (is
     (and (list-of-p 'user users)
          (loop for user in users
             when (equal '("a_warm_zephyr") (slot-value user 'materials))
             return user)))))

(test get-listings-by-color
  (is (get-listings-by-color "pink" :limit 10 :detail-level :high)))

(test get-gift-guides
  (is (list-of-p 'gift-guide
                 (get-gift-guides))))

(test get-gift-guide-listings
    (is (list-of-p 'listing
                   (get-gift-guide-listings (slot-value (first (get-gift-guides)) 'guide-id)))))

(test get-top-categories
  (is (list-of-p 'string (get-top-categories))))

(test get-child-categories
  (is (list-of-p 'string (get-child-categories "art:drawing"))))



(test get-top-tags
  (is (list-of-p 'string (get-top-tags))))

(test get-child-tags
  (is (list-of-p 'string (get-child-tags "wood"))))

(test get-listings-by-category
  (is (list-of-p 'listing
                 (get-listings-by-category "art:drawing:original_drawing"
                                           :detail-level :high))))

(test get-favorite-listings-of-user
  (is (list-of-p 'listing
                 (get-favorite-listings-of-user "bhyde"))))

(test get-shop-listings
  (is (list-of-p 'listing
                 (get-shop-listings "mck254"))))
  
(test get-shops-by-name
  (is (list-of-p 'shop
                 (get-shops-by-name "doll"))))

(test get-favorite-shops-of-user
  (is (list-of-p 'shop
                 (get-favorite-shops-of-user "mck254"))))

(test get-featured-sellers
  (is (list-of-p 'user
                 (get-featured-sellers))))

(test get-shop-details
  (is (list-of-p 'shop
                 (get-shop-details "mck254"))))


                

       
       
