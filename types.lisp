;; -*- mode: lisp; syntax: common-lisp; -*-

(in-package "ETSY")

(defmacro dumb-printing (stream object format-string &rest slots)
  `(let* ((object ,object)
          ,@(loop for slot in slots
               collect `(,slot (if (slot-boundp object ',slot)
                                   (slot-value object ',slot)
                                   "<unbound>"))))
    (print-unreadable-object (object stream :type t)
      (format ,stream ,format-string ,@slots))))


;;; One marshalling function for each parameter type 

(defun marshall-int (x) (format nil "~S" x))
(defun marshall-string (x) x)
(defun marshall-detail-level (x) (string-downcase (symbol-name x)))
(defun marshall-user-id-or-name (x) (format nil "~(~A~)" x))
(defun marshall-sort-on-a (x) (string-downcase (symbol-name x)))
(defun marshall-sort-on-b (x) (string-downcase (symbol-name x)))
(defun marshall-sort-order (x) (string-downcase (symbol-name x)))
(defun marshall-boolean (x) (if x "true" "false"))
(defun marshall-float (x) (format nil "~S" x))
(defun marshall-array-of-strings (x) (format nil "~{~A~^;~}" (coerce x 'list)))
(defun marshall-array-of-ints (x) (format nil "~{~D~^;~}" (coerce x 'list)))
(defun marshall-color-triplet (x) (if (integerp x)
                                      (format nil "~X" x)
                                      (format nil "~D;~D;~D" (aref x 0) (aref x 1) (aref x 2))))
(defun demarshall-int (x) x)
(defun demarshall-string (x) x)
(defun demarshall-category (x) x)
(defun demarshall-tag (x) x)
(defun demarshall-boolean (x) x)
(defun demarshall-float (x) x)
(defun demarshall-hsv-color (x)
  (loop
     with result = (make-array 3) finally (return result)
     for i in (cl-ppcre:split ";" x)
     as j from 0
     do (setf (svref result j) (parse-integer i))))
(defun demarshall-rgb-color (x)
  (parse-integer (subseq x 1) :radix 16))
(defun demarshall-enum-to-keyword (x)
  (intern (string-upcase x) (symbol-package :keyword)))
(defun demarshall-image-booklet (booklet)
  (loop 
     for image-set in booklet
     collect image-set))



(define-unusual-json-mapping 'image-url-25x25 :image--url--25-x-25)
(define-unusual-json-mapping 'image-url-30x30 :IMAGE--URL--30-X-30)
(define-unusual-json-mapping 'image-url-50x50 :IMAGE--URL--50-X-50)
(define-unusual-json-mapping 'image-url-75x75 :IMAGE--URL--75-X-75)
(define-unusual-json-mapping 'image-url-155x125 :IMAGE--URL--155-X-125)
(define-unusual-json-mapping 'image-url-200x200 :IMAGE--URL--200-X-200)
(define-unusual-json-mapping 'image-url-430xN :image--url--430-x-n )


(def-api-class user ()
  "User records represent a single user of the site, who may or may not be a seller."
  ((user-name :level :low :type string
              :doc "The user's login name.")
   (user-id :level :low :type int
            :doc "The user's numeric ID.  This is also valid as the user's shop ID.")
   (url :level :low :type string
        :doc "The full URL to the user's shop, if s/he is a seller, or to the user's public profile.")
   (image-url-25x25 :level :low :type string
                    :doc "The full URL to the user's 25x25 avatar thumbnail.")
   (image-url-30x30 :level :low :type string
                    :doc "The full URL to the user's 30x30 avatar thumbnail.")
   (image-url-50x50 :level :low :type string
                    :doc "The full URL to the user's 50x50 avatar thumbnail.")
   (image-url-75x75 :level :low :type string
                    :doc "The full URL to the user's 75x75 avatar (full resolution).")
   (join-epoch :level :low :type float
               :doc "The date and time the user joined the site, in epoch seconds.")
   (city :level :low :type string
         :doc "The user's city and state (freeform entry; may be blank).")
   (gender :level :medium :type string
           :doc "The user's gender (female, male, or private).")
   (lat :level :medium :type float
        :doc "The user's latitude (may be blank).")
   (lon :level :medium :type float
        :doc "The user's longitude (may be blank).")
   (transaction-buy-count :level :medium :type int
                          :doc "The number of items the user has purchased.")
   (transaction-sold-count :level :medium :type int
                           :doc "The number of items the user has sold.")
   (is-seller :level :medium :type boolean
              :doc "true if user is a seller.")
   (was-featured-seller :level :medium :type boolean
                        :doc "true if user was previously featured.")
   (materials :level :medium :type string
              :doc "The user's favorite materials.")
   (last-login-epoch :level :medium :type float
                     :doc "The date and time of the user's last login, in epoch seconds.")
   (referred-user-count :level :high :type int
                        :doc "The number of members the user has referred to the site.")
   (birth-day :level :high :type int
              :doc "The day portion of the user's birthday (may be blank).")
   (birth-month :level :high :type int
                :doc "The month portion of the user's birthday (may be blank).")
   (bio :level :high :type string
        :doc "The user's biography (may be blank).")
   (feedback-count :level :high :type int
                   :doc "The total count of feedback by and about this user.")
   (feedback-percent-positive :level :favorites :type int
                              :doc "The percentage of feedback by or about this user that is positive.")
   (favorite-creation-epoch :level :favorites :type float
                            :doc "The date and time that the user was favorited (only available in the commands getFavorersOfShop, getFavorersOfListing and getFavoriteShopsOfUser.)")
   (status :level :favorites :type enum-to-keyword
           :doc "public or private. If private, user is a Secret Admirer, and no information about the user can be shown.")))

(defmethod print-object ((x user) stream)
  (dumb-printing stream x "~D: ~A" user-id user-name))

(def-api-class shop (user)
  "Shop records extend user records to include information about the seller's shop. In addition to all the user fields listed above; shops have these additional fields:"
  ((banner-image-url :level :low :type string :doc "The full URL to the shops's banner image.")
   (last-updated-epoch :level :low :type string :doc "The date and time the shop was last updated, in epoch seconds.")
   (creation-epoch :level :low :type string :doc "The date and time the shop was created, in epoch seconds.")
   (listing-count :level :low :type int :doc "The number of active listings in the shop.")
   (shop-name :level :medium :type string :doc "The shop's name.")
   (title :level :medium :type string :doc "A brief heading for the shop's main page.")
   (sale-message :level :medium :type string :doc "A message that is sent to users who buy from this shop.")
   (announcement :level :high :type string :doc "An announcement to buyers that displays on the shop's homepage.")
   (is-vacation :level :high :type int :doc "If the seller is not currently accepting purchases, 1, blank otherwise.")
   (vacation-message :level :high :type string :doc "If is-vacation=1, a message to buyers.")
   (currency-code :level :high :type string :doc "The ISO code (alphabetic) for the seller's native currency.")
   (policy-welcome :level :high :type string :doc "The introductory text from the top of the seller's policies page (may be blank).")
   (policy-payment :level :high :type string :doc "The seller's policy on payment (may be blank).")
   (policy-shipping :level :high :type string :doc "The seller's policy on shippinh (may be blank).")
   (policy-refunds :level :high :type string :doc "The seller's policy on refunds (may be blank).")
   (policy-additional :level :high :type string :doc "Any additional policy information the seller provides (may be blank).")
   (sections :level :high :type array-of-shop-sections :doc "(shop-section) 	An array of shop-section objects (see below).")))

(def-api-class shop-section ()
  "Some sellers may choose to organize their listings into sections.  Each section is specific to a shop and has a numeric ID and a title."
  ((section-id :type int
               :doc "The numeric ID of the shop section.")
   (title :type string
          :doc "The title of the section.")
   (listing-count :type int
                  :doc "The number of active listings currently in the section.")))

(defmethod print-object ((x shop-section) stream)
  (dumb-printing stream x "~D: ~A" section-id title))

(defun demarshall-array-of-shop-sections (x)
  (loop for section-json in x
       as shop-section = (make-instance 'shop-section)
       collect (fill-out-etsy-object-from-json shop-section section-json)))

(def-api-class listing ()
  "Listing records represent an item for sale on Etsy."
  ((listing-id :level low :type int
               :doc "The numeric ID for this listing.")
   (state :level low :type string
          :doc "One of active, removed, soldout, expired, or alchemy.")
   (title :level low :type string
          :doc "The listing's title.")
   (url :level low :type string
        :doc "The full URL to the listing's page.")
   (image-url-25x25 :level low :type string
                    :doc "The full URL to a 25x25 thumbnail of the listing's image.")
   (image-url-50x50 :level low :type string
                    :doc "The full URL to a 50x50 thumbnail of the listing's image.")
   (image-url-75x75 :level low :type string
                    :doc "The full URL to a 75x75 thumbnail of the listing's image.")
   (image-url-155x125 :level low :type string
                      :doc "The full URL to a 155x125 thumbnail of the listing's image.")
   (image-url-200x200 :level low :type string
                      :doc "The full URL to a 200x200 thumbnail of the listing's image.")
   (image-url-430xN :level low :type string :json-key :IMAGE_URL_430X-N
                    :doc "The full URL to the listing's image, always 430 pixels wide.")
   (creation-epoch :level low :type float
                   :doc "The date and time the listing was posted, in epoch seconds.")
   (views :level medium :type int
          :doc "How many times the item has been viewed.")
   (tags :level medium :type string
         :doc "A dot-separated list of tags for the item.")
   (materials :level medium :type string
              :doc "A dot-separated list of materials used in the item.")
   (price :level medium :type float
          :doc "The item's price.")
   (currency-code :level medium :type string
                  :doc "The ISO (alphabetic) code for the item's currency.")
   (ending-epoch :level medium :type float
                 :doc "The listing's expiration date and time, in epoch seconds.")
   (sold-out-epoch :level medium :type float
                   :doc "When the item sold out.")
   (user-id :level medium :type int
            :doc "The numeric ID of the user who posted the item. (User IDs are also shop IDs).")
   (user-name :level medium :type string
              :doc "The login name of the user who posted the item.")
   (quantity :level medium :type int
             :doc "The quantity of this item available for sale.")
   (hsv-color :level medium :type hsv-color
              :doc "(int) 	The average color of the listing's primary image, in HSV format.")
   (rgb-color :level medium :type rgb-color
              :doc "The average color of the listing's primary image, in RGB hexadecimal (\"web\") format.")
   (description :level high :type string
                :doc "A description of the item.")
   (lat :level high :type float
        :doc "The latitude of the user selling the item (may be blank).")
   (lon :level high :type float
        :doc "The longitude of the user selling the item (may be blank).")
   (city :level high :type string
         :doc "The user's city and state (user-supplied; may be blank).")
   (section-id :level high :type int
               :doc "If the shop uses sections, the numeric ID of the section to which this listing belongs.")
   (section-title :level high :type string
                  :doc "The title of the section to which this listing belongs.")
   (favorite-creation-epoch :level favorites :type float
                            :doc "The date and time that the user was favorited (only available in the command getFavoriteListingsOfUser.)")
   ;; and also
   (user-image-id :level high :type int
                  :doc "No idea what this is")
   (num-favorers :level high :type int
                 :doc "Presumably, the number of hearts.")
   (all-images :level high :type image-booklet
               :doc "A list of set objects")
   ))

(defmethod print-object ((x listing) stream)
  (dumb-printing stream x "~D: ~A" listing-id title))

(def-api-class gift-guide ()
  "Gift guides display summary information about each gift guide on the Etsy website.  Gift Guides don't support the detail_level parameter; all fields are available at all times."
  ((guide-id :level :low :type int :doc "The numeric ID of this guide.")
   (creation-tsz-epoch :level :low :type float :doc "The date and time the gift guide was created, in epoch seconds.")
   (description :level :low :type string :doc "A short description of the guide")
   (title :level :low  :type string :doc "The guide's main title.")
   (display-order :level :low :type int  :doc "A field on which the guides can be sorted.")
   (guide-section-id :level :low :type int :doc "The numeric ID of the guide's parent section.")
   (guide-section-title :level :low :type string :doc "The title of the guide's parent section.")))

(defmethod print-object ((x gift-guide) stream)
  (dumb-printing stream x "~D: ~A" guide-id title))

(def-api-class feedback ()
  "Feedback records represent a comment left by either the buyer or seller in a transaction.  Every sale on Etsy creates two opportunities for a user to leave feedback: one, by the buyer commenting on the seller, and the second by the seller commenting on the buyer."
  ((feedback-id :type int
                :doc "The numeric ID of the feedback record.")
   (listing-id :type int
               :doc "The numeric ID of the sold item.")
   (title :type string
          :doc "The sold item's title.")
   (url :type string
        :doc "The fully-qualified URL to the sold item's listing page.")
   (creation-epoch :type float
                   :doc "The date and time the feedback was posted, in epoch seconds.")
   (author-user-id :type int
                   :doc "The user ID of the person user leaving the feedback.")
   (subject-user-id :type int
                    :doc "The user ID of the person receiving feedback.")
   (seller-user-id :type int
                   :doc "The user ID of the user acting as seller in this transaction.")
   (buyer-user-id :type int
                  :doc "The user ID of the user acting as buyer in this transaction.")
   (message :type string
            :doc "A short message, left by the author.")
   (disposition :type enum-to-keyword
                :doc "One of positive, neutral or negative.")
   (value :type int
          :doc "The numeric value of the feedback disposition (-1..1).")
   (image-url-25x25 :type string
                    :doc "The full URL to a thumbnail of an image posted by the feedback author (may be blank).")
   (image-url-fullxfull :type string
                        :doc "The full URL to an image posted by the feedback author (may be blank).")))

(defmethod print-object ((x feedback) stream)
  (dumb-printing stream x "~D: ~A ~A" feedback disposition title))

(def-api-class api-method ()
  "Method records are returned by the getMethodTable command, which
    lists all the functionality available in the Etsy API. Method records
    don't support the detail_level parameter; all fields are available at
    all times."
  ((name :level :low :type string :doc "The name for this Method")
   (description :level :low :type string :doc "A bit-o documenation")
   (uri :level :low :type string :doc "A template for the URL")
   (params :level :low :type method-parameter-table :doc "A json dictionary of parameter names and their types.")
   (type :level :low :type string :doc "The type of the result elements")
   (http-method :level :low  :type enum-to-keyword :doc "The HTTP method, currently always GET")))

(defun demarshall-method-parameter-table (x)
  x)

(defun demarshall-method (x)
  "Since method is locked, we avoid overloading it."
  (demarshall-api-method x))

(defmethod print-object ((x api-method) stream)
  (dumb-printing stream x "~a -> ~a" name type))
