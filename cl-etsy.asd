;; -*- mode: lisp; syntax: common-lisp; -*-

(defsystem cl-etsy
  :version "0.2"
  :author "Ben Hyde <bhyde@pobox.com>"
  :licence "Apache 2.0"
  :depends-on (cl-ppcre cl-json drakma)
  :serial t
  :components ((:file "packages")
               (:file "base")
               (:file "api-utils")
               (:file "types")
               (:file "methods")
               (:file "main")
               #+5am (:file "tests")
               ))

