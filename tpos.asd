(defpackage #:tpos
  (:use #:cl #:asdf))
(in-package #:tpos)

(defsystem #:tpos
    :name TPOS
    :author "Matthew George <matt@mattgeorge.net>"
    :description "Twitter Machine Learning" 
    :serial t
    :components ((:file "distance")
		 (:file "scripts"))
    :depends-on ("cl-json"
		 "cl-utilities"
		 "vana-inflector"
		 "cl-ppcre"
		 "cl-heap"))
