;;;; cl-tiff.asd

(asdf:defsystem #:cl-tiff
  :serial t
  :description "TIFF file parser per Rev 6.0"
  :author "Nick Patrick <npatrick04@gmail.com>"
  :license "BSD 3-clause"
  :depends-on (#:binary-data)
  :components ((:file "package")
               (:file "tiff-ids" :depends-on ("package"))
               (:file "cl-tiff" :depends-on ("package" "tiff-ids"))))

