;;;; package.lisp

(defpackage #:cl-tiff
  (:use #:cl #:binary-data #:monkey-types)
  (:shadowing-import-from :alexandria if-let when-let appendf)
  (:export #:read-tiff #:read-tiff-file))

