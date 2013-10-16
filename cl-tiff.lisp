;;;; cl-tiff.lisp

(in-package #:cl-tiff)
(proclaim '(optimize (speed 0) (safety 3) (debug 3)))
;;; "cl-tiff" goes here. Hacks and glory await!

;;; Binary Types

;; tiff-endian per Section 2 Image File Header
;; II = Intel Endianness
;; MM = Motorola Endianness
(define-binary-type tiff-endian ()
  (:reader (in)
	   (let ((chars (read-value 'iso-8859-1-string in :length 2)))
	     (cond
	       ((equal chars "II") :little-endian)
	       ((equal chars "MM") :big-endian)
	       (t (error "Invalid endianness in TIFF header")))))
  (:writer (out data)
	   (cond
             ((equal data :big-endian) (format out "MM"))
             ((equal data :little-endian) (format out "II"))
             (t (error "Invalid endianness: ~A" data)))))


(define-binary-type srational () (rational :type 's4-o))
(define-enumeration ifd-type 
    (u1           1)
  (null-terminated-string 2)
  (u2		3)
  (u4		4)
  (rational     5)
  (s1		6)
  (undefined-byte 7)
  (s2		8)
  (s4		9)
  (srational    10)
  (single-float	11)
  (double-float  12)
  (ifd          13))

(define-binary-type ifd-entries (count)
  (:reader (in)
           ;; debug
	   ;;(format t "~%Reading ifd-entries, count=~A~%" count)
	   (let ((result (make-array count :element-type 'ifd-entry)))
	     (handler-bind ((no-symbol-for-value-enum #'binary-data:return-unknown-value))
               (dotimes (i count result)
                 (setf (elt result i) (read-value 'ifd-entry in))
                 ;; debug
                 ;; (print (elt result i))
                 ))))
  (:writer (out data)
	   (map 'nil (lambda (entry)
                       (write-value 'ifd-entry out entry))
                data)))

(define-binary-type ifd-pointer (type)
  (:reader (in)
           (let ((pointer (read-value 'ifd-pointer-type in)))
             (setf (value-type pointer) type)
             pointer))
  (:writer (out data)
           (write-value 'ifd-pointer-type out data)))

(define-binary-class ifd-pointer-type ()
  ((offset u4)
   (value-type (optional))
   (value (optional))
   (entry-count (optional))))
(defmethod print-object ((obj ifd-pointer-type) stream)
  (print-unreadable-object (obj stream :type t)
    (with-slots (offset value-type value) obj
      (format stream "type:~A at offset:~A ~A" value-type offset
	      (if value
		  (format nil "evaluated to ~A" value)
		  "unevaluated")))))

(define-condition invalid-ifd-type (error)
  ((ifd-type :initarg :type :reader ifd-type)))
(defun get-type-size (type)
  (cond
    ((listp type) (get-type-size (car type)))
    ((member type '(u1 s1-o iso-8859-1-string undefined-byte null-terminated-string)) 1)
    ((member type '(u2 s2-o u2-o)) 2)
    ((member type '(u4 s4-o u4-o single-float)) 4)
    ((member type '(double-float rational srational)) 8)
    (t (restart-case (error 'invalid-ifd-type :ifd-type type)
	 (return-zero () 0)))))

(defun read-non-maybe-pointer (type entry-count in)
  ;;(format t "Reading non-maybe-pointer~%")
  (let ((result (if (> entry-count 1)
                    (let ((final-value (make-array entry-count)))
                      (dotimes (i entry-count final-value)
                        (setf (elt final-value i)
                              (eval (type->read-value type in)))))
                    (eval (type->read-value type in)))))

    ;; Ignore the rest of the data
    (let ((ignored-data-size (- 4 (* entry-count (get-type-size type)))))
      (unless (zerop ignored-data-size)
        ;; (incf (file-position in) ignored-data-size)
        (file-position in
                       (+ (file-position in)
                          ignored-data-size))))

    result))

(define-binary-type maybe-pointer (type entry-count)
  (:reader (in)
	   (handler-bind ((invalid-ifd-type #'return-zero))
	     (let ((type-size (get-type-size type)))
	       ;; (format t "maybe-pointer: id:~A type:~A entry-count:~A type-size:~A~%"
	       ;; 	       (id (current-binary-object)) type entry-count type-size)
	       (if (<= (* entry-count type-size) 4)
                   (progn (setf (value (current-binary-object))
                                (read-non-maybe-pointer type entry-count in))
                          nil)
                   (read-value 'ifd-pointer-type in)))))
  (:writer (out data)
           (declare (ignorable out data))
           (error "No writer defined for maybe-pointer")
	   ;; TODO something
	   ))

(define-binary-type ifd-pointer-space (offset)
  (:reader (in)
           (file-position in
                          (+ offset
                             (offset
                              (first-ifd-pointer
                               (current-binary-object)))))
           (do ((ifd-list (list (read-value 'ifd in))
                          (cons (read-value 'ifd in) ifd-list)))
               ((zerop (offset (next-ifd (car ifd-list)))) (nreverse ifd-list))
             (file-position in (+ offset
                                  (offset (next-ifd (car ifd-list)))))
             
             ;; (format t "Finished an ifd, at offset ~A~%"
             ;;         (- (file-position in) offset))
             ))
  (:writer (out data)
           (error "Fix writer for ifd-pointer-space")
	   (write-value 'ifd-pointer out data)))

;;; Section 2: TIFF Structure Definition

(define-binary-class image-file-header ()
  ((endian tiff-endian
           :post-read (setf monkey-types:*endianness* endian)
           :post-write (setf monkey-types:*endianness* endian))
   (fourty-two u2
               :post-read (unless (= 42 fourty-two)
                            (error "Fourty-two in the TIFF header reads as ~A" fourty-two)))
   (first-ifd-pointer (ifd-pointer-type))
   (ifd-list (ifd-pointer-space :offset tiff-start)
             :post-read (setf monkey-types:*endianness* :big-endian)
             :post-write (setf monkey-types:*endianness* :big-endian))))

(defmethod print-object ((obj image-file-header) stream)
  (print-unreadable-object (obj stream :type t)
    (with-slots (endian fourty-two) obj
      (format stream "endianness:~A, fourty-two:~A"
	      endian fourty-two))))

;;; Image File Directory per Section 2
(define-binary-class ifd ()
  ((num-entries u2)

   ;;Per rev 6.0, entries should be sorted in ascending order by tag
   (entries (ifd-entries :count num-entries)) 
   (next-ifd (ifd-pointer :type 'ifd))))
(defmethod print-object ((obj ifd) stream)
  (print-unreadable-object (obj stream :type t)
    (with-slots (num-entries next-ifd) obj
      (format stream "~A entries~A"
              num-entries
              (if next-ifd
                  " with pointer to another IFD"
                  "")))))

;;; IFD entry per Section 2 under Image File directory
(define-binary-class ifd-entry ()
  ((id (ifd-tag :type 'u2))
   (entry-type (ifd-type :type 'u2))
   (entry-count u4)
   (value (optional))
   (pointer (maybe-pointer :type entry-type
			   :entry-count entry-count))))
(defmethod print-object ((obj ifd-entry) stream)
  (print-unreadable-object (obj stream :type t)
    (with-slots (id entry-type entry-count pointer value) obj
      (format stream "id:~A, type:~A, count:~A"
	      id (if (listp entry-type)
		     (car entry-type)
		     entry-type)
	      entry-count)
      (if value
          (format stream ", value:~A" value)
          (format stream ", pointer:~A" pointer)))))

(define-enumeration ifd-type 
  (u1           1)
  (null-terminated-string 2)
  (u2		3)
  (u4		4)
  (rational     5)
  (s1-o		6)
  (undefined-byte 7)
  (s2-o		8)
  (s4-o		9)
  (srational    10)
  (single-float	11)
  (double-float  12))

(define-binary-type null-terminated-string ()
  (iso-8859-1-terminated-string :terminator +null+))

(defun read-ifd-entries (ifd in)
  (dotimes (i (num-entries ifd))
    (let ((entry (elt (entries ifd) i)))
      ;;(print (pointer entry))
      (when (typep (pointer entry) 'ifd-pointer-type)
        (file-position in (offset (pointer entry)))
        (setf (value entry)
              (eval (type->read-value (entry-type entry) in)))))))

(defun read-all-ifds (tiff in)
  (map 'nil (lambda (ifd) (read-ifd-entries ifd in))
       (ifd-list tiff))
  tiff)

;;; Public Interface
(defun read-tiff (in &optional size)
  (let ((tiff-start (file-position in)))
    (declare (special tiff-start))
    (prog1 (read-value 'image-file-header in)
      (when size (file-position in (+ tiff-start size))))))

(defun read-tiff-file (file &key eager)
  (with-open-file (in file
                      :direction :input
                      :element-type '(unsigned-byte 8))
    (let ((tiff (read-tiff in)))
      (if eager
          (read-all-ifds tiff in)
          tiff))))
