;;;; catalogs.lisp -- XML Catalogs  -*- Mode: Lisp; readtable: runes -*-
;;;;
;;;; This file is part of the CXML parser, released under (L)LGPL.
;;;; See file COPYING for details.
;;;;
;;;; Developed 2004 for headcraft - http://headcraft.de/
;;;; Copyright: David Lichteblau

(in-package :cxml)

;;; http://www.oasis-open.org/committees/entity/spec.html
;;;
;;; Bugs:
;;;   - We validate using the Catalog DTD while parsing, which is too strict
;;;     and will will fail to parse files using other parser's extensions.
;;;     (Jedenfalls behauptet das die Spec.)
;;;     A long-term solution might be an XML Schema validator.

(defvar *prefer* nil)
(defvar *catalog*
    '(;; libxml standard
      "/etc/xml/catalog"
      ;; FreeBSD
      "/usr/local/share/xml/catalog.ports"))

(defstruct (entry-file (:conc-name ""))
  (system-entries)                      ;extid 2
  (rewrite-system-entries)              ;      3
  (delegate-system-entries)             ;      4
  (public-entries)                      ;      5
  (delegate-public-entries)             ;      6
  (uri-entries)                         ;uri 2
  (rewrite-uri-entries)                 ;    3
  (delegate-uri-entries)                ;    4
  (next-catalog-entries)                ;    5/7
  )

(defun starts-with-p (string prefix)
  (let ((mismatch (mismatch string prefix)))
    (or (null mismatch) (= mismatch (length prefix)))))

(defun normalize-public (str)
  (flet ((whitespacep (c)
           (find c #.(map 'string #'code-char '(#x9 #xa #xd #x20)))))
    (let ((start (position-if-not #'whitespacep str))
          (end (position-if-not #'whitespacep str :from-end t))
          (spacep nil))
      (with-output-to-string (out)
        (when start
          (loop for i from start to end do
                (let ((c (char str i)))
                  (cond
                    ((whitespacep c)
                      (unless spacep
                        (setf spacep t)
                        (write-char #\space out)))
                    (t
                      (setf spacep nil)
                      (write-char c out))))))))))

(defun normalize-uri (str)
  ;; assumes that UTF-8 encoding has already been done
  (with-output-to-string (out)
    (loop for ch across str do
          (let ((c (char-code ch)))
            (if (< c 15)
                (write-string (string-upcase (format nil "%~2,'0X" c)) out)
                (write-char ch out))))))

(defun unwrap-publicid (str)
  (normalize-public
   (with-output-to-string (out)
     (let ((i (length "urn:publicid:"))
           (n (length str)))
       (while (< i n)
         (let ((c (char str i)))
           (case c
             (#\+ (write-char #\space out))
             (#\: (write-string "//" out))
             (#\; (write-string "::" out))
             (#\%
               (let ((code
                      (parse-integer str
                                     :start (+ i 1)
                                     :end (+ i 3)
                                     :radix 16)))
                 (write-char (code-char code) out))
               (incf i 2))
             (t (write-char c out))))
         (incf i))))))

(defun resolve-extid (public system catalog)
  (setf public (normalize-public public))
  (setf system (normalize-uri system))
  (when (and system (starts-with-p system "urn:publicid:"))
    (let ((new-public (unwrap-publicid system)))
      (assert (or (null public) (equal public new-public)))
      (setf public new-public
            system nil)))
  #+(or)
  (when system
    (dolist (entry (system-entries file))))
  (dolist (entry catalog)
    (destructuring-bind (type from to prefer) entry
      (case type
        (:public
          (when (and (or (eq prefer :public) (null system))
                     (equal from public))
            (return to)))
        (:system
          (when (equal from system)
            (return to)))
        (:rewrite-system
          (when (starts-with-p system from)
            (return
              ;; XXX choose longest match
              (concatenate 'string
                to
                (subseq system 0 (length from))))))
        (:delegate-public
          (when (and (or (eq prefer :public) (null system))
                     (starts-with-p public from))
            ;; FIXME
            (resolve-extid public system to)))
        (:delegate-system
          (when (starts-with-p system from)
            ;; FIXME
            (resolve-extid public system to)))))))

(defun resolve-uri (uri catalog)
  (setf uri (normalize-uri uri))
  (when (starts-with-p uri "urn:publicid:")
    (return-from resolve-uri (resolve-extid (unwrap-publicid uri) nil catalog)))
  (dolist (entry catalog)
    (destructuring-bind (type from to) entry
      (case type
        (:uri
          (when (equal from uri)
            (return to)))
        (:rewrite-uri
          (when (starts-with-p uri from)
            (return
              ;; XXX choose longest match
              (concatenate 'string
                to
                (subseq uri 0 (length from))))))
        (:delegate-uri
          (when (starts-with-p uri from)
            ;; FIXME
            (resolve-uri uri to)))))))

(defun find-catalog-file (uri catalog)
  (setf uri (puri:intern-uri
             (if (stringp uri) (safe-parse-uri uri) uri)
             catalog))
  (setf (getf (puri:uri-plist uri) 'catalog)
        (parse-catalog-file uri)))

(defun make-catalog (files)
  (let ((result (puri:make-uri-space)))
    (dolist (file files)
      (find-catalog-file file result)) 
    result))

(defun parse-catalog-file (uri)
  (handler-case
      (parse-catalog-file/strict uri)
    (file-error () nil)
    (parser-error () nil)))

(defun parse-catalog-file/strict (uri)
  (let* ((cxml
          (slot-value (asdf:find-system :cxml) 'asdf::relative-pathname))
         (dtd
          (pathname-to-uri (merge-pathnames "catalog.dtd" cxml))))
    (with-open-stream (s (open (uri-to-pathname uri)
                               :element-type '(unsigned-byte 8)
                               :direction :input))
      (parse-stream s
                    (make-instance 'catalog-parser :uri uri)
                    :validate t
                    :dtd (make-extid nil dtd)
                    :root #"catalog"))))

(defclass catalog-parser ()
  ((result :initform '() :accessor result)
   (next :initform '() :accessor next)
   (prefer-stack :initform (list *prefer*) :accessor prefer-stack)
   (base-stack :accessor base-stack)))

(defmethod initialize-instance :after
    ((instance catalog-parser) &key uri)
  (setf (base-stack instance) (list uri)))

(defmethod prefer ((handler catalog-parser))
  (car (prefer-stack handler)))

(defmethod base ((handler catalog-parser))
  (car (base-stack handler)))

(defun get-attribute/lname (name attributes)
  (let ((a (find name attributes
                 :key (lambda (a)
                        (or (sax:attribute-local-name a)
                            (sax:attribute-qname a)))
                 :test #'rod=)))
    (and a (sax:attribute-value a))))

(defmethod sax:start-element ((handler catalog-parser) uri lname qname attrs)
  (declare (ignore uri))
  (setf lname (or lname qname))
  ;; we can dispatch on lnames only because we validate against the DTD,
  ;; which disallows other namespaces.
  (push (string-or (get-attribute/lname #"prefer" attrs) (prefer handler))
        (prefer-stack handler))
  (push (string-or (get-attribute/lname #"base" attrs) (base handler))
        (base-stack handler))
  (flet ((geturi (lname)
           (puri:merge-uris
            (safe-parse-uri (rod-string (get-attribute/lname lname attrs)))
            (base handler))))
    (cond
      ((rod= lname #"public")
        (push (list (normalize-public (get-attribute/lname #"publicId" attrs))
                    (geturi #"uri")
                    (prefer handler))
              (public-entries (result handler))))
      ((rod= lname #"system")
        (push (list (normalize-uri (get-attribute/lname #"systemId" attrs))
                    (geturi #"uri"))
              (system-entries (result handler))))
      ((rod= lname #"uri")
        (push (list (normalize-uri (get-attribute/lname #"name" attrs))
                    (geturi #"uri"))
              (uri-entries (result handler))))
      ((rod= lname #"rewriteSystem")
        (push (list (normalize-uri
                     (get-attribute/lname #"systemIdStartString" attrs))
                    (get-attribute/lname #"rewritePrefix" attrs))
              (rewrite-system-entries (result handler))))
      ((rod= lname #"rewriteURI")
        (push (list (normalize-uri
                     (get-attribute/lname #"uriStartString" attrs))
                    (get-attribute/lname #"rewritePrefix" attrs))
              (rewrite-uri-entries (result handler))))
      ((rod= lname #"delegatePublic")
        (push (list (normalize-public
                     (get-attribute/lname #"publicIdStartString" attrs))
                    (geturi #"catalog")
                    (prefer handler))
              (delegate-public-entries (result handler))))
      ((rod= lname #"delegateSystem")
        (push (list (normalize-uri
                     (get-attribute/lname #"systemIdStartString" attrs))
                    (geturi #"catalog"))
              (delegate-system-entries (result handler))))
      ((rod= lname #"delegateURI")
        (push (list (normalize-uri
                     (get-attribute/lname #"uriStartString" attrs))
                    (geturi #"catalog"))
              (delegate-uri-entries (result handler))))
      ((rod= lname #"nextCatalog")
        (push (geturi #"catalog")
              (next-catalog-entries (result handler)))))))

(defmethod sax:end-element ((handler catalog-parser) uri lname qname)
  (declare (ignore uri lname qname))
  (pop (base-stack handler))
  (pop (prefer-stack handler)))

(defmethod sax:end-document ((handler catalog-parser))
  (result handler))
