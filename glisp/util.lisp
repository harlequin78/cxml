;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: GLISP; -*-
;;; ---------------------------------------------------------------------------
;;;     Title: Some common utilities for the Closure browser
;;;   Created: 1997-12-27
;;;    Author: Gilbert Baumann <unk6@rz.uni-karlsruhe.de>
;;;   License: GPL (See file COPYING for details).
;;; ---------------------------------------------------------------------------
;;;  (c) copyright 1997-1999 by Gilbert Baumann

;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this program; if not, write to the Free Software
;;; Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.

;; Changes
;;
;;  When        Who     What
;; ----------------------------------------------------------------------------
;;  1999-08-24  GB      = fixed MULTIPLE-VALUE-OR it now takes any number of
;;                        subforms
;;

(in-package :glisp)

;;; --------------------------------------------------------------------------------
;;;  Meta functions

(defun curry (fun &rest args)
  #'(lambda (&rest more)
      (apply fun (append args more))))

(defun rcurry (fun &rest args)
  #'(lambda (&rest more)
      (apply fun (append more args))))

(defun compose (f g)
  #'(lambda (&rest args)
      (funcall f (apply g args))))

(defun always (value)
  #'(lambda (&rest args)
      (declare (ignore args))
      value))

(defun true (&rest x)
  (declare (ignore x))
  t)

(defun false (&rest x)
  (declare (ignore x))
  nil)

;;; --------------------------------------------------------------------------------
;;;  while and until

(defmacro while (test &body body)
  `(until (not ,test) ,@body))

(defmacro until (test &body body)
  `(do () (,test) ,@body))

;;; --------------------------------------------------------------------------------
;;;  Strings

(defun white-space-p (ch)
  ;;(declare #.cl-user:+optimize-very-fast-trusted+)
  (or (eq ch #\Return)
      (eq ch #\Newline)
      (eq ch #\Space)
      (eq ch #\Tab)
      (eq ch #\Page)))

(define-compiler-macro white-space-p (ch)
  `(member ,ch '(#\Return #\Newline #\Space #\Tab #\Page)) )

;;;; -----------------------------------------------------------------------------------------
;;;;  Homebrew stream classes
;;;;

;; I am really tired of standard Common Lisp streams and thier incompatible implementations.

;; A gstream is an objects with obeys to the following protocol:

;; g/read-byte stream &optional (eof-error-p t) eof-value
;; g/unread-byte byte stream
;; g/read-char stream &optional (eof-error-p t) eof-value
;; g/unread-char char stream
;; g/write-char char stream
;; g/write-byte byte stream
;; g/finish-output stream
;; g/close stream &key abort

;; Additionally the follwing generic functions are implemented based
;; on the above protocol and may be reimplemented for any custom
;; stream class for performance.

;; g/write-string string stream &key start end
;; g/read-line stream &optional (eof-error-p t) eof-value
;; g/read-line* stream &optional (eof-error-p t) eof-value
;; g/read-byte-sequence sequence stream &key start end
;; g/read-char-sequence sequence stream &key start end
;; g/write-byte-sequence sequence stream &key start end
;; g/write-char-sequence sequence stream &key start end


;; The following classes exists

;; gstream
;; use-char-for-byte-stream-flavour
;; use-byte-for-char-stream-flavour
;; cl-stream
;; cl-byte-stream
;; cl-char-stream

(defclass gstream () ())

;;; use-char-for-byte-stream-flavour 

(defclass use-char-for-byte-stream-flavour () ())

(defmethod g/read-byte ((self use-char-for-byte-stream-flavour) &optional (eof-error-p t) eof-value)
  (let ((r (g/read-char self eof-error-p :eof)))
    (if (eq r :eof)
        eof-value
      (char-code r))))

(defmethod g/unread-byte (byte (self use-char-for-byte-stream-flavour))
  (g/unread-char (or (and #+CMU (<= byte char-code-limit) (code-char byte))
                     (error "Cannot stuff ~D. into a character." byte))
                 self))

(defmethod g/write-byte (byte (self use-char-for-byte-stream-flavour))
  (g/write-char (or (and #+CMU (<= byte char-code-limit) (code-char byte))
                    (error "Cannot stuff ~D. into a character." byte))
                self))

;;; use-byte-for-char-stream-flavour

(defclass use-byte-for-char-stream-flavour () ())

(defmethod g/read-char ((self use-byte-for-char-stream-flavour) &optional (eof-error-p t) eof-value)
  (let ((byte (g/read-byte self eof-error-p :eof)))
    (if (eq byte :eof)
        eof-value
      (let ((res (and #+CMU (<= byte char-code-limit) (code-char byte))))
        (or res
            (error "The byte ~D. could not been represented as character in your LISP implementation." byte))))))

(defmethod g/unread-char (char (self use-byte-for-char-stream-flavour))
  (g/unread-byte (char-code char) self))

(defmethod g/write-char (char (self use-byte-for-char-stream-flavour))
  (g/write-byte (char-code char) self))

;;; ------------------------------------------------------------
;;; Streams made up out of Common Lisp streams

;;; cl-stream

(defclass cl-stream (gstream)
  ((cl-stream :initarg :cl-stream)))

(defmethod g/finish-output ((self cl-stream))
  (with-slots (cl-stream) self
    (finish-output cl-stream)))

(defmethod g/close ((self cl-stream) &key abort)
  (with-slots (cl-stream) self
    (close cl-stream :abort abort)))

;;; cl-byte-stream

(defclass cl-byte-stream (use-byte-for-char-stream-flavour cl-stream)
  ((lookahead :initform nil)))

(defmethod g/read-byte ((self cl-byte-stream) &optional (eof-error-p t) eof-value)
  (with-slots (cl-stream lookahead) self
    (if lookahead
        (prog1 lookahead 
          (setf lookahead nil))
      (read-byte cl-stream eof-error-p eof-value))))

(defmethod g/unread-byte (byte (self cl-byte-stream))
  (with-slots (cl-stream lookahead) self
    (if lookahead
        (error "You cannot unread twice.")
      (setf lookahead byte))))

(defmethod g/write-byte (byte (self cl-byte-stream))
  (with-slots (cl-stream) self
    (write-byte byte cl-stream)))

(defmethod g/read-byte-sequence (sequence (input cl-byte-stream) &key (start 0) (end (length sequence)))
  (with-slots (cl-stream) input
    (read-byte-sequence sequence cl-stream :start start :end end)))

(defmethod g/write-byte-sequence (sequence (sink cl-byte-stream) &key (start 0) (end (length sequence)))
  (with-slots (cl-stream) sink
    (cl:write-sequence sequence cl-stream :start start :end end)))

;;; cl-char-stream

(defclass cl-char-stream (use-char-for-byte-stream-flavour cl-stream)
  ())

(defmethod g/read-char ((self cl-char-stream) &optional (eof-error-p t) eof-value)
  (with-slots (cl-stream) self
    (read-char cl-stream eof-error-p eof-value)))

(defmethod g/unread-char (char (self cl-char-stream))
  (with-slots (cl-stream) self
    (unread-char char cl-stream)))

(defmethod g/write-char (char (self cl-char-stream))
  (with-slots (cl-stream) self
    (write-char char cl-stream)))

;;; ------------------------------------------------------------
;;; General or fall back stream methods

(defmethod g/write-string (string (stream t) &key (start 0) (end (length string)))
  (do ((i start (+ i 1)))
      ((>= i end))
    (g/write-char (char string i) stream)))

(defmethod g/read-line ((stream t) &optional (eof-error-p t) eof-value) 
  (let ((res nil))
    (do ((c (g/read-char stream eof-error-p :eof)
            (g/read-char stream nil :eof)))
        ((or (eq c :eof) (char= c #\newline))
         (cond ((eq c :eof) 
                (values (if (null res) eof-value (coerce (nreverse res) 'string))
                        t))
               (t
                (values (coerce (nreverse res) 'string)
                        nil))))
      (push c res))))

(defmethod g/read-line* ((stream t) &optional (eof-error-p t) eof-value) 
  ;; Like read-line, but accepts CRNL, NL, CR as line termination
  (let ((res nil))
    (do ((c (g/read-char stream eof-error-p :eof)
            (g/read-char stream nil :eof)))
        ((or (eq c :eof) (char= c #\newline) (char= c #\return))
         (cond ((eq c :eof) 
                (values (if (null res) eof-value (coerce (nreverse res) 'string))
                        t))
               (t
                (when (char= c #\return)
                  (let ((d (g/read-char stream nil :eof)))
                    (unless (or (eq d :eof) (char= d #\newline))
                      (g/unread-char d stream))))
                (values (coerce (nreverse res) 'string)
                        nil))))
      (push c res))))

(defmethod g/read-byte-sequence (sequence (input t) &key (start 0) (end (length sequence)))
  (let ((i start) c)
    (loop
      (when (>= i end)
        (return i))
      (setf c (g/read-byte input nil :eof))
      (when (eq c :eof)
        (return i))
      (setf (elt sequence i) c)
      (incf i))))

(defmethod g/read-char-sequence (sequence (input t) &key (start 0) (end (length sequence)))
  (let ((i start) c)
    (loop
      (when (>= i end)
        (return i))
      (setf c (g/read-char input nil :eof))
      (when (eq c :eof)
        (return i))
      (setf (elt sequence i) c)
      (incf i))))

(defmethod g/write-byte-sequence (sequence (sink t) &key (start 0) (end (length sequence)))
  (do ((i start (+ i 1)))
      ((>= i end) i)
    (g/write-byte (aref sequence i) sink)))

;;; ----------------------------------------------------------------------------------------------------
;;;  Vector streams
;;;

;; Output

(defclass vector-output-stream (use-byte-for-char-stream-flavour)
  ((buffer :initarg :buffer)))

(defun g/make-vector-output-stream (&key (initial-size 100))
  (make-instance 'vector-output-stream
    :buffer (make-array initial-size :element-type '(unsigned-byte 8)
                        :fill-pointer 0
                        :adjustable t)))

(defmethod g/close ((self vector-output-stream) &key abort)
  (declare (ignorable self abort))
  nil)

(defmethod g/finish-output ((self vector-output-stream))
  nil)

(defmethod g/write-byte (byte (self vector-output-stream))
  (with-slots (buffer) self
    (vector-push-extend byte buffer 100)))

(defmethod g/write-byte-sequence (sequence (self vector-output-stream) &key (start 0) (end (length sequence)))
  (with-slots (buffer) self
    (adjust-array buffer (+ (length buffer) (- end start)))
    (replace buffer sequence :start1 (length buffer) :start2 start :end2 end)
    (setf (fill-pointer buffer) (+ (length buffer) (- end start)))
    end))

;;----------------------------------------------------------------------------------------------------

(defun g/peek-char (&optional (peek-type nil) (source *standard-input*)
                              (eof-error-p t) eof-value)
  (cond ((eq peek-type t)
         (do ((ch (g/read-char source eof-error-p '%the-eof-object%)
                  (g/read-char source eof-error-p '%the-eof-object%)))
             ((or (eq ch '%the-eof-object%)
                  (not (white-space-p ch)))
              (cond ((eq ch '%the-eof-object%) eof-value)
                    (t (g/unread-char ch source) ch)) )))
        ((eq peek-type nil)
         (let ((ch (g/read-char source eof-error-p '%the-eof-object%)))
           (cond ((eq ch '%the-eof-object%) eof-value)
                 (t (g/unread-char ch source)
                    ch))))
        ((characterp peek-type)
         (do ((ch (g/read-char source eof-error-p '%the-eof-object%)
                  (g/read-char source eof-error-p '%the-eof-object%)))
             ((or (eq ch '%the-eof-object%) (eql ch peek-type))
              (cond ((eq ch '%the-eof-object%) eof-value)
                    (t (g/unread-char ch source) ch)))))))



(defun cl-byte-stream->gstream (stream)
  (make-instance 'cl-byte-stream :cl-stream stream))

(defun cl-char-stream->gstream (stream)
  (make-instance 'cl-char-stream :cl-stream stream))

;; Stolen from Eclipse (http://elwoodcorp.com/eclipse/unique.htm

(defmacro with-unique-names ((&rest names) &body body)
  `(let (,@(mapcar (lambda (x) (list x `(gensym ',(concatenate 'string (symbol-name x) "-")))) names))
     .,body))


(defun gstream-as-string (gstream &optional (buffer-size 4096))
  (let ((buffer (g/make-string buffer-size :adjustable t)))
    (do* ((i 0 j)
          (j (g/read-char-sequence buffer gstream :start 0 :end buffer-size)
             (g/read-char-sequence buffer gstream :start i :end (+ i buffer-size)) ))
        ((= j i) (subseq buffer 0 j))
      (adjust-array buffer (list (+ j buffer-size))) )))

;;;; Generic hash tables

;; TODO: 
;; - automatic size adjustment
;; - sensible printer
;; - make-load-form?!

(defstruct g/hash-table
  hash-function                         ;hash function
  compare-function                      ;predicate to test for equality
  table                                 ;simple vector of chains
  size                                  ;size of hash table
  (nitems 0))                           ;number of items

(defun g/make-hash-table (&key (size 100) (hash-function #'sxhash) (compare-function #'eql))
  "Creates a generic hashtable;
   `size' is the default size of the table.
   `hash-function' (default #'sxhash) is a specific hash function
   `compare-function' (default #'eql) is a predicate to test for equality."
  (setf size (nearest-greater-prime size))
  (make-g/hash-table :hash-function hash-function
                     :compare-function compare-function
                     :table (make-array size :initial-element nil)
                     :size size
                     :nitems 0))
        
(defun g/hashget (hashtable key &optional (default nil))
  "Looks up the key `key' in the generic hash table `hashtable'. 
   Returns three values:
   value     - value, which as associated with the key, or `default' is no value 
               present.
   successp  - true, iff the key was found.
   key       - the original key in the hash table."
  ;; -> value ; successp ; key
  (let ((j (mod (funcall (g/hash-table-hash-function hashtable) key)
                (g/hash-table-size hashtable))))
    (let ((q (assoc key (aref (g/hash-table-table hashtable) j)
                    :test (g/hash-table-compare-function hashtable))))
      (if q
          (values (cdr q) t (car q))
        (values default nil)))))

(defun (setf g/hashget) (new-value hashtable key &optional (default nil))
  (declare (ignore default))
  (let ((j (mod (funcall (g/hash-table-hash-function hashtable) key)
                (g/hash-table-size hashtable))))
    (let ((q (assoc key (aref (g/hash-table-table hashtable) j)
                    :test (g/hash-table-compare-function hashtable))))
      (cond ((not (null q))
             (setf (cdr q) new-value))
            (t
             (push (cons key new-value)
                   (aref (g/hash-table-table hashtable) j))
             (incf (g/hash-table-nitems hashtable))))))
  new-value)

(defun resize-hash-table (hashtable new-size)
  "Adjust the size of a generic hash table. (the size is round to the next greater prime number)."
  (setf new-size (nearest-greater-prime new-size))
  (let ((new-table (make-array new-size :initial-element nil)))
    (dotimes (i (g/hash-table-size hashtable))
      (dolist (k (aref (g/hash-table-table hashtable) i))
        (push k (aref new-table
                      (mod (funcall (g/hash-table-hash-function hashtable) (car k))
                           new-size)))))
    (setf (g/hash-table-table hashtable) new-table
          (g/hash-table-size hashtable) new-size)
    hashtable))

(defun g/clrhash (hashtable)
  "Clears a generic hash table."
  (dotimes (i (g/hash-table-size hashtable))
    (setf (aref (g/hash-table-table hashtable) i) nil))
  (setf (g/hash-table-nitems hashtable) nil)
  hashtable)

;; hash code utilities

(defconstant +fixnum-bits+
    (1- (integer-length most-positive-fixnum))
  "Pessimistic approximation of the number of bits of fixnums.")

(defconstant +fixnum-mask+
    (1- (expt 2 +fixnum-bits+))
  "Pessimistic approximation of the largest bit-mask, still being a fixnum.")

(defun stir-hash-codes (a b)
  "Stirs two hash codes together; always returns a fixnum.
   When applied sequenitally the first argument should be used as accumulator."
  ;; ich mach das mal wie Bruno
  (logand +fixnum-mask+
          (logxor (logior (logand +fixnum-mask+ (ash a 5))
                          (logand +fixnum-mask+ (ash a (- 5 +fixnum-bits+)))) 
                  b)))

(defun hash-sequence (sequence hash-function &optional (accu 0))
  "Applies the hash function `hash-function' to each element of `sequence' and
   stirs the resulting hash codes together using STIR-HASH-CODE starting from 
   `accu'."
  (map nil (lambda (item)
             (setf accu (stir-hash-codes accu (funcall hash-function item))))
       sequence)
  accu)

;; some specific hash functions

(defun hash/string-equal (string)
  "Hash function compatible with STRING-EQUAL."
  (hash-sequence string (lambda (char)
                          (sxhash (char-upcase char)))))

;; some specific hash tables

(defun make-string-equal-hash-table (&rest options)
  "Constructs a new generic hash table using STRING-EQUAL as predicate."
  (apply #'g/make-hash-table 
         :hash-function #'hash/string-equal
         :compare-function #'string-equal
         options))

;; prime numbers

(defun primep (n)
  "Returns true, iff `n' is prime."
  (and (> n 2)
       (do ((i 2 (+ i 1)))
           ((> (* i i) n) t)
         (cond ((zerop (mod n i)) (return nil))))))

(defun nearest-greater-prime (n)
  "Returns the smallest prime number no less than `n'."
  (cond ((primep n) n)
        ((nearest-greater-prime (+ n 1)))))
