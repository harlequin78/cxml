;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: XML; readtable: runes; Encoding: utf-8; -*-
;;; ---------------------------------------------------------------------------
;;;     Title: Dump canonic XML according to J.Clark
;;;   Created: 1999-09-09
;;;    Author: Gilbert Baumann <unk6@rz.uni-karlsruhe.de>
;;;   License: LGPL (See file COPYING for details).
;;; ---------------------------------------------------------------------------
;;;  © copyright 1999 by Gilbert Baumann

;;; This library is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU Library General Public
;;; License as published by the Free Software Foundation; either
;;; version 2 of the License, or (at your option) any later version.
;;;
;;; This library is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; Library General Public License for more details.
;;;
;;; You should have received a copy of the GNU Library General Public
;;; License along with this library; if not, write to the 
;;; Free Software Foundation, Inc., 59 Temple Place - Suite 330, 
;;; Boston, MA  02111-1307  USA.

(in-package :cxml)

;; 
;; | Canonical XML
;; | =============
;; |                                      
;; | This document defines a subset of XML called canonical XML. The
;; | intended use of canonical XML is in testing XML processors, as a
;; | representation of the result of parsing an XML document.
;; | 
;; | Every well-formed XML document has a unique structurally equivalent
;; | canonical XML document. Two structurally equivalent XML documents have
;; | a byte-for-byte identical canonical XML document. Canonicalizing an
;; | XML document requires only information that an XML processor is
;; | required to make available to an application.
;; | 
;; | A canonical XML document conforms to the following grammar:
;; |
;; |    CanonXML    ::= Pi* element Pi*
;; |    element     ::= Stag (Datachar | Pi | element)* Etag
;; |    Stag        ::= '<'  Name Atts '>'
;; |    Etag        ::= '</' Name '>'
;; |    Pi          ::= '<?' Name ' ' (((Char - S) Char*)? - (Char* '?>' Char*)) '?>'
;; |    Atts        ::= (' ' Name '=' '"' Datachar* '"')*
;; |    Datachar    ::= '&amp;' | '&lt;' | '&gt;' | '&quot;'
;; |                     | '&#9;'| '&#10;'| '&#13;'
;; |                     | (Char - ('&' | '<' | '>' | '"' | #x9 | #xA | #xD))
;; |    Name        ::= (see XML spec)
;; |    Char        ::= (see XML spec)
;; |    S           ::= (see XML spec)
;; |
;; | Attributes are in lexicographical order (in Unicode bit order).
;; |  
;; | A canonical XML document is encoded in UTF-8.
;; |  
;; | Ignorable white space is considered significant and is treated
;; | equivalently to data.
;;
;; -- James Clark (jjc@jclark.com)


;;;; SINK: a rune output "stream"

(defclass sink ()
    ((high-surrogate :initform nil)
     (column :initform 0 :accessor column)
     (width :initform 79 :initarg :width :accessor width)
     (indentation :initform nil :initarg :indentation :accessor indentation)
     (current-indentation :initform 0 :accessor current-indentation)))

(defclass vector-sink (sink)
    ((target-vector :initform (make-buffer))))

(defclass character-stream-sink (sink)
    ((target-stream :initarg :target-stream)))

;; WRITE-OCTET als generisch zu machen ist vielleicht nicht die schnellste
;; Loesung, aber die einfachste.
(defgeneric write-octet (octet sink))

(defun make-buffer ()
  (make-array 1
              :element-type '(unsigned-byte 8)
              :adjustable t
              :fill-pointer 0))

(defmethod write-octet :after (octet sink)
  (with-slots (column) sink
    (setf column (if (eql octet 10) 0 (1+ column)))))

(defmethod write-octet (octet (sink vector-sink))
  (let ((target-vector (slot-value sink 'target-vector)))
    (vector-push-extend octet target-vector (length target-vector))))

(defmethod write-octet (octet (sink character-stream-sink))
  (write-char (code-char octet) (slot-value sink 'target-stream)))

(defun unparse-document (doc character-stream &rest initargs)
  (let ((sink (apply #'make-instance 'character-stream-sink
                     :target-stream character-stream
                     initargs)))
    (map nil (rcurry #'unparse-node sink) (dom:child-nodes doc))))

(defun unparse-document-to-octets (doc &rest initargs)
  (let ((sink (apply #'make-instance 'vector-sink initargs)))
    (map nil (rcurry #'unparse-node sink) (dom:child-nodes doc))
    (slot-value sink 'target-vector)))


;;;; DOM serialization

(defun sink-fresh-line (sink)
  (unless (zerop (column sink))
    (write-rune-0 10 sink)
    (indent sink)))

(defun unparse-node (node sink)
  (let ((indentation (indentation sink)))
    (cond
        ((dom:element-p node)
         (when indentation
           (sink-fresh-line sink)
           (start-indentation-block sink))
         (write-rune #/< sink)
         (write-rod (dom:tag-name node) sink)
         ;; atts
         (let ((atts (sort (copy-list (dom:items (dom:attributes node)))
                           #'rod< :key #'dom:name)))
           (dolist (a atts)
             (write-rune #/space sink)
             (write-rod (dom:name a) sink)
             (write-rune #/= sink)
             (write-rune #/\" sink)
             (map nil (lambda (c) (unparse-datachar c sink)) (dom:value a))
             (write-rune #/\" sink)))
         (write-rod '#.(string-rod ">") sink)
         (dom:do-node-list (k (dom:child-nodes node))
           (unparse-node k sink))
         (when indentation
           (end-indentation-block sink)
           (sink-fresh-line sink))
         (write-rod '#.(string-rod "</") sink)
         (write-rod (dom:tag-name node) sink)
         (write-rod '#.(string-rod ">") sink))
        ((dom:processing-instruction-p node)
         (unless (rod-equal (dom:target node) '#.(string-rod "xml"))
           (write-rod '#.(string-rod "<?") sink)
           (write-rod (dom:target node) sink)
           (write-rune #/space sink)
           (write-rod (dom:data node) sink)
           (write-rod '#.(string-rod "?>") sink)))
        ((dom:cdata-section-p node)
         (when indentation
           (sink-fresh-line sink))
         (write-rod #"<![CDATA[" sink)
         ;; XXX signal error if body is unprintable?
         (map nil (lambda (c) (write-rune c sink)) (dom:data node))
         (write-rod #"]]>" sink))
        ((dom:text-node-p node)
         (if indentation
             (unparse-indented-text (dom:data node) sink)
             (map nil (lambda (c) (unparse-datachar c sink)) (dom:data node))))
        ((dom:comment-p node))
        (t
         (error "Oops in unparse: ~S." node)))))

(defun indent (sink)
  (dotimes (x (current-indentation sink))
    (write-rune-0 32 sink)))

(defun start-indentation-block (sink)
  (incf (current-indentation sink) (indentation sink)))

(defun end-indentation-block (sink)
  (decf (current-indentation sink) (indentation sink)))

(defun unparse-indented-text (data sink)
  (flet ((whitespacep (x)
           (or (rune= x #/U+000A) (rune= x #/U+0020))))
    (let ((pos 0)
          (n (length data))
          (have-newline nil))
      (while (< pos n)
        (let ((next (or (position-if #'whitespacep data :start pos) n)))
          (when (> next (1+ pos))
            (cond
              ((not have-newline)
                (sink-fresh-line sink)
                (setf have-newline t))
              ((< (+ (column sink) next (- pos)) (width sink))
                (write-rune-0 32 sink))
              (t
                (sink-fresh-line sink)))
            (loop
                for i from pos below next do
                  (unparse-datachar-readable (elt data i) sink)))
          (setf pos (1+ next)))))))

(defun unparse-datachar (c sink)
  (cond ((rune= c #/&) (write-rod '#.(string-rod "&amp;") sink))
        ((rune= c #/<) (write-rod '#.(string-rod "&lt;") sink))
        ((rune= c #/>) (write-rod '#.(string-rod "&gt;") sink))
        ((rune= c #/\") (write-rod '#.(string-rod "&quot;") sink))
        ((rune= c #/U+0009) (write-rod '#.(string-rod "&#9;") sink))
        ((rune= c #/U+000A) (write-rod '#.(string-rod "&#10;") sink))
        ((rune= c #/U+000D) (write-rod '#.(string-rod "&#13;") sink))
        (t
         (write-rune c sink))))

(defun unparse-datachar-readable (c sink)
  (cond ((rune= c #/&) (write-rod '#.(string-rod "&amp;") sink))
        ((rune= c #/<) (write-rod '#.(string-rod "&lt;") sink))
        ((rune= c #/>) (write-rod '#.(string-rod "&gt;") sink))
        ((rune= c #/\") (write-rod '#.(string-rod "&quot;") sink))
        (t
          (write-rune c sink))))


;;;; UTF-8 output for SINKs

(defun write-rod (rod sink)
  (map nil (lambda (c) (write-rune c sink)) rod))

(defun write-rune (rune sink)
  (let ((code (rune-code rune)))
    (with-slots (high-surrogate) sink
      (cond
        ((<= #xD800 code #xDBFF)
          (setf high-surrogate code))
        ((<= #xDC00 code #xDFFF)
          (let ((q (logior (ash (- high-surrogate #xD7C0) 10)
                           (- code #xDC00))))
            (write-rune-0 q sink))
          (setf high-surrogate nil))
        (t
          (write-rune-0 code sink))))))

(defun write-rune-0 (code sink)
  (labels ((wr (x)
             (write-octet x sink)))
    (cond ((<= #x00000000 code #x0000007F) 
           (wr code))
          ((<= #x00000080 code #x000007FF)
           (wr (logior #b11000000 (ldb (byte 5 6) code)))
           (wr (logior #b10000000 (ldb (byte 6 0) code))))
          ((<= #x00000800 code #x0000FFFF)
           (wr (logior #b11100000 (ldb (byte 4 12) code)))
           (wr (logior #b10000000 (ldb (byte 6 6) code)))
           (wr (logior #b10000000 (ldb (byte 6 0) code))))
          ((<= #x00010000 code #x001FFFFF)
           (wr (logior #b11110000 (ldb (byte 3 18) code)))
           (wr (logior #b10000000 (ldb (byte 6 12) code)))
           (wr (logior #b10000000 (ldb (byte 6 6) code)))
           (wr (logior #b10000000 (ldb (byte 6 0) code))))
          ((<= #x00200000 code #x03FFFFFF)
           (wr (logior #b11111000 (ldb (byte 2 24) code)))
           (wr (logior #b10000000 (ldb (byte 6 18) code)))
           (wr (logior #b10000000 (ldb (byte 6 12) code)))
           (wr (logior #b10000000 (ldb (byte 6 6) code)))
           (wr (logior #b10000000 (ldb (byte 6 0) code))))
          ((<= #x04000000 code #x7FFFFFFF)
           (wr (logior #b11111100 (ldb (byte 1 30) code)))
           (wr (logior #b10000000 (ldb (byte 6 24) code)))
           (wr (logior #b10000000 (ldb (byte 6 18) code)))
           (wr (logior #b10000000 (ldb (byte 6 12) code)))
           (wr (logior #b10000000 (ldb (byte 6 6) code)))
           (wr (logior #b10000000 (ldb (byte 6 0) code)))))))
