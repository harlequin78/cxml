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
