;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: CL-USER; -*-
;;; ---------------------------------------------------------------------------
;;;     Title: CLISP dependent stuff + fixups
;;;   Created: 1999-05-25 22:32
;;;    Author: Gilbert Baumann <unk6@rz.uni-karlsruhe.de>
;;;   License: GPL (See file COPYING for details).
;;; ---------------------------------------------------------------------------
;;;  (c) copyright 1999 by Gilbert Baumann

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

(in-package :CL-USER)

(eval-when (compile load eval)
  (if (fboundp 'cl::define-compiler-macro)
      (pushnew 'define-compiler-macro *features*)))

(setq lisp:*load-paths* '(#P"./"))

(import 'lisp:read-byte-sequence :glisp)
(export 'lisp:read-byte-sequence :glisp)
(import 'lisp:read-char-sequence :glisp)
(export 'lisp:read-char-sequence :glisp)
(export 'glisp::compile-file :glisp)
(export 'glisp::run-unix-shell-command :glisp)
(export 'glisp::make-server-socket :glisp)

#+DEFINE-COMPILER-MACRO
(cl:define-compiler-macro ldb (bytespec value &whole whole)
  (let (pos size)
    (cond ((and (consp bytespec)
                (= (length bytespec) 3)
                (eq (car bytespec) 'byte)
                (constantp (setq size (second bytespec)))
                (constantp (setq pos (third bytespec))))
           `(logand ,(if (eql pos 0) value `(ash ,value (- ,pos)))
                    (1- (ash 1 ,size))))
          (t
           whole))))

#-DEFINE-COMPILER-MACRO
(progn
  (export 'glisp::define-compiler-macro :glisp)
  (defmacro glisp::define-compiler-macro (name args &body body)
    (declare (ignore args body))
    `(progn
       ',name)))

(defmacro glisp::defsubst (name args &body body)
  `(progn
     (declaim (inline ,name))
     (defun ,name ,args .,body)))
