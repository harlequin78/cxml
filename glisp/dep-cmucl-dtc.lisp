;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: CL-USER; -*-
;;; ---------------------------------------------------------------------------
;;;     Title: CMUCL dependent stuff + fixups
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

(defmacro glisp::defsubst (name args &body body)
  `(progn
     (declaim (inline ,name))
     (defun ,name ,args .,body)))
