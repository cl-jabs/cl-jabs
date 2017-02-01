;; Copyright (C) 2013 Alexander aka CosmonauT Vynnyk
;;
;;  This file is part of jab.
;;
;; jab is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; jab is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this software; see the file COPYING.  If not, see
;; <http://www.gnu.org/licenses/>.

;; Commentary:
;;
;; Dynamically loads JAB into CL virtualmachine
;;
;; Code:

(in-package :cl-user)

#-sbcl
(jlog:crit "This lisp implementation is not supported.")

#+sbcl(require :sb-posix)

(load #P"src/wrappers/jab/jab.lisp")

#+sbcl
(sb-ext:save-lisp-and-die
 "jab"
 :toplevel (lambda () (jab:jab) 0)
 :compression t
 :executable t)

;; #+cmucl
;; (save-lisp "jab" :init-function (lambda ()
;;            (jab:jab)
;;            0)
;;      :executable t)

;; #+clisp
;; (ext:saveinitmem "jab" :init-function (lambda ()
;;            (jab:jab)
;;            (ext:quit))
;;                  :executable t :keep-global-handlers t :norc nil :documentation "The JAB Executable")


;; #+ccl
;; (progn
;;  (ccl:save-application "jab" :prepend-kernel t :toplevel-function #'jab:jab)
;; )


;; #+ecl
;; (jab:jab)
