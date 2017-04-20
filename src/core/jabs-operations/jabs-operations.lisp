;;; -*- Mode: Lisp -*-
#|
MIT License

Copyright (c) 2017 Alexander Vynnyk <cosmonaut.ok@zoho.com>

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.
|#
(defpackage operations@core@plugin@jabs
  (:use :cl :tools@jabs :re@jabs :jabs
				:skeleton@core@plugin@jabs))

(in-package :operations@core@plugin@jabs)

(make-instance 'jabs::plugin :name :operations :type :core :version jabs::+jabs-version+)

;;;; define common jabs operations here

(in-package :jabs)

(defgeneric load-op (project &optional source-force)
  )

(defmethod load-op ((project project) &optional source-force)
  (if source-force
      (asdf:operate 'asdf:load-source-op (get-project-name project))
      (asdf:operate 'asdf:load-op (get-project-name project))))

(defgeneric load-dependencies-op (project)
  )

(defmethod load-dependencies-op ((project project))
  (load-project-dependencies project))

(defgeneric compile-op (project)
  )

(defmethod compile-op ((project project))
  (asdf:operate 'asdf:compile-op (get-project-name project)))

(defgeneric test-op (project)
  )

(defmethod test-op ((project project))
  (run-tests project))

(defgeneric lib-op (project)
  )

(defmethod lib-op ((project project))
  (jlog:err "lib-op Not implemented"))

(defgeneric link-op (project)
  )

(defmethod link-op ((project project))
  (jlog:err "link-op Not implemented"))

(defgeneric prepare-op (project)
  )

(defmethod prepare-op ((project project))
  (jlog:err "prepare-op Not implemented"))

(defgeneric archive-op (project)
  )

(defmethod archive-op ((project project))
  (archive@core@plugin@jabs::make-project-archives project))

;; + implemented
;; - will not be implemented
;; / should be implemented
;; ? is it needed?

;;- basic-load-op
;;- basic-compile-op
;;/ prepare-op
;;+ load-op
;;+ compile-op
;;? prepare-source-op
;;? load-source-op
;;/ test-op
;;? build-op
;;? bundle-op
;;? monolithic-op
;;? monolithic-bundle-op
;;/ link-op
;;? basic-compile-bundle-op
;;? prepare-bundle-op
;;/ lib-op
;;? compile-bundle-op
;;? load-bundle-op
;;/ dll-op
;;- deliver-asd-op
;;- monolithic-deliver-asd-op
;;? monolithic-load-bundle-op
;;? monolithic-lib-op
;;? monolithic-dll-op
;;+ image-op
;;? program-op
;;- basic-concatenate-source-op
;;- basic-load-concatenated-source-op
;;- basic-compile-concatenated-source-op
;;- basic-load-compiled-concatenated-source-op
;;? concatenate-source-op
;;? load-concatenated-source-op
;;? compile-concatenated-source-op
;;? load-compiled-concatenated-source-op
;;? monolithic-load-concatenated-source-op
;;? monolithic-compile-concatenated-source-op
