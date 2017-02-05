(defpackage operations@core@plugin@jabs
  (:use :cl :tools@jabs :re@jabs :jabs
				:skeleton@core@plugin@jabs))

(in-package :operations@core@plugin@jabs)

(make-instance 'jabs::plugin :name :operations :type :core :version jabs::+jabs-version+)

;;;; define common jabs operations here

(in-package :jabs)

(defmethod load-op ((project project) &optional source-force)
  (if source-force
      (asdf:operate 'asdf:load-source-op (get-project-name project))
      (asdf:operate 'asdf:load-op (get-project-name project))))

(defmethod load-dependencies-op ((project project))
  (load-project-dependencies project))

(defmethod compile-op ((project project))
  (asdf:operate 'asdf:compile-op (get-project-name project)))

(defmethod test-op ((project project))
  (run-tests project))

(defmethod lib-op ((project project))
  (jlog:err "lib-op Not implemented"))

(defmethod link-op ((project project))
  (jlog:err "link-op Not implemented"))

(defmethod prepare-op ((project project))
  (jlog:err "prepare-source-op Not implemented"))

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
