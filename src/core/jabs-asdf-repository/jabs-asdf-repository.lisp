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
(defpackage jabs-asdf-repository@core@plugin@jabs
  (:use :cl :jabs))

(in-package :jabs-asdf-repository@core@plugin@jabs)

(make-instance 'jabs::plugin :name :jabs-asdf-repository :type :core :version jabs::+jabs-version+)

(defpackage asdf@repository@plugin@jabs
  (:use :cl :jabs :tools@jabs :skeleton@core@plugin@jabs))

;; HACK: we need repository plugin to interact
;; with it as with plugin for external repositories
(make-instance 'jabs::plugin :name :asdf :type :repository :version jabs::+jabs-version+)

(in-package :asdf@repository@plugin@jabs)

(defun add-project-related-asdf-paths ()
  "Add project-related paths to ASDF registry"
  (let ((project (and *jabs-project-to-run* (find-project *jabs-project-to-run*))))
    (when project
      (let*
          ((skel-name (get-project-skeleton-name project))
           (skel (or
                  (find-skeleton skel-name)
                  (and
                   (load-skeleton skel-name)
                   (find-skeleton skel-name))))
           (lib (or (try (car (get-skeleton-lib skel)))
                    (get-skeleton-lib skel)))
           (contrib (or (try (car (get-skeleton-contrib skel)))
                        (get-skeleton-contrib skel)))
           (opt (get-skeleton-opt skel))
           (local-asdf-dirs (mapcar #'(lambda (x)
                                        (when x (merge-pathnames x (os-pwd))))
                                    (list lib contrib opt)))
           (local-asdf-dirs-dsl))

        (when local-asdf-dirs
          (dolist (dir local-asdf-dirs)
            (when dir
              (jlog:dbg "Adding directory ``~a'' to ASDF registry" dir)
              (push
               (list :tree (tostr dir))
               local-asdf-dirs-dsl)))
          (eval `(asdf:initialize-source-registry
                  '(:source-registry ,@local-asdf-dirs-dsl :inherit-configuration)))
          )))))

(add-hook *define-project-hook*
  #'(lambda (x)
      (declare (ignore x))
      (add-project-related-asdf-paths)))

(defun find-repository-project (name) ;; (project project))
  (check-type name keyword)
  (try (asdf:find-system name)))

;;;; repository API function
(defun load-repository-project (name) ;; (project project))
  (check-type name keyword)
  (when (find-repository-project name)
    (asdf:load-system name)))

(defun remove-repository-project (name)
  "Do not really works. Just clear system from preloaded registry. I don`t know, is it really needed"
  (asdf:clear-system name))

(defun repository-project-version (name)
  (check-type name keyword)
  (let ((system (find-repository-project name)))
    (when system
      (slot-value (asdf:find-system :cl-actors) 'asdf/system::version))))

(defun update-repository-project-list ()
  ;; clear source registry
  (asdf:clear-source-registry)
  ;; initialize default source registry
  (asdf:initialize-source-registry)
  ;; add project-related asdf paths to source registry
  (add-project-related-asdf-paths))

(defun repository-project-dependencies (name)
  (check-type name keyword)
  (let ((system (find-repository-project name)))
    (when system
      (slot-value system 'asdf/system::depends-on))))

(defun initialize-repository (path)
  (declare (ignore path))
  (update-repository-project-list))

;; make asdf@repository plugin shared
(share-plugin (find-plugin :asdf :repository))
