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
(in-package :tools@jabs)

(defvar *jabs-namespace-tree*
  '(:jabs (:tools
           :regexp
           :logger
           :core
           :plugin (:repository (:quicklisp
                                 :github))
           :test))
  "Namespace tree for JABS")

(defvar *stdin* *standard-input*
  "the original standard input stream at startup")

(defmacro try (&body body)
  "Try to do something without execption returning"
  `(handler-bind ((warning #'muffle-warning))
     (ignore-errors
       ,@body)))

;; Define "lazy" functions (from asdf), which executes during call,
;; but not inline
(macrolet
    ((defdef (def* def)
       `(defmacro ,def* (name formals &rest rest)
          `(progn
             #+(or ecl (and gcl (not gcl-pre2.7))) (fmakunbound ',name)
             #-gcl                      ; gcl 2.7.0 notinline functions lose secondary return values :-(
             ,(when (and #+ecl (symbolp name)) ; fails for setf functions on ecl
                    `(declaim (notinline ,name)))
             (,',def ,name ,formals ,@rest)))))
  (defdef defgeneric* defgeneric)
  (defdef defmethod* defgeneric)
  (defdef defun* defun))

(defun setup-stdin ()
  (setf *stdin*
        #.(or #+clozure 'ccl::*stdin*
              #+(or cmu scl) 'system:*stdin*
              #+(or clasp ecl) 'ext::+process-standard-input+
              #+sbcl 'sb-sys:*stdin*
              '*standard-input*)))

(defvar *stdout* *standard-output*
  "the original standard output stream at startup")

(defun setup-stdout ()
  (setf *stdout*
        #.(or #+clozure 'ccl::*stdout*
              #+(or cmu scl) 'system:*stdout*
              #+(or clasp ecl) 'ext::+process-standard-output+
              #+sbcl t ;; 'sb-sys:*stdout* ;; TODO: not woerking
              '*standard-output*)))

(defvar *stderr* *error-output*
  "the original error output stream at startup")

(defun setup-stderr ()
  (setf *stderr*
        #.(or #+allegro 'excl::*stderr*
              #+clozure 'ccl::*stderr*
              #+(or cmu scl) 'system:*stderr*
              #+(or clasp ecl) 'ext::+process-error-output+
              #+sbcl *error-output* ;; 'sb-sys:*stderr* ;; TODO: not woerking
              '*error-output*)))

  ;; Run them now. In image.lisp, we'll register them to be run at image restart.
(setup-stdin) (setup-stdout) (setup-stderr)

(defun flatten (list)
  (cond ((null list) nil)
        ((atom (car list))
         (cons (car list) (flatten (cdr list))))
        (t (append (flatten (car list)) (flatten (cdr list))))))

(defun terminate (&optional (status 0))
  (when (not (eq status 0))
    (format *error-output* "KNOCKED OUT!~%"))
  #+sbcl (sb-ext:exit :code status)      ; SBCL
  #+ccl ( ccl:quit status)               ; Clozure CL
  #+clisp ( ext:quit status)             ; GNU CLISP
  #+cmu ( unix:unix-exit status)         ; CMUCL
  #+abcl ( ext:quit :status status)      ; Armed Bear CL
  #+allegro ( excl:exit status :quiet t) ; Allegro CL
  #-(or sbcl cmu clisp abcl allegro ecl ccl)(cl-user::quit))

(defun tolist (stuff)
  (cond ((or
          (stringp stuff)
          (listp stuff))
         (concatenate 'list stuff))
        ((or
          (numberp stuff)
          (symbolp stuff))
         (concatenate 'list (princ-to-string stuff)))
        (t
         (jlog:err "Converting type ``~a'' of stuff ``~a'' to list not implemented"
                   (type-of stuff) stuff))))

(defun tostr (stuff &optional downcase)
  (let ((string
         (cond ((listp stuff)
                (or
                 (try (concatenate 'string stuff))
                 (try
                   (mapcar #'(lambda (x)
                               (character (princ-to-string x))) stuff))
                 (princ-to-string stuff)))
               ((symbolp stuff)
                (princ-to-string stuff))
               ((stringp stuff)
                stuff)
               (t
                (princ-to-string stuff)))))

    (if downcase
        (string-downcase string)
        string)))

(defun tosymbol (symbol &optional (package :keyword))
  (when (not (null symbol)) ;; don`t make symbol from NIL
    (if (and (stringp symbol) (string-equal "" symbol))
        nil
        (intern (string-upcase (princ-to-string symbol)) package))))

(defun tokeyword (symbol)
  (tosymbol symbol :keyword))

(defmacro concat-strings (&body strings)
  `(concatenate 'string ,@strings))

(defun concat-symbols (package &rest symbols)
  (tosymbol
   (eval
    (cons 'concat-strings
          (mapcar #'(lambda (x) (tostr x)) symbols)))
   package))

(defmacro concat-keywords (&body symbols)
  `(concat-symbols :keyword ,@symbols))

(defun concat-symbols-w-delimiter (delimiter package &rest symbols)
  (tosymbol
   (concat-strings
     (tostr (car symbols))
     (eval
      (cons 'concat-strings
            (mapcar #'(lambda (x)
                        (concat-strings delimiter (tostr x)))
                    (cdr symbols)))))
   package))

(defmacro concat-keywords-w-delimiter (delimiter &body symbols)
  `(concat-symbols-w-delimiter ,delimiter :keyword ,@symbols))

(defun argv (&optional argnumber)
  (let ((argv
         (or
          #+clisp (ext:argv)
          #+sbcl (cdr sb-ext:*posix-argv*)
          #+abcl ext:*command-line-argument-list*
          #+clozure ccl:*unprocessed-command-line-arguments* ;(ccl::command-line-arguments)
          #+gcl si:*command-args*
          #+ecl (loop for i from 0 below (si:argc) collect (si:argv i))
          #+cmu extensions:*command-line-strings*
          #+allegro (sys:command-line-arguments)
          #+lispworks sys:*line-arguments-list*
          nil)))
    (if (not (null argnumber))
        (nth argnumber argv)
        argv)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Alexandria
(defun maphash-keys (function table)
  "Like MAPHASH, but calls FUNCTION with each key in the hash table TABLE."
  (maphash (lambda (k v)
             (declare (ignore v))
             (funcall function k))
           table))

(defun maphash-values (function table)
  "Like MAPHASH, but calls FUNCTION with each key in the hash table TABLE."
  (maphash (lambda (k v)
             (declare (ignore k))
             (funcall function v))
           table))

(defmacro if-let (bindings &body (then-form &optional else-form)) ;; from alexandria
  ;; bindings can be (var form) or ((var1 form1) ...)
  (let* ((binding-list (if (and (consp bindings) (symbolp (car bindings)))
                           (list bindings)
                           bindings))
         (variables (mapcar #'car binding-list)))
    `(let ,binding-list
       (if (and ,@variables)
           ,then-form
           ,else-form))))

(defun copy-hash-table (table &key key test size
                                rehash-size rehash-threshold)
  "Returns a copy of hash table TABLE, with the same keys and values
as the TABLE. The copy has the same properties as the original, unless
overridden by the keyword arguments.
Before each of the original values is set into the new hash-table, KEY
is invoked on the value. As KEY defaults to CL:IDENTITY, a shallow
copy is returned by default."
  (setf key (or key 'identity))
  (setf test (or test (hash-table-test table)))
  (setf size (or size (hash-table-size table)))
  (setf rehash-size (or rehash-size (hash-table-rehash-size table)))
  (setf rehash-threshold (or rehash-threshold (hash-table-rehash-threshold table)))
  (let ((copy (make-hash-table :test test :size size
                               :rehash-size rehash-size
                               :rehash-threshold rehash-threshold)))
    (maphash (lambda (k v)
               (setf (gethash k copy) (funcall key v)))
             table)
    copy))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; /Alexandria

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; FAD
(defun component-present-p (value)
  "Helper function for DIRECTORY-PATHNAME-P which checks whether VALUE
   is neither NIL nor the keyword :UNSPECIFIC."
  (and value (not (eql value :unspecific))))

(defun directory-pathname-p (pathspec)
  "Returns NIL if PATHSPEC \(a pathname designator) does not designate
a directory, PATHSPEC otherwise.  It is irrelevant whether file or
directory designated by PATHSPEC does actually exist."
  (and
   (not (component-present-p (pathname-name pathspec)))
   (not (component-present-p (pathname-type pathspec)))
   pathspec))

(defun pathname-as-directory (pathspec)
  "Converts the non-wild pathname designator PATHSPEC to directory
form."
  (let ((pathname (pathname pathspec)))
    (when (wild-pathname-p pathname)
      (jlog:crit "Can't reliably convert wild pathnames."))
    (cond ((not (directory-pathname-p pathspec))
           (make-pathname :directory (append (or (pathname-directory pathname)
                                                 (list :relative))
                                             (list (file-namestring pathname)))
                          :name nil
                          :type nil
                          :defaults pathname))
          (t pathname))))

(defun pathname-as-file (pathspec)
  "Converts the non-wild pathname designator PATHSPEC to file form."
  (let ((pathname (pathname pathspec)))
    (when (wild-pathname-p pathname)
      (jlog:crit "Can't reliably convert wild pathnames."))
    (cond ((directory-pathname-p pathspec)
           (let* ((directory (pathname-directory pathname))
                  (name-and-type (pathname (first (last directory)))))
             (make-pathname :directory (butlast directory)
                            :name (pathname-name name-and-type)
                            :type (pathname-type name-and-type)
                            :defaults pathname)))
          (t pathname))))

(defun file-pathname-p (pathspec)
  "Returns T if PATHSPEC \(a pathname designator) does not designate
a directory, PATHSPEC otherwise.  It is irrelevant whether file or
directory designated by PATHSPEC does actually exist."
  (and
   (component-present-p (pathname-name pathspec))
   pathspec))

(defun directory-wildcard (dirname)
  "Returns a wild pathname designator that designates all files within
the directory named by the non-wild pathname designator DIRNAME."
  (when (wild-pathname-p dirname)
    (jlog:crit "Can only make wildcard directories from non-wildcard directories."))
  (make-pathname :name #-:cormanlisp :wild #+:cormanlisp "*"
                 :type #-(or :clisp :cormanlisp) :wild
                 #+:clisp nil
                 #+:cormanlisp "*"
                 :defaults (pathname-as-directory dirname)))

#+:clisp
(defun clisp-subdirectories-wildcard (wildcard)
  "Creates a wild pathname specifically for CLISP such that
sub-directories are returned by DIRECTORY."
  (make-pathname :directory (append (pathname-directory wildcard)
                                    (list :wild))
                 :name nil
                 :type nil
                 :defaults wildcard))

(defun list-directory (dirname &key (follow-symlinks t))
  "Returns a fresh list of pathnames corresponding to all files within
   the directory named by the non-wild pathname designator DIRNAME.
   The pathnames of sub-directories are returned in directory form -
   see PATHNAME-AS-DIRECTORY.

  If FOLLOW-SYMLINKS is true, then the returned list contains
truenames (symlinks will be resolved) which essentially means that it
might also return files from *outside* the directory.  This works on
all platforms.

  When FOLLOW-SYMLINKS is NIL, it should return the actual directory
contents, which might include symlinks.  Currently this works on SBCL
and CCL."
  (declare (ignorable follow-symlinks))
  (when (wild-pathname-p dirname)
    (jlog:crit "Can only list concrete directory names."))
  #+:ecl
  (let ((dir (pathname-as-directory dirname)))
    (concatenate 'list
                 (directory (merge-pathnames (pathname "*/") dir))
                 (directory (merge-pathnames (pathname "*.*") dir))))
  #-:ecl
  (let ((wildcard (directory-wildcard dirname)))
    #+:abcl (system::list-directory dirname)
    #+:sbcl (directory wildcard :resolve-symlinks follow-symlinks)
    #+(or :cmu :scl :lispworks) (directory wildcard)
    #+(or :openmcl :digitool) (directory wildcard :directories t :follow-links follow-symlinks)
    #+:allegro (directory wildcard :directories-are-files nil)
    #+:clisp (nconc (directory wildcard :if-does-not-exist :keep)
                    (directory (clisp-subdirectories-wildcard wildcard)))
    #+:cormanlisp (nconc (directory wildcard)
                         (cl::directory-subdirs dirname)))
  #-(or :sbcl :cmu :scl :lispworks :openmcl :allegro :clisp :cormanlisp :ecl :abcl :digitool)
  (jlog:crit "LIST-DIRECTORY not implemented"))

(defun file-exists-p (pathspec)
  "Checks whether the file named by the pathname designator PATHSPEC
exists and returns its truename if this is the case, NIL otherwise.
The truename is returned in `canonical' form, i.e. the truename of a
directory is returned as if by PATHNAME-AS-DIRECTORY."
  #+(or :sbcl :lispworks :openmcl :ecl :digitool) (probe-file pathspec)
  #+:allegro (or (excl:probe-directory (pathname-as-directory pathspec))
                 (probe-file pathspec))
  #+(or :cmu :scl :abcl) (or (probe-file (pathname-as-directory pathspec))
                             (probe-file pathspec))
  #+:cormanlisp (or (and (ccl:directory-p pathspec)
                         (pathname-as-directory pathspec))
                    (probe-file pathspec))
  #+:clisp (or (try
                 (let ((directory-form (pathname-as-directory pathspec)))
                   (when (ext:probe-directory directory-form)
                     directory-form)))
               (try
                 (probe-file (pathname-as-file pathspec))))
  #-(or :sbcl :cmu :scl :lispworks :openmcl :allegro :clisp :cormanlisp :ecl :abcl :digitool)
  (jlog:crit "FILE-EXISTS-P not implemented"))

(defun directory-exists-p (pathspec)
  "Checks whether the file named by the pathname designator PATHSPEC
exists and if it is a directory.  Returns its truename if this is the
case, NIL otherwise.  The truename is returned in directory form as if
by PATHNAME-AS-DIRECTORY."
  #+:allegro
  (and (excl:probe-directory pathspec)
       (pathname-as-directory (truename pathspec)))
  #+:lispworks
  (and (lw:file-directory-p pathspec)
       (pathname-as-directory (truename pathspec)))
  #-(or :allegro :lispworks)
  (let ((result (file-exists-p pathspec)))
    (and result
         (directory-pathname-p result)
         result)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *stream-buffer-size* 8192)

(defun copy-stream (from to &optional (checkp t))
  "Copies into TO \(a stream) from FROM \(also a stream) until the end
of FROM is reached, in blocks of *stream-buffer-size*.  The streams
should have the same element type.  If CHECKP is true, the streams are
checked for compatibility of their types."
  (when checkp
    (unless (subtypep (stream-element-type to) (stream-element-type from))
      (jlog:crit "Incompatible streams ``~A'' and ``~A''" from to)))
  (let ((buf (make-array *stream-buffer-size*
                         :element-type (stream-element-type from))))
    (loop
       (let ((pos #-(or :clisp :cmu) (read-sequence buf from)
                  #+:clisp (ext:read-byte-sequence buf from :no-hang nil)
                  #+:cmu (sys:read-n-bytes from buf 0 *stream-buffer-size* nil)))
         (when (zerop pos) (return))
         (write-sequence buf to :end pos))))
  (values))

(defun copy-file (from to &key overwrite)
  "Copies the file designated by the non-wild pathname designator FROM
to the file designated by the non-wild pathname designator TO.  If
OVERWRITE is true overwrites the file designtated by TO if it exists."
  #+:allegro (excl.osi:copy-file from to :overwrite overwrite)
  #-:allegro
  (let ((element-type #-:cormanlisp '(unsigned-byte 8)
                      #+:cormanlisp 'unsigned-byte))
    (with-open-file (in from :element-type element-type)
      (with-open-file (out to :element-type element-type
                              :direction :output
                              :if-exists (if overwrite
                                           :supersede
                                           #-:cormanlisp :error
                                           #+:cormanlisp nil))
        #+:cormanlisp
        (unless out
          (jlog:crit (make-condition 'file-error
                                 :pathname to
                                 :format-control "File already exists.")))
        (copy-stream in out))))
  (values))

(defun delete-directory-and-files (dirname &key (if-does-not-exist :error))
  "Recursively deletes all files and directories within the directory
designated by the non-wild pathname designator DIRNAME including
DIRNAME itself.  IF-DOES-NOT-EXIST must be one of :ERROR or :IGNORE
where :ERROR means that an error will be signaled if the directory
DIRNAME does not exist.

NOTE: this function is dangerous if the directory that you are
removing contains symlinks to files outside of it - the target files
might be removed instead!  This is currently fixed for SBCL and CCL."

  #+:allegro (excl.osi:delete-directory-and-files dirname
                                                  :if-does-not-exist if-does-not-exist)

  #+:sbcl
  (if (directory-exists-p dirname)
      (sb-ext:delete-directory dirname :recursive t)
      (ecase if-does-not-exist
        (:error  (jlog:crit "~S is not a directory" dirname))
        (:ignore nil)))

  #+:ccl-has-delete-directory
  (if (directory-exists-p dirname)
      (ccl:delete-directory dirname)
      (ecase if-does-not-exist
        (:error  (jlog:crit "~S is not a directory" dirname))
        (:ignore nil)))

  #-(or :allegro :sbcl :ccl-has-delete-directory)
  (walk-directory dirname
                  (lambda (file)
                    (cond ((directory-pathname-p file)
                           #+:lispworks (lw:delete-directory file)
                           #+:cmu (multiple-value-bind (ok err-number)
                                      (unix:unix-rmdir (namestring (truename file)))
                                    (unless ok
                                      (jlog:crit "Error number ``~A'' when trying to delete ``~A''"
                                             err-number file)))
                           #+:scl (multiple-value-bind (ok errno)
                                      (unix:unix-rmdir (ext:unix-namestring (truename file)))
                                    (unless ok
                                      (jlog:crit "~@<Error deleting ~S: ~A~@:>"
                                             file (unix:get-unix-error-msg errno))))
                           #+:clisp (ext:delete-dir file)
                           #+:openmcl (cl-fad-ccl:delete-directory file)
                           #+:cormanlisp (win32:delete-directory file)
                           #+:ecl (si:rmdir file)
                           #+(or :abcl :digitool) (delete-file file))
                          (t (delete-file file))))
                  :follow-symlinks nil
                  :directories t
                  :if-does-not-exist if-does-not-exist)
  (values))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; /FAD

(defun get-cars (list)
  ;; Clear dependencies from repository definitions etc
  (cond ((null list) nil)
        ((atom (car list))
         (cons (car list) (get-cars (cdr list))))
        (t (cons (caar list) (get-cars (cdr list))))))

(defun emptyp (x)
  "Predicate that is true for an empty sequence"
  (or (null x) (and (vectorp x) (zerop (length x)))))

(defun find-package* (package-designator &optional (error-p t))
  (let ((package (find-package package-designator)))
    (cond
     (package package)
     (error-p (jlog:crit "No package named ~S" (string package-designator)))
     (t nil))))

(defun find-symbol* (name package-designator &optional (error-p t))
  "Find a symbol in a package of given string'ified NAME;
unlike CL:FIND-SYMBOL, work well with 'modern' case sensitive syntax
by letting you supply a symbol or keyword for the name;
also works well when the package is not present.
If optional ERROR argument is NIL, return NIL instead of an error
when the symbol is not found."
  (block nil
    (let ((package (find-package* package-designator error-p)))
      (when package ;; package error handled by find-package* already
        (multiple-value-bind (symbol status) (find-symbol (string name) package)
          (cond
           (status (return (values symbol status)))
           (error-p (jlog:crit "There is no symbol ~S in package ~S" name (package-name package))))))
      (values nil nil))))

(defun symbol-call (package name &rest args)
  "Call a function associated with symbol of given name in given package,
with given ARGS. Useful when the call is read before the package is loaded,
or when loading the package is optional."
  (apply (find-symbol* name package) args))

(defun finish-outputs (&rest streams)
  "Finish output on the main output streams as well as any specified one.
Useful for portably flushing I/O before user input or program exit."
  ;; CCL notably buffers its stream output by default.
  (dolist (s (append streams
                     (list *stdout* *stderr* *error-output* *standard-output* *trace-output*
                           *debug-io* *terminal-io* *query-io*)))
    (try (finish-output s)))
  (values))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Features
(defun featurep (x &optional (*features* *features*))
  "Checks whether a feature expression X is true with respect to the *FEATURES* set,
as per the CLHS standard for #+ and #-. Beware that just like the CLHS,
we assume symbols from the KEYWORD package are used, but that unless you're using #+/#-
your reader will not have magically used the KEYWORD package, so you need specify
keywords explicitly."
  (cond
   ((atom x) (and (member x *features*) t))
   ((eq :not (car x)) (assert (null (cddr x))) (not (featurep (cadr x))))
   ((eq :or (car x)) (some #'featurep (cdr x)))
   ((eq :and (car x)) (every #'featurep (cdr x)))
   (t (jlog:crit "Malformed feature specification ~S" x))))

;;;; implementation-identifier
;;
;; produce a string to identify current implementation.
;; Initially stolen from SLIME's SWANK, completely rewritten since.
;; We're back to runtime checking, for the sake of e.g. ABCL.

(defun first-feature (feature-sets)
  "A helper for various feature detection functions"
  (dolist (x feature-sets)
    (multiple-value-bind (short long feature-expr)
        (if (consp x)
            (values (first x) (second x) (cons :or (rest x)))
          (values x x x))
      (when (featurep feature-expr)
        (return (values short long))))))

#+clozure
(defun ccl-fasl-version ()
  ;; the fasl version is target-dependent from CCL 1.8 on.
  (or (let ((s 'ccl::target-fasl-version))
        (and (fboundp s) (funcall s)))
      (and (boundp 'ccl::fasl-version)
           (symbol-value 'ccl::fasl-version))
      (jlog:crit "Can't determine fasl version.")))

(defun lisp-version-string ()
  "return a string that identifies the current Lisp implementation version"
  (let ((s (lisp-implementation-version)))
    (car                                ; as opposed to OR, this idiom prevents some unreachable code warning
     (list
      #+allegro
      (format nil "~A~@[~A~]~@[~A~]~@[~A~]"
              excl::*common-lisp-version-number*
              ;; M means "modern", as opposed to ANSI-compatible mode (which I consider default)
              (and (eq excl:*current-case-mode* :case-sensitive-lower) "M")
              ;; Note if not using International ACL
              ;; see http://www.franz.com/support/documentation/8.1/doc/operators/excl/ics-target-case.htm
              (excl:ics-target-case (:-ics "8"))
              (and (member :smp *features*) "S"))
      #+armedbear (format nil "~a-fasl~a" s system::*fasl-version*)
      #+clisp
      (subseq s 0 (position #\space s)) ; strip build information (date, etc.)
      #+clozure
      (format nil "~d.~d-f~d"           ; shorten for windows
              ccl::*openmcl-major-version*
              ccl::*openmcl-minor-version*
              (logand (ccl-fasl-version) #xFF))
      #+cmu (substitute #\- #\/ s)
      #+scl (format nil "~A~A" s
                    ;; ANSI upper case vs lower case.
                    (ecase ext:*case-mode* (:upper "") (:lower "l")))
      #+clasp (format nil "~A-~A"
                      s (core:lisp-implementation-id))
      #+(and ecl (not clasp)) (format nil "~A~@[-~A~]" s
                                      (let ((vcs-id (ext:lisp-implementation-vcs-id)))
                                        (subseq vcs-id 0 (min (length vcs-id) 8))))
      #+gcl (subseq s (1+ (position #\space s)))
      #+genera
      (multiple-value-bind (major minor) (sct:get-system-version "System")
        (format nil "~D.~D" major minor))
      #+mcl (subseq s 8)                ; strip the leading "Version "
      s))))

;;;; Other system information
(defun os-hostname ()
  "return the hostname of the current host"
  ;; Note: untested on RMCL
  #+(or abcl clasp clozure cmu ecl genera lispworks mcl mkcl sbcl scl xcl) (machine-instance)
  #+cormanlisp "localhost" ;; is there a better way? Does it matter?
  #+allegro (symbol-call :excl.osi :gethostname)
  #+clisp (first (split-string (machine-instance) :separator " "))
  #+gcl (system:gethostname))

;;; Current directory
#+cmu
(defun parse-unix-namestring* (unix-namestring)
  "variant of LISP::PARSE-UNIX-NAMESTRING that returns a pathname object"
  (multiple-value-bind (host device directory name type version)
      (lisp::parse-unix-namestring unix-namestring 0 (length unix-namestring))
    (make-pathname :host (or host lisp::*unix-host*) :device device
                   :directory directory :name name :type type :version version)))


;;;; -----------------------------------------------------------------
;;;; Windows shortcut support.  Based on:
;;;;
;;;; Jesse Hager: The Windows Shortcut File Format.
;;;; http://www.wotsit.org/list.asp?fc=13

;; #-(or clisp genera) ; CLISP doesn't need it, and READ-SEQUENCE annoys old Genera that doesn't need it
;; (with-upgradability ()
;;   (defparameter *link-initial-dword* 76)
;;   (defparameter *link-guid* #(1 20 2 0 0 0 0 0 192 0 0 0 0 0 0 70))

;;   (defun read-null-terminated-string (s)
;;     "Read a null-terminated string from an octet stream S"
;;     ;; note: doesn't play well with UNICODE
;;     (with-output-to-string (out)
;;       (loop :for code = (read-byte s)
;;             :until (zerop code)
;;             :do (write-char (code-char code) out))))

;;   (defun read-little-endian (s &optional (bytes 4))
;;     "Read a number in little-endian format from an byte (octet) stream S,
;; the number having BYTES octets (defaulting to 4)."
;;     (loop :for i :from 0 :below bytes
;;           :sum (ash (read-byte s) (* 8 i))))

;;   (defun parse-file-location-info (s)
;;     "helper to parse-windows-shortcut"
;;     (let ((start (file-position s))
;;           (total-length (read-little-endian s))
;;           (end-of-header (read-little-endian s))
;;           (fli-flags (read-little-endian s))
;;           (local-volume-offset (read-little-endian s))
;;           (local-offset (read-little-endian s))
;;           (network-volume-offset (read-little-endian s))
;;           (remaining-offset (read-little-endian s)))
;;       (declare (ignore total-length end-of-header local-volume-offset))
;;       (unless (zerop fli-flags)
;;         (cond
;;           ((logbitp 0 fli-flags)
;;            (file-position s (+ start local-offset)))
;;           ((logbitp 1 fli-flags)
;;            (file-position s (+ start
;;                                network-volume-offset
;;                                #x14))))
;;         (strcat (read-null-terminated-string s)
;;                 (progn
;;                   (file-position s (+ start remaining-offset))
;;                   (read-null-terminated-string s))))))

;;   (defun parse-windows-shortcut (pathname)
;;     "From a .lnk windows shortcut, extract the pathname linked to"
;;     ;; NB: doesn't do much checking & doesn't look like it will work well with UNICODE.
;;     (with-open-file (s pathname :element-type '(unsigned-byte 8))
;;       (handler-case
;;           (when (and (= (read-little-endian s) *link-initial-dword*)
;;                      (let ((header (make-array (length *link-guid*))))
;;                        (read-sequence header s)
;;                        (equalp header *link-guid*)))
;;             (let ((flags (read-little-endian s)))
;;               (file-position s 76)        ;skip rest of header
;;               (when (logbitp 0 flags)
;;                 ;; skip shell item id list
;;                 (let ((length (read-little-endian s 2)))
;;                   (file-position s (+ length (file-position s)))))
;;               (cond
;;                 ((logbitp 1 flags)
;;                  (parse-file-location-info s))
;;                 (t
;;                  (when (logbitp 2 flags)
;;                    ;; skip description string
;;                    (let ((length (read-little-endian s 2)))
;;                      (file-position s (+ length (file-position s)))))
;;                  (when (logbitp 3 flags)
;;                    ;; finally, our pathname
;;                    (let* ((length (read-little-endian s 2))
;;                           (buffer (make-array length)))
;;                      (read-sequence buffer s)
;;                      (map 'string #'code-char buffer)))))))
;;         (end-of-file (c)
;;           (declare (ignore c))
;;           nil)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun os-cd (x)
  "Change current directory, as per POSIX chdir(2), to a given pathname object"
  (if-let (x (pathname x))
          #+(or abcl genera xcl) (setf *default-pathname-defaults* (truename x)) ;; d-p-d is canonical!
          #+allegro (excl:chdir x)
          #+clisp (ext:cd x)
          #+clozure (setf (ccl:current-directory) x)
          #+(or cmu scl) (unix:unix-chdir (ext:unix-namestring x))
          #+cormanlisp (unless (zerop (win32::_chdir (namestring x)))
                         (jlog:crit "Could not set current directory to ``~A''" x))
          #+(or clasp ecl) (ext:chdir x)
          #+gcl (system:chdir x)
          #+lispworks (hcl:change-directory x)
          #+mkcl (mk-ext:chdir x)
          #+sbcl (progn (require :sb-posix) (symbol-call :sb-posix :chdir (sb-ext:native-namestring x)))
          #-(or abcl allegro clasp clisp clozure cmu cormanlisp ecl gcl genera lispworks mkcl sbcl scl xcl)
          (jlog:crit "chdir not supported on your implementation")))

(defmacro os-chdir (x)
  `(os-cd ,x))

(defun os-dirname (pathname)
  "Returns a new pathname with same HOST, DEVICE, DIRECTORY as PATHNAME,
and NIL NAME, TYPE and VERSION components"
  (when pathname
    (make-pathname :name nil :type nil :version nil :defaults pathname)))

(defun list-files-recursively (dir &optional files)
  (declare (list files))
  (cond ((null dir) nil)
        ((null files)
         (if (directory-pathname-p dir)
             (let ((list-dir (list-directory dir)))
               (append
                (if (atom dir) (list dir) dir)
                (list-files-recursively (car list-dir) (cdr list-dir))))
             (list dir)))
        (t
         (if (directory-pathname-p dir)
             (let ((list-dir (list-directory dir)))
               (append (if (atom dir) (list dir) dir)
                       (append
                        (list-files-recursively (car list-dir) (cdr list-dir))
                        (list-files-recursively (car files) (cdr files)))))
             (cons dir (list-files-recursively (car files) (cdr files)))))))

(defun find-by-name (name list)
  (cond ((null list) nil)
        ((search name (pathname-name (car list)))
         (cons (car list) (find-by-name name (cdr list))))
        (t (find-by-name name (cdr list)))))

(defun find-by-type (type list)
  (cond ((null list) nil)
        ((string-equal type (pathname-type (car list)))
         (cons (car list) (find-by-type type (cdr list))))
        (t (find-by-type type (cdr list)))))

(defun find-directories (list)
  (cond ((null list) nil)
        ((directory-pathname-p (car list))
         (cons (car list) (find-directories (cdr list))))
        (t (find-directories (cdr list)))))

(defun merge-n-directories (&rest dirs)
  (cond ((not (null (cdr dirs)))
         (merge-pathnames
          (eval `(merge-n-directories ,@(cdr dirs)))
          (pathname-as-directory (car dirs))))
        ((not (null dirs))
         (pathname-as-directory (car dirs)))
        (t "")))

;;;; FS functions

;;;; Stat section
(defun os-stat (pathname)
  (check-type pathname (or string pathname))
  #+sbcl(sb-posix:stat pathname)
  #-sbcl(jlog:crit "This lisp implementation is not supported by ``os-stat''"))

(defun os-stat-atime (pathname)
  (let ((stat (os-stat pathname)))
    (when stat
      #+sbcl(sb-posix:stat-atime stat)
      #-sbcl(jlog:crit "This lisp implementation is not supported by ``os-stat-atime''")
      )))

(defun os-stat-ctime (pathname)
  (let ((stat (os-stat pathname)))
    (when stat
      #+sbcl(sb-posix:stat-ctime stat)
      #-sbcl(jlog:crit "This lisp implementation is not supported by ``os-stat-ctime''")
      )))

(defun os-stat-dev (pathname)
  (let ((stat (os-stat pathname)))
    (when stat
      #+sbcl(sb-posix:stat-dev stat)
      #-sbcl(jlog:crit "This lisp implementation is not supported by ``os-stat-dev''")
      )))

(defun os-stat-gid (pathname)
  (let ((stat (os-stat pathname)))
    (when stat
      #+sbcl(sb-posix:stat-gid stat)
      #-sbcl(jlog:crit "This lisp implementation is not supported by ``os-stat-gid''")
      )))

(defun os-stat-ino (pathname)
  (let ((stat (os-stat pathname)))
    (when stat
      #+sbcl(sb-posix:stat-ino stat)
      #-sbcl(jlog:crit "This lisp implementation is not supported by ``os-stat-ino''")
      )))

(defun os-stat-mode (pathname)
  (let ((stat (os-stat pathname)))
    (when stat
      #+sbcl(sb-posix:stat-mode stat)
      #-sbcl(jlog:crit "This lisp implementation is not supported by ``os-stat-mode''")
      )))

(defun os-stat-mtime (pathname)
  (let ((stat (os-stat pathname)))
    (when stat
      #+sbcl(sb-posix:stat-mtime stat)
      #-sbcl(jlog:crit "This lisp implementation is not supported by ``os-stat-mtime''")
      )))

(defun os-stat-nlink (pathname)
  (let ((stat (os-stat pathname)))
    (when stat
      #+sbcl(sb-posix:stat-nlink stat)
      #-sbcl(jlog:crit "This lisp implementation is not supported by ``os-stat-nlink''")
      )))

(defun os-stat-rdev (pathname)
  (let ((stat (os-stat pathname)))
    (when stat
      #+sbcl(sb-posix:stat-rdev stat)
      #-sbcl(jlog:crit "This lisp implementation is not supported by ``os-stat-rdev''")
      )))

(defun os-stat-size (pathname)
  (let ((stat (os-stat pathname)))
    (when stat
      #+sbcl(sb-posix:stat-size stat)
      #-sbcl(jlog:crit "This lisp implementation is not supported by ``os-stat-size''")
      )))

(defun os-stat-uid (pathname)
  (let ((stat (os-stat pathname)))
    (when stat
      #+sbcl(sb-posix:stat-uid stat)
      #-sbcl(jlog:crit "This lisp implementation is not supported by ``os-stat-uid''")
      )))
;;;; /Stat section

(defun os-find (path &key name extension (type t))
  (check-type path (or string pathname))
  (check-type name (or null string))
  (check-type extension (or null string))
  (check-type type (or null symbol))
  (let* ((all-files (list-files-recursively (pathname-as-directory path)))
         (by-name (if (not (null name)) (find-by-name name all-files) all-files))
         (by-ext (if (not (null extension)) (find-by-type extension by-name) by-name)))
    (case type
      (:file
       (remove-if 'directory-pathname-p by-ext))
      (:directory
       (find-directories by-ext))
      (t by-ext))))

(defun cat-to-string (file)
  "cat Concatenate and print (display) the content of files"
  (flet ((slurp-stream (stream)
           (with-output-to-string (out)
             (let ((seq (make-array 1024 :element-type 'character
                                    :adjustable t
                                    :fill-pointer 1024)))
               (loop
                  (setf (fill-pointer seq) (read-sequence seq stream))
                  (when (zerop (fill-pointer seq))
                    (return))
                  (write-sequence seq out))))))
    (with-open-file (s file :direction :input)
      (slurp-stream s))))

(defun cat-to-list (file &aux result list (rt (copy-readtable)))
  "Converts file content to list of sequences"
  (with-open-file (stream file)
    (let ((*readtable* rt))
      (loop for item = (read stream nil stream)
         until (eq item stream)
         when item do (push item list)
         finally (when list (push (nreverse list) result)))
      (car (nreverse result)))))

(defun os-cat (file &optional (output-type :string)) ; types: :string :list
  "cat Concatenate and print (display) the content of files"
  (case output-type
    (:list (cat-to-list file))
    (:string (cat-to-string file))))

(defun os-pwd ()
  "Get the current working directory as per POSIX getcwd(3), as a pathname object"
  (or #+(or abcl genera xcl) (truename *default-pathname-defaults*) ;; d-p-d is canonical!
      #+allegro (excl::current-directory)
      #+clisp (ext:default-directory)
      #+clozure (ccl:current-directory)
      #+(or cmu scl) (#+cmu parse-unix-namestring* #+scl lisp::parse-unix-namestring
                            (strcat (nth-value 1 (unix:unix-current-directory)) "/"))
      #+cormanlisp (pathname (pl::get-current-directory)) ;; Q: what type does it return?
      #+(or clasp ecl) (ext:getcwd)
      #+gcl (let ((*default-pathname-defaults* #p"")) (truename #p""))
      #+lispworks (hcl:get-working-directory)
      #+mkcl (mk-ext:getcwd)
      #+sbcl (sb-ext:parse-native-namestring (sb-unix:posix-getcwd/))
      #+xcl (extensions:current-directory)
      (jlog:crit "getcwd not supported by your implementation")))

(defmacro os-getcwd ()
  `(os-pwd))

(defun os-mkdir (dirname)
  (check-type dirname (or string pathname))
  (jlog:dbg "Making directory ``~a''" dirname)
  (when (null (nth-value 1 (ensure-directories-exist (pathname-as-directory (parse-namestring dirname)))))
    (jlog:wrn "Directory ``~a'' already exists. Skipping" dirname)))

(defmacro os-mv (from to)
  (rename-file from to))

(defun os-cp (from to &key recursive force)
  (let ((to-file (if (directory-pathname-p to)
                     (merge-pathnames (make-pathname :name (pathname-name from)
                                                     :type (pathname-type from))
                                      to) to))
        (from-file (if (directory-exists-p (pathname-as-directory from))
                       (pathname-as-directory from) from)))
    (flet ((copy-file-local (from to force)
             (jlog:dbg "Copying from ``~a'' to ``~a''" from to)
             (when (not (directory-pathname-p from))
               (copy-file from to :overwrite force))))
      ;;
      (cond ((and (directory-pathname-p from-file) (not recursive))
             (jlog:err "Skipping copying. ``~a'' is directory and copying is not recursive" from-file))
            ((and (directory-pathname-p from-file) (eq recursive t))
             (dolist (file (os-find from-file))
               (let* ((relative-dir-file
                       (cons :relative (cdr (member
                                             (car (last (pathname-directory from-file)))
                                             (cdr (pathname-directory file))
                                             :test 'equal))))
                      (new-file-path
                       (merge-pathnames
                        (make-pathname :directory relative-dir-file
                                       :name (pathname-name file)
                                       :type (pathname-type file))
                        to-file)))
                 ;; make directory, if does not exists
                 (let ((dir (make-pathname :directory (pathname-directory new-file-path))))
                   (when (not (directory-exists-p dir))
                     (os-mkdir dir)))
                 ;; copying regular file
                 (copy-file-local file new-file-path force))))
            (t
             (copy-file-local from-file to-file force))))))

(defun os-rm (pathname &key recursive)
  (cond ((not (file-exists-p pathname))
         (jlog:err "File ``~a'' does not exists. Skipping" pathname))
        ((and (directory-pathname-p pathname) (not recursive))
         (jlog:err "Skipping removing. ``~a'' is directory and removing is not recursive" pathname))
        ((and (not (directory-pathname-p pathname))
              (not (directory-exists-p (pathname-as-directory pathname)))) ;; checking for incorrect filename input ;)
         (jlog:info "Removing file ``~a''" pathname)
         (delete-file pathname))
        (t (jlog:info "Removing directory ``~a''" pathname)
           (delete-directory-and-files pathname :if-does-not-exist nil))))

(defun os-touch (pathname)
  (if (directory-pathname-p pathname)
      (jlog:err "Can not touch. ``~a'' is a directory" pathname)
      (close (open pathname :direction :probe :if-does-not-exist :create))))

(defun os-ln (oldpath newpath &optional (symbolic-p nil))
  #+sbcl(if symbolic-p
            (sb-posix:symlink oldpath newpath)
            (sb-posix:link oldpath newpath))
  #-sbcl(jlog:crit "This lisp implementation is not supported by ``os-ln''"))

(defun os-ls (pathname)
  (check-type pathname (or string pathname))
  (list-directory (pathname-as-directory pathname)))

;;;; Environment variables: getting them, and parsing them.
(defun os-getenv (x)
  "Query the environment, as in C getenv.
Beware: may return empty string if a variable is present but empty;
use getenvp to return NIL in such a case."
  (declare (ignorable x))
  #+(or abcl clasp clisp ecl xcl) (ext:getenv x)
  #+allegro (sys:getenv x)
  #+clozure (ccl:getenv x)
  #+cmu (unix:unix-getenv x)
  #+scl (cdr (assoc x ext:*environment-list* :test #'string=))
  #+cormanlisp
  (let* ((buffer (ct:malloc 1))
         (cname (ct:lisp-string-to-c-string x))
         (needed-size (win:getenvironmentvariable cname buffer 0))
         (buffer1 (ct:malloc (1+ needed-size))))
    (prog1 (if (zerop (win:getenvironmentvariable cname buffer1 needed-size))
               nil
               (ct:c-string-to-lisp-string buffer1))
      (ct:free buffer)
      (ct:free buffer1)))
  #+gcl (system:getenv x)
  #+genera nil
  #+lispworks (lispworks:environment-variable x)
  #+mcl (ccl:with-cstrs ((name x))
          (let ((value (_getenv name)))
            (unless (ccl:%null-ptr-p value)
              (ccl:%get-cstring value))))
  #+mkcl (#.(or (find-symbol* 'getenv :si nil) (find-symbol* 'getenv :mk-ext nil)) x)
  #+sbcl (sb-ext:posix-getenv x)
  #-(or abcl allegro clasp clisp clozure cmu cormanlisp ecl gcl genera lispworks mcl mkcl sbcl scl xcl)
  (jlog:crit "~S is not supported on your implementation" 'getenv))

(defsetf os-getenv (x) (val)
  "Set an environment variable."
  (declare (ignorable x val))
  #+allegro `(setf (sys:getenv ,x) ,val)
  #+clisp `(system::setenv ,x ,val)
  #+clozure `(ccl:setenv ,x ,val)
  #+cmu `(unix:unix-setenv ,x ,val 1)
  #+ecl `(ext:setenv ,x ,val)
  #+lispworks `(hcl:setenv ,x ,val)
  #+mkcl `(mkcl:setenv ,x ,val)
  #+sbcl `(progn (require :sb-posix) (symbol-call :sb-posix :setenv ,x ,val 1))
  #-(or allegro clisp clozure cmu ecl lispworks mkcl sbcl)
  '(jlog:crit "~S ~S is not supported on your implementation" 'setf 'getenv))

(defun os-getenvp (x)
  "Predicate that is true if the named variable is present in the libc environment,
then returning the non-empty string value of the variable"
  (let ((g (os-getenv x))) (and (not (emptyp g)) g)))

(defun implementation-type ()
  "The type of Lisp implementation used, as a short UIOP-standardized keyword"
  (first-feature
   '(:abcl (:acl :allegro) (:ccl :clozure) :clisp (:corman :cormanlisp)
     (:cmu :cmucl :cmu) :clasp :ecl :gcl
     (:lwpe :lispworks-personal-edition) (:lw :lispworks)
     :mcl :mkcl :sbcl :scl (:smbx :symbolics) :xcl)))

(defvar *implementation-type* (implementation-type)
  "The type of Lisp implementation used, as a short UIOP-standardized keyword")

(defun os-architecture ()
  "The CPU architecture of the current host"
  (first-feature
   '((:x64 :x86-64 :x86_64 :x8664-target :amd64 (:and :word-size=64 :pc386))
     (:x86 :x86 :i386 :i486 :i586 :i686 :pentium3 :pentium4 :pc386 :iapx386 :x8632-target)
     (:ppc64 :ppc64 :ppc64-target) (:ppc32 :ppc32 :ppc32-target :ppc :powerpc)
     :hppa64 :hppa :sparc64 (:sparc32 :sparc32 :sparc)
     :mipsel :mipseb :mips :alpha (:arm :arm :arm-target) :imach
     ;; Java comes last: if someone uses C via CFFI or otherwise JNA or JNI,
     ;; we may have to segregate the code still by architecture.
     (:java :java :java-1.4 :java-1.5 :java-1.6 :java-1.7))))

(defun operating-system ()
  "The operating system of the current host"
  (first-feature
   '(:cygwin
     (:win :windows :mswindows :win32 :mingw32)      ;; try cygwin first!
     (:linux :linux :linux-target)                   ;; for GCL at least, must appear before :bsd
     (:macosx :macosx :darwin :darwin-target :apple) ; also before :bsd
     (:solaris :solaris :sunos)
     (:bsd :bsd :freebsd :netbsd :openbsd :dragonfly)
     :unix
     :genera)))

(defun implementation-identifier ()
  "Return a string that identifies the ABI of the current implementation,
suitable for use as a directory name to segregate Lisp FASLs, C dynamic libraries, etc."
  (substitute-if
   #\_ #'(lambda (x) (find x " /:;&^\\|?<>(){}[]$#`'\""))
   (format nil "~(~a~@{~@[-~a~]~}~)"
           (or (implementation-type) (lisp-implementation-type))
           (or (lisp-version-string) (lisp-implementation-version))
           (or (operating-system) (software-type))
           (or (os-architecture) (machine-type)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun subtree (name list)
  (cond ((null list) nil)
        ((null name)
         (if (listp list) list t))
        ((atom (car list))
         (if (equal (car name) (car list))
             (subtree (cdr name) (cadr list))
             (subtree name (cdr list))))
        (t ;; (car list) is list
         (subtree name (cdr list)))))

(defun replace-subtree (name list sublist)
  "TODO: not done"
  (cond ((null list) nil)
        ((null name) sublist)
        ((eq name t) list)
        ((atom (car list))
         (if (equal (car name) (car list))
             (if (listp (cadr list))
                 (cons (car list) (append (list (replace-subtree (cdr name) (cadr list) sublist))
                                          (replace-subtree (or (cdr name) t) (cddr list) sublist)))
                 (cons (car list) (append (replace-subtree (cdr name) (cadr list) sublist)
                                          (replace-subtree (or (cdr name) t) (cddr list) sublist))))
             (cons (car list) (replace-subtree name (cdr list) sublist))))
        (t ;; (car list) is list
         (cons (car list) (replace-subtree name (cdr list) sublist)))))

(defun add-subtree (name list sublist)
  "TODO: not done"
  (cond ((null list) nil)
        ((null name) (append list sublist))
        ((eq name t) list)
        ((atom (car list))
         (if (equal (car name) (car list))
             (if (listp (cadr list))
                 (cons (car list) (append (list (add-subtree (cdr name) (cadr list) sublist))
                                          (add-subtree (or (cdr name) t) (cddr list) sublist)))
                 (cons (car list) (append (add-subtree (cdr name) (cadr list) sublist)
                                          (add-subtree (or (cdr name) t) (cddr list) sublist))))
             (cons (car list) (add-subtree name (cdr list) sublist))))
        (t ;; (car list) is list
         (cons (car list) (add-subtree name (cdr list) sublist)))))

(defmacro namespace-subtree-p (&rest name)
  "Get namespace subtree by name. Ex.: (namespace-subtree :jabs :plugin)"
  `(subtree ',name *jabs-namespace-tree*))

(defmacro add-namespace-subtree (subtree &rest name)
  "Get namespace subtree by name. Ex.: (namespace-subtree :jabs :plugin)"
  `(let ((tree (add-subtree ',name *jabs-namespace-tree* ',subtree)))
     (setf *jabs-namespace-tree* tree)))

(defmacro replace-namespace-subtree (subtree &rest name)
  "Get namespace subtree by name. Ex.: (namespace-subtree :jabs :plugin)"
  `(replace-subtree ',name *jabs-namespace-tree* ',subtree))

(defun find-file-from-list-by-filename (name files)
	"Find file by name from list of files"
	(cond ((null files) nil)
				((equal name (pathname-name (car files)))
				 (car files))
				(t (find-file-from-list-by-filename name (cdr files)))))

;; (defun get-namespace-subtree-level (&rest name)
;;   (let ((subtree (apply #'namespace-subtree name))
;;         (collection))
;;     (cond ((eq subtree t) t)
;;           ((eq subtree nil) nil)
;;           (t
;;            (dolist (v subtree)
;;              (push (car v) collection)))
;;           collection)))

;; (defun set-namespace-subtree (space tree)
;;   (cond ((null space) t)
;;         ((member (car space) (get-cars tree))
;;          (cons

(defun pathname-root (pathname)
  (make-pathname :directory '(:absolute)
                 :name nil :type nil :version nil
                 :defaults pathname ;; host device, and on scl, *some*
                 ;; scheme-specific parts: port username password, not others:
                 . #.(or #+scl '(:parameters nil :query nil :fragment nil))))

(defun absolute-pathname-p (pathspec)
  (and (typep pathspec '(or pathname string))
       (eq :absolute (car (pathname-directory (pathname pathspec))))))

(defun truenamize (pathname &optional (defaults *default-pathname-defaults*))
  "Resolve as much of a pathname as possible"
  (block nil
    (when (typep pathname '(or null logical-pathname)) (return pathname))
    (let ((p (merge-pathnames pathname defaults)))
      (when (typep p 'logical-pathname) (return p))
      (let ((found (file-exists-p p)))
        (when found (return found)))
      (unless (absolute-pathname-p p)
        (let ((true-defaults (ignore-errors (truename defaults))))
          (when true-defaults
            (setf p (merge-pathnames pathname true-defaults)))))
      (unless (absolute-pathname-p p) (return p))
      (let ((sofar (file-exists-p (pathname-root p))))
        (unless sofar (return p))
        (flet ((solution (directories)
                 (merge-pathnames
                  (make-pathname :host nil :device nil
                                 :directory `(:relative ,@directories)
                                 :name (pathname-name p)
                                 :type (pathname-type p)
                                 :version (pathname-version p))
                  sofar)))
          (loop :with directory = (pathname-directory p)
             :for component :in (cdr directory)
             :for rest :on (cdr directory)
             :for more = (file-exists-p
                          (merge-pathnames
                           (make-pathname :directory `(:relative ,component))
                           sofar)) :do
             (if more
                 (setf sofar more)
                 (return (solution rest)))
             :finally
             (return (solution nil))))))))

(defun os-realpath (path)
  "Get realpath path of file (resolve symlinks)"
  #-allegro (truenamize path)
  #+allegro (if (typep path 'logical-pathname)
                path
                (excl:pathname-resolve-symbolic-links path)))

(defun implementation-signature ()
	"Return a string suitable for discriminating different
implementations, or similar implementations with possibly-incompatible
FASLs."
	;; FIXME: Will this have problems with stuff like threads vs
	;; non-threads fasls?
	(let ((*print-pretty* nil)) ;; TODO: what is *print-pretty*?
		(format nil "lisp-implementation-type: ~A~%~
                 lisp-implementation-version: ~A~%~
                 machine-type: ~A~%~
                 machine-version: ~A~%"
						(lisp-implementation-type)
						(lisp-implementation-version)
						(machine-type)
						(machine-version))))

(defun os-env ()
  #+sbcl(sb-ext:posix-environ)
  #-sbcl(jlog:crit "os-env note implemented for this lisp")
  )

;; os-ps

(defun mypid ()
  #+sbcl(sb-posix:getpid)
  #-sbcl(jlog:crit "This lisp implementation is not supported by ``mypid''")
  )

(defun myuid ()
  #+sbcl(sb-posix:getuid)
  #-sbcl(jlog:crit "This lisp implementation is not supported by ``myuid''")
  )

(defun mygid ()
  #+sbcl(sb-posix:getgid)
  #-sbcl(jlog:crit "This lisp implementation is not supported by ``mygid''")
  )

(defun mygroup ()
  #+sbcl(sb-posix:getgrgid (mygid))
  #-sbcl(jlog:crit "This lisp implementation is not supported by ``mygroup''")
  )

(defun process-parent-pid (pid)
  #+sbcl(sb-posix:getpgid pid)
  #-sbcl(jlog:crit "This lisp implementation is not supported by ``process-parent-pid''")
  )

(defun process-status (process)
  #+sbcl(sb-ext:process-status process)
  #-sbcl(jlog:crit "This lisp implementation is not supported by ``process-status''")
  )

(defun process-exit-code (process)
  #+sbcl(sb-ext:process-exit-code process)
  #-sbcl(jlog:crit "This lisp implementation is not supported by ``process-exit-code''")
  )

(defun os-exec (program args &key
                               (input *standard-input*)
                               if-input-does-not-exist
                               (output *standard-output*)
                               (if-output-exists :error)
                               (error *error-output*)
                               (if-error-exists :error)
                               ;; (environment (os-env))
                               status-hook
                               detach-p
                               (directory (os-pwd))
                               ;; (collect-output-to-string-p nil)
                               )
  (jlog:dbg "Executing binary ``~a'' with arguments ``~a''" program args)
  (let ((process-status))
    #+sbcl(sb-ext:run-program
	   program args ;; (stringify-args args)
	   ;; :env environment
	   :wait (not detach-p)
	   :search t
	   :input input
	   :output output
	   :error error
	   :if-input-does-not-exist if-input-does-not-exist
	   :if-output-exists if-output-exists
	   :if-error-exists if-error-exists
	   :status-hook #'(lambda (x)
			    (progn
			      (when (not detach-p)
				(setf process-status (process-exit-code x)))
			      (when status-hook
				(funcall status-hook x))))
	   :directory directory
	   )
    #-sbcl(jlog:crit "Can not exec ``~a''. Lisp implementation is not supported" program)
    process-status))

;;;; KILL
(defvar *signal-mapping*
  '((:hangup . 1)
    (:interrupt . 2)
    (:quit . 3)
    (:illegal-instruction . 4)
    (:breakpoint-trap . 5)
    (:abort . 6)
    (:emulation-trap . 7)
    (:arithmetic-exception . 8)
    (:killed . 9)
    (:bus-error . 10)
    (:segmentation-fault . 11)
    (:bad-system-call . 12)
    (:broken-pipe . 13)
    (:alarm-clock . 14)
    (:terminate . 15)
    ))

(defun convert-signal (signal)
  (check-type signal keyword)
  (or
   (cdr (assoc signal *signal-mapping*))
   (jlog:crit "Symbolic signal ``~A'' not supported." signal)))

(defun os-kill (process &optional (signal 15))
  (check-type signal (or keyword integer))
  #+sbcl(let ((real-signal (if (keywordp signal) (convert-signal signal) signal))
              (real-pid (if (sb-ext:process-p process) (sb-ext:process-pid process) process)))
          (sb-posix:kill real-pid real-signal))
  #-sbcl(jlog:crit "This lisp implementation is not supported by ``os-kill''")
  )

;;;; ps section
#+linux(defvar +linux-proc-directory+ (make-pathname :directory '(:absolute "proc")))

(defun list-pids ()
  #+linux(remove nil
                 (mapcar #'(lambda (v)
                             (when (and
                                    (directory-pathname-p v)
                                    (file-exists-p (merge-pathnames (make-pathname :name "stat")  v))
                                    (not (directory-exists-p
                                          (merge-pathnames (make-pathname :name "stat")  v)))
                                    (file-pathname-p (merge-pathnames (make-pathname :name "stat")  v)))
                               (parse-integer (tostr (pathname-name (pathname-as-file v))))))
                         (list-directory +linux-proc-directory+)))
  #-linux(jlog:crit "This OS is not supported by ``pid-list''"))

#+linux(defun linux-proc-file (pid filename)
              (check-type pid integer)
              (check-type filename (or string pathname))
              (file-exists-p
               (merge-pathnames
                (parse-namestring filename)
                (merge-pathnames
                 (make-pathname :directory (list :relative (tostr pid)))
                 +linux-proc-directory+))))

#+linux(defun linux-pid-stat (pid)
         (check-type pid integer)
         (let ((file (linux-proc-file pid "stat")))
           (when file
             (cat-to-list file))))

(defun parent-pid (pid)
  #+linux(nth 3 (linux-pid-stat pid))
  #-linux(jlog:crit "This OS is not supported by ``parent-pid''"))

(defun pid-uid (pid)
  #+linux(car (cat-to-list (linux-proc-file pid "loginuid")))
  #-linux(jlog:crit "This OS is not supported by ``pid-uid''"))

(defun pid-name (pid)
  #+linux(nth 1 (linux-pid-stat pid))
  #-linux(jlog:crit "This OS is not supported by ``pid-name''"))

(defun pid-status (pid)
  #+linux(nth 2 (linux-pid-stat pid))
  #-linux(jlog:crit "This OS is not supported by ``pid-status''"))

(defun pid-env (pid)
  #+linux(split #\  (cat-to-string (linux-proc-file pid "environ")))
  #-linux(jlog:crit "This OS is not supported by ``pid-env''"))

(defun pid-command (pid)
  #+linux(split #\  (cat-to-string (linux-proc-file pid "cmdline")))
  #-linux(jlog:crit "This OS is not supported by ``pid-cmdline''"))

(defun list-user-pids (&optional (uid (myuid)))
  (remove nil
          (mapcar
           #'(lambda (x)
               (when (equal uid
                            (car (cat-to-list (linux-proc-file (parse-integer x) "loginuid"))))
                 (parse-integer x)))
                  (list-pids))))
