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
(in-package :re@jabs)

(defun tolist (string)
  (check-type string string)
  (concatenate 'list string))

(defun tostr (list)
  (concatenate 'string list))

(defun get-before (char list)
  "Get part of list before character
gives whole list, when char does not exists in list"
  (check-type char character)
  (check-type list list)
  (cond ((or
          (null list)
          (equal (car list) char))
         nil)
        (t (cons (car list) (get-before char (cdr list))))))

(defun get-after (char list)
  (check-type char character)
  (check-type list list)
  (cdr (member char list)))

(defun get-first-n (n list)
  "Get first N elements of list"
  (cond ((or (= n 0) (null list)) nil)
        (t
         (cons (car list)
               (get-first-n (- n 1) (cdr list))))))

(defun cut-first-n (n list)
  "Get first N elements of list"
  (if (= n 0) list (cut-first-n (- n 1) (cdr list))))

(defun split (char string)
  (check-type string string)
  (check-type char character)
  (let ((list (tolist string)) (splitted))
    (labels ((split-list (char list)
                         (let ((before (get-before char list)))
                           (if (equal before list)
                               (push list splitted)
                             (progn
                               (push before splitted)
                               (split-list char (cdr (member char list :test 'equal)))))))
             (sublists-to-strings (list)
                                  (cond ((null list) nil)
                                        (t
                                         (cons (tostr (car list))
                                               (sublists-to-strings (cdr list)))))))
      (sublists-to-strings (reverse (split-list char list))))))

;; split
;; (string-trim '(#\[ #\]) "[ 'test', 'test2', 'test3' ]")

(defun replace-list (list replace replace-to &key all)
  "Replace some part of list to other part"
  (cond ((null list) nil)
        ((equal (car list) (car replace))
         (let ((r-length (length replace)))
           (if (equal replace (get-first-n r-length list))
               (if all
                   (append replace-to
                           (replace-list (cut-first-n r-length list) replace replace-to :all all))
                 (append replace-to
                         (cut-first-n r-length list)))

             (cons (car list) (replace-list (cdr list) replace replace-to  :all all)))))
        (t (cons (car list) (replace-list (cdr list) replace replace-to  :all all)))))

(defun replace-inside-list (list-in replacement &optional start-symbol end-symbol)
  "Replace part of list between start-symbol and end-symbol to other list part"
  (let ((begin (when start-symbol (get-before start-symbol list-in)))
        (end (when end-symbol (reverse (get-before end-symbol (reverse list-in))))))
    (append begin replacement end)))

(defun replace-string (string replace replace-to)
  (let ((s-list (tolist string))
        (f-list (tolist replace))
        (t-list (tolist replace-to)))
    (tostr (replace-list s-list f-list t-list))))

(defun replace-string-all (string replace replace-to)
  (let ((s-list (tolist string))
        (f-list (tolist replace))
        (t-list (tolist replace-to)))
    (tostr (replace-list s-list f-list t-list :all t))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; ^ $ | *

(defvar *expressions* '(#\^ #\$ #\* #\| #\.))
(defvar *exphash* (make-hash-table))

(defun begin-match-list (sublist mainlist)
  "Returns sublist if it match with begining of mainlist"
  (cond ((or (null mainlist) (null sublist)) nil)
        ((equal (car sublist) (car mainlist))
         (cons (car sublist)
               (begin-match-list (cdr sublist) (cdr mainlist))))
        (t nil)))

(defun begin-scan (sub string)
  "Scan substring at the begining of string"
  (let ((sublist (tolist sub))
        (stringlist (tolist string)))
    (if (equal sublist (begin-match-list sublist stringlist))
        (tostr sublist) nil)))

(defun middle-scan (startsymbol endsymbol string)
  "Scan substring between startsymbol and endsymbol in string"
    (labels ((middle-match-list (start end mainlist &optional scan-started)
                                (cond ((null mainlist) nil)
                                      (scan-started
                                       (if (equal end (car mainlist))
                                           nil
                                         (cons (car mainlist)
                                               (middle-match-list start end (cdr mainlist) scan-started))))

                                      (t (if (equal start (car mainlist))
                                             (middle-match-list start end (cdr mainlist) t)
                                           (middle-match-list start end (cdr mainlist)))))))
      (let ((result (middle-match-list startsymbol endsymbol (tolist string))))
        (when result (tostr result)))))

(defun scan-all-to-list (startsymbol endsymbol target-string &optional collector) ; TODO: seems, not used. Remove?
  (let* ((stringlist (concatenate 'list target-string))
         (mml (concatenate 'list (middle-scan startsymbol endsymbol target-string)))
         (position (+ 2 (- (length stringlist) (length (member startsymbol stringlist))) (length mml))))
    (jlog:dbg "stringlist ``~a''~%mml ``~a''~%position ``~a''" stringlist mml position)
    (if (null stringlist) (reverse collector)
        (progn
          (jlog:dbg "position ``~a''~%rest ``~a''"
                    position (cut-first-n position stringlist))
          (when mml (push (concatenate 'string mml) collector))
          (scan-all-to-list startsymbol endsymbol
                            (concatenate 'string
                                         (cut-first-n position stringlist)) collector)))))

(defun scan (sub string)
  "Scan for substring in string"
  (let ((sublist (tolist sub))
        (stringlist (tolist string)))
    (labels ((scan-list (sublist list)
                        (cond ((or (null list) (null sublist)) nil)
                              ((equal (car sublist) (car list))
                               (cons (car sublist)
                                     (scan-list (cdr sublist) (cdr list))))
                              (t (scan-list sublist (cdr list))))))
      (equal sublist (scan-list sublist stringlist)))))

(defun end-scan (sub string)
  "Scan substring at the end end of string"
  (let ((sublist (reverse (tolist sub)))
        (stringlist (reverse (tolist string))))
    (if (equal sublist (begin-match-list sublist stringlist))
        (tostr (reverse sublist)) nil)))

(defun end-cut (sub string)
  (let* ((sublist (reverse (tolist sub)))
         (stringlist (reverse (tolist string)))
         (sublen (length sublist)))
    (when (equal sublist (begin-match-list sublist stringlist))
      (tostr (reverse (cut-first-n sublen stringlist))))))

(defun begin-cut (sub string)
  (let* ((sublist (tolist sub))
         (stringlist (tolist string))
         (sublen (length sublist)))
    (when (equal sublist (begin-match-list sublist stringlist))
      (tostr (cut-first-n sublen stringlist)))))

(defun replace-inside-string (string replacement &optional start-symbol end-symbol)
  "Replace part of string between start-symbol and end-symbol"
  (let ((s-list (tolist string))
        (r-list (tolist replacement)))
    (concatenate 'string (replace-inside-list s-list r-list start-symbol end-symbol))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; JABS ONLY ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun concatenate-to-string-with-delimiter (delimiter &rest strings)
  (cond ((null strings) nil)
        ((and (not (null (car strings))) (null (cdr strings)))
         (princ-to-string (car strings)))
        (t (concatenate 'string (princ-to-string (car strings)) (princ-to-string delimiter)
                        (apply 'concatenate-to-string-with-delimiter (cons delimiter (cdr strings)))))))

(defun parse-complex-string (string)
  "Parse name, which consists of name and/or package and/or plugin"
  (let ((name (nth 0 (split "@" string)))
        (package (nth 1 (split "@" string)))
        (plugin (nth 2 (split "@" string))))
    (values name
            (when (not (string-equal "" package)) package)
            (when (not (string-equal "" plugin)) plugin))))
