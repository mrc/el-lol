;;; lol.el --- functions ported from Lisp Over Lambda

;; Author: Doug Hoyte (Original Common Lisp version)
;; Author: Matt Curtis <matt.r.curtis@gmail.com> (Emacs lisp)
;; Maintainer: Matt Curtis <matt.r.curtis@gmail.com>
;; Version: 0.1
;; Keywords: lisp
;; URL: https://github.com/mrc/el-lol

;;; Commentary:
;; Ported from code from Doug Hoyte's great book "Let Over Lambda".
;; Please refer to the book for details and evolution.

;; Original code: http://letoverlambda.com/lol.lisp
;; License from original code:
;; This code is (C) 2002-2008, Doug Hoyte.
;;
;; You are free to use, modify, and re-distribute
;; this code however you want, except that any
;; modifications must be clearly indicated before
;; re-distribution. There is no warranty,
;; expressed nor implied.
;;
;; Attribution of this code to me, Doug Hoyte, is
;; appreciated but not necessary. If you find the
;; code useful, or would like documentation,
;; please consider buying the book!

(require 'cl)

(defun mkstr (&rest args)
  "Concatenate the string representations of ARGS."
  (with-output-to-string
    (dolist (a args) (princ a))))

(defun symb (&rest args)
  "Make a symbol from a ARGS."
  (intern (apply #'mkstr args)))

(defun flatten (x)
  "Flatten a sexpr to a single list."
  (labels ((rec (x acc)
                (cond ((null x) acc)
                      ((atom x) (cons x acc))
                      (t (rec
                          (car x)
                          (rec (cdr x) acc))))))
    (rec x nil)))

(defun leader!-symbol-p (s leader)
  (let* ((leader! (mkstr leader "!"))
         (leader!-len (length leader!)))
    (and (symbolp s)
         (> (length (symbol-name s)) leader!-len)
         (string= (substring (symbol-name s) 0 leader!-len)
                  leader!))))

(defun g!-symbol-p (s) (leader!-symbol-p s "g"))
(defun o!-symbol-p (s) (leader!-symbol-p s "o"))

(defun o!-symbol-to-g!-symbol (s)
  (symb "g!"
        (subseq (symbol-name s) 2)))

(defmacro defmacro/g! (name args &rest body)
  (let ((syms (remove-duplicates
               (remove-if-not #'g!-symbol-p
                              (flatten body)))))
    `(defmacro ,name ,args
       (let ,(mapcar
              (lambda (s)
                `(,s (gensym ,(subseq
                               (symbol-name s)
                               2))))
              syms)
         ,@body))))

(defmacro defmacro! (name args &rest body)
  (let* ((os (remove-if-not #'o!-symbol-p args))
         (gs (mapcar #'o!-symbol-to-g!-symbol os)))
    `(defmacro/g! ,name ,args
       `(let ,(mapcar* #'list  (list ,@gs) (list ,@os))
          ,(progn ,@body)))))

(provide 'lol)

;;; lol.el ends here
