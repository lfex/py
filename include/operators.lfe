;;;; This inlcude wraps functions provided by the Python operator module.
;;;;
;;;; The following functions are provided in Python by this module:
;;;;   https://docs.python.org/3.4/library/operator.html
;;;;
(eval-when-compile

  (defun get-operator-funcs ()
    '(;; Equality
      (lt 2)
      (le 2)
      (eq 2)
      (ne 2)
      (ge 2)
      (gt 2)
      ;; Logic
      (not- 1)
      (truth 1)
      (is- 2)
      (is-not 2)
      ;; Arithmetic
      (add 2)
      (floordiv 2)
      (index 1)
      (mod 2)
      (mul 2)
      (neg 1)
      (pos 1)
      (sub 2)
      (truediv 2)
      ;; Bitwise
      (and- 2)
      (inv 1)
      (invert 1)
      (lshift 2)
      (or- 2)
      (rshift 2)
      (xor 2)
      ;; Sequences
      (concat 2)
      (contains 2)
      (countOf 2)
      (getitem 2)
      (indexOf 2)
      (length_hint 1) (length_hint 2)
      ;; Misc
      (attrgetter 1)
      (itemgetter 1)
      (methodcaller 1) (methodcaller 2))))

(defmacro generate-operators-api ()
  `(progn ,@(py-util:make-funcs (get-operator-funcs) 'operator)))

(generate-operators-api)

(defun loaded-operators ()
  "This is just a dummy function for display purposes when including from the
  REPL (the last function loaded has its name printed in stdout).
  This function needs to be the last one in this include."
  'ok)
