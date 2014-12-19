(defmodule lsci-np
  (export all))

(include-lib "lsci/include/np.lfe")

(defun version ()
  (lsci:py-const 'numpy 'version.version 'str))

;;; Array Attributes
(defun shape (array)
  (lsci:py-attr array 'shape))

(defun size (array)
  (lsci:py-attr array 'size))

;;; Array Methods
;;
;; Array conversion
;;
(defun item (array args)
  (lsci:py-call array 'item args))

(defun tolist (array)
  (lsci:py-call array 'tolist))

(defun tostring (array)
  (tobytes array))

(defun tostring (array order)
  "order may be either the string C or F."
  (tobytes array order))

(defun tobytes (array)
  (lsci:py-call array 'tobytes))

(defun tobytes (array order)
  "order may be either the string C or F."
  (lsci:py-call
    array
    'tostring
    '()
    `(#(order ,(list_to_binary order)))))

(defun dump (array filename)
  (lsci:py-call array 'dump `(,(list_to_binary filename)))
  'ok)

(defun dumps (array)
  (lsci:py-call array 'dumps))

(defun getfield (array dtype)
  (lsci:py-call array 'getfield `(,(list_to_binary dtype))))

(defun getfield (array dtype offset)
  (lsci:py-call
    array
    'tostring
    `(,(list_to_binary dtype))
    `(#(offset , offset))))

;; Shape manipulation
;;
;; Item selection and manipulation
;;
;; Calculation
;;

