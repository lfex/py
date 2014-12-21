(defmodule lsci-np
  (export all))

(include-lib "lsci/include/np.lfe")

(defun version ()
  (lsci-py:const 'numpy 'version.version 'str))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Array Attributes
;;;

;; Memory layout
;;
(defun flags (array)
  (lsci-py:attr array 'flags))

(defun shape (array)
  (lsci-py:attr array 'shape))

(defun strides (array)
  (lsci-py:attr array 'strides))

(defun ndim (array)
  (lsci-py:attr array 'ndim))

(defun data (array)
  (lsci-py:attr array 'data))

(defun size (array)
  (lsci-py:attr array 'size))

(defun itemsize (array)
  (lsci-py:attr array 'itemsize))

(defun nbytes (array)
  (lsci-py:attr array 'nbytes))

(defun base (array)
  (lsci-py:attr array 'base))

;; Data type
;;
(defun dtype (array)
  (lsci-py:attr array 'dtype))

;; Other attributes
;;
(defun real (array)
  (lsci-py:attr array 'real))

(defun imag (array)
  (lsci-py:attr array 'imag))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Array Methods
;;;

;; Array conversion
;;
(defun item (array args)
  (lsci-py:method-call array 'item args))

(defun tolist (array)
  (lsci-py:method-call array 'tolist))

(defun ->list (array)
  (lsci-py:method-call array 'tolist))

(defun tostring (array)
  (tobytes array))

(defun tostring (array order)
  "order may be either the string C or F."
  (tobytes array order))

(defun ->str (array)
  (tobytes array))

(defun ->str (array order)
  (tobytes array order))

(defun tobytes (array)
  (lsci-py:method-call array 'tobytes))

(defun tobytes (array order)
  "order may be either the string C or F."
  (lsci-py:method-call
    array
    'tostring
    '()
    `(#(order ,(list_to_binary order)))))

(defun ->bytes (array)
  (lsci:py-call array 'tobytes))

(defun ->bytes (array order)
  (lsci-py:method-call
    array
    'tostring
    '()
    `(#(order ,(list_to_binary order)))))

(defun dump (array filename)
  (lsci-py:method-call array 'dump `(,(list_to_binary filename)))
  'ok)

(defun dumps (array)
  (lsci-py:method-call array 'dumps))

(defun getfield (array dtype)
  (lsci-py:method-call array 'getfield `(,(list_to_binary dtype))))

(defun getfield (array dtype offset)
  (lsci-py:method-call
    array
    'tostring
    `(,(list_to_binary dtype))
    `(#(offset , offset))))

;; Shape manipulation
;;
;; TBD

;; Item selection and manipulation
;;
;; TBD

;; Calculation
;;
;; TBD

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Wrappers - the following functions couldn't be simply wrapped with the
;;;            same macros that wrap most of the other NumPy functions due to
;;;            the fact that these needed some sort of special handling.
;;;

;; Numerical ranges
;;
(defun linspace (start stop kwargs)
  (lsci-py:func-call 'numpy 'linspace `(,start ,stop) kwargs))

(defun meshgrid (coords)
  (lsci-py:func-call 'numpy 'meshgrid coords))

(defun meshgrid (coords kwargs)
  (lsci-py:func-call 'numpy 'meshgrid coords kwargs))

;; I/O
;;
(defun genfromtxt (filename)
  (genfromtxt filename '()))

(defun genfromtxt (filename kwargs)
  (lsci:py-func-call 'numpy 'genfromtxt `(,(list_to_binary filename)) kwargs))
