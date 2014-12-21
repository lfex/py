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
  (lsci-py:method-call array 'tobytes))

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
(defun reshape (array shape)
  (lsci-py:method-call array 'reshape `(,shape)))

(defun reshape (array shape kwargs)
  (lsci-py:method-call array 'reshape `(,shape) kwargs))

(defun resize (array shape)
  (lsci-py:method-call array 'resize `(,shape)))

(defun resize (array shape kwargs)
  (lsci-py:method-call array 'resize `(,shape) kwargs))

(defun transpose (array)
  (lsci-py:method-call array 'transpose))

(defun transpose (array axes)
  (lsci-py:method-call array 'transpose axes))

(defun swapaxes (array axis-1 axis-2)
  (lsci-py:method-call array 'swapaxes `(,axis-1 ,axis-2)))

(defun flatten (array kwargs)
  (lsci-py:method-call array 'flatten '() kwargs))

(defun ravel (array order)
  (lsci-py:method-call array 'ravel `(,order)))

(defun squeeze (array kwargs)
  (lsci-py:method-call array 'squeeze '() kwargs))

;; Item selection and manipulation
;;
;; TBD

;; Calculation
;;
(defun max (array)
  (lsci-py:method-call array 'max))

(defun max (array kwargs)
  (lsci-py:method-call array 'max '() kwargs))

(defun argmax (array)
  (lsci-py:method-call array 'argmax))

(defun argmax (array kwargs)
  (lsci-py:method-call array 'argmax '() kwargs))

(defun min (array)
  (lsci-py:method-call array 'min))

(defun min (array kwargs)
  (lsci-py:method-call array 'min '() kwargs))

(defun argmin (array)
  (lsci-py:method-call array 'argmin))

(defun argmin (array kwargs)
  (lsci-py:method-call array 'argmin '() kwargs))

(defun ptp (array)
  (lsci-py:method-call array 'ptp))

(defun ptp (array kwargs)
  (lsci-py:method-call array 'ptp '() kwargs))

(defun clip (array min max)
  (lsci-py:method-call array 'clip `(,min ,max)))

(defun clip (array min max kwargs)
  (lsci-py:method-call array 'clip `(,min ,max) kwargs))

(defun conj (array)
  (lsci-py:method-call array 'conj))

(defun round (array)
  (lsci-py:method-call array 'round))

(defun round (array kwargs)
  (lsci-py:method-call array 'round '() kwargs))

(defun trace (array)
  (lsci-py:method-call array 'trace))

(defun trace (array kwargs)
  (lsci-py:method-call array 'trace '() kwargs))

(defun sum (array)
  (lsci-py:method-call array 'sum))

(defun sum (array kwargs)
  (lsci-py:method-call array 'sum '() kwargs))

(defun cumsum (array)
  (lsci-py:method-call array 'cumsum))

(defun cumsum (array kwargs)
  (lsci-py:method-call array 'cumsum '() kwargs))

(defun prod (array)
  (lsci-py:method-call array 'prod))

(defun prod (array kwargs)
  (lsci-py:method-call array 'prod '() kwargs))

(defun cumprod (array)
  (lsci-py:method-call array 'cumprod))

(defun cumprod (array kwargs)
  (lsci-py:method-call array 'cumprod '() kwargs))

(defun mean (array)
  (lsci-py:method-call array 'mean))

(defun mean (array kwargs)
  (lsci-py:method-call array 'mean '() kwargs))

(defun var (array)
  (lsci-py:method-call array 'var))

(defun var (array kwargs)
  (lsci-py:method-call array 'var '() kwargs))

(defun std (array)
  (lsci-py:method-call array 'std))

(defun std (array kwargs)
  (lsci-py:method-call array 'std '() kwargs))

(defun all (array)
  (lsci-py:method-call array 'all))

(defun all (array kwargs)
  (lsci-py:method-call array 'all '() kwargs))

(defun any (array)
  (lsci-py:method-call array 'any))

(defun any (array kwargs)
  (lsci-py:method-call array 'any '() kwargs))

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
  (lsci-py:func-call 'numpy 'genfromtxt `(,(list_to_binary filename)) kwargs))
