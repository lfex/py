(defmodule lsci
  (export all))

(defun start ()
  ;;(ec_application:start_with_dependencies 'lsci))
  (encurses:initscr)
  (lsci-py:start)
  ;; Later, we can add things like the following:
  ;;(lsci-jl:start)
  ;;(lsci-clj:start)
  'ok)

(defun stop ()
  (lsci-py:stop)
  ;; Later, we can add things like the following:
  ;;(lsci-jl:stop)
  ;;(lsci-clj:stop)
  (application:stop 'lsci))

(defun py (mod func)
  (lsci-py:raw-call mod func '()))

(defun py (mod func args)
  (lsci-py:raw-call mod func args))

(defun py (mod func args kwargs)
  (lsci-py:raw-call mod func args kwargs))
