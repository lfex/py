(defmodule lsci
  (export all))

(defun start ()
  ;;(ec_application:start_with_dependencies 'lsci))
  (lsci-python:py-start)
  'ok)

(defun stop ()
  (application:stop 'lsci))

(defun py (mod func)
  (py mod func '()))

(defun py (mod func args)
  (python:call (lsci-python:py-pid) mod func args))

(defun py-const (mod func type)
  (py mod (list_to_atom (++ (atom_to_list func)
                            "."
                            "__"
                            (atom_to_list type)
                            "__"))))
