(defmodule lsci
  (export all))

(defun start ()
  ;;(ec_application:start_with_dependencies 'lsci))
  (lsci-python:start)
  'ok)

(defun stop ()
  (application:stop 'lsci))

(defun py (mod func)
  (py mod func '()))

(defun py (mod func args)
  (python:call (lsci-python:pid) mod func args))

(defun py-const (mod func type)
  (py mod (list_to_atom (++ (atom_to_list func)
                            "."
                            "__"
                            (atom_to_list type)
                            "__"))))

(defun py-attr (obj attr-name)
  (let* ((pid (lsci-python:pid))
         (attr (list_to_binary attr-name)))
    (python:call pid 'lsci 'obj.attr `(,obj ,attr))))

(defun py-call (obj attr-name)
  (py-call obj attr-name '() '()))

(defun py-call (obj attr-name args)
  (py-call obj attr-name args '()))

(defun py-call (obj attr-name args kwargs)
  (let* ((pid (lsci-python:pid))
         (attr (list_to_binary attr-name)))
    (python:call pid 'lsci 'obj.call `(,obj ,attr ,args ,kwargs))))
