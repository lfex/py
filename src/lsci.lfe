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

(defun py-attr
  ((obj attr-name) (when (is_list attr-name))
    (py-attr obj (list_to_atom attr-name)))
  ((obj attr-name) (when (is_atom attr-name))
    (let* ((pid (lsci-python:pid))
           (attr (atom_to_binary attr-name 'latin1)))
      (py 'lsci 'obj.attr `(,obj ,attr)))))

(defun py-call (obj attr-name)
  (py-method-call obj attr-name '() '()))

(defun py-call (obj attr-name args)
  (py-method-call obj attr-name args '()))

(defun py-call (obj attr-name args kwargs)
  (py-method-call obj attr-name args kwargs))

(defun py-method-call (obj attr-name args kwargs)
  (py-general-call obj attr-name args kwargs 'obj.call_method))

(defun py-func-call (module func-name)
  (py-func-call module func-name '() '()))

(defun py-func-call (module func-name args)
  (py-func-call module func-name args '()))

(defun py-func-call (module func-name args kwargs)
  (py-general-call (atom_to_binary module 'latin1)
                   func-name
                   args
                   kwargs
                   'obj.call_func))

(defun py-general-call
  ((obj attr-name args kwargs type) (when (is_list attr-name))
    (py-general-call obj (list_to_atom attr-name) args kwargs type))
  ((obj attr-name args raw-kwargs type) (when (is_atom attr-name))
    (let* ((pid (lsci-python:pid))
           (attr (atom_to_binary attr-name 'latin1))
           (kwargs (lsci-util:proplist-to-binary raw-kwargs)))
      (py 'lsci type `(,obj ,attr ,args ,kwargs)))))

(defun py-dir (obj)
  (lfe_io:format "~p~n"
                 `(,(py 'builtins 'dir `(,obj)))))

(defun py-vars (obj)
  (lfe_io:format "~p~n"
                 `(,(py 'builtins 'vars `(,obj)))))

(defun py-type (obj)
  (let* ((class (py-attr obj '__class__))
         (repr (py 'builtins 'repr `(,class))))
    (list_to_atom (cadr (string:tokens repr "'")))))

(defun py-repr
  ((`#(,opaque ,lang ,data))
    (io:format "#(~s ~s~n  #B(~ts))~n"
               `(,opaque ,lang ,data))))
