(defmodule lsci-py
  (export all))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Application functions
;;;
(defun start ()
  (let ((`#(ok ,pid) (python:start '(#(python_path "./python")))))
    (erlang:register (lsci-config:get-server-pid-name) pid)
    (raw-call 'lsci 'init.setup)
    #(ok started)))

(defun stop ()
  (python:stop (pid))
  (erlang:unregister (lsci-config:get-server-pid-name))
  #(ok stopped))

(defun restart ()
  (stop)
  (start)
  #(ok restarted))

(defun pid ()
  (erlang:whereis (lsci-config:get-server-pid-name)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; REPL functions
;;;
(defun dir (obj)
  (lfe_io:format "~p~n"
                 `(,(raw-call 'builtins 'dir `(,obj)))))

(defun vars (obj)
  (lfe_io:format "~p~n"
                 `(,(raw-call 'builtins 'vars `(,obj)))))

(defun type (obj)
  (let* ((class (attr obj '__class__))
         (repr (raw-call 'builtins 'repr `(,class))))
    (list_to_atom (cadr (string:tokens repr "'")))))

(defun repr
  ((`#(,opaque ,lang ,data))
    (io:format "#(~s ~s~n  #B(~ts))~n"
               `(,opaque ,lang ,data))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Call functions
;;;

;; ErlPort Calls
;;
(defun raw-call (mod func)
  (raw-call mod func '()))

(defun raw-call (mod func args)
  (python:call (pid) mod func args))

;; Python object and module constants
;;
(defun const (obj type)
  (method-call obj (list_to_atom (++ "__"
                                     (atom_to_list type)
                                     "__"))))

(defun const (mod func type)
  (raw-call mod (list_to_atom (++ (atom_to_list func)
                                  "."
                                  "__"
                                  (atom_to_list type)
                                  "__"))))

;; Python object attributes
;;
(defun attr
  ((obj attr-name) (when (is_list attr-name))
    (attr obj (list_to_atom attr-name)))
  ((obj attr-name) (when (is_atom attr-name))
    (let* ((pid (pid))
           (attr (atom_to_binary attr-name 'latin1)))
      ;; Now call to the 'attr' function in the Python module 'lsci.obj'
      (raw-call 'lsci 'obj.attr `(,obj ,attr)))))

;; Python method calls
;;
(defun method-call (obj method-name)
  (method-call obj method-name '() '()))

(defun method-call (obj method-name args)
  (method-call obj method-name args '()))

(defun method-call (obj method-name args kwargs)
  (general-call obj method-name args kwargs 'obj.call_method))

;; Python module function and function object calls
;;
(defun func-call (func-name)
    (func-call func-name '() '()))

(defun func-call
  ((module func-name) (when (is_atom module))
    (func-call module func-name '() '()))
  ((func-name args) (when (is_list args))
    (func-call func-name args '())))

(defun func-call
  ((module func-name args) (when (is_atom module))
    (func-call module func-name args '()))
  ((func-name args raw-kwargs) (when (is_list args))
    (let ((kwargs (lsci-util:proplist->binary raw-kwargs)))
      ;; Now call to the 'call_callable' function in the Python
      ;; module 'lsci.obj'
      (raw-call 'lsci 'obj.call_callable `(,func-name ,args ,kwargs)))))

(defun func-call (module func-name args kwargs)
  ;; Now call to the 'call_func' function in the Python module 'lsci.obj'
  (general-call (atom_to_binary module 'latin1)
                   func-name
                   args
                   kwargs
                   'obj.call_func))

(defun general-call (obj attr-name args raw-kwargs type)
  (let* ((attr (atom_to_binary attr-name 'latin1))
         (kwargs (lsci-util:proplist->binary raw-kwargs)))
    (raw-call 'lsci type `(,obj ,attr ,args ,kwargs))))

;; Creating Python class instances
;;
(defun instantiate (module class)
  (instantiate module class '() '()))

(defun instantiate (module class args)
  (instantiate module class args '()))

(defun instantiate (module class args kwargs)
  (func-call module class args kwargs))
