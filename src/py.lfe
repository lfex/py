(defmodule py
  (export all))

(include-lib "py/include/builtins.lfe")
(include-lib "py/include/operators.lfe")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Python server functions
;;;
(defun start_link ()
  (start_link '(py)))

(defun start_link (child-id)
  (let* ((python-path (py-config:get-python-path))
         (options `(#(python_path ,python-path)))
         (result (python:start_link `#(local ,child-id) options))
         (first-worker (car (py-util:get-worker-names))))
    ;; Initialize the Python components, but don't use the scheduler
    ;; tp get the pid, since the supervisor hasn't finished yet.
    (python:call (erlang:whereis first-worker) 'lfe 'init.setup '())
    result))

(defun start ()
  (application:start 'py)
  #(ok started))

(defun stop ()
  (application:stop 'py)
  #(ok stopped))

(defun restart ()
  (stop)
  (start)
  #(ok restarted))

(defun get-sup-pid ()
  (py-sup:get-pid))

(defun get-python-pids ()
  (py-sup:get-children-pids))

(defun add-server (child-id)
  "Add another Python ErlPort server to the supervision tree."
  (py-sup:add-server (get-sup-pid) child-id))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Call functions
;;;

;; ErlPort Calls
;;
(defun pycall (mod func)
  (pycall mod func '()))

(defun pycall (mod func args)
  (python:call (py-sched:get-next-pid) mod func args))

;; Creating Python class instances
;;
(defun init (module class)
  (init module class '() '()))

(defun init (module class args)
  (init module class args '()))

(defun init (module class args kwargs)
  (func module class args kwargs))

;; Python object and module constants
;;
(defun const
  ((mod attr-name) (when (is_atom mod))
    (let* ((pid (py-sched:get-next-pid))
           (attr (atom_to_binary attr-name 'latin1)))
      ;; Now call to the 'const' function in the Python module 'lfe.obj'
      (pycall 'lfe 'obj.const `(,mod ,attr))))
  ((obj type)
    (method obj (list_to_atom (++ "__"
                                  (atom_to_list type)
                                  "__")))))


(defun const (mod func type)
  (pycall mod (list_to_atom (++ (atom_to_list func)
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
    (let* ((pid (py-sched:get-next-pid))
           (attr (atom_to_binary attr-name 'latin1)))
      ;; Now call to the 'attr' function in the Python module 'lfe.obj'
      (pycall 'lfe 'obj.attr `(,obj ,attr)))))

;; Python method calls
;;
(defun method (obj method-name)
  (method obj method-name '() '()))

(defun method (obj method-name args)
  (method obj method-name args '()))

(defun method (obj method-name args kwargs)
  (general-call obj method-name args kwargs 'obj.call_method))

;; Python module function and function object calls
;;
(defun func (func-name)
    (func func-name '() '()))

(defun func
  ((module func-name) (when (is_atom module))
    (func module func-name '() '()))
  ((func-name args) (when (is_list args))
    (func func-name args '())))

(defun func
  ((module func-name args) (when (is_atom module))
    (func module func-name args '()))
  ((func-name args raw-kwargs) (when (is_list args))
    (let ((kwargs (py-util:proplist->binary raw-kwargs)))
      ;; Now call to the 'call_callable' function in the Python
      ;; module 'lfe.obj'
      (pycall 'lfe 'obj.call_callable `(,func-name ,args ,kwargs)))))

(defun func (module func-name args kwargs)
  ;; Now call to the 'call_func' function in the Python module 'lfe.obj'
  (general-call (atom_to_binary module 'latin1)
                   func-name
                   args
                   kwargs
                   'obj.call_func))

(defun general-call (obj attr-name args raw-kwargs type)
  (let* ((attr (atom_to_binary attr-name 'latin1))
         (kwargs (py-util:proplist->binary raw-kwargs)))
    (pycall 'lfe type `(,obj ,attr ,args ,kwargs))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Wrappers for Builtins
;;;
(defun compile (source filename mode kwargs)
  (func 'builtins 'compile `(,source ,filename ,mode) kwargs))

(defun dict (proplist)
  (func 'builtins 'dict '() proplist))

(defun int (integer kwargs)
  (func 'builtins 'int `(,integer) kwargs))

(defun open (file kwargs)
  (func 'builtins 'open `(,file) kwargs))

(defun print (objects kwargs)
  (func 'builtins 'open `(,objects) kwargs))

(defun property (kwargs)
  (func 'builtins 'property '() kwargs))

(defun pylist ()
  (pycall 'builtins 'list '()))

(defun pylist (data)
  (pycall 'builtins 'list `(,data)))

(defun str (object kwargs)
  (func 'builtins 'str `(,object) kwargs))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Convenience Functions
;;;
(defun pdir (obj)
  (lfe_io:format "~p~n"
                 `(,(pycall 'builtins 'dir `(,obj)))))

(defun pvars (obj)
  (lfe_io:format "~p~n"
                 `(,(pycall 'builtins 'vars `(,obj)))))

(defun ptype (obj)
  (let* ((class (attr obj '__class__))
         (repr (pycall 'builtins 'repr `(,class))))
    (list_to_atom (cadr (string:tokens repr "'")))))

(defun prepr
  ((`#(,opaque ,lang ,data))
    (io:format "#(~s ~s~n  #B(~ts))~n"
               `(,opaque ,lang ,data))))
