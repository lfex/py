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
         (`#(,mod ,func) (py-config:get-worker-on-start)))
    (call mod func child-id)
    result))

(defun start ()
  (py-logger:setup)
  (application:start 'py)
  #(ok started))

(defun stop ()
  (application:stop 'py)
  #(ok stopped))

(defun restart ()
  (stop)
  (start)
  #(ok restarted))

(defun on-startworker (proc-name)
  "Initialize the Python components, but don't use the scheduler
  to get the pid, since the supervisor hasn't finished yet."
  (python:call (erlang:whereis proc-name) 'lfe 'init.setup '()))

(defun get-sup-pid ()
  (py-sup:get-pid))

(defun get-python-pids ()
  (py-sup:get-child-pids))

(defun add-server (child-id)
  "Add another Python ErlPort server to the supervision tree."
  (py-sup:add-server child-id))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Call functions
;;;

;; ErlPort Calls
;;
(defun pycall (mod func)
  (pycall mod func '()))

(defun pycall (mod func args)
  (let ((pid (py-sched:get-next-pid)))
    ;; XXX add a lager DEBUG call here for sanity-checking schedulers
    (python:call pid mod func args)))

;; Creating Python class instances
;;
(defun init (mod-class)
  (apply #'init/2 (py-util:split-dotted mod-class)))

(defun init
  ((mod-class args) (when (is_list args))
    (apply #'init/4 (++ (py-util:split-dotted mod-class)
                        `(,args ()))))
  ((module class)
    (init module class '() '())))

(defun init
  ((mod-class args kwargs) (when (is_list args))
    (apply #'init/4 (++ (py-util:split-dotted mod-class)
                        `(,args ,kwargs))))
  ((module class args)
    (init module class args '())))

(defun init (module class args kwargs)
  (func module class args kwargs))

;; Python object and module constants
;;
(defun const (mod-attr)
  (apply #'const/2 (py-util:split-dotted mod-attr)))

(defun const
  ((mod attr-name) (when (is_atom mod))
    (let ((attr (atom_to_binary attr-name 'latin1)))
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

(defun call-dotten (func-name))

;; Python module function and function object calls
;;
(defun func
  ((func-name) (when (is_atom func-name))
    (if (py-util:dotted? func-name)
      (apply #'func/2 (py-util:split-dotted func-name))
      (func func-name '() '())))
  ((callable)
    (func callable '() '())))

(defun func
  ((func-name args) (when (andalso (is_atom func-name) (is_list args)))
    (if (py-util:dotted? func-name)
      (apply #'func/4 (++ (py-util:split-dotted func-name)
                          `(,args ())))
      (func func-name args '())))
  ((module func-name) (when (is_atom module))
    (func module func-name '() '()))
  ((callable args)
    (func callable args '())))

(defun func
  ((func-name args raw-kwargs) (when (andalso (is_atom func-name)
                                              (is_list args)))
    (if (py-util:dotted? func-name)
      (apply #'func/4 (++ (py-util:split-dotted func-name)
                          `(,args ,raw-kwargs)))
      (let ((kwargs (py-util:proplist->binary raw-kwargs)))
        ;; Now call to the 'call_callable' function in the Python
        ;; module 'lfe.obj'
        (pycall 'lfe 'obj.call_callable `(,func-name ,args ,kwargs)))))
  ((module func-name args) (when (is_atom module))
    (func module func-name args '()))
  ((callable args raw-kwargs)
    (let ((kwargs (py-util:proplist->binary raw-kwargs)))
      (pycall 'lfe 'obj.call_callable `(,callable ,args ,kwargs)))))

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
(defun dict (proplist)
  (func 'builtins 'dict '() proplist))

(defun pylist ()
  (pycall 'builtins 'list '()))

(defun pylist (data)
  (pycall 'builtins 'list `(,data)))

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
