(defmodule py-util
  (export all))

(defun get-erlport-version ()
  (lutil:get-app-version 'erlport))

(defun get-py-version ()
  (lutil:get-app-version 'py))

(defun get-python-version ()
  (lists:map #'string:strip/1
             (string:tokens (py:pycall 'sys 'version.__str__ '())
                            "\n")))

(defun get-versions ()
  (++ (lutil:get-versions)
      `(#(erlport ,(get-erlport-version))
        #(py ,(get-py-version))
        #(python ,(get-python-version)))))

(defun proplist->binary (proplist)
  "Convert all the keys to binary."
  (lists:map
    (match-lambda
      ((`#(,key ,value)) (when (is_atom key))
        `(,(atom_to_binary key 'latin1) ,value))
      ((`#(,key ,value)) (when (is_list key))
        `(,(list_to_binary key) ,value)))
    proplist))

(defun make-func
  ((`(,lfe-func-name ,func-arity) mod)
    (let ((py-func-name (kla:replace-dash lfe-func-name))
          (func-args (kla:make-args func-arity)))
      `(defun ,lfe-func-name ,func-args
        (py:pycall ',mod ',py-func-name (list ,@func-args))))))

(defun make-funcs (func-list mod)
  (lists:map
    (lambda (x)
      (make-func x mod))
    func-list))

(defun make-kwarg-func
  ((`(,lfe-func-name ,func-arity) mod)
    (let* ((py-func-name (kla:replace-dash lfe-func-name))
           (func-args (kla:make-args func-arity))
           (`#(,args (,kwargs)) (split-last func-args)))
      `(defun ,lfe-func-name ,func-args
        (py:func ',mod ',py-func-name (list ,@args) ,kwargs)))))

(defun make-kwarg-funcs (func-list mod)
  (lists:map
    (lambda (x)
      (make-kwarg-func x mod))
    func-list))

(defun split-last (all-args)
  (lists:split (- (length all-args) 1) all-args))

(defun get-worker-names ()
  (lists:map
    (lambda (x)
      (list_to_atom (++ "py-" (integer_to_list x))))
    (lists:seq 1 (py-config:get-worker-count))))

(defun split-dotted (dotted-name)
  (let* ((parts (string:tokens (atom_to_list dotted-name) "."))
         (`#(,first (,last)) (split-last parts)))
    (lists:map
      #'list_to_atom/1
      `(,(string:join first ".")
        ,last))))

(defun dotted? (name)
  (if (> (length (string:tokens (atom_to_list name) ".")) 1)
    'true
    'false))
