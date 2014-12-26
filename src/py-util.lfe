(defmodule py-util
  (export all))

(defun get-py-version ()
  (lutil:get-app-src-version "src/py.app.src"))

(defun get-python-version ()
  (lists:map #'string:strip/1
             (string:tokens (py:module 'sys 'version.__str__ '())
                            "\n")))

(defun get-versions ()
  (++ (lutil:get-version)
      `(#(lfe-py ,(get-py-version))
        #(python ,(get-python-version)))))

(defun proplist->binary (proplist)
  "Convert all the keys to binary."
  (lists:map
    (match-lambda
      ((`#(,key ,value))
        `(,(atom_to_binary key 'latin1) ,value)))
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
