(defmodule py-sup
  (behaviour supervisor)
  (export all))

(defun start_link ()
  (let* ((python-path (py-config:get-python-path))
         (options `(#(python_path ,python-path))))
    (supervisor:start_link
      `#(local ,(MODULE))
      (MODULE)
      `(,options))))

(defun init (options)
  `#(ok #(,(get-supervisor-spec)
          (,(get-child-spec)))))

(defun get-supervisor-spec ()
  #(one_for_one 1 1))

(defun get-child-spec ()
  `#(py #(py start_link ())
        permanent 2000 worker (py)))
