(defmodule py-sup
  (behaviour supervisor)
  (export all))

(defun start_link ()
  (supervisor:start_link
    `#(local ,(MODULE))
    (MODULE)
    '()))

(defun init (_)
  `#(ok #(,(get-supervisor-spec)
          ,(get-children-specs))))

;; XXX move restart numbers to config
(defun get-supervisor-spec ()
  '#(one_for_one 3 1))

(defun get-children-specs ()
  `(,(get-child-spec)))

;; XXX move timeout to config
(defun get-child-spec ()
  '#(py #(py start_link ())
        permanent
        2000
        worker
        (py)))
