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

(defun get-supervisor-spec ()
  `#(one_for_one ,(py-config:get-max-restarts)
                 ,(py-config:get-restart-threshold)))

(defun get-children-specs ()
  `(,(get-child-spec)))

(defun get-child-spec ()
  `#(py #(py start_link ())
        permanent
        ,(py-config:get-shutdown-timeout)
        worker
        (py)))
