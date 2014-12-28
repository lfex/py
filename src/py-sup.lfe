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
  (lists:map #'get-child-spec/1 (py-util:get-worker-names)))

(defun get-child-spec ()
  (get-child-spec 'py))

(defun get-child-spec (child-id)
  `#(,child-id #(py start_link (,child-id))
               permanent
               ,(py-config:get-shutdown-timeout)
               worker
               (py)))

(defun add-server (sup-pid child-id)
  (supervisor:start_child sup-pid (get-child-spec child-id)))

(defun get-pid ()
  (erlang:whereis (MODULE)))

(defun get-status ()
  (let ((`#(status ,_ ,_ ,status) (sys:get_status (get-pid))))
    status))

(defun filter-state
  ((`(#(header ,_) #(data ,_) #(data (#("State" ,state)))))
    `#(true ,state))
  ((_)
    'false))

(defun get-state ()
  (car (lists:filtermap #'filter-state/1 (get-status))))

; (defun get-children ()
;   (element 4 (get-state)))

(defun get-children ()
  (supervisor:which_children (get-pid)))

(defun get-children-pids ()
  (lists:map
    (lambda (x)
      (element 2 x))
    (get-children)))

