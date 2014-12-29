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

(defun get-pid ()
  (let ((pid (erlang:whereis (MODULE))))
    (sys:statistics pid 'true)
    pid))

(defun add-server (child-id)
  (add-server (get-pid) child-id))

(defun add-server (sup-pid child-id)
  (supervisor:start_child sup-pid (get-child-spec child-id)))

(defun get-status ()
  ;; We want to get statisitics here, too -- so use (get-pid) which checks
  ;; to make sure that the supervisor process has statisics enabled
  (sys:get_status (get-pid)))

(defun get-state ()
  (sys:get_state (MODULE)))

(defun get-children ()
  (lists:map
    (match-lambda ((`#(,name ,pid ,_ ,_))
      `#(,pid ,name)))
    (supervisor:which_children (MODULE))))

(defun get-child-pids ()
  (lists:map
    (lambda (x)
      (element 2 x))
    (get-children)))

(defun get-stats ()
  (let ((`#(ok ,stats) (sys:statistics (get-pid) 'get)))
    (++ `(#(,(MODULE) ,stats))
        (get-child-stats))))

(defun get-child-stats ()
  (lists:map
    (match-lambda ((`#(,_ ,name))
      (sys:statistics name 'true)
      (let ((`#(ok ,stats) (sys:statistics name 'get)))
        `#(,name ,stats))))
    (get-children)))

(defun get-child-stats (key)
  (lists:map
    (match-lambda ((`#(,name ,stats))
      `#(,(proplists:get_value key stats) ,name)))
    (get-child-stats)))

(defun get-child-reductions ()
  (get-child-stats 'reductions))

(defun get-child-messages-in ()
  (get-child-stats 'messages_in))

(defun get-child-start-time ()
  (get-child-stats 'start_time))
