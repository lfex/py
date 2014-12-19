(defmodule lsci-python
  (behaviour e2_task_supervisor)
  (export all))

(defun start ()
  (let ((`#(ok ,pid) (python:start '(#(python_path "./python")))))
    (erlang:register (lsci-config:get-server-pid-name) pid)
    'ok))

(defun stop ()
  (python:stop (pid))
  (erlang:unregister (lsci-config:get-server-pid-name))
  'ok)

(defun restart ()
  (stop)
  (start))

(defun pid ()
  (erlang:whereis (lsci-config:get-server-pid-name)))

(defun start_link ()
  (e2_task_supervisor:start_link
    (MODULE) 'mydb-client-handler '(registered)))

(defun start_handler (socket)
  (e2_task_supervisor:start_task (MODULE) `(,socket)))
