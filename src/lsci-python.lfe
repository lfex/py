(defmodule lsci-python
  (behaviour e2_task_supervisor)
  (export all))

(defun py-start ()
  (let ((`#(ok ,pid) (python:start)))
    (erlang:register (lsci-config:get-server-pid-name) pid)))

(defun py-stop ()
  (python:stop (py-pid))
  (erlang:unregister (lsci-config:get-server-pid-name)))

(defun py-restart ()
  (py-stop)
  (py-start))

(defun py-pid ()
  (erlang:whereis (lsci-config:get-server-pid-name)))

(defun start_link ()
  (e2_task_supervisor:start_link
    (MODULE) 'mydb-client-handler '(registered)))

(defun start_handler (socket)
  (e2_task_supervisor:start_task (MODULE) `(,socket)))
