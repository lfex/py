;; XXX Rename this module to lsci-py
(defmodule lsci-python
  ;; XXX move supervisor stuff into its own module
  (behaviour e2_task_supervisor)
  (export all))

;; XXX move supervisor stuff into its own module
;;; Supervisor functions
(defun start_link ()
  (e2_task_supervisor:start_link
    (MODULE) 'mydb-client-handler '(registered)))

(defun start_handler (socket)
  (e2_task_supervisor:start_task (MODULE) `(,socket)))

;;; Application functions
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

;;; Library functions
;; XXX move lsci:py-* funcs here and remove 'py-' from function names
