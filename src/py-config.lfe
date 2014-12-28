(defmodule py-config
  (export all))

(defun app-name () "" 'py)

(defun get-loaded-apps ()
  (proplists:get_keys
    (application:loaded_applications)))

(defun loaded? ()
  (lists:any
    (lambda (x) (== x (app-name)))
    (get-loaded-apps)))

(defun load-config ()
  (if (loaded?)
      'already-loaded
      (application:load (app-name))))

(defun get (key)
  (load-config)
  (let ((result (application:get_env (app-name) key)))
    (case result
      (`#(ok ,data)
        data)
      (_ result))))

(defun get-python-path ()
  (get 'python-path))

(defun get-max-restarts ()
  (get 'erlport-max-restarts))

(defun get-restart-threshold ()
  (get 'erlport-restart-threshold))

(defun get-shutdown-timeout ()
  (get 'erlport-shutdown-timeout))

(defun call-scheduler ()
  (call (get 'scheduler-mod) (get 'scheduler-func)))

(defun get-worker-count ()
  (get 'worker-count))
