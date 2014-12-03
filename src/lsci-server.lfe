(defmodule lsci-server
  (export (start 0)
          (stop 0)))

(defun start ()
  (ec_application:start_with_dependencies 'lsci-server))

(defun stop ()
  (application:stop 'lsci-server))
