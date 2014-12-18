(defmodule lsci-app
  (behaviour e2_application)
  (export (init 0)))

;;;===================================================================
;;; e2_application callbacks
;;;===================================================================

(defun init ()
  (e2_log:info "Setting up lsci ...")
    `#(ok (#(lsci-python py-start ()))))
