(defmodule lsci-app
  (behaviour e2_application)
  (export (init 0)))

;;;===================================================================
;;; e2_application callbacks
;;;===================================================================

(defun init ()
  (e2_log:info "TODO: configure top-level processes for your app")
  #(ok ()))
