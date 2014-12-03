(defmodule lsci-util
  (export all))

(defun get-lsci-version ()
  (lutil:get-app-src-version "src/lsci.app.src"))

(defun get-versions ()
  (++ (lutil:get-version)
      `(#(lsci ,(get-lsci-version)))))
