(defmodule lsci-util
  (export all))

(defun get-lsci-version ()
  (lutil:get-app-src-version "src/lsci.app.src"))

(defun get-py-version ()
  (lists:map #'string:strip/1
             (string:tokens (lsci:py 'sys 'version.__str__ '())
                            "\n")))

(defun get-versions ()
  (++ (lutil:get-version)
      `(#(lsci ,(get-lsci-version))
        #(python ,(get-py-version)))))
