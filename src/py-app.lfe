(defmodule py-app
  (behaviour application)
  (export all))

(defun start ()
  (start 'normal '()))

(defun start (_type _args)
  (let ((result (py-sup:start_link)))
    (case result
      (`#(ok ,pid)
        result)
      (_
        `#(error ,result)))))

(defun stop ()
  (stop '()))

(defun stop (_state)
  'ok)
