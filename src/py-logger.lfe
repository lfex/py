(defmodule py-logger
  (export all))

(defun setup ()
  (application:load 'lager)
  (application:set_env
    'lager
    'handlers
    `(#(lager_console_backend ,(py-config:get-log-level))
      #(lager_file_backend (#(file "log/error.log")
                            #(level error)
                            #(size 10485760)
                            #(date "$D0")
                            #(count 5)))
      #(lager_file_backend (#(file "log/console.log")
                            #(level info)
                            #(size 10485760)
                            #(date "$D0")
                            #(count 5)))))
  (lager:start))

(defun log (level msg)
  (lager:log level '() msg))

(defun log (level format args)
  (log level (io_lib:format format args)))

(defun log-mod-func (level mod func format args)
  (log level (++ "[~s:~s] " format) (++ `(,mod ,func) args)))

(defun debug (mod func format args)
  (log-mod-func 'debug mod func format args))

(defun info (mod func format args)
  (log-mod-func 'info mod func format args))

(defun notice (mod func format args)
  (log-mod-func 'notice mod func format args))

(defun warning (mod func format args)
  (log-mod-func 'warning mod func format args))

(defun error (mod func format args)
  (log-mod-func 'error mod func format args))

(defun critical (mod func format args)
  (log-mod-func 'critical mod func format args))

(defun alert (mod func format args)
  (log-mod-func 'alert mod func format args))

(defun emergency (mod func format args)
  (log-mod-func 'emergency mod func format args))
