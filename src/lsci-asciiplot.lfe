(defmodule lsci-asciiplot
  (export all))

(defun point (x y marker)
  "Plot a point at the given coordinate."
  (encurses:mvaddstr (trunc (- x 1))
                     (trunc (- y 1))
                     marker))

(defun points (xs ys marker)
  "Plot points at coordinates constructed from given x and y values."
  (lists:zipwith
    (lambda (x y)
      (point x y marker))
    xs
    ys))

(defun finish (y-max)
  (finish 0 (- y-max 1)))

(defun finish (x y)
  (encurses:move x y)
  (encurses:refresh))

(defun get-maxs ()
  (get-maxs 0 0))

(defun get-maxs (y-buffer)
  (get-maxs 0 y-buffer))

(defun get-maxs (x-buffer y-buffer)
  (let ((`#(,x ,y) (encurses:getmaxxy)))
    `#(,(- x x-buffer) ,(- y y-buffer))))

(defun scale-data (xs ys x-max y-max)
  (scale-data xs ys 0 x-max 0 y-max))

(defun scale-data (xs ys x-min x-max y-min y-max)
  `#(,(lsci-np:scale1d xs `(#(min ,x-min) #(max ,x-max)))
     ,(lsci-np:scale1d ys `(#(min ,y-min) #(max ,y-max)))))

(defun cartesian->ncurses (ys y-max)
  (lsci-math:sub y-max ys))

;; Example usage:
;;
;; > (set data (lsci-np:genfromtxt
;;               "examples/polyfit/filip.csv"
;;               `(#(delimiter ,(list_to_binary ","))
;;                 #(names true))))
;; #($erlport.opaque python ...)
;; > (set xs (lsci-np:get data 'x))
;; #($erlport.opaque python ...)
;; > (set ys (lsci-np:get data 'y))
;; #($erlport.opaque python ...)
;; > (scatter xs ys "o")
;;
(defun scatter (xs ys)
  (scatter xs ys '()))

(defun scatter (xs ys options)
  (encurses:erase)
  (encurses:refresh)
  (let* ((marker (proplists:get_value 'marker options "o"))
         (y-top-buff (proplists:get_value 'y-top-buff options 3))
         (y-bot-buff (proplists:get_value 'y-bot-buff options 1))
         (`#(,x-max ,y-max) (get-maxs))
         (`#(,xs ,ys) (scale-data xs ys
                                  0 (- x-max 1)
                                  y-top-buff (- y-max y-top-buff)))
         (ys (cartesian->ncurses ys
                                 (+ (- y-max (+ y-bot-buff 1)) y-top-buff))))
    (points (lsci-np:->list xs)
            (lsci-np:->list ys)
            marker)
    (finish y-max)))
