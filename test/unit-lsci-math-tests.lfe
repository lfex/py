(defmodule unit-lsci-math-tests
  (behaviour ltest-unit)
  (export all))

(include-lib "ltest/include/ltest-macros.lfe")

(defun constants_test_case (_)
  (list
    (lambda ()
      (is-equal 2.718281828459045 (lsci-math:e))
      (is-equal 3.141592653589793 (lsci-math:pi)))))

(defun rounding_test_case (_)
  (list
    (lambda ()
      (is-equal 5 (lsci-math:ceil 4.2))
      (is-equal 4 (lsci-math:floor 4.2))
      (is-equal 4 (lsci-math:round 4.2))
      (is-equal 5 (lsci-math:round 4.667))
      (is-equal 4.7 (lsci-math:round 4.667 1))
      (is-equal 4.667 (lsci-math:round 4.6666667 3)))))

(defun set-up ()
  (lsci:start))

(defun tear-down (_)
  (lsci:stop))

(deftestgen test-suite
  (tuple
    'foreach
    (lambda () (set-up))
    (lambda (_) (tear-down '()))
    (list
      (lambda (_) (constants_test_case '()))
      (lambda (_) (rounding_test_case '())))))
