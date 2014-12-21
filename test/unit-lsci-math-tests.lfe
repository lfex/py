(defmodule unit-lsci-math-tests
  (behaviour ltest-unit)
  (export all))

(include-lib "ltest/include/ltest-macros.lfe")

(defun constants_test_case (_)
  (list
    (lambda ()
      (is-equal 2.718281828459045 (lsci-math:e))
      (is-equal 3.141592653589793 (lsci-math:pi)))))

(defun phi_test_case (_)
  (list
    (lambda ()
      (is-equal 0.15865525393145702 (lsci-math:phi -1))
      (is-equal 0.5 (lsci-math:phi 0))
      (is-equal 0.841344746068543 (lsci-math:phi 1))
      (is-equal 0.9772498680518207 (lsci-math:phi 2))
      (is-equal 0.9999997133484282 (lsci-math:phi 5))
      (is-equal 1.0 (lsci-math:phi 10))
      (is-equal 1.0 (lsci-math:phi 100)))))

(defun div_test_case (_)
  (list
    (lambda ()
      (is-equal 1.0 (lsci-math:div 1 1))
      (is-equal 0.5 (lsci-math:div 1 2))
      (is-equal 2.0 (lsci-math:div 2 1))
      (is-equal 0.01 (lsci-math:div 1 100)))))

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
      (lambda (_) (phi_test_case '()))
      (lambda (_) (div_test_case '()))
      (lambda (_) (rounding_test_case '())))))
