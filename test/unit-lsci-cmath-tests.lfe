(defmodule unit-lsci-cmath-tests
  (behaviour ltest-unit)
  (export all))

(include-lib "ltest/include/ltest-macros.lfe")

(defun constants_test_case (_)
  (list
    (lambda ()
      (is-equal 2.718281828459045 (lsci-cmath:e))
      (is-equal 3.141592653589793 (lsci-cmath:pi)))))

(defun complex_numbers_test_case (_)
  (list
    (lambda ()
      (let ((cmplx1 (lsci-cmath:complex -1.0 2.0))
            (cmplx2 (lsci-cmath:complex -1.0 -2.0)))
        (is-equal 'complex (lsci-py:type cmplx1))
        (is-equal 'complex (lsci-py:type cmplx2))
        (is-equal -1.0 (lsci-cmath:real cmplx1))
        (is-equal -1.0 (lsci-cmath:real cmplx2))
        (is-equal 2.0 (lsci-cmath:imag cmplx1))
        (is-equal -2.0 (lsci-cmath:imag cmplx2))
        (is-equal "(-1-2j)" (lsci-cmath:->str (lsci-cmath:conj cmplx1)))
        (is-equal "(-1+2j)" (lsci-cmath:->str (lsci-cmath:conj cmplx2)))))))

(defun phase_test_case (_)
  (list
    (lambda ()
      (let ((cmplx1 (lsci-cmath:complex -1.0 0.0))
            (cmplx2 (lsci-cmath:complex -1.0 -0.0)))
        (is-equal 3.141592653589793 (lsci-cmath:phase cmplx1))
        ;; XXX The following fails in Erlang due to there not being support
        ;; for negative zero
        ;;(is-equal -3.141592653589793 (lsci-cmath:phase cmplx2))
        ))))

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
      (lambda (_) (complex_numbers_test_case '()))
      (lambda (_) (phase_test_case '())))))
