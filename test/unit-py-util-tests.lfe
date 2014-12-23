(defmodule unit-py-util-tests
  (behaviour ltest-unit)
  (export all))

(include-lib "ltest/include/ltest-macros.lfe")

(deftest proplist->binary
  (let ((data '(#(a 1) #(b 2) #(c 3)))
        (expected '((#b(97) 1) (#b(98) 2) (#b(99) 3))))
    (is-equal
      expected
      (py-util:proplist->binary data))))
