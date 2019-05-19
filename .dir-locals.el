;;; Directory Local Variables
;;; For more information see (info "(emacs) Directory Variables")

(
  (haskell-mode . (
    (dante-repl-command-line . ("cabal" "new-repl" dante-target "--builddir=dist-newstyle/dante"))))

  ("src" (haskell-mode (dante-target . "ticket-reservation")))
  ("test" (haskell-mode (dante-target . "unit-tests")))
)
