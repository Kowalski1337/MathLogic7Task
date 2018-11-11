module Axioms where
  import Expression
  zero = Variable "0"

  log1 = Binary Impl (Variable "A") (Binary Impl (Variable "B") (Variable "A"))
  log2 = Binary Impl (Binary Impl (Variable "A") (Variable "B")) (Binary Impl (Binary Impl (Variable "A") (Binary Impl (Variable "B") (Variable "C")))(Binary Impl (Variable "A") (Variable "C")))
  log3 = Binary Impl (Variable "A") (Binary Impl (Variable "B") (Binary Conj (Variable "A") (Variable "B")))
  log4 = Binary Impl (Binary Conj (Variable "A") (Variable "B")) (Variable "A")
  log5 = Binary Impl (Binary Conj (Variable "A") (Variable "B")) (Variable "B")
  log6 = Binary Impl (Variable "A") (Binary Disj (Variable "A") (Variable "B"))
  log7 = Binary Impl (Variable "A") (Binary Disj (Variable "A") (Variable "B"))
  log8 = Binary Impl (Binary Impl (Variable "A") (Variable "C")) (Binary Impl (Binary Impl (Variable "B") (Variable "C")) (Binary Impl (Binary Disj (Variable "A")(Variable "B")) (Variable "C")))
  log9 = Binary Impl (Binary Impl (Variable "A") (Variable "B")) (Binary Impl (Binary Impl (Variable "A") (Negation (Variable "B"))) (Negation (Variable "A")))
  log10 = Binary Impl (Negation (Negation (Variable "A"))) (Variable "A")

  fa1 = Binary Impl (Binary Equality (Variable "a") (Variable "b")) (Binary Equality (Next (Variable "a"))(Next (Variable "b")))
  fa2 = Binary Impl (Binary Equality (Variable "a") (Variable "b")) (Binary Impl (Binary Equality (Variable "b") (Variable "c")) (Binary Equality (Variable "a") (Variable "c")))
  fa3 = Binary Impl (Binary Equality (Next (Variable "a"))(Next (Variable "b"))) (Binary Equality (Variable "a") (Variable "b"))
  fa4 = Negation (Binary Equality (Next (Variable "a")) zero)
  fa5 = Binary Equality (Binary Add (Variable "a") (Next (Variable "b"))) (Next (Binary Add (Variable "a") (Variable "b")))
  fa6 = Binary Equality (Binary Add (Variable "a") zero) (Variable "a")
  fa7 = Binary Equality (Binary Mul (Variable "a") zero) zero
  fa8 = Binary Equality (Binary Mul (Variable "a") (Next (Variable "b"))) (Binary Add (Binary Mul (Variable "a") (Variable "b")) (Variable "a"))
