module Axioms where

import Expression
zero = Named "0" []

--check =Binary Impl (Binary Impl (Named "A" []) (Binary Impl (Named "B" []) (Named "A" []))) (Binary Impl (Named "B" []) (Binary Impl (Named "A" []) (Binary Impl (Named "B" []) (Named "A" []))))
simpleAxioms :: [Expression]
simpleAxioms = [Binary Impl (Named "A" []) (Binary Impl (Named "B" []) (Named "A" [])),
                Binary Impl (Binary Impl (Named "A" []) (Named "B" [])) (Binary Impl (Binary Impl (Named "A" []) (Binary Impl (Named "B" []) (Named "C" [])))(Binary Impl (Named "A" []) (Named "C" []))),
                Binary Impl (Named "A" []) (Binary Impl (Named "B" []) (Binary Conj (Named "A" []) (Named "B" []))),
                Binary Impl (Binary Conj (Named "A" []) (Named "B" [])) (Named "A" []),
                Binary Impl (Binary Conj (Named "A" []) (Named "B" [])) (Named "B" []),
                Binary Impl (Named "A" []) (Binary Disj (Named "A" []) (Named "B" [])),
                Binary Impl (Named "A" []) (Binary Disj (Named "A" []) (Named "B" [])),
                Binary Impl (Binary Impl (Named "A" []) (Named "C" [])) (Binary Impl (Binary Impl (Named "B" []) (Named "C" [])) (Binary Impl (Binary Disj (Named "A" [])(Named "B" [])) (Named "C" []))),
                Binary Impl (Binary Impl (Named "A" []) (Named "B" [])) (Binary Impl (Binary Impl (Named "A" []) (Unary Neg (Named "B" []))) (Unary Neg (Named "A" []))),
                Binary Impl (Unary Neg (Unary Neg (Named "A" []))) (Named "A" []),
                Binary Impl (Binary Equal (Named "a" []) (Named "b" [])) (Binary Impl (Binary Equal (Named "b" []) (Named "c" [])) (Binary Equal (Named "A" []) (Named "c" []))),
                Binary Impl (Binary Equal (Named "a" []) (Named "b" [])) (Binary Equal (Unary Next (Named "a" []))(Unary Next (Named "b" []))),
                Binary Impl (Binary Equal (Unary Next (Named "a" []))(Unary Next (Named "b" []))) (Binary Equal (Named "a" []) (Named "b" [])),
                Unary Neg (Binary Equal (Unary Next (Named "a" [])) zero),
                Binary Equal (Binary Add (Named "a" []) (Unary Next (Named "b" []))) (Unary Next (Binary Add (Named "a" []) (Named "b" []))),
                Binary Equal (Binary Add (Named "a" []) zero) (Named "a" []),
                Binary Equal (Binary Mul (Named "a" []) zero) zero,
                Binary Equal (Binary Mul (Named "a" []) (Unary Next (Named "b" []))) (Binary Add (Binary Mul (Named "a" []) (Named "b" [])) (Named "a" []))]
