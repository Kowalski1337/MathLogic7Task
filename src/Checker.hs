module Checker where

import           Axioms
import           Expression

{-import           Data.Set
import qualified Data.Set as S-}
import           Data.List
import qualified Data.List as L
import           Data.Map
import qualified Data.Map as M

check :: (([Expression], Expression), [Expression]) -> String
check ((hyp, smth), proof) = checkAll "" hyp [] proof

checkAll :: String -> [Expression] -> [Expression] -> [Expression] -> String
checkAll s hyp proofed [] = "Доказательство корректно"
checkAll s hyp proofed need =
  let
    next = L.head need
    union = hyp ++ proofed
    (ax11, extra11) = check11 next
    in if ((checkHyp union next) || checkSimpleAxioms next || (checkMP proofed next) || ax11) then (checkAll s hyp (proofed ++ [next])  (L.drop 1 need) ) else ("Вывод не корректен начиная со формулы №" ++ show (L.length proofed + 1) ++ extra11)


checkEqualsStructure :: Map Expression Expression -> Expression -> Expression -> (Bool, Map Expression Expression)
checkEqualsStructure mp expected vvar@(Named var []) = if (var == "0") then ((expected == vvar), mp) else case (M.lookup vvar mp) of
  Nothing     -> (True, M.insert (Named var []) expected mp)
  Just actual -> (actual == expected, mp)
checkEqualsStructure mp (Binary op1 f1 s1) (Binary op2 f2 s2) = let
  (b1, m1) = checkEqualsStructure mp f1 f2
  (b2, m2) = checkEqualsStructure m1 s1 s2
  in ((op1 == op2 && b1 && b2), m2)
checkEqualsStructure mp (Unary op1 expr1) (Unary op2 expr2) = let
  (b, m) = checkEqualsStructure mp expr1 expr2
  in ((op1 == op2 && b), m)
checkEqualsStructure mp (Named var1 list1) (Named var2 list2) = let
  (b, m) = checkEquals mp list1 list2
  in ((var1 == var2) && b,m)
    where
      checkEquals :: Map Expression Expression -> [Expression] -> [Expression] -> (Bool, Map Expression Expression)
      checkEquals mp [] [] = (True, mp)
      checkEquals mp _ []  = (False, mp)
      checkEquals mp [] _  = (False, mp)
      checkEquals mp f s = let
        (b, m)   = checkEqualsStructure mp (L.head f) (L.head s)
        (ch, mm) = checkEquals m (L.drop 1 f) (L.drop 1 s)
        in (b && ch, mm)
checkEqualsStructure mp (Quant type1 q1 expr1) (Quant type2 q2 expr2) = let
  (b, m) = checkEqualsStructure mp expr1 expr2
  in ((type1 == type2) && (q1 == q2) && b, m)
checkEqualsStructure mp _ _ = (False, mp)

expToFind :: Expression
expToFind = Binary Conj (Binary Impl (Named "X3" []) (Named "Y2" [])) (Binary Disj (Binary Equal (Named "x" []) (Named "0" [])) (Quant Any "x1" (Quant Exist "b0"(Binary Equal (Binary Add (Named "x1" []) (Named "b" [])) (Named "y"[]) ))))

findFreeVars :: Expression -> [String]
findFreeVars (Named var []) = if ((var !! 0) >= 'a' && (var !! 0) <= 'z') then [var] else []
findFreeVars (Named var list) =  L.nub (L.concatMap findFreeVars list)
findFreeVars (Binary _ l r) = L.union (findFreeVars l) (findFreeVars r)
findFreeVars (Quant _ s exp) = L.delete s (findFreeVars exp)
findFreeVars (Unary _ exp) = findFreeVars exp

findEachScope :: [String] -> String -> Expression -> [String]
findEachScope found s (Named var []) = if (var == s) then found else []
findEachScope found s (Named var list) = (L.concatMap (findEachScope found s) list)
findEachScope found s (Binary _ l r) = L.union (findEachScope found s l) (findEachScope found s r)
findEachScope found s (Quant _ ss exp) = findEachScope (found ++ [ss]) s exp
findEachScope found s (Unary _ exp) = findEachScope found s exp

-----------------Check Hypotheses----------------

checkHyp :: [Expression] -> Expression -> Bool
checkHyp list element = elem element list


-----------------Check Axioms--------------------

checkSimpleAxioms :: Expression -> Bool
checkSimpleAxioms ch = L.any (\x -> fst $ checkEqualsStructure M.empty ch x) simpleAxioms

-----------------Check MP------------------------

checkMP :: [Expression] -> Expression -> Bool
checkMP exprs expr = not ([] == L.intersect exprs (L.map myMap (L.filter (myFilter expr) exprs)))
  where
    myFilter :: Expression -> Expression -> Bool
    myFilter arg (Binary Impl f s) = arg == s
    myFilter _ _ = False

    myMap :: Expression -> Expression
    myMap (Binary Impl f s) = f
    myMap a = a


-----------------Check 11 Axiom------------------------

what :: Expression
what = Binary Impl (Quant Any "x" (Quant Exist "b" (Binary Equal (Named "x" []) (Named "0" [])))) ((Quant Exist "b" (Binary Equal (Named "b" []) (Named "0" []))))

expr1 :: Expression
expr1 = Quant Exist "b" (Binary Equal (Named "x" []) (Named "b" []))

expr2 :: Expression
expr2 = Quant Exist "b" (Binary Equal (Named "b" []) (Named "b" []))

check11 :: Expression -> (Bool, String)
check11 (Binary Impl (Quant Any var expr1) expr2) = let
  (b, m) = checkEqualsStructure M.empty expr2 expr1
  in case (M.lookup (Named var []) m) of
    Just expr -> if (b && (M.size m == 1)) then (((L.intersect (findFreeVars expr) (findEachScope [] var expr1)) == []), (" терм " ++ show expr ++ " не свободен для подстановки для подстановки в формулу " ++ show expr1 ++ " вместо переменной " ++ var)) else (False, "")
    Nothing -> (False, "")
check11 _ = (False, "")
