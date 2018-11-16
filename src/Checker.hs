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
    (ax, extra1) = check1112 next
    (rules, extra2) = checkQants union next
    in if ((checkHyp union next) || checkSimpleAxioms next || (checkMP proofed next) || ax || (checkInduction next) || rules) then (checkAll s hyp (proofed ++ [next])  (L.drop 1 need) ) else ("Вывод не корректен начиная с формулы №" ++ show (L.length proofed + 1) ++ (max extra1 extra2))


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
expToFind = Quant Any "x" (Named "P" [Named "x" []])

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


-----------------Check 1112 Axiom------------------------

what :: Expression
what = Binary Impl (Quant Any "a" expr1) expr2

expr1 :: Expression
expr1 = Quant Any "b" (Quant Any "c" (Binary Impl (Binary Equal (Named "a" []) (Named "b" []))(Binary Impl (Binary Equal (Named "a" []) (Named "c" [])) (Binary Equal (Named "b" []) (Named "c" [])))))

expr2 :: Expression
expr2 = Quant Any "b" (Quant Any "c" (Binary Impl (Binary Equal (Binary Add (Named "a" []) (Named "0" [])) (Named "b" []))(Binary Impl (Binary Equal (Binary Add (Named "a" []) (Named "0" [])) (Named "c" [])) (Binary Equal
  (Named "b" []) (Named "c" [])))))

check1112 :: Expression -> (Bool, String)
check1112 (Binary Impl (Quant Any var expr1) expr2) = let
  (b, mm) = checkEqualsStructure M.empty expr2 expr1
  m = M.filterWithKey (\a b -> a/=b) mm
  in case (M.lookup (Named var []) m) of
    Just expr -> if (b && (M.size m <= 1)) then (((L.intersect (findFreeVars expr) (L.delete var (findEachScope [] var expr1))) == []), (" терм " ++ show expr ++ " не свободен для подстановки в формулу " ++ show expr1 ++ " вместо переменной " ++ var)) else (False, " 1")
    Nothing -> ((M.size m == 0), " 2")
check1112 (Binary Impl expr2 (Quant Exist var expr1)) = let
  (b, mm) = checkEqualsStructure M.empty expr2 expr1
  m = M.filterWithKey (\a b -> a/=b) mm
  in case (M.lookup (Named var []) m) of
    Just expr -> if (b && (M.size m <= 1)) then (((L.intersect (findFreeVars expr) (L.delete var (findEachScope [] var expr1))) == []), (" терм " ++ show expr ++ " не свободен для подстановки в формулу " ++ show expr1 ++ " вместо переменной " ++ var)) else (False, " 1")
    Nothing -> (M.size m == 0, " 2")
check1112 _ = (False, " 3")

-----------------Check Induction------------------------
toCheckInd :: Expression
toCheckInd = (Binary Impl (Binary Conj (e1) (Quant Any "y" (Binary Impl e2 e3))) (e2))

e1 :: Expression
e1 = Binary Equal (Binary Add (Named "x"[]) (Unary Next (Named "0"[]))) (Unary Next (Binary Add (Named "x"[])  (Named "0"[])))
e2 :: Expression
e2 = Binary Equal (Binary Add (Named "x"[]) (Unary Next (Named "y"[]))) (Unary Next (Binary Add (Named "x"[])  (Named "y"[])))
e3 :: Expression
e3 = Binary Equal (Binary Add (Named "x"[]) (Unary Next (Unary Next (Named "y"[])))) (Unary Next (Binary Add (Named "x"[])  (Unary Next (Named "y"[]))))
checkInduction :: Expression ->Bool
checkInduction (Binary Impl (Binary Conj (expr1) (Quant Any var (Binary Impl expr2 expr3))) (expr4)) = let
  (b1, mm1) = checkEqualsStructure M.empty expr1 expr2
  (b2, mm2) =  checkEqualsStructure M.empty expr3 expr2
  m1 = M.filterWithKey (\a b -> a/=b) mm1
  m2 = M.filterWithKey (\a b -> a/=b) mm2
  b = (b1 && b2 && (M.size m1 == 1) && (M.size m2 == 1) && (expr2==expr4) )
  bb = case (M.lookup (Named var []) m1) of
    Just (Named var1 []) -> (var1 == "0")
    otherwise -> False
  bbb = case (M.lookup (Named var []) m2) of
    Just (Unary Next (Named var1 [])) -> var1 == var
    otherwise -> False
  in b && bb && bbb
checkInduction _ = False

-----------------Check Quants------------------------

checkQants :: [Expression] ->Expression -> (Bool, String)
checkQants list (Binary Impl expr1 (Quant Any var expr2)) = if  (L.elem (Binary Impl expr1 expr2) list) then (L.notElem var (findFreeVars expr1), " переменная " ++ var ++ " входит свободно в формулу " ++ show expr1) else (False, "")
checkQants list (Binary Impl (Quant Exist var expr2) expr1) = if  (L.elem (Binary Impl expr2 expr1) list) then (L.notElem var (findFreeVars expr1), " переменная " ++ var ++ " входит свободно в формулу " ++ show expr1) else (False, "")
checkQants _ _ = (False, "")
