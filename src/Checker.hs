module Checker where

import           Axioms
import           Expression
import           Data.List
import qualified Data.List as L
import           Data.Map
import qualified Data.Map  as M
import           Debug.Trace

check :: (([Expression], Expression), [Expression]) -> String
check ((hyp, smth), proof) = checkAll hyp [] proof

checkAll :: [Expression] -> [Expression] -> [Expression] -> String
checkAll hyp proofed [] = "Доказательство корректно"
checkAll hyp proofed need =
  let
    next             = L.head need
    union            = hyp ++ proofed
    (a11, extra1)    = check11 next
    (a12, extra2)    = check12 next
    (anny, extra3)   = checkAny proofed next
    (exxist, extra4) = checkExist proofed next
  in
    if ((checkHyp union next) || checkSimpleAxioms next || (checkMP proofed next) || a11 || a12 || anny || exxist || (checkInduction next))
    then (checkAll hyp (proofed ++ [next])  (L.drop 1 need) )
    else ("Вывод не корректен начиная с формулы №" ++ show (L.length proofed + 1) ++ (max (max extra1 extra2) (max extra3 extra4)))


eqStr :: Map Expression Expression -> Expression -> Expression -> (Bool, Map Expression Expression)
eqStr mp expected vvar@(Named var []) =
  if (var == "0")
  then ((expected == vvar), mp)
  else
    case (M.lookup vvar mp) of
      Nothing     -> (True, M.insert (Named var []) expected mp)
      Just actual -> (actual == expected, mp)
eqStr mp (Binary op1 f1 s1) (Binary op2 f2 s2) =
  let
    (b1, m1) = eqStr mp f1 f2
    (b2, m2) = eqStr m1 s1 s2
  in
    ((op1 == op2 && b1 && b2), m2)
eqStr mp (Unary op1 expr1) (Unary op2 expr2) =
  let
    (b, m) = eqStr mp expr1 expr2
  in
    ((op1 == op2 && b), m)
eqStr mp (Named var1 list1) (Named var2 list2) =
  let
    (b, m) = checkEquals mp list1 list2
  in
    ((var1 == var2) && b,m)
  where
    checkEquals :: Map Expression Expression -> [Expression] -> [Expression] -> (Bool, Map Expression Expression)
    checkEquals mp [] [] = (True, mp)
    checkEquals mp _ []  = (False, mp)
    checkEquals mp [] _  = (False, mp)
    checkEquals mp f s =
      let
        (b, m)   = eqStr mp (L.head f) (L.head s)
        (ch, mm) = checkEquals m (L.drop 1 f) (L.drop 1 s)
      in
        (b && ch, mm)
eqStr mp (Quant type1 q1 expr1) (Quant type2 q2 expr2) =
  let
    (b, m) = eqStr mp expr1 expr2
  in
    ((type1 == type2) && (q1 == q2) && b, m)
eqStr mp _ _ = (False, mp)

findFreeVars :: Expression -> [String]
findFreeVars (Named var []) =
  if ((var !! 0) >= 'a' && (var !! 0) <= 'z')
  then [var] else []
findFreeVars (Named var list) = L.nub (L.concatMap findFreeVars list)
findFreeVars (Binary _ l r)   = L.union (findFreeVars l) (findFreeVars r)
findFreeVars (Quant _ s exp)  = L.delete s (findFreeVars exp)
findFreeVars (Unary _ exp)    = findFreeVars exp

findEachScope :: [String] -> String -> Expression -> [String]
findEachScope found s (Named var [])   = if (var == s) then found else []
findEachScope found s (Named var list) = (L.concatMap (findEachScope found s) list)
findEachScope found s (Binary _ l r)   = L.union (findEachScope found s l) (findEachScope found s r)
findEachScope found s (Quant _ ss exp) = findEachScope (found ++ [ss]) s exp
findEachScope found s (Unary _ exp)    = findEachScope found s exp

-----------------Check Hypotheses----------------

checkHyp :: [Expression] -> Expression -> Bool
checkHyp list element = elem element list


-----------------Check Axioms--------------------

checkSimpleAxioms :: Expression -> Bool
checkSimpleAxioms ch = L.any (\x -> fst $ eqStr M.empty ch x) simpleAxioms

-----------------Check MP------------------------

checkMP :: [Expression] -> Expression -> Bool
checkMP exprs expr = not ([] == L.intersect exprs (L.map myMap (L.filter (myFilter expr) exprs)))
  where
    myFilter :: Expression -> Expression -> Bool
    myFilter arg (Binary Impl f s) = arg == s
    myFilter _ _                   = False

    myMap :: Expression -> Expression
    myMap (Binary Impl f s) = f
    myMap a = a


-----------------Check 1112 Axiom------------------------

check11 :: Expression -> (Bool, String)
check11 (Binary Impl (Quant Any var expr1) expr2) =
  let
    (b, mm) = eqStr M.empty expr2 expr1
    m       = M.filterWithKey (\a b -> a/=b) mm
  in case (M.lookup (Named var []) m) of
    Just expr ->
      if (b && (M.size m == 1))
      then (((L.intersect (findFreeVars expr) (L.delete var (findEachScope [] var expr1))) == []), (" терм " ++ show expr ++ " не свободен для подстановки в формулу " ++ show expr1 ++ " вместо переменной " ++ var))
      else (False, " 1")
    Nothing -> ((b && M.size m == 0), " 2")
check11 _ = (False, " 3")

check12 :: Expression -> (Bool, String)
check12 (Binary Impl expr2 (Quant Exist var expr1)) =
  let
    (b, mm) = eqStr M.empty expr2 expr1
    m       = M.filterWithKey (\a b -> a/=b) mm
  in case (M.lookup (Named var []) m) of
    Just expr ->
      if (b && (M.size m == 1))
      then (((L.intersect (findFreeVars expr) (L.delete var (findEachScope [] var expr1))) == []), (" терм " ++ show expr ++ " не свободен для подстановки в формулу " ++ show expr1 ++ " вместо переменной " ++ var))
      else (False, " 1")
    Nothing -> ((b && M.size m == 0), " 2")
check12 _ = (False, " 3")

-----------------Check Induction------------------------

checkInduction :: Expression ->Bool
checkInduction (Binary Impl (Binary Conj (expr1) (Quant Any var (Binary Impl expr2 expr3))) (expr4)) =
  let
    (b1, mm1) = eqStr M.empty expr1 expr2
    (b2, mm2) =  eqStr M.empty expr3 expr2
    m1        = M.filterWithKey (\a b -> a/=b) mm1
    m2        = M.filterWithKey (\a b -> a/=b) mm2
    b         = (b1 && b2 && (M.size m1 == 1) && (M.size m2 == 1) && (expr2==expr4) )
    bb        = case (M.lookup (Named var []) m1) of
                  Just (Named var1 []) -> (var1 == "0")
                  Nothing              -> False
    bbb       = case (M.lookup (Named var []) m2) of
                  Just (Unary Next (Named var1 [])) -> var1 == var
                  Nothing                           -> False
  in b && bb && bbb
checkInduction _ = False

-----------------Check Quants------------------------


checkAny :: [Expression] ->Expression -> (Bool, String)
checkAny list (Binary Impl expr1 (Quant Any var expr2)) =
  if (L.elem (Binary Impl expr1 expr2) list)
  then (L.notElem var (findFreeVars expr1), " переменная " ++ var ++ " входит свободно в формулу " ++ show expr1)
  else (False, "")
checkAny _ _ = (False, "")

checkExist :: [Expression]->Expression->(Bool, String)
checkExist list (Binary Impl (Quant Exist var expr2) expr1) =
  if  (L.elem (Binary Impl expr2 expr1) list)
  then (L.notElem var (findFreeVars expr1), " переменная " ++ var ++ " входит свободно в формулу " ++ show expr1)
  else (False, "")
checkExist _ _ = (False, "")
