module Checker where

import           Axioms
import           Expression

import           Data.List
import qualified Data.List as L
import           Data.Map
import qualified Data.Map as M

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
checkEqualsStructure mp _ _ = (False, mp)

-----------------Check Hypotheded----------------

checkHyp :: [Expression] -> Expression -> Bool
checkHyp list element = elem element list


-----------------Check Axioms--------------------

checkSimpleAxioms :: Expression -> Bool
checkSimpleAxioms ch = L.any (\x -> fst $ checkEqualsStructure empty ch x) simpleAxioms
