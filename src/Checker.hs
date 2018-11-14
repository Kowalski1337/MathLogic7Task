module Checker where

import           Axioms
import           Expression

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
    in if ((checkHyp union next) || checkSimpleAxioms next || (checkMP proofed next)) then (checkAll s hyp (proofed ++ [next])  (L.drop 1 need) ) else ("Доказательство не корректно начиная со строчки №" ++ show (L.length proofed + 1){- ++ show proofed-})


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

-----------------Check Hypotheses----------------

checkHyp :: [Expression] -> Expression -> Bool
checkHyp list element = elem element list


-----------------Check Axioms--------------------

checkSimpleAxioms :: Expression -> Bool
checkSimpleAxioms ch = L.any (\x -> fst $ checkEqualsStructure empty ch x) simpleAxioms

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
