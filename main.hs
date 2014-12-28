module Main(module Prelude, (^), (&&), (||)) where

import qualified Prelude as P
import Prelude hiding ((^), (&&), (||))

data Logic = Atom String | Neg Logic | Or Logic Logic | And Logic Logic
instance Show Logic where
  show x = case x of
    Atom a  -> show a
    Neg p   -> "￢" ++ show p
    Or  p q -> show p ++ "∨" ++ show q
    And p q -> show p ++ "∧" ++ show q 
  

(^) :: Logic -> Logic
(^) p = Neg p

(||) :: Logic -> Logic -> Logic
(||) p q = p `Or` q

(&&) :: Logic -> Logic -> Logic
(&&) p q = p `And` q

(==>) :: Logic -> Logic -> Logic
(==>) p q = (Neg p) `Or`  q

(<=>) :: Logic -> Logic -> Logic
(<=>) p q = ((Neg p) `Or` q) `And` (p `Or` (Neg q))
