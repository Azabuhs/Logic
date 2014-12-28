module Logic(module Prelude, (^), (&&), (||)) where

import qualified Prelude as P
import Prelude hiding ((^), (&&), (||))

data Logic = Tautology | Paradox | Atom String | Neg Logic | Or Logic Logic | And Logic Logic

instance Show Logic where
  show x = case x of
    Atom a  -> a
    Neg p   -> "￢" ++ show p
    Or  p q -> "("++ show p ++ " ∨ " ++ show q ++ ")"
    And p q -> "("++ show p ++ " ∧ " ++ show q ++ ")"

p = Atom "p" :: Logic
q = Atom "q" :: Logic
r = Atom "r" :: Logic

t = Tautology :: Logic
f = Paradox   :: Logic

(^) :: Logic -> Logic
(^) p = Neg p

(||) :: Logic -> Logic -> Logic
(||) p q = p `Or` q

(&&) :: Logic -> Logic -> Logic
(&&) p q = p `And` q

(==>) :: Logic -> Logic -> Logic
(==>) p q = (Neg p) ||  q

(<=>) :: Logic -> Logic -> Logic
(<=>) p q = ((Neg p) || q) && (p || (Neg q))

main = putStrLn "hello"
