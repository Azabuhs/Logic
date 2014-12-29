module Logic where

import Prelude hiding ((^), (&&), (||))
import Data.List (intercalate)
data Logic = T | F | Atom String | Neg Logic | Or [Logic] | And [Logic]

instance Show Logic where
  show x = case x of
    Atom a -> a
    Neg p  -> "￢" ++ show p
    Or ps  -> intercalate " ∨ " (map show ps)
    And ps -> intercalate " ∧ " (map show ps)
    T      -> "T"
    F      -> "F"

p = Atom "p" :: Logic
q = Atom "q" :: Logic
r = Atom "r" :: Logic


(^) :: Logic -> Logic
(^) p = Neg p

(||) :: Logic -> Logic -> Logic
(||) p q = Or [p, q]

(&&) :: Logic -> Logic -> Logic
(&&) = case x of
T q = q
(&&) p T = p
(&&) F _ = F
(&&) _ F = F
(&&) p q = And [p, q]

(==>) :: Logic -> Logic -> Logic
(==>) p q = (Neg p) ||  q

(<=>) :: Logic -> Logic -> Logic
(<=>) p q = ((Neg p) || q) && (p || (Neg q))

