module Cis194.Hw.Party where

import Data.Monoid
import Cis194.Hw.Employee

glCons :: Employee -> GuestList -> GuestList
glCons e (GL l f) = GL (e : l) (f + empFun e)

instance Monoid GuestList where
    mempty = GL [] 0
    mappend l r = GL (ll ++ rl) (ls + rs)
        where GL ll ls = l
              GL rl rs = r

getFun :: GuestList -> Fun
getFun (GL _ fun) = fun

moreFun :: GuestList -> GuestList -> GuestList
moreFun l r = if getFun l > getFun r then l else r
