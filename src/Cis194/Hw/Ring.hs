-- Define a class of mathematical rings
-- See also http://en.wikipedia.org/wiki/Ring_(mathematics)

module Ring where

 -- You can optionally include a list of symbols after an import statement
 -- to say exactly what you're importing from the other module. This is sometimes
 -- useful for documentation, and to avoid name clashes between modules.
import Control.Arrow ( first )
import Data.Maybe    ( listToMaybe )

class Ring a where
  addId  :: a            -- additive identity
  addInv :: a -> a       -- additive inverse
  mulId  :: a            -- multiplicative identity

  add :: a -> a -> a     -- addition
  mul :: a -> a -> a     -- multiplication

-- Something is parsable if there is a way to read it in from a string
class Parsable a where
  -- | If successful, 'parse' returns the thing parsed along with the
  -- "leftover" string, containing all of the input string except what
  -- was parsed
  parse :: String -> Maybe (a, String)

-- The canonical instance for integers:
instance Ring Integer where
  addId  = 0
  addInv = negate
  mulId  = 1

  add = (+)
  mul = (*)

instance Parsable Integer where
  parse = listToMaybe . reads
    -- Look up these functions online. Think about why this combination works.
    -- (It should work for any member of the `Read` class. Like `Integer`.)

-- | A datatype for storing and manipulating ring expressions.
data RingExpr a = Lit a
                | AddId
                | AddInv (RingExpr a)
                | MulId
                | Add (RingExpr a) (RingExpr a)
                | Mul (RingExpr a) (RingExpr a)
  deriving (Show, Eq)

instance Ring (RingExpr a) where
  addId  = AddId
  addInv = AddInv
  mulId  = MulId

  add = Add
  mul = Mul

instance Parsable a => Parsable (RingExpr a) where
  parse = fmap (first Lit) . parse

-- | Evaluate a 'RingExpr a' using the ring algebra of 'a'.
eval :: Ring a => RingExpr a -> a
eval (Lit a)    = a
eval AddId      = addId
eval (AddInv x) = addInv (eval x)
eval MulId      = mulId
eval (Add x y)  = add (eval x) (eval y)
eval (Mul x y)  = mul (eval x) (eval y)
