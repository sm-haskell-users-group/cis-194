-- Applicative parser for infix arithmetic expressions without any
-- dependency on hackage. Builds an explicit representation of the
-- syntax tree to fold over using client-supplied semantics.
module Parser (parseRing) where
import           Control.Applicative
import           Control.Arrow
import           Data.Char
import           Data.List           (foldl')
import           Data.Maybe
import           Data.Monoid
import           Ring

-- Building block of a computation with some state of type @s@
-- threaded through it, possibly resulting in a value of type @r@
-- along with some updated state.
newtype State s r = State (s -> Maybe (r, s))

instance Functor (State s) where
    fmap f (State g) = State $ fmap (first f) . g

instance Applicative (State s) where
    pure x = State $ \s -> Just (x, s)
    State f <*> State g = State $ \s ->
                          case f s of
                            Nothing -> Nothing
                            Just (r, s') -> fmap (first r) . g $ s'

instance Alternative (State s) where
    empty = State $ const Nothing
    State f <|> State g = State $ \s -> maybe (g s) Just (f s)

-- A parser threads some 'String' state through a computation that
-- produces some value of type @a@.
type Parser a = State String a

-- Parse a single white space character.
space :: Parser ()
space = State $ parseSpace
    where parseSpace [] = Nothing
          parseSpace (c:cs)
              | isSpace c = Just ((), cs)
              | otherwise = Nothing

-- Consume zero or more white space characters.
eatSpace :: Parser ()
eatSpace = const () <$> many space

-- Parse a specific character.
char :: Char -> Parser Char
char c = State parseChar
    where parseChar [] = Nothing
          parseChar (x:xs) | x == c = Just (c, xs)
                           | otherwise = Nothing

-- Succeed only if the end of the input has been reached.
eof :: Parser ()
eof = State parseEof
    where parseEof [] = Just ((),[])
          parseEof _  = Nothing

don'tParse :: Parser a
don'tParse = State (const Nothing)

-- Parse an infix arithmetic expression consisting of literals, plus
-- signs, multiplication signs, and parentheses.
parseExpr, parseTerm, parseAtom :: (Ring a, Parsable a) => Parser a
parseExpr = (buildOp <$> parseTerm <*> optional ((,) <$> op '+' <*> parseExpr)) <* eatSpace

parseTerm = buildOp <$> parseAtom <*> optional ((,) <$> op '*' <*> parseTerm)

parseAtom = eatSpace *> (parens parseExpr <|> parseLit)

parseLit :: Parsable a => Parser a
parseLit = State parse

buildOp x Nothing        = x
buildOp x (Just (op, y)) = x `op` y

-- Parse one of our two supported operator symbols.
op :: Ring a => Char -> Parser (a -> a -> a)
op '+' = eatSpace *> (const add <$> char '+')
op '*' = eatSpace *> (const mul <$> char '*')
op _   = don'tParse

-- Parse something enclosed in parentheses
parens :: Parser a -> Parser a
parens p = eatSpace *> char '(' *> p <* eatSpace <* char ')'

-- Run a parser over a 'String' returning the parsed value and the
-- remaining 'String' data.
execParser :: Parser a -> String -> Maybe (a, String)
execParser (State f) = f

-- Run a parser over a 'String' returning the parsed value.
evalParser :: Parser a -> String -> Maybe a
evalParser = (fmap fst .) . execParser

-- Parse an arithmetic expression using the supplied semantics for
-- integral constants, addition, and multiplication.
parseRing :: (Ring a, Parsable a) => String -> Maybe a
parseRing = evalParser (parseExpr <* eof)
