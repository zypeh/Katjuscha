{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE PatternSynonyms #-}

module Flatparse where

import GHC.Exts
import System.IO.Unsafe

import qualified Data.ByteString as B
import qualified Data.ByteString.Unsafe as B

data Error e = Default | Custom e
  deriving Show

type Offset = Addr#
type Result# e a = (# (# a, Offset, Int# #) | (# Error e, Offset #) #)

pattern OK# :: a -> Offset -> Int# -> Result# e a
pattern OK# a s j = (# (# a, s, j #) | #)

pattern Err# :: Error e -> Offset -> Result# e a
pattern Err# e s = (# | (# e, s #) #)
{-# complete OK#, Err# #-}

newtype Parser e a = Parser
  { runParser# :: Addr# -> Int# -> Addr# -> Int# -> Result# e a
  }

-- | Return error with @Custom@ e
err :: e -> Parser e a
err e = Parser \_ i s j -> Err# (Custom e) s

-- | Return error with no error message
err_ :: Parser e a
err_ = Parser \_ _ s _ -> Err# Default s

-- | Accepts a parser and silent the error message
err_p :: Parser e a -> Parser e a
err_p pa = Parser \e i s j -> case runParser# pa e i s j of
  Err# _ s -> Err# Default s
  x -> x

instance Functor (Parser e) where
  fmap f (Parser g) = Parser \e i s j -> case g e i s j of
    OK# a s j -> OK# a' s j where !a' = f a
    Err# e s -> Err# e s

instance Applicative (Parser e) where
  pure a = Parser \_ _ s j -> OK# a s j

  (<*>) (Parser ff) (Parser fa) = Parser \e i s j -> case ff e i s j of
    Err# e s -> Err# e s
    OK# f s j -> case fa e i s j of
      Err# e s -> Err# e s
      OK# a s j -> OK# (f a) s j

instance Monad (Parser e) where
  return = pure

  (>>=) (Parser f) p = Parser \e i s j -> case f e i s j of
    Err# e s -> Err# e s
    OK# a s j -> runParser# (p a) e i s j

  (>>) (Parser f) (Parser g) = Parser \e i s j -> case f e i s j of
    Err# e s -> Err# e s
    OK# a s j -> g e i s j
   
-- | Choose between two parsers.
-- | The second parser is tried if the first one fails, regardless of how much
-- | the first parser consumed.
infixr 6 <!>
(<!>) :: Parser e a -> Parser e a -> Parser e a
(<!>) (Parser f) (Parser g) = Parser \e i s j ->
  case f e i s j of
    Err# _ _ -> g e i s j -- failed, then goes to second parser
    x -> x

-- | Choose between two parsers.
-- | The second parser is only tried if the first one fails without having consumed input.
infixr 6 <|>
(<|>) :: Parser e a -> Parser e a -> Parser e a
(<|>) (Parser f) (Parser g) = Parser \e i offset j ->
  case f e i offset j of
    Err# err offset' -> case eqAddr# offset offset' of
                          1# -> g e i offset j
                          _ -> Err# err offset'
    x -> x

derefChar8# :: Addr# -> Char#
derefChar8# addr = indexCharOffAddr# addr 0#
 
anyAsciiChar :: Parser e Char
anyAsciiChar = Parser \e i s j -> case eqAddr# e s of
  1# -> Err# Default s
  _ -> case derefChar8# s of
    c1 -> case c1 `leChar#` '\x7F'# of -- maximum ascii code is 0x7F
      1# -> OK# (C# c1) (plusAddr# s 1#) j -- and ASCII is 1 byte, increment 1
      _  -> Err# Default s

satisfyAscii :: (Char -> Bool) -> Parser e Char
satisfyAscii predicate = Parser \e i s j -> case runParser# anyAsciiChar e i s j of
  OK# c s j | predicate c -> OK# c s j
  OK# c _ _ -> Err# Default s
  Err# e s  -> Err# e s

-- | Skip a parser zero or more times. This fails if the given parser fails with having consumed
-- | input.
many_ :: Parser e a -> Parser e ()
many_ (Parser f) = go where
  go = Parser \e i s j -> case f e i s j of
    Err# e s' -> case eqAddr# s s' of
      1# -> OK# () s j
      _ -> Err# e s'
    OK# a s j -> runParser# go e i s j

data Pos = Pos Addr#

instance Eq Pos where
  (==) (Pos p1) (Pos p2) = isTrue# (eqAddr# p1 p2)

data Span = Span !Pos !Pos deriving Eq

spanned :: Parser e a -> Parser e (a, Span)
spanned (Parser f) = Parser \e i s j -> case f e i s j of
  Err# e s -> Err# e s
  OK# a s' j' -> OK# (a, Span (Pos s) (Pos s')) s' j'

data Result e a
  = OK a B.ByteString
  | Err (Error e) B.ByteString -- remaining string
  deriving Show

runParser :: Parser e a -> B.ByteString -> Result e a
runParser (Parser f) b = unsafeDupablePerformIO do
  B.unsafeUseAsCString b \(Ptr buf) -> do
    let !(I# len) = B.length b
    let end = plusAddr# buf len

    case f end 0# buf 0# of
      Err# e s -> do
        let offset = minusAddr# s buf
        pure (Err e (B.drop (I# offset) b))

      OK# a s j -> do
        let offset = minusAddr# s buf
        pure (OK a (B.drop (I# offset) b))

testParser :: Parser e a -> String -> Result e a
testParser = undefined

someFunc :: String
someFunc = "aaa"
