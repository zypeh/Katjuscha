{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE PatternSynonyms #-}

module Flatparse where

import GHC.Exts

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
err e = Parser \_ _ s _ -> Err# (Custom e) s

-- | Return error with no error message
err_ :: Parser e a
err_ = Parser \_ _ s _ -> Err# Default s

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

someFunc :: String
someFunc = "aaa"
