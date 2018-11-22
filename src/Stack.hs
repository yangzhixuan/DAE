{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Stack where

import Prelude hiding (lookup)
import Control.Monad.Freer
import Control.Monad.Freer.Internal
import qualified Data.List as DL


data Stack e s where
  Push :: !e -> Stack e ()
  Pop  :: Stack e e
  IsEmpty :: Stack e Bool

push :: Member (Stack e) r => e -> Eff r ()
push = send . Push

pop :: Member (Stack e) r => Eff r e
pop = send Pop

-- isEmpty :: Member (Stack e) r => Eff r Bool
-- isEmpty = send IsEmpty



-- A pure handler
runStackPure :: Eq e => [e] -> Eff (Stack e ': r) a -> Eff r (a, [e])
runStackPure s (Val x) = return (x, s)
runStackPure s (E u q) = case decomp u of
  Right (Push v)   -> runStackPure (v : s)  (qApp q ())
  Right Pop        -> runStackPure (tail s) (qApp q (head s))
  Right IsEmpty    -> runStackPure s (qApp q (null s))
  Left  u'         -> E u' (tsingleton (\x -> runStackPure s (qApp q x)))


p1 :: Eff '[Stack Int] (Int, Bool)
p1 = do push (1 :: Int)
        push (2 :: Int)
        push (3 :: Int)
        l <- pop
        e <- (send (IsEmpty :: Stack Int Bool))
        return (l, e)
