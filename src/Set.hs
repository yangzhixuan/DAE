{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Set where

import Prelude hiding (lookup)
import Control.Monad.Freer
import Control.Monad.Freer.Internal
import qualified Data.List as DL


data Set e s where
  Insert :: !e -> Set e ()
  Delete :: !e -> Set e ()
  Lookup :: !e -> Set e Bool
  Empty  :: Set e ()

insert :: Member (Set e) r => e -> Eff r ()
insert = send . Insert

delete :: Member (Set e) r => e -> Eff r ()
delete = send . Delete

lookup :: Member (Set e) r => e -> Eff r Bool
lookup = send . Lookup



-- A pure handler
runSetPure :: Eq e => [e] -> Eff (Set e ': r) a -> Eff r (a, [e])
runSetPure s (Val x) = return (x, s)
runSetPure s (E u q) = case decomp u of
  Right (Insert v) -> runSetPure (insert' v s) (qApp q ())
  Right (Delete v) -> runSetPure (DL.delete v s) (qApp q ())
  Right (Lookup v) -> runSetPure s (qApp q (DL.elem v s))
  Right Empty      -> runSetPure [] (qApp q ())
  Left  u'         -> E u' (tsingleton (\x -> runSetPure s (qApp q x)))
  where insert' v l = if DL.elem v l then l else v : l


p1 :: Eff '[Set Int] Bool
p1 = do insert (1 :: Int)
        insert (2 :: Int)
        delete (2 :: Int)
        insert (3 :: Int)
        insert (3 :: Int)
        lookup (2 :: Int)
