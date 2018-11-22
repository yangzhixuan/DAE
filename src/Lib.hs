{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeOperators #-}


module Lib where

import Prelude
import Control.Monad.Freer
import Control.Monad.Freer.Internal
import qualified Data.List as DL
import Set as S
import Stack as Stk

type Vertex = Int

data BiGraph s where
  GetLeftEdge   :: Vertex -> BiGraph (Maybe Vertex)
  GetRightEdge  :: Vertex -> BiGraph (Maybe Vertex)
  SetLeftEdge   :: Vertex -> (Maybe Vertex) -> BiGraph ()
  SetRightEdge  :: Vertex -> (Maybe Vertex) -> BiGraph ()
  NewNode       :: BiGraph Vertex


getLeftEdge :: Member BiGraph r => Vertex -> Eff r (Maybe Vertex)
getLeftEdge = send . GetLeftEdge

getRightEdge :: Member BiGraph r => Vertex -> Eff r (Maybe Vertex)
getRightEdge = send . GetRightEdge

setLeftEdge :: Member BiGraph r => Vertex -> Maybe Vertex -> Eff r ()
setLeftEdge v e = send $ SetLeftEdge v e

setRightEdge :: Member BiGraph r => Vertex -> Maybe Vertex -> Eff r ()
setRightEdge v e = send $ SetRightEdge v e

newNode :: Member BiGraph r => Eff r Vertex
newNode = send NewNode




-- A pure handler
type BiGraphPureRep = [(Vertex, (Maybe Vertex, Maybe Vertex))]
emptyGraph = []


runBiGraphPure :: BiGraphPureRep -> Eff (BiGraph ': r) a -> Eff r (BiGraphPureRep, a)
runBiGraphPure g = handleRelayS g (\g a -> return (g, a)) handler
  where handler :: BiGraphPureRep -> BiGraph v -> (BiGraphPureRep -> Arr effs v b) -> Eff effs b
        handler g (GetLeftEdge v) sk = sk g (lookupLeft g v)
        handler g (GetRightEdge v) sk = sk g (lookupRight g v)
        handler g (SetLeftEdge v e) sk = sk (updateG g v (e, lookupRight g v)) ()
        handler g (SetRightEdge v e) sk = sk (updateG g v (lookupLeft g v, e)) ()
        handler g NewNode sk = let nv = if null g then 0 else DL.maximum (map fst g) + 1
                               in sk ((nv, (Nothing, Nothing)) : g) nv

        lookupLeft  g v = maybe Nothing fst (DL.lookup v g)
        lookupRight g v = maybe Nothing snd (DL.lookup v g)
        updateG [] v es = [(v, es)]
        updateG ((x, xes) : xs) v es = if v == x then (v, es) : xs 
                                                 else (x, xes) : updateG xs v es



-- Examples of using the BiGraph effect

traverseRec :: Vertex -> Eff '[S.Set Vertex, BiGraph] ()
traverseRec root  = do { send (S.Empty :: S.Set Vertex ()); t1 (Just root) }
  where t1 :: Maybe Vertex -> Eff '[S.Set Vertex, BiGraph] ()
        t1 Nothing = return ()
        t1 (Just v) = 
          do visited <- S.lookup v
             if visited
               then return ()
               else do S.insert v
                       getLeftEdge v >>= t1
                       getRightEdge v >>= t1


traverseSW :: Vertex -> Eff '[Stk.Stack Bool, S.Set Vertex, BiGraph] ()
traverseSW root = do { send (S.Empty :: S.Set Vertex ()); t2 (Just root) Nothing }
  where t2 :: Maybe Vertex -> Maybe Vertex -> Eff '[Stk.Stack Bool, S.Set Vertex, BiGraph] ()
        t2 Nothing p = restore Nothing p
        t2 (Just v) p = do visited <- S.lookup v
                           if visited
                             then restore (Just v) p
                             else do S.insert v
                                     l <- getLeftEdge v
                                     setLeftEdge v p
                                     Stk.push False
                                     t2 l (Just v)
        restore _ Nothing = return ()
        restore a (Just p) = do sndRet <- Stk.pop
                                if not sndRet 
                                  then do pp <- getLeftEdge p
                                          setLeftEdge p a
                                          r <- getRightEdge p
                                          setRightEdge p pp
                                          Stk.push True
                                          t2 r (Just p)
                                  else do pp <- getRightEdge p
                                          setRightEdge p a
                                          restore (Just p) pp


g1 :: Eff '[BiGraph] Vertex
g1 = do n <- newNode
        a <- newNode
        b <- newNode
        setLeftEdge  n (Just a)
        setRightEdge n (Just b)
        return n

g2 :: Eff '[BiGraph] Vertex
g2 = do n <- newNode
        a <- newNode
        b <- newNode
        setLeftEdge  n (Just a)
        setRightEdge n (Just b)
        setRightEdge b (Just a)
        setRightEdge b (Just n)
        return n

tree :: Int -> Eff '[BiGraph] Vertex
tree n = if n == 1 
           then newNode
           else do rt <- newNode
                   l <- tree (n-1)
                   r <- tree (n-1)
                   setLeftEdge rt (Just l)
                   setRightEdge rt (Just r)
                   return rt

ex1 :: Eff '[BiGraph] Vertex -> Eff '[Stk.Stack Bool, S.Set Vertex, BiGraph] ()
ex1 g = raise $ do n <- raise g
                   traverseRec n

ex2 ::  Eff '[BiGraph] Vertex -> Eff '[Stk.Stack Bool, S.Set Vertex, BiGraph] ()
ex2 g = do n <- raise $ raise g
           traverseSW n

runEx :: Eff '[Stk.Stack Bool, S.Set Vertex, BiGraph] a -> (BiGraphPureRep, ((a, [Bool]), [Vertex]))
runEx = run . runBiGraphPure emptyGraph . S.runSetPure [] . Stk.runStackPure []

someFunc :: IO ()
someFunc = putStrLn "someFunc"
