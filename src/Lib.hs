{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeOperators #-}

module Lib where

import Prelude
import Control.Monad.Freer
import Control.Monad.Freer.Internal
import Set as S

type Vertex = Int

data BiGraph s where
  GetLeftEdge   :: Vertex -> BiGraph (Maybe Vertex)
  GetRightEdge  :: Vertex -> BiGraph (Maybe Vertex)
  SetLeftEdge   :: (Maybe Vertex) -> BiGraph ()
  SetRightEdge  :: (Maybe Vertex) -> BiGraph ()
  NewNode       :: BiGraph Vertex


getLeftEdge :: Member BiGraph r => Vertex -> Eff r (Maybe Vertex)
getLeftEdge = send . GetLeftEdge

getRightEdge :: Member BiGraph r => Vertex -> Eff r (Maybe Vertex)
getRightEdge = send . GetRightEdge

setLeftEdge :: Member BiGraph r => Maybe Vertex -> Eff r ()
setLeftEdge = send . SetLeftEdge

setRightEdge :: Member BiGraph r => Maybe Vertex -> Eff r ()
setRightEdge = send . SetRightEdge

newNode :: Member BiGraph r => Eff r Vertex
newNode = send NewNode




-- A pure handler
type BiGraphRep = [(Vertex, (Maybe Vertex, Maybe Vertex))]

-- runGraphPure :: Eff (BiGraph ': r) a -> Eff r a



-- Examples of using the BiGraph effect

traverseRec :: Vertex -> Eff '[BiGraph, S.Set Vertex] ()
traverseRec root  = do { send (S.Empty :: S.Set Vertex ()); t1 (Just root) }
  where t1 :: Maybe Vertex -> Eff '[BiGraph, S.Set Vertex] ()
        t1 Nothing = return ()
        t1 (Just v) = 
          do visited <- S.lookup v
             if visited
               then return ()
               else do S.insert v
                       getLeftEdge v >>= t1
                       getRightEdge v >>= t1


someFunc :: IO ()
someFunc = putStrLn "someFunc"
