{-# LANGUAGE RecordWildCards #-}

module DFA
  ( module DFA
  , These (..)
  ) where

import Control.Applicative ((<|>))
import Data.Bifunctor (first)
import Data.These


data DFA s e a = DFA
  { dfaState :: s
  , dfaEdges :: s -> e -> Maybe (These s a)
  } deriving (Functor)


newDFA :: s -> DFA s e a
newDFA s = DFA s . const $ const Nothing


addEdge'
    :: (s -> s -> Bool)
    -> (e -> e -> Bool)
    -> s
    -> e
    -> These s a
    -> DFA s e a
    -> DFA s e a
addEdge' fs fe s e r dfa = dfa
  { dfaEdges = \s' e' -> dfaEdges dfa s' e'
      <|> if fs s s' && fe e e'
             then Just r
             else Nothing
  }


addEdge :: (Eq s, Eq e) => s -> e -> These s a -> DFA s e a -> DFA s e a
addEdge = addEdge' (==) (==)


addFallthrough :: Eq s => s -> These s a -> DFA s e a -> DFA s e a
addFallthrough s s' =
  addEdge' (==)
           (const $ const True)
           s
           undefined
           s'


runDFA :: DFA s e a -> e -> These (DFA s e a) a
runDFA dfa@DFA{..} e =
  case dfaEdges dfaState e of
    Just t -> first (\s -> dfa { dfaState = s }) t
    Nothing -> This dfa

