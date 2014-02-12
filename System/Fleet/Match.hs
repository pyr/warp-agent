{-# LANGUAGE OverloadedStrings #-}
module System.Fleet.Match where
import System.Fleet.Types
import System.Process
import Data.List(intersperse, isInfixOf, find, null)
import qualified Data.Map as M

runMatch :: String -> Facts -> Matcher -> Bool

runMatch _ _ MatchAll = True

runMatch host _ (MatchHost candidate) = (host == candidate)

runMatch _ facts (MatchFact key val) = case M.lookup key facts of
  Nothing -> False
  Just myfact -> (myfact == val)

runMatch host facts (MatchNot matcher) = not $ runMatch host facts matcher

runMatch host facts (MatchOr matchers) =
  case find (runMatch host facts) matchers of
    Nothing -> False
    Just _ -> True

runMatch host facts (MatchAnd matchers) =
  null $ [ x | x <- matchers, not $ runMatch host facts x]
