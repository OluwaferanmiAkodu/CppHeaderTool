{-# LANGUAGE InstanceSigs #-}

module Config where

import ParserTypes
import Data.Set (Set)
import Data.Map (Map)
import qualified Data.Map as Map

newtype ParserGraph a = ParserGraph { parserGraph :: Map ParserInjectionPoint [ParserGraph a -> Parser a] }

data ParserInjectionPoint = Global | InjectAt String
    deriving(Eq, Ord, Show)

data ParserConfig a = ParserConfig
    {
        parserName :: String,
        parser :: String -> ParserGraph a -> Parser a,
        parserInjectionPoints :: Set ParserInjectionPoint
    }

instance  Eq (ParserConfig a) where
    (==) :: ParserConfig a -> ParserConfig a -> Bool
    x == y = parserName x == parserName y

instance  Ord (ParserConfig a) where
    (<=) :: ParserConfig a -> ParserConfig a -> Bool
    x <= y = parserName x <= parserName y

data Config a = Config
    {
        tags :: Set String,
        parsers :: Set (ParserConfig a)
    }

makeParserGraph :: Set (ParserConfig a) -> ParserGraph a
makeParserGraph = foldr addToParserGraph (ParserGraph { parserGraph = Map.empty })

addToParserGraph :: ParserConfig a -> ParserGraph a -> ParserGraph a
addToParserGraph config graph = foldr helper graph (parserInjectionPoints config)
    where helper injectionPoint graph' = graph' { parserGraph = Map.insertWith (++) injectionPoint [parser config (parserName config)] (parserGraph graph') }

getParsers :: ParserInjectionPoint -> ParserGraph a -> [Parser a]
getParsers injectionPoint graph = maybe [] helper hit
    where hit    = Map.lookup injectionPoint (parserGraph graph)
          helper = map (\f -> f graph)