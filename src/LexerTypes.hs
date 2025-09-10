{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE InstanceSigs #-}

module LexerTypes where

import Text.Megaparsec
import Data.Void
import Data.Proxy
import Data.List.NonEmpty(NonEmpty)
import qualified Data.List.NonEmpty as NonEmpty

type Lexer = Parsec Void String

data CppToken
    = Preprocessor
    | Whitespace
    | Comment String
    | Keyword String
    | Punctuator Char
    | Tag String
    | Identifier String
    | Digit Int
    deriving(Show, Eq, Ord)
    
instance VisualStream [CppToken] where
  showTokens :: Proxy [CppToken] -> NonEmpty (Token [CppToken]) -> String
  showTokens Proxy y = show (NonEmpty.head y)
  tokensLength :: Proxy [CppToken] -> NonEmpty (Token [CppToken]) -> Int
  tokensLength Proxy = length