{-# LANGUAGE TupleSections #-}

module Lexer (lexicalAnalysis) where

import Constants
import LexerTypes

import Util
import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Void
import Data.Char
import Data.List
import Data.Functor
import Data.Set (Set)
import qualified Data.Set as Set

-- === Exported Functions ===
    
lexicalAnalysis :: Set String -> String -> String -> Either (ParseErrorBundle String Void) [CppToken]
lexicalAnalysis tags text fileName = parse (tokeniseHeader tags) fileName (spliceLines text) <&> discardUselessTokens

-- === Preprocessing ===

spliceLines :: String -> String
spliceLines xs = helper ([], xs)
    where helper (ys, [])    = reverse ys
          helper (ys, z:zs)  = maybe (helper (z:ys, zs)) (helper . (ys,)) (stripPrefix "\\\n" zs)

-- === Postprocessing ===

discardUselessTokens :: [CppToken] -> [CppToken]
discardUselessTokens = filter (not . helper)
    where helper Whitespace   = True
          helper Preprocessor = True
          helper (Comment _)  = True
          helper _            = False

-- === Tokenisers ===

tokeniseKeyword :: (String -> CppToken) -> String -> Lexer CppToken
tokeniseKeyword f x = do
    _ <- chunk x
    notFollowedBy alphaNumChar
    return $ f x

tokenisePunctuator :: (Char -> CppToken) -> Char -> Lexer CppToken
tokenisePunctuator f x = do
    _ <- single x
    return $ f x

tokenisePreprocessor :: Lexer CppToken
tokenisePreprocessor = do
    _ <- single '#'
    _ <- many (anySingleBut '\n')
    return Preprocessor

tokeniseSingleLineComment :: Lexer CppToken
tokeniseSingleLineComment = do
    _ <- chunk "//"
    c <- many (anySingleBut '\n')
    return $ Comment c

tokeniseMultiLineComment :: Lexer CppToken
tokeniseMultiLineComment = do
    _ <- chunk "/*"
    c <- manyTill anySingle (chunk "*/")
    return $ Comment c

tokeniseDigit :: Lexer CppToken
tokeniseDigit = digitChar <&> (Digit . digitToInt)

tokeniseIdentifier :: Lexer CppToken
tokeniseIdentifier = do
    x <- letterChar <|> single '_'
    xs <- many (alphaNumChar <|> single '_')
    return $ Identifier (x:xs)

tokeniseWhitespace :: Lexer CppToken
tokeniseWhitespace = do
    _ <- some spaceChar
    return Whitespace

tokeniseHeader :: Set String -> Lexer [CppToken]
tokeniseHeader tags = tokenisers tags >>| eof

tokenisers :: Set String -> [Lexer CppToken]
tokenisers tags = concat
    [ [tokenisePreprocessor, tokeniseSingleLineComment, tokeniseMultiLineComment]
    , map (uncurry tokeniseKeyword) $ sortBy comparison (map (Keyword,) keywords ++ map (Tag,) (Set.toList tags))
    , map (tokenisePunctuator Punctuator) punctuators
    , [tokeniseIdentifier, tokeniseDigit, tokeniseWhitespace]
    ]
    where comparison x y = if length (snd x) < length (snd y) then GT else LT