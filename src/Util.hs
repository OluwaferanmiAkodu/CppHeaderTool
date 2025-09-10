module Util where

import Text.Megaparsec

applyParsers parsers = choice (map try parsers)

applyParsersUntil :: (Ord a, Stream b) => [Parsec a b c] -> Parsec a b d -> Parsec a b [c]
applyParsersUntil parsers terminator = helper []
    where helper xs = do
            let onEnd = const $ return $ reverse xs
            let onContinue = applyParsers parsers >>= helper . (:xs)

            isEnd <- optional (lookAhead terminator)
            maybe onContinue onEnd isEnd

applyParsersUntil' :: (Ord a, Stream b) => [Parsec a b c] -> Parsec a b d -> Parsec a b [c]
applyParsersUntil' parsers terminator = helper []
    where helper xs = do
            let onEnd = const $ return $ reverse xs
            let onContinue = do
                    hit <- optional (applyParsers parsers) 
                    maybe (anySingle >>= const (helper xs)) (helper . (:xs)) hit

            isEnd <- optional (lookAhead terminator)
            maybe onContinue onEnd isEnd


(>>|) :: (Ord a, Stream b) => [Parsec a b c] -> Parsec a b d -> Parsec a b [c]
(>>|) = applyParsersUntil

(>>|*) :: (Ord a, Stream b) => [Parsec a b c] -> Parsec a b d -> Parsec a b [c]
(>>|*) = applyParsersUntil'

consumeTill x = manyTill anySingle x
{-
parseEnumDefaultTag :: Parser String
parseEnumDefaultTag = do
    _ <- enumDefaultTag
    _ <- single (Punctuator '(')
    x <- qualifiedName
    _ <- single (Punctuator ')')
    return x

parseEnumTag :: Parser CppAstNode
parseEnumTag = do
    _ <- enumTag
    _ <- single (Punctuator '(')
    x <- parseEnumDefaultTag
    _ <- single (Punctuator ')')
    parseEnum True x

parseClassTag :: Parser CppAstNode
parseClassTag = do
    _ <- classTag
    _ <- optional (chunk [Punctuator '(', Punctuator ')'])
    parseClass True

    enumDefaultTag :: Parser CppToken
enumDefaultTag = single (Tag "SMX_ENUM_DEFAULT")

enumTag :: Parser CppToken
enumTag = single (Tag "SMX_ENUM")

classTag :: Parser CppToken
classTag = single (Tag "SMX_CLASS")

propertyTag :: Parser CppToken
propertyTag = single (Tag "SMX_PROPERTY")

parseProperty :: Parser CppAstNode
parseProperty = do
    _ <- propertyTag
    _ <- optional (chunk [Punctuator '(', Punctuator ')'])
    _ <- qualifiedName
    x <- identifier
    _ <- manyTill anySingle (lookAhead (single (Punctuator ';')))
    return $ Property x


parseEnumMember :: Parser CppAstNode
parseEnumMember = do
    x <- identifier
    _ <- manyTill anySingle (lookAhead (satisfy (\y -> y == Punctuator ',' || y == Punctuator '}')))
    return $ EnumMember x
    -}