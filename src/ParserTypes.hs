module ParserTypes where

import Text.Megaparsec
import Data.Void
import LexerTypes

type Parser = Parsec Void [CppToken]

type CppAttributeSpecifier = [CppToken]

data CppAccessSpecifier = Public | Private | Protected
    deriving (Show)

data CppTypeSpecifier = CppTypeSpecifier
    {
        typeSpecifierName :: String,
        typeAttributeSpecifiers :: [CppAttributeSpecifier]
    }
    deriving (Show)

data CppEnumHead = CppEnumHead
    {
        enumAttributeSpecifiers :: [CppAttributeSpecifier],
        enumHeadName :: String,
        enumBaseClause :: [CppTypeSpecifier]
    }
    deriving (Show)

data CppBaseSpecifier = CppBaseSpecifier
    {
        baseTypeSpecifier :: String,
        baseAccessSpecifier :: CppAccessSpecifier,
        baseAttributeSpecifiers :: [CppAttributeSpecifier],
        isBaseVirtual :: Bool
    }
    deriving (Show)

data CppClassHead = CppClassHead
    {
        classAttributeSpecifiers :: [CppAttributeSpecifier],
        classHeadName :: String,
        isClassFinal :: Bool,
        classBaseClause :: [CppBaseSpecifier]
    }
    deriving (Show)

data CppVariableDeclaration = CppVariableDeclaration
    {
        variableName :: String,
        variableType :: String,
        variableDeclSpecifiers :: [CppToken],
        variableAttributeSpecifiers :: [CppAttributeSpecifier]
    }
    deriving (Show)

class CppAstNode a where
    --getAstNodeType :: a -> String
    getChildren :: a -> [a]
    setChildren :: a -> [a] -> a

class (CppAstNode a) => CppHeader a where
    makeHeader :: [a] -> a

class (CppAstNode a) => CppNamespace a where
    makeNamespace :: String -> [a] -> a
    getNamespaceName :: a -> Maybe String

class (CppAstNode a) => CppEnum a where
    makeEnum :: CppEnumHead -> [a] -> a
    getEnumInfo :: a -> Maybe CppEnumHead

class (CppAstNode a) => CppClass a where
    makeClass :: CppClassHead -> [a] -> a
    getClassInfo :: a -> Maybe CppClassHead

class (CppAstNode a) => CppEnumerator a where
    makeEnumerator :: String -> [CppToken] -> a
    getEnumeratorName :: a -> Maybe String
    getEnumeratorPostEqTokens :: a -> Maybe [CppToken]

class (CppAstNode a) => CppMemberVariable a where
    makeMemberVariable :: CppVariableDeclaration -> a
    getMemberVariableDeclaration :: a -> Maybe CppVariableDeclaration