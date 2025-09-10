--todo check where some should be used insted of many 
module Parser where

import LexerTypes

import Text.Megaparsec
import Data.Void
import Data.Maybe
import Util
import Data.Functor
import Config
import ParserTypes
import Data.List (partition, unsnoc)

parseHeader ::  (CppHeader a) => ParserGraph a -> Parser a
parseHeader graph = (getParsers Global graph >>|* eof) <&> makeHeader

buildAst ::  (CppHeader a) => ParserGraph a -> [CppToken] -> String -> Either (ParseErrorBundle [CppToken] Void) a
buildAst config xs fileName = parse (parseHeader config) fileName xs

tokenToString :: CppToken -> String
tokenToString (Comment x) = x
tokenToString (Keyword x) = x
tokenToString (Punctuator x) = [x]
tokenToString (Tag x) = x
tokenToString (Identifier x) = x
tokenToString (Digit x) = show x
tokenToString _ = ""

tokensToString :: [CppToken] -> String
tokensToString = concatMap tokenToString

punctuator :: Char -> Parser Char
punctuator x = do
    let isPunctuator (Punctuator y) = y == x
        isPunctuator _           = False
    Punctuator y <- satisfy isPunctuator <?> unwords ["punctuator", [x]]
    return y

keyword :: String -> Parser String
keyword x = do
    let isKeyword (Keyword y) = y == x
        isKeyword _           = False
    Keyword y <- satisfy isKeyword <?> unwords ["keyword", x]
    return y

identifier :: Parser String
identifier = do
    let isIdentifier (Identifier _) = True
        isIdentifier _              = False
    Identifier x <- satisfy isIdentifier <?> "identifier"
    return x

tag :: String -> Parser String
tag x = do
    let isTag (Tag y) = y == x
        isTag _           = False
    Keyword y <- satisfy isTag <?> unwords ["tag", x]
    return y

matchIdentifier :: String -> Parser String
matchIdentifier x = do
    let isIdentifier (Identifier y) = y == x
        isIdentifier _              = False
    Identifier y <- satisfy isIdentifier <?> unwords ["identifier", x]
    return y

qualifiedName :: Parser String
qualifiedName = do
    let helper  = do
            _ <- punctuator ':'
            _ <- punctuator ':'
            x <- identifier
            return ("::" ++ x)

    x <- identifier
    xs <- many $ try helper
    return $ concat (x:xs)

-- ===================
-- * Namespace Grammar
-- ===================

{-|
BNF Grammar:

@
namespace-definition:	 
 	named-namespace-definition
 	unnamed-namespace-definition
@

See https://alx71hub.github.io/hcb/#namespace.def for more context
-}
namespaceDefinition :: (CppNamespace a) => String -> ParserGraph a -> Parser a
namespaceDefinition parserID graph = choice $ map try
    [
        namedNamespaceDefinition parserID graph,
        unnamedNamespaceDefinition parserID graph
    ]

{-|
BNF Grammar:

@
named-namespace-definition:	 
 	original-namespace-definition
 	extension-namespace-definition

original-namespace-definition:	 
 	inline (opt) namespace identifier { namespace-body }

extension-namespace-definition:	 
 	inline (opt) namespace original-namespace-name { namespace-body }
@

Notes:

* @ original-namespace-name @ = @ identifier @

* @ namespace-body @ is parsed by other parsers injected via the ParserGraph

See https://alx71hub.github.io/hcb/#namespace.def for more context
-}
namedNamespaceDefinition :: (CppNamespace a) => String -> ParserGraph a -> Parser a
namedNamespaceDefinition parserID graph = do
    _ <- optional $ keyword "inline"
    _ <- keyword "namespace"
    x <- identifier
    _ <- punctuator '{'
    xs <- getParsers (InjectAt parserID) graph >>|* punctuator '}'
    _ <- punctuator '}'
    return $ makeNamespace x xs

{-|
@
unnamed-namespace-definition:	 
 	inline (opt) namespace { namespace-body }
@

Notes:

* @ original-namespace-name @ = @ identifier @

* @ namespace-body @ is parsed by other parsers injected via the ParserGraph

See https://alx71hub.github.io/hcb/#namespace.def for more context
-}
unnamedNamespaceDefinition :: (CppNamespace a) => String -> ParserGraph a -> Parser a
unnamedNamespaceDefinition parserID graph = do
    _ <- optional $ keyword "inline"
    _ <- keyword "namespace"
    _ <- punctuator '{'
    xs <- getParsers (InjectAt parserID) graph >>|* punctuator '}'
    _ <- punctuator '}'
    return $ makeNamespace "" xs

-- ==============
-- * Enum Grammar
-- ==============

{-|
BNF Grammar:

@
enum-specifier:	 
 	enum-head { enumerator-list (opt) }
 	enum-head { enumerator-list , }
@

Notes:

* @ enumerator-list @ is parsed by other parsers injected via the ParserGraph

See https://alx71hub.github.io/hcb/#dcl.enum for more context
-}
enumSpecifier :: (CppEnum a) => String -> ParserGraph a -> Parser a
enumSpecifier parserID graph = do
    x <- enumHead
    _ <- punctuator '{'
    xs <- getParsers (InjectAt parserID) graph >>|* punctuator '}'
    _ <- punctuator '}'
    return $ makeEnum x xs

{-|
BNF Grammar:

@
enum-head:	 
 	enum-key attribute-specifier-seq (opt) identifier (opt) enum-base (opt)
 	enum-key attribute-specifier-seq (opt) nested-name-specifier identifier enum-base (opt)
@

See https://alx71hub.github.io/hcb/#dcl.enum for more context
-}
enumHead :: Parser CppEnumHead
enumHead = do
    _ <- enumKey
    x <- optional attributeSpecifierSeq
    y <- optional qualifiedName
    z <- optional enumBase
    return $ CppEnumHead
        {
            enumAttributeSpecifiers = fromMaybe [] x,
            enumHeadName = fromMaybe [] y,
            enumBaseClause = fromMaybe [] z
        }

{-|
BNF Grammar:

@
enum-key:	 
 	enum
 	enum class
 	enum struct
@

See https://alx71hub.github.io/hcb/#dcl.enum for more context
-}
enumKey :: Parser ()
enumKey = do
    _ <- keyword "enum"
    _ <- optional (keyword "class" <|> keyword "struct")
    return ()

{-|
BNF Grammar:

@
enumerator-definition:	 
 	enumerator
 	enumerator = constant-expression
@

See https://alx71hub.github.io/hcb/#dcl.enum for more context
-}
enumBase :: Parser [CppTypeSpecifier]
enumBase = do
    _ <- punctuator ':'
    typeSpecifierSeq


enumeratorDefinition :: (CppEnumerator a) => Parser a
enumeratorDefinition = do
    x <- identifier
    _ <- optional $ punctuator '='
    xs <- consumeTill $ lookAhead (punctuator ',' <|> punctuator '}')
    return $ makeEnumerator x xs


-- ===============
-- * Class Grammar
-- ===============

{-|
BNF Grammar:

@
class-specifier:	 
 	class-head { member-specification (opt) }
@

Notes:

* @ member-specification @ is parsed by other parsers injected via the ParserGraph

See https://alx71hub.github.io/hcb/#class for more context
-}
classSpecifier :: (CppClass a) => String -> ParserGraph a -> Parser a
classSpecifier parserID graph = do
    x <- classHead
    _ <- punctuator '{'
    xs <- getParsers (InjectAt parserID) graph >>|* punctuator '}'
    _ <- punctuator '}'
    return $ makeClass x xs

{-|
BNF Grammar:

@
class-head:	 
 	class-key attribute-specifier-seq (opt) class-head-name class-virt-specifier-seq (opt) base-clause (opt)
 	class-key attribute-specifier-seq (opt) base-clause (opt)
@

See https://alx71hub.github.io/hcb/#class for more context
-}
classHead :: Parser CppClassHead
classHead = do
    _ <- classKey
    x <- optional attributeSpecifierSeq
    y <- optional qualifiedName
    z <- maybe (return Nothing) (const $ optional classVirtSpecifierSeq) y
    w <- optional baseClause
    return $ CppClassHead
        {
            classAttributeSpecifiers = fromMaybe [] x,
            classHeadName = fromMaybe "" y,
            isClassFinal = isJust z,
            classBaseClause = fromMaybe [] w
        }

{-|
BNF Grammar:

@
class-key:	 
 	class
 	struct
 	union
@

See https://alx71hub.github.io/hcb/#class for more context
-}
classKey :: Parser ()
classKey = do
    _ <- keyword "class" <|> keyword "struct" <|> keyword "union"
    return ()

{-|
BNF Grammar:

@
class-virt-specifier-seq:	 
 	class-virt-specifier
 	class-virt-specifier-seq class-virt-specifier  
@

Notes:

* Grammar for for @ class-virt-specifier-seq @ is recursive and therfore can be sumarised to @ some ... @ in haskell

See https://alx71hub.github.io/hcb/#class for more context
-}
classVirtSpecifierSeq :: Parser [String]
classVirtSpecifierSeq = some classVirtSpecifier

{-|
BNF Grammar:

@
class-virt-specifier:	 
 	final
@

See https://alx71hub.github.io/hcb/#class for more context
-}
classVirtSpecifier :: Parser String
classVirtSpecifier = matchIdentifier "final"

baseClause :: Parser [CppBaseSpecifier]
baseClause = do
    _ <- punctuator ':'
    baseSpecifierList

{-|
BNF Grammar:

@
base-specifier-list:	 
 	base-specifier ... (opt)
 	base-specifier-list , base-specifier ... (opt)
@

Notes:

* Grammar for for @ base-specifier-list @ is recursive and therfore can be sumarised to @ some ... @ in haskell

See https://alx71hub.github.io/hcb/#class.derived for more context
-}
baseSpecifierList :: Parser [CppBaseSpecifier]
baseSpecifierList = some baseSpecifier

baseSpecifier :: Parser CppBaseSpecifier
baseSpecifier = do
    let accessSpecifierSeqA = do
            _ <- keyword "virtual"
            y <- optional accessSpecifier
            return (True, fromMaybe Private y)

    let accessSpecifierSeqB = do
            x <- accessSpecifier
            y <- optional $ keyword "virtual"
            return (isJust y, x)

    x <- optional attributeSpecifierSeq
    y <- optional (accessSpecifierSeqA <|> accessSpecifierSeqB)
    z <- classOrDeclType
    return $ CppBaseSpecifier
        {
            baseAttributeSpecifiers = fromMaybe [] x,
            isBaseVirtual = maybe False fst y,
            baseAccessSpecifier = maybe Private snd y,
            baseTypeSpecifier=  z
        }

{-|
BNF Grammar:

@
access-specifier:	 
 	private
 	protected
 	public
@

See https://alx71hub.github.io/hcb/#class.derived for more context
-}
accessSpecifier :: Parser CppAccessSpecifier
accessSpecifier = do
    x <- keyword "public" <|> keyword "private" <|> keyword "protected"
    case x of
        "public" -> return Public
        "private" -> return Private
        "protected" -> return Protected

{-|
BNF Grammar:

@
class-or-decltype:	 
 	:: (opt) nested-name-specifieropt class-name
 	decltype-specifier
@

Notes:

* Currently @ decltype-specifier @ is not accepted as a valid @ class-or-decltype @

* __ TODO __ : Support @ decltype-specifier @ as a valid @ class-or-decltype @

See https://alx71hub.github.io/hcb/#class.derived for more context
-}
classOrDeclType :: Parser String
classOrDeclType = className

{-|
BNF Grammar:

@
class-name:	 
 	identifier
 	simple-template-id 
@

See https://alx71hub.github.io/hcb/#class for more context
-}
className :: Parser String
className = choice $ map try
    [
        simpleTemplateID,
        qualifiedName
    ]

-- ================================================
-- * Attribute Specifier Grammar #dcl.attr.grammar#
-- ================================================

{-|
BNF Grammar:

@
attribute-specifier-seq:	 
 	attribute-specifier
 	attribute-specifier-seq attribute-specifier
@

Notes:

* Grammar for for @ attribute-specifier-seq @ is recursive and therfore can be sumarised to @ some ... @ in haskell

See https://alx71hub.github.io/hcb/#dcl.attr.grammar for more context
-}
attributeSpecifierSeq :: Parser [CppAttributeSpecifier]
attributeSpecifierSeq = some attributeSpecifier

{-|
BNF Grammar:

@
attribute-specifier:	 
 	[ [ attribute-list ] ]
 	alignment-specifier

alignment-specifier:	 
 	alignas ( type-id ... (opt) )
 	alignas ( assignment-expression ... (opt) )
@

Notes:

* @ attribute-specifier @ is parsed as "token soup" (an array of tokens, how you deal with this is up to you)

See https://alx71hub.github.io/hcb/#dcl.attr.grammar for more context
-}

attributeSpecifier :: Parser CppAttributeSpecifier
attributeSpecifier = do
    let attributeSpecifierA = do
            let prefix = do
                    _ <- punctuator '['
                    _ <- punctuator '['
                    return ()

            let suffix = do
                    _ <- punctuator ']'
                    _ <- punctuator ']'
                    return ()

            _ <- prefix
            xs <- consumeTill suffix
            return $ concat [[Punctuator '[',Punctuator '[' ], xs, [Punctuator ']',Punctuator ']' ]]

    let attributeSpecifierB = do
            _ <- keyword "alignas"
            _ <- punctuator '('
            xs <- consumeTill $ punctuator ')'
            return $ concat [[Keyword "alignas", Punctuator '('], xs, [Punctuator ')']]

    attributeSpecifierA <|> attributeSpecifierB

-- ===================================
-- * Type Specifier Grammar #dcl.type#
-- ===================================

{-|
BNF Grammar:

@
type-specifier-seq:	 
 	type-specifier attribute-specifier-seq (opt)
 	type-specifier type-specifier-seq
@

Notes:

* Grammar for for @ type-specifier-seq @ is recursive and therfore can be sumarised to @ some ... @ in haskell

See https://alx71hub.github.io/hcb/#dcl.type for more context
-}
typeSpecifierSeq :: Parser [CppTypeSpecifier]
typeSpecifierSeq = some helper
    where helper = do
            x <- typeSpecifier
            y <- optional attributeSpecifierSeq
            return $ CppTypeSpecifier { typeSpecifierName = x, typeAttributeSpecifiers = fromMaybe [] y }

{-|
BNF Grammar:

@
type-specifier:	 
 	trailing-type-specifier
 	class-specifier
 	enum-specifier
@

Notes:

* Currently @ class-specifier @ and @ enum-specifier @ are not accepted as a valid @ type-specifier @

* __ TODO __ : Support @ class-specifier @ and  @ enum-specifier @ as a valid @ type-specifier @

See https://alx71hub.github.io/hcb/#dcl.type for more context
-}
typeSpecifier :: Parser String
typeSpecifier = trailingTypeSpecifier

{-|
BNF Grammar:

@
trailing-type-specifier:	 
 	simple-type-specifier
 	elaborated-type-specifier
 	typename-specifier
 	cv-qualifier
@

Notes:

* Currently @ elaborated-type-specifier @ and @ typename-specifier @ are not accepted as a valid @ trailing-type-specifier @

* __ TODO __ : Support @ elaborated-type-specifier @ and @ typename-specifier @ as a valid @ trailing-type-specifier @

See https://alx71hub.github.io/hcb/#dcl.type for more context
-}
trailingTypeSpecifier :: Parser String
trailingTypeSpecifier = choice $ map try
    [
        simpleTypeSpecifier,
        cvQualifier
    ]

{-|
BNF Grammar:

@
simple-type-specifier:	 
 	:: (opt) nested-name-specifier (opt) type-name
 	:: (opt) nested-name-specifier template simple-template-id
 	char
 	char16_t
 	char32_t
 	wchar_t
 	bool
 	short
 	int
 	long
 	signed
 	unsigned
 	float
 	double
 	void
 	auto
 	decltype-specifier
@

Notes:

* @ nested-name-specifier (opt) type-name @ is handled via the function @ typeName @ 

    * @ typeName @ uses the function @ qualifiedName @ which internally implements @ nested-name-specifier @ grammar

* Currently @ decltype-specifier @ is not accepted as a valid @ trailing-type-specifier @

* __ TODO __ : Support @ decltype-specifie @ as a valid @ simple-type-specifier @

* __ TODO __ : support optional preceeding @ :: @ for @ :: (opt) nested-name-specifier (opt) type-name @ and @ :: (opt) nested-name-specifier template simple-template-id @ 

See https://alx71hub.github.io/hcb/#dct.type.simple for more context
-}
simpleTypeSpecifier :: Parser String
simpleTypeSpecifier = choice $ map try
    [
        typeName,
        helper,
        keyword "char",
        keyword "char16_t",
        keyword "char32_t",
        keyword "wchar_t",
        keyword "bool",
        keyword "short",
        keyword "int",
        keyword "long",
        keyword "signed",
        keyword "unsigned",
        keyword "float",
        keyword "double",
        keyword "void",
        keyword "auto"
    ]
    where helper = do
            x <- qualifiedName
            _ <- keyword "template"
            y <- simpleTemplateID
            return $ x ++ y

{-|
BNF Grammar:

@
type-name:	 
 	class-name
 	enum-name
 	typedef-name
 	simple-template-id
@

Notes:

* @ enum-name @ = @ identifier @, @ typedef-name @ = @ identifier @, @ class-name @ = @ identifier @ or @ simple-template-id @

    * From this we can see that a @ type-name @  is either an @ identifier @ or simple @ template-id @

    * Everywhere @ type-name @ occurs it is preceded by @ nested-name-specifier @ so lets just use @ qualifiedName @ instead of @ identifier @

See https://alx71hub.github.io/hcb/#dct.type.simple for more contex
-}
typeName :: Parser String
typeName = choice $ map try
    [
        simpleTemplateID,
        qualifiedName
    ]

{-|
BNF Grammar:

@
simple-template-id:	 
 	template-name < template-argument-list (opt) >
@

Notes:

* @ template-name @ = @ identifier @

* @ simple-template-id @ is parsed as a single string, for example @ vector<int> @ will be parsed completely as @ "vector<int>" @

See https://alx71hub.github.io/hcb/#dct.type.simple for more contex
-}
simpleTemplateID :: Parser String
simpleTemplateID = do
    let openBracket x = do
            y <- punctuator '<'
            return (x + 1, [y])

    let closeBracket x = do
            y <- punctuator '>'
            return (x - 1, [y])

    let other x = do
            y <- anySingle
            return (x, tokenToString y)

    let helper x y z =
            if x == 0 && not z then
                return $ concat $ reverse y
            else do
                w <- choice $ map (\f -> try $ f x) [openBracket, closeBracket, other]
                helper (fst w) (snd w:y) False

    x <- qualifiedName
    _ <- lookAhead $ punctuator '<'
    y <- helper 0 [] True
    return $ x ++ y

{-|
BNF Grammar:

@
cv-qualifier:	 
 	const
 	volatile
@

See https://alx71hub.github.io/hcb/#dcl.decl for more contex
-}
cvQualifier :: Parser String
cvQualifier = keyword "const" <|> keyword "volatile"

-- ======================
-- * Class Member Grammar
-- ======================

simpleVariableDeclaration :: (CppMemberVariable a) => Parser a
simpleVariableDeclaration = do
    let specifiers = map Keyword [ "register", "static", "thread_local", "extern", "mutable", "inline", "virtual", "explicit", "friend","typedef", "constexpr" ]

    x <- optional attributeSpecifierSeq
    xs <- consumeTill (punctuator ';' <|> punctuator '=' <|> punctuator '{')
    let (ys, ys') = partition (`elem` specifiers) xs

    let subParser = do
            y <- typeSpecifier
            z <- many (punctuator '*' <|> punctuator '&')
            return (y ++ z)

    let zs = parse (some subParser) "" ys'

    case zs of
        (Right ws) -> case unsnoc ws of
                            (Just (us, u)) -> return $ makeMemberVariable $ CppVariableDeclaration
                                {
                                    variableName = u,
                                    variableType = unwords us,
                                    variableDeclSpecifiers = ys,
                                    variableAttributeSpecifiers = fromMaybe [] x
                                }
                            _ -> fail ""

        (Left err) -> fail ""
