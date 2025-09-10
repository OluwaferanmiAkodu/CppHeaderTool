module CodeGenTypes where
import ParserTypes
import LexerTypes

data CodeGenAttribute = Reflected 
                      | Serialized
                      | Default String
    deriving (Show, Eq)

data ExampleAstNode = HeaderNode [ExampleAstNode]
                         | NamespaceNode String [ExampleAstNode]
                         | EnumNode CppEnumHead [ExampleAstNode]
                         | ClassNode CppClassHead [ExampleAstNode]
                         | Enumerator String [CppToken]
                         | MemberVariable CppVariableDeclaration
    deriving (Show)

instance CppAstNode ExampleAstNode where
    getChildren (HeaderNode xs) = xs
    getChildren (NamespaceNode _ xs) = xs
    getChildren (EnumNode _ xs) = xs
    getChildren (ClassNode _ xs) = xs
    getChildren _ = []

    setChildren (HeaderNode _) xs = HeaderNode xs
    setChildren (NamespaceNode x _) xs = NamespaceNode x xs
    setChildren (EnumNode x _) xs = EnumNode x xs
    setChildren (ClassNode x _) xs = ClassNode x xs
    setChildren x _ = x

instance CppHeader ExampleAstNode where
    makeHeader = HeaderNode

instance CppNamespace ExampleAstNode where
    makeNamespace = NamespaceNode

    getNamespaceName (NamespaceNode x _) = Just x
    getNamespaceName _ = Nothing

instance CppEnum ExampleAstNode where
    makeEnum = EnumNode

    getEnumInfo (EnumNode x _) = Just x
    getEnumInfo _ = Nothing

instance CppClass ExampleAstNode where
    makeClass = ClassNode

    getClassInfo (ClassNode x _) = Just x
    getClassInfo _ = Nothing

instance CppEnumerator ExampleAstNode where
    makeEnumerator = Enumerator

    getEnumeratorName (Enumerator x _) = Just x
    getEnumeratorName _ = Nothing

    getEnumeratorPostEqTokens (Enumerator _ xs) = Just xs
    getEnumeratorPostEqTokens _ = Nothing

instance CppMemberVariable ExampleAstNode where
    makeMemberVariable = MemberVariable

    getMemberVariableDeclaration (MemberVariable x) = Just x
    getMemberVariableDeclaration _ = Nothing