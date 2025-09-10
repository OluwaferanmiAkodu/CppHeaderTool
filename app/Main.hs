-- Main.hs (updated with output directory support)
module Main (main) where

import Lexer
import Parser
import Text.Megaparsec
import Data.Foldable (Foldable(toList))
import Config
import CodeGen
import CodeGenTypes
import qualified Data.Set as Set
import System.Environment (getArgs)
import System.Exit (exitFailure, exitSuccess)
import System.IO (hPutStrLn, stderr)
import System.Directory (createDirectoryIfMissing, doesDirectoryExist)
import System.FilePath (takeFileName, replaceExtension, (</>))
import Control.Monad (when, unless)

-- | Command line mode
data Mode = GenerateCode | DumpJSON deriving (Show, Eq)

-- | Command line arguments
data Options = Options
    { optMode :: Mode
    , optFiles :: [FilePath]
    , optOutputDir :: Maybe FilePath
    } deriving (Show)

-- | Default configuration for the C++ header parser
config :: Config ExampleAstNode
config = Config
    {
        tags = Set.empty,
        parsers = Set.fromList
            [
                ParserConfig
                {
                    parserName = "Namespace",
                    parser = namespaceDefinition,
                    parserInjectionPoints = Set.fromList [Global, InjectAt "Namespace"]
                },
                ParserConfig
                {
                    parserName = "Class",
                    parser = classSpecifier,
                    parserInjectionPoints = Set.fromList [Global, InjectAt "Namespace", InjectAt "Class"]
                },
                ParserConfig
                {
                    parserName = "Enum",
                    parser = enumSpecifier,
                    parserInjectionPoints = Set.fromList [Global, InjectAt "Namespace", InjectAt "Class"]
                },
                ParserConfig
                {
                    parserName = "Enumerator",
                    parser = \_ _ -> enumeratorDefinition,
                    parserInjectionPoints = Set.fromList [InjectAt "Enum"]
                },
                ParserConfig
                {
                    parserName = "MemberVariable",
                    parser = \_ _ -> simpleVariableDeclaration,
                    parserInjectionPoints = Set.fromList [InjectAt "Class"]
                }
            ]
    }

-- | Parse command line arguments
parseArgs :: [String] -> Either String Options
parseArgs [] = Left "No arguments provided. Use --help for usage information."
parseArgs args = go (Options GenerateCode [] Nothing) args
  where
    go opts [] = Right opts
    go opts ("--code" : rest) = go (opts { optMode = GenerateCode }) rest
    go opts ("--json" : rest) = go (opts { optMode = DumpJSON }) rest
    go opts ("-o" : dir : rest) = go (opts { optOutputDir = Just dir }) rest
    go opts ("--output" : dir : rest) = go (opts { optOutputDir = Just dir }) rest
    go opts ("--help" : _) = Left usage
    go opts (file : rest)
        | head file == '-' = Left $ "Unknown option: " ++ file ++ "\n" ++ usage
        | otherwise = go (opts { optFiles = optFiles opts ++ [file] }) rest

-- | Usage information
usage :: String
usage = unlines
    [ "CppHeaderTool - C++ Header Parser and Code Generator"
    , "Usage:"
    , "  cppheadertool --code [OPTIONS] <file1.h> [file2.h ...]  # Generate C++ code for enums"
    , "  cppheadertool --json [OPTIONS] <file1.h> [file2.h ...]  # Dump AST as JSON"
    , "  cppheadertool --help                                   # Show this help message"
    , ""
    , "Options:"
    , "  -o, --output DIR       Output directory for generated files"
    , ""
    , "Examples:"
    , "  cppheadertool --code -o generated/ MyEnums.h"
    , "  cppheadertool --json --output ast-dump/ MyHeader.h AnotherHeader.h"
    , "  cppheadertool --code MyEnums.h              # Output to stdout"
    ]

-- | Get output file path based on input file and mode
getOutputFilePath :: Maybe FilePath -> FilePath -> Mode -> IO FilePath
getOutputFilePath Nothing _ _ = return ""  -- Empty means stdout
getOutputFilePath (Just outputDir) inputFile mode = do
    createDirectoryIfMissing True outputDir
    let baseName = takeFileName inputFile
    let ext = case mode of
                GenerateCode -> ".gen.inl"
                DumpJSON -> ".json"
    return $ outputDir </> replaceExtension baseName ext

-- | Write content to file or stdout
writeOutput :: FilePath -> String -> IO ()
writeOutput "" content = putStr content  -- Write to stdout
writeOutput outputFile content = do
    writeFile outputFile content
    putStrLn $ "Generated: " ++ outputFile

-- | Process a single header file
processFile :: Options -> FilePath -> IO ()
processFile opts fileName = do
    putStrLn $ "Processing: " ++ fileName
    
    -- Get output file path
    outputFile <- getOutputFilePath (optOutputDir opts) fileName (optMode opts)
    when (optOutputDir opts /= Nothing) $
        putStrLn $ "Output: " ++ if null outputFile then "stdout" else outputFile
    
    headerFile <- readFile fileName
    
    let tokens = lexicalAnalysis (tags config) headerFile fileName
    
    case tokens of
        Left err -> do
            hPutStrLn stderr $ "Lexical error in " ++ fileName ++ ":\n" ++ errorBundlePretty err
            exitFailure
        
        Right xs -> case buildAst (makeParserGraph (parsers config)) xs fileName of
            Left err -> do
                hPutStrLn stderr $ "Parse error in " ++ fileName ++ ":\n" ++ parseErrorPretty (head $ toList (bundleErrors err))
                exitFailure
            
            Right ast -> do
                let flattened = flattenAst ast
                let content = case optMode opts of
                                GenerateCode -> unlines
                                    [
                                        concat ["#include \"", fileName, "\""],
                                        generateCode flattened
                                    ]
                                DumpJSON -> generateJSON flattened
                
                writeOutput outputFile content

main :: IO ()
main = do
    args <- getArgs
    case parseArgs args of
        Left errMsg -> do
            hPutStrLn stderr errMsg
            exitFailure
        
        Right opts -> do
            let files = optFiles opts
            if null files
                then do
                    hPutStrLn stderr "No input files specified"
                    exitFailure
                else do
                    -- Validate output directory if specified
                    case optOutputDir opts of
                        Just dir -> do
                            exists <- doesDirectoryExist dir
                            unless exists $ createDirectoryIfMissing True dir
                        Nothing -> return ()
                    
                    mapM_ (processFile opts) files
            exitSuccess