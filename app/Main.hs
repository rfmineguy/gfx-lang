module Main (main) where

{-
 Example source
  
  settings {
    size 600 600
    framerate 30
  }
  begin {
    x = 4
    y = 6
    angle = 0
  }
  draw {
    angle += 3
  }

============================

void settings() {
  size(600, 600)
}

int main () {
  
}

-}


import Text.Parsec as P
import Text.Parsec.String as P
import Text.ParserCombinators.Parsec.Number as P
import Options.Applicative as O
import System.IO
import Data.Set

--- AST Decl
type Var = String
data Param = IntParam Int | StringParam String
  deriving (Eq, Show)
data Statement = Assign Var Int | Call String [Param] 
  deriving (Eq, Show)
data Procedure = Section String [Param] [Statement] 
  deriving (Eq, Show)
type Code = [Procedure]

--
--- Parsing
--
assignParser :: P.Parser Statement
assignParser = Assign <$> many1 letter <*> (spaces >> char '=' >> spaces >> int)

callParser :: P.Parser Statement
callParser = Call <$> many1 letter <*> multiParamParser

statementParser :: P.Parser Statement
statementParser = P.try assignParser P.<|> P.try callParser

multiStatementParser :: P.Parser [Statement]
multiStatementParser = P.spaces >> P.many (statementParser <* P.spaces)

paramParser :: P.Parser Param
paramParser = ((IntParam <$> int) P.<|> (StringParam <$> many1 letter)) <* spaces

multiParamParser :: P.Parser [Param]
multiParamParser = spaces >> char '(' >> P.many (spaces >> paramParser) <* spaces <* char ')'

blockParser :: P.Parser [Statement]
blockParser = spaces >> char '{' >> multiStatementParser <* char '}'

procedureParser :: P.Parser Procedure
procedureParser = Section <$> (spaces >> many1 letter) <*> (P.many space >> multiParamParser) <*> (P.many space >> blockParser) <* spaces

codeParser :: P.Parser Code
codeParser = P.many procedureParser <* eof

--
--- Extraction
--
extractVariablesProc :: Procedure -> [Var]
extractVariablesProc (Section s ps stmts) = extractVariablesStatements stmts

extractVariablesStatements :: [Statement] -> [Var]
extractVariablesStatements = concatMap extractVariablesStatement

extractVariablesStatement :: Statement -> [Var]
extractVariablesStatement (Assign var val) = [var]
extractVariablesStatement (Call s ps) = []

--
--- Codegen
--
codeGenerator :: Code -> String
codeGenerator code = boilerplateHead ++ varDeclsGenerator vars ++ concatMap procedureGenerator code ++ boilerplateTail
  where vars = toList (fromList (concatMap extractVariablesProc code))

boilerplateHead :: String
boilerplateHead = "void size(int, int);\n" ++ "void settings();\n" ++ "void begin();\n" ++ "void draw(float dt);\n"

boilerplateTail :: String
boilerplateTail = "#include \"gfxcore.c\"\n"

procedureGenerator :: Procedure -> String
procedureGenerator (Section s p stmts) = "void " ++ s ++ "(" ++ paramsGenerator p ++ ")" ++ "{\n" ++ statementsGenerator stmts ++ "}\n"

paramsGenerator :: [Param] -> String
paramsGenerator [] = ""
paramsGenerator [p] = paramGenerator p
paramsGenerator (p:ps) = paramGenerator p ++ "," ++ paramsGenerator ps

paramGenerator :: Param -> String
paramGenerator (StringParam i) = "int " ++ i
paramGenerator (IntParam _) = undefined

statementsGenerator :: [Statement] -> String
statementsGenerator = concatMap statementGenerator

statementGenerator :: Statement -> String
statementGenerator (Assign v a) = "\t" ++ v ++ " = " ++ show a ++ ";\n"
statementGenerator (Call s ps) = "\t" ++ s ++ "(" ++ argsGenerator ps ++ ");\n"

argsGenerator :: [Param] -> String
argsGenerator [] = ""
argsGenerator [p] = argGenerator p
argsGenerator (p:ps) = argGenerator p ++ "," ++ argsGenerator ps

argGenerator :: Param -> String
argGenerator (IntParam i) = show i
argGenerator (StringParam s) = s

varDeclsGenerator :: [Var] -> String
varDeclsGenerator = concatMap varDeclGenerator

varDeclGenerator :: Var -> String
varDeclGenerator v = "int " ++ v ++ ";\n"

--
--- Arg parsing
--
data Options = Options
  { file :: String,
    output :: String
  }

options :: O.Parser Options
options = Options
        <$> O.strOption ( long "file" <> short 'f' <> help "Supply file" )
        <*> O.strOption ( long "output" <> short 'o' <> help "Output file" ) 

opts :: ParserInfo Options
opts = info (options <**> helper)
        ( fullDesc
        <> progDesc "Transpile gfx program to C"
        <> header "" )

main :: IO ()
main = run =<< execParser opts

run :: Options -> IO ()
run (Options s o) = do
  putStrLn $ "{InputFile: " ++ s ++ " ; OutputFile: " ++ o ++ "}"
  handle <- openFile s ReadMode
  contents <- hGetContents handle
  let c = parse codeParser "" contents
  case c of
    Left e -> print e
    Right code -> do {
      let variables = toList (fromList (concatMap extractVariablesProc code))
    ; let decl = varDeclsGenerator variables
    ; print ("Variables = ", variables, " => {" ++ decl ++ "}")
    ; writeFile o (codeGenerator code)
    }
  print c
