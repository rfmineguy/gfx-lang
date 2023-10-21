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
data VarType = ConstCharP | Int
  deriving (Eq, Show, Ord)
data Var = Variable VarType String
  deriving (Eq, Show, Ord)
data Param = IntParam Int
  deriving (Eq, Show)
data Statement = AssignInt Var Int | AssignStr Var String | Call String [Param] 
  deriving (Eq, Show)
data Procedure = Section String [Param] [Statement] 
  deriving (Eq, Show)
type Code = [Procedure]

--
--- Parsing
--

-- Credit Aadit M Shah (https://stackoverflow.com/questions/24106314/parser-for-quoted-string-using-parsec)
escape :: P.Parser String
escape = do
    d <- char '\\'
    c <- oneOf "\\\"0nrvtbf" -- all the characters which can be escaped
    return [d, c]

nonEscape :: P.Parser Char
nonEscape = noneOf "\\\"\0\n\r\v\t\b\f"

character :: P.Parser String
character = fmap return nonEscape P.<|> escape

parseString :: P.Parser String
parseString = do
    char '"'
    strings <- P.many character
    char '"'
    return $ concat strings
--

assignIntParser :: P.Parser Statement
assignIntParser = AssignInt <$> (Variable Int <$> many1 letter) <*> (spaces >> char '=' >> spaces >> int)

-- assignIntParser = AssignInt <$> (VarType <$> ConstCharP <*> many1 letter) <*> (spaces >> char '=' >> spaces >> int)

assignStrParser :: P.Parser Statement
assignStrParser = AssignStr <$> (Variable ConstCharP <$> many1 letter) <* (spaces >> char '=') <*> (spaces >> parseString)

callParser :: P.Parser Statement
callParser = Call <$> many1 letter <*> multiParamParser

statementParser :: P.Parser Statement
statementParser = P.try assignIntParser P.<|> P.try assignStrParser P.<|> P.try callParser

multiStatementParser :: P.Parser [Statement]
multiStatementParser = P.spaces >> P.many (statementParser <* P.spaces)

paramParser :: P.Parser Param
paramParser = (IntParam <$> int) <* spaces

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
extractVariablesStatement (AssignInt var s) = [var]
extractVariablesStatement (AssignStr var s) = [var]
extractVariablesStatement (Call s ps) = []

--
--- Codegen
--
codeGenerator :: Code -> String
codeGenerator code = boilerplateInclude ++ varDeclsGenerator vars ++ concatMap procedureGenerator code ++ boilerplateMain
  where vars = toList (fromList (concatMap extractVariablesProc code))

boilerplateInclude :: String
boilerplateInclude = "#define OLIVEC_IMPLEMENTATION\n" ++ "#include \"olive.c\"\n" ++ "#include <stdlib.h>\n" ++ "#define STB_IMAGE_WRITE_IMPLEMENTATION\n" ++ "#include \"stb_image_write.h\"\n" ++  "Olivec_Canvas oc;\nuint32_t *pixels;\n"

boilerplateMain :: String
boilerplateMain =  "int main(void) {\n" ++ boilerplateOlivec ++ "\trun();\n" ++ boilerplateOlivecSave ++ "\treturn 0;\n }"

boilerplateOlivec :: String
boilerplateOlivec = "\tsettings();\n" ++ "\tpixels = malloc(width * height * sizeof(uint32_t));\n" ++  "\toc = olivec_canvas(pixels, width, height, width);\n"

boilerplateOlivecSave :: String
boilerplateOlivecSave = "\tif (!stbi_write_png(outFile, width, height, 4, pixels, sizeof(uint32_t)*width)) { return 1; }\n" ++ "\tfree(pixels);\n"

procedureGenerator :: Procedure -> String
procedureGenerator (Section s p stmts) = "void " ++ s ++ "(" ++ paramsGenerator p ++ ")" ++ "{\n" ++ statementsGenerator stmts ++ "}\n"

paramsGenerator :: [Param] -> String
paramsGenerator [] = ""
paramsGenerator [p] = paramGenerator p
paramsGenerator (p:ps) = paramGenerator p ++ "," ++ paramsGenerator ps

paramGenerator :: Param -> String
paramGenerator (IntParam n) = "int " ++ show n

statementsGenerator :: [Statement] -> String
statementsGenerator = concatMap statementGenerator

statementGenerator :: Statement -> String
statementGenerator (AssignInt (Variable t v) i) = "\t" ++ v ++ " = " ++ show i ++ ";\n"
statementGenerator (AssignStr (Variable t v) a) = "\t" ++ v ++ " = " ++ show a ++ ";\n"
statementGenerator (Call s ps) = "\tolivec_" ++ s ++ "(oc," ++ argsGenerator ps ++ ");\n"

argsGenerator :: [Param] -> String
argsGenerator [] = ""
argsGenerator [p] = argGenerator p
argsGenerator (p:ps) = argGenerator p ++ "," ++ argsGenerator ps

argGenerator :: Param -> String
argGenerator (IntParam i) = show i

varDeclsGenerator :: [Var] -> String
varDeclsGenerator = concatMap varDeclGenerator

varDeclGenerator :: Var -> String
varDeclGenerator (Variable Int v) = "int " ++ v ++ ";\n"
varDeclGenerator (Variable ConstCharP v) = "const char* " ++ v ++ ";\n"

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
  handle <- openFile s ReadMode
  contents <- hGetContents handle
  let c = parse codeParser "" contents
  case c of
    Left e -> print e
    Right code -> do {
    --   let variables = toList (fromList (concatMap extractVariablesProc code))
    -- ; let decl = varDeclsGenerator variables
    -- ; print ("Variables = ", variables, " => {" ++ decl ++ "}")
    ; writeFile o (codeGenerator code)
    ; putStrLn "This \"language\" requires the use of Tsoding's library OliveC and stbi_image."
    ; putStrLn "To get \"OliveC\", run \"wget https://raw.githubusercontent.com/tsoding/olive.c/master/olive.c\""
    ; putStrLn "To get \"stbi_image_write\", run \"wget https://raw.githubusercontent.com/nothings/stb/master/stb_image_write.h\""
    }
  -- print c
