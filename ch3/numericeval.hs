module Main where 

import Control.Monad 
import System.Environment 
import Text.ParserCombinators.Parsec hiding (spaces)
import Numeric (readOct, readHex)

main :: IO ()
main = getArgs >>= putStrLn . show . eval . readExpr . (!! 0)

symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<=?>@^_~#"

spaces :: Parser () 
spaces = skipMany1 space 

readExpr :: String -> LispVal
readExpr input = case parse parseExpr "lisp" input of
        Left err -> String $ "No match: " ++ show err
        Right val -> val

data LispVal = Atom String 
               | List [LispVal] 
               | DottedList [LispVal] LispVal
               | Number Integer 
               | String String 
               | Bool Bool 

parseString :: Parser LispVal
parseString = do 
                char '"' 
                x <- many ( noneOf "\"" ) 
                char '"'
                return $ String x 

parseAtom :: Parser LispVal
parseAtom = do 
                first <- letter <|> symbol 
                rest <- many (letter <|> digit <|> symbol)
                let atom = [first] ++ rest 
                return $ case atom of 
                    "#t" -> Bool True 
                    "#f" -> Bool False 
                    otherwise -> Atom atom 

parseNumber :: Parser LispVal 
parseNumber = liftM (Number . read ) $ many1 digit

parseList :: Parser LispVal 
parseList = liftM List $ sepBy parseExpr spaces 

parseDottedList :: Parser LispVal 
parseDottedList = do 
                    head <- endBy parseExpr spaces 
                    tail <- char '.' >> spaces >> parseExpr 
                    return $ DottedList head tail 

parseQuoted :: Parser LispVal 
parseQuoted = do 
                char '\''
                x <- parseExpr
                return $ List [Atom "quote", x]

parseExpr :: Parser LispVal 
parseExpr = parseAtom 
            <|> parseString 
            <|> parseNumber
            <|> parseQuoted 
            <|> do 
                    char '(' 
                    x <- (try parseList) <|> parseDottedList
                    char ')'
                    return x

showVal :: LispVal -> String 
showVal (String content) = "\"" ++ content ++ "\""
showVal (Atom name) = name
showVal (Number n) = show n 
showVal (Bool True) = "#t"
showVal (Bool False) ="#f"
showVal (List content) = "(" ++ unwordsList content ++ ")"
showVal (DottedList head tail) = "(" ++ unwordsList head ++ " . " ++ showVal tail ++ ")"

unwordsList :: [LispVal] -> String 
unwordsList = unwords . map showVal 

instance Show LispVal where
    show = showVal

eval :: LispVal -> LispVal 
eval x@(String _) = x
eval x@(Number _) = x
eval x@(Bool _) = x
eval (List [Atom "quote", x]) = x
eval (List (Atom f : args)) = apply f $ map eval args

apply :: String -> [LispVal] -> LispVal 
apply f args = maybe (Bool False) ($ args) $ lookup f primitives 

primitives :: [(String, [LispVal] -> LispVal)]
primitives = [("+", numericBinop(+)),
              ("-", numericBinop(-)),
              ("*", numericBinop(*)),
              ("/", numericBinop div),
              ("mod", numericBinop mod),
              ("quotient", numericBinop quot),
              ("remainder", numericBinop rem)
              ]

numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> LispVal
numericBinop op nums = Number $ foldl1 op $ map unpackNum nums 

unpackNum :: LispVal -> Integer 
unpackNum (Number n) = n 
unpackNum (String str) = let parsed = reads str in 
                            if null parsed then 0
                            else fst $ parsed !! 0
unpackNum (List [n]) = unpackNum n 
unpackNum _ = 0