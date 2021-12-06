{-# OPTIONS_GHC -Wno-deprecations -Wno-missing-methods #-}

module Main where 

import Control.Monad 
import System.Environment 
import Text.ParserCombinators.Parsec hiding (spaces)
import Control.Monad.Error

main :: IO ()
main = do 
    args <- getArgs 
    evaluated <- return $ liftM show $ readExpr (args !! 0) >>= eval 
    putStrLn $ extractValue $ trapError evaluated

symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<=?>@^_~#"

spaces :: Parser () 
spaces = skipMany1 space 

readExpr :: String -> ThrowsError LispVal
readExpr input = case parse parseExpr "lisp" input of
        Left err -> throwError $ Parser err
        Right val -> return val

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

instance Show LispVal where
    show = showVal

unwordsList :: [LispVal] -> String 
unwordsList = unwords . map showVal 

eval :: LispVal -> ThrowsError LispVal 
eval x@(String _) = return x
eval x@(Number _) = return x
eval x@(Bool _) = return x
eval (List [Atom "quote", x]) = return x
eval (List (Atom f : args)) = mapM eval args >>= apply f 
eval badForm = throwError $ BadSpecialForm "unrecognised bad form" badForm

apply :: String -> [LispVal] -> ThrowsError LispVal 
apply f args = maybe (throwError $ NotFunction "Unrecognised primitive function args" $ f) ($ args) (lookup f primitives)

primitives :: [(String, [LispVal] -> ThrowsError LispVal)]
primitives = [("+", numericBinop(+)),
              ("-", numericBinop(-)),
              ("*", numericBinop(*)),
              ("/", numericBinop div),
              ("mod", numericBinop mod),
              ("quotient", numericBinop quot),
              ("remainder", numericBinop rem)
              ]

numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> ThrowsError LispVal
numericBinop op singleVal@[_] = throwError $ NumArgs 2 singleVal
numericBinop op nums = mapM unpackNum nums >>= return . Number . foldl1 op

unpackNum :: LispVal -> ThrowsError Integer
unpackNum (Number n) = return n 
unpackNum (String n) = let parsed = reads n in 
                            if null parsed then throwError $ TypeMismatch "number" $ String n
                            else fst $ parsed !! 0
unpackNum (List [n]) = unpackNum n 
unpackNum wrongType = throwError $ TypeMismatch "number" wrongType 

data LispError = NumArgs Integer [LispVal]
                | TypeMismatch String LispVal 
                | Parser ParseError 
                | BadSpecialForm String LispVal 
                | NotFunction String String 
                | UnboundVar String String 
                | Default String 

instance Show LispError where show = showError

instance Read LispError

instance Error LispError where 
    noMsg = Default "An error has occurred"
    strMsg = Default

showError :: LispError -> String 
showError (NumArgs expected found) = "Expected " ++ show expected ++ " args: found values " ++ unwordsList found 
showError (TypeMismatch expected found) = "Invalid type: expected " ++ expected ++ ", found " ++ show found 
showError (Parser err) = "Parse error at " ++ show err 
showError (BadSpecialForm message form) = message ++ ": " ++ show form 
showError (NotFunction message function) = message ++ ": " ++ show function
showError (UnboundVar message name) = message ++ ": " ++ name

type ThrowsError = Either LispError 

trapError action = catchError action (return . show)

extractValue :: ThrowsError a -> a 
extractValue (Right val) = val 
