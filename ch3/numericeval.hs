{-# OPTIONS_GHC -Wno-deprecations -Wno-missing-methods #-}
{-# LANGUAGE ExistentialQuantification #-}

module Main where 

import Control.Monad 
import System.Environment 
import Text.ParserCombinators.Parsec hiding (spaces)
import Control.Monad.Error
import System.IO hiding (try)

main :: IO ()
main = do 
    args <- getArgs 
    case length args of 
        0 -> runREPL
        1 -> evalAndPrint $ args !! 0 
        otherwise -> putStrLn ("Program only takes 0 or 1 argument.")

symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<=?>@^_~#"

spaces :: Parser () 
spaces = skipMany1 space 

flushStr :: String -> IO ()
flushStr str = putStr str >> hFlush stdout

readPrompt :: String -> IO String
readPrompt prompt = flushStr prompt >> getLine 

evalString :: String -> IO String 
evalString expr = return $ extractValue $ trapError (liftM show $ readExpr expr >>= eval)

evalAndPrint :: String -> IO ()
evalAndPrint expr = evalString expr >>= putStrLn

until_ :: (Monad m) => (a -> Bool) -> m a -> (a -> m ()) -> m ()
until_ pred prompt action = do 
                                result <- prompt 
                                if pred result then return () 
                                else action result >> until_ pred prompt action 

runREPL :: IO ()
runREPL = until_ (=="quit") (readPrompt "YDScheme>>> ") evalAndPrint

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
eval (List [Atom "if", pred, conseq, alt]) = do 
                                                 result <- eval pred 
                                                 case result of 
                                                     Bool False -> eval alt 
                                                     otherwise -> eval conseq 
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
              ("remainder", numericBinop rem),
              ("=", numBoolBinop(==)),
              ("<", numBoolBinop(<)),
              (">", numBoolBinop(>)),
              ("/=", numBoolBinop(/=)),
              (">=", numBoolBinop(>=)),
              ("<=", numBoolBinop(<=)),
              ("&&", boolBoolBinop(&&)),
              ("||", boolBoolBinop(||)),
              ("string=?", strBoolBinop(==)),
              ("string?", strBoolBinop(>)),
              ("string<=?", strBoolBinop(<=)),
              ("string>=?", strBoolBinop(>=)),
              ("car", car),
              ("cdr", cdr),
              ("cons", cons),
              ("eq?", eqv),
              ("eqv?", eqv),
              ("equal?", equal)
              ]

numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> ThrowsError LispVal
numericBinop op singleVal@[_] = throwError $ NumArgs 2 singleVal
numericBinop op nums = mapM unpackNum nums >>= return . Number . foldl1 op

boolBinop :: (LispVal -> ThrowsError a) -> (a -> a -> Bool) -> [LispVal] -> ThrowsError LispVal
boolBinop unpacker op args = if length args /= 2 then throwError $ NumArgs 2 args 
                             else do 
                                left <- unpacker $ args !! 0
                                right <- unpacker $ args !! 1
                                return $ Bool $ left `op` right 

numBoolBinop = boolBinop unpackNum
strBoolBinop = boolBinop unpackString 
boolBoolBinop = boolBinop unpackBool 

unpackNum :: LispVal -> ThrowsError Integer
unpackNum (Number n) = return n 
unpackNum (String n) = let parsed = reads n in 
                            if null parsed then throwError $ TypeMismatch "number" $ String n
                            else fst $ parsed !! 0
unpackNum (List [n]) = unpackNum n 
unpackNum wrongType = throwError $ TypeMismatch "number" wrongType 

unpackString :: LispVal -> ThrowsError String 
unpackString (String str) = return str 
unpackString (Number n) = return $ show n
unpackString (Bool bool) = return $ show bool 
unpackString wrongType = throwError $ TypeMismatch "string" wrongType

unpackBool :: LispVal -> ThrowsError Bool 
unpackBool (Bool bool) = return bool 
unpackBool wrongType = throwError $ TypeMismatch "bool" wrongType

data Unpacker = forall a. (Eq a) => AnyUnpacker (LispVal -> ThrowsError a)

unpackEquals :: LispVal -> LispVal -> Unpacker -> ThrowsError Bool 
unpackEquals x y (AnyUnpacker unpack) = do 
                                            unpackedx <- unpack x 
                                            unpackedy <- unpack y 
                                            return $ unpackedx == unpackedy
                                            `catchError` (const $ return False)

equal :: [LispVal] -> ThrowsError LispVal 
equal [x, y] = do 
                   primitiveEquals <- liftM or $ mapM (unpackEquals x y) [AnyUnpacker unpackNum, AnyUnpacker unpackString, AnyUnpacker unpackBool]
                   eqvEquals <- eqv [x, y]
                   return $ Bool $ (primitiveEquals || let (Bool bool) = eqvEquals in bool)
equal wrongNumArgs = throwError $ NumArgs 2 wrongNumArgs

car :: [LispVal] -> ThrowsError LispVal 
car [(List (x:xs))] = return x
car [DottedList (x:xs) _] = return x 
car [wrongType] = throwError $ TypeMismatch "pair" wrongType
car wrongNumArgs = throwError $ NumArgs 1 wrongNumArgs

cdr :: [LispVal] -> ThrowsError LispVal
cdr [(List (x:xs))] = return $ List xs 
cdr [DottedList (_:xs) x] = return $ DottedList xs x 
cdr [DottedList xs x] = return x 
cdr [wrongType] = throwError $ TypeMismatch "pair" wrongType
cdr wrongNumArgs = throwError $ NumArgs 1 wrongNumArgs

cons :: [LispVal] -> ThrowsError LispVal 
cons [x, List []] = return $ List [x]
cons [x, List xs] = return $ List $ [x] ++ xs
cons [x, DottedList xs last] = return $ DottedList ([x] ++ xs) last 
cons [x, y] = return $ DottedList [x] y 
cons wrongNumArgs = throwError $ NumArgs 2 wrongNumArgs

eqv :: [LispVal] -> ThrowsError LispVal 
eqv [(Bool x), (Bool y)] = return $ Bool $ x == y
eqv [(Number x), (Number y)] = return $ Bool $ x == y
eqv [(String x), (String y)] = return $ Bool $ x == y
eqv [(Atom x), (Atom y)] = return $ Bool $ x == y
eqv [(List x), (List y)] = return $ Bool $ (length x == length y) && (and $ map eqvPair $ zip x y)
    where eqvPair (a, b) = case eqv [a, b] of 
                               Left err -> False 
                               Right (Bool val) -> val   
eqv [(DottedList xs x), (DottedList ys y)] = eqv [List $ xs ++ [x], List $ ys ++ [y]]
eqv [_, _] = return $ Bool False
eqv wrongNumArgs = throwError $ NumArgs 2 wrongNumArgs

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
