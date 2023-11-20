module Parser (parse_expr, parse_code) where

import Control.Monad
import Control.Applicative
import Expr
import Data.Char

{--
data Expr = Variable String
          | Function String Expr
          | Application Expr Expr
          | Macro String
--}

{--
    <expr> ::= <func_expr> | <atom_expr>

    <func_expr> ::= '\' <variable> '.' <expr>

    <atom_expr> ::= <paren_expr> | <var_expr>

    <paren_expr> ::= '(' <expr> ')'

    <var_expr> ::= <variable>

    <variable> ::= 'a' | 'b' | 'c' | ... | 'z'

--}


-- Parser data type
newtype Parser a = Parser {
    parse :: String -> Maybe(a, String)
}

--- type declaration ---

instance Monad Parser where
    
    mp >>= f = Parser $ \s -> case parse mp s of
        Nothing -> Nothing
        Just(x, s') -> parse (f x) s'
    return x = Parser $ \s -> Just (x, s)

instance Applicative Parser where
    pure x = return x
    pf <*> px = do
        f <- pf
        x <- px
        return $ f x

instance Functor Parser where
    fmap f px = do
        x <- px
        return $ f x

--- type declaration over ---

instance Alternative Parser where
    empty = failParser
    p1 <|> p2 = Parser $ \s -> case parse p1 s of
        Nothing -> parse p2 s
        ok->ok

failParser :: Parser a
failParser = Parser $ \s -> Nothing

charParser :: Char -> Parser Char
charParser c = Parser $ charP
    where charP [] = Nothing
          charP (x:xs) 
            | x == c = Just (c, xs)
            | otherwise = Nothing 

predicateParser :: (Char -> Bool) -> Parser Char
predicateParser p = Parser $ \s -> case s of
    (x:xs) -> if p x then Just(x, xs) else Nothing
    _ -> Nothing

plusParser :: (Parser a) -> Parser [a]
plusParser p = 
    do
        x <- p
        xs <- starParser p
        return (x:xs)

starParser :: (Parser a) -> Parser [a]
starParser p = (plusParser p) <|> (return [])

varParser :: Parser String
varParser =
    do
        x <- predicateParser isAlpha
        return [x]

stringParser :: Parser String
stringParser =
    do
        x <- predicateParser isAlpha
        xs <- starParser $ (predicateParser isAlphaNum)
        return (x : xs)


varExprParser :: Parser Expr
varExprParser = Variable <$> varParser

funcExprParser :: Parser Expr
funcExprParser = do
    charParser '\\'
    v <- stringParser
    charParser '.'
    e <- atomExprParser
    return $ Function v e

atomExprParser :: Parser Expr
atomExprParser =  macroParser <|> parenExprParser <|> varExprParser <|> funcExprParser

macroParser :: Parser Expr
macroParser = do
    charParser '$'
    name <- stringParser
    return $ Macro name

parenExprParser :: Parser Expr
parenExprParser = do
    charParser '('
    e <- exprParser
    charParser ')'
    return e

simpleAppParser :: Parser Expr
simpleAppParser = 
    do
        e1 <- atomExprParser
        charParser ' '
        ex <- exprParser
        return $ Application ex e1

exprParser :: Parser Expr
exprParser = simpleAppParser <|> atomExprParser


revString :: String -> String
revString s = aux s [] []
  where
    aux :: String -> String -> String -> String
    aux [] wordAcc acc = wordAcc ++ acc
    aux (x:xs) wordAcc acc
      | x == '(' = aux (drop ((length (new_word xs)) + 1) xs) (wordAcc ++ "(" ++ (revString (new_word xs)) ++ ")") acc
      | x == ' ' = aux xs [] (" " ++ wordAcc ++ acc)
      | otherwise = aux xs (wordAcc ++ [x]) acc



new_word :: String -> String
new_word s = helper s [] 1
    where
        helper s acc 0 = init acc
        helper (x:xs) acc par
            | x == ')' = helper xs (acc ++ [x]) (par - 1)
            | x == '(' = helper xs (acc ++ [x]) (par + 1)
            | otherwise = helper xs (acc ++ [x]) par


parse_expr :: String -> Expr
parse_expr s = case parse exprParser (revString s) of
    Just (e, "") -> e
    Just (_, rest) -> error ("Parsing failed. Remaining input: " ++ rest)
    Nothing -> error ("Invalid input string. Or I messed up again. The latter is more likely.")

assignParser :: Parser Code
assignParser =
    do
        expr <- exprParser
        plusParser $ (predicateParser (\c -> c == ' '))
        charParser '='
        starParser $ (charParser ' ')
        name <- stringParser
        return $ Assign name expr

evaluateParser :: Parser Code
evaluateParser =
    do
        expr <- exprParser
        return $ Evaluate expr

codeParser :: Parser Code
codeParser = assignParser <|> evaluateParser

correctString :: String -> String
correctString s = auxHelp s []
    where
        auxHelp [] acc = acc
        auxHelp (x:xs) acc = if x /= '=' then auxHelp xs (acc ++ [x]) else auxHelp xs (acc ++ " = ")

-- TODO 4.2. parse code
parse_code :: String -> Code
parse_code s = case parse codeParser (revString (correctString s)) of
    Just (e, "") -> e
    Just (_, rest) -> error ("Parsing failed " ++ s ++ ". Remaining input: " ++ rest)
    Nothing -> error ("Invalid input string. Or I messed up again. The latter is more likely.")
