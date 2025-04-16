import Control.Applicative
import Control.Monad.State
import Data.Char

newtype Parser a = Parser {
    runParser :: String -> Maybe (String, a)
}

instance Functor Parser where
    fmap f (Parser p) = Parser $ \input -> do
        (rest, x) <- p input
        Just (rest, f x)

instance Applicative Parser where
    pure x =  Parser $ \input -> Just (input, x)
    (Parser p1) <*> (Parser p2) = Parser $ \input -> do
        (rest, f) <- p1 input
        (rest', x) <- p2 rest
        Just (rest', f x)

instance Monad Parser where
    (Parser p) >>= f = Parser $ \input -> do
        (input', x) <- p input
        runParser (f x) input'

instance Alternative Parser where
    empty = Parser $ \_ -> Nothing
    (Parser p1) <|> (Parser p2) = Parser $ \input -> p1 input <|> p2 input

charP :: Char -> Parser Char
charP x = Parser f
    where
        f (y:ys)
            | y == x = Just (ys, x)
            | otherwise = Nothing
        f [] = Nothing

stringP :: String -> Parser String
stringP = sequenceA . map charP

spanP :: (Char -> Bool) -> Parser String
spanP f = Parser $ \input -> let (token, rest) = span f input
    in Just (rest, token)


num :: Parser Int
num = read <$> spanP isDigit

mulP :: StateT ParserEnabled Parser Int
mulP = do
    state <- get
    if state
        then lift $ stringP "mul(" *> mul <* charP ')'
        else lift ignore
    where mul = (*) <$> num <* charP ',' <*> num

ignore :: Parser Int
ignore = Parser $ \input -> case input of
    []     -> Nothing
    (_:xs) -> Just (xs, 0)


parse :: StateT ParserEnabled Parser [Int]
parse = many (mulP <|> doP *> lift ignore <|> dontP *> lift ignore <|> lift ignore)

solve :: String -> Int
solve = undefined
-- solve input = 
--     case execStateT (parse >>= runParser) True of
--         Just (rest, final) -> sum final
--         Nothing -> 0

type ParserEnabled = Bool

doP :: StateT ParserEnabled Parser ()
doP = lift (stringP "do()") >> put True

dontP :: StateT ParserEnabled Parser ()
dontP = lift (stringP "don't") >> put False

main :: IO ()
main = do
    input <- readFile "input.txt"
    print $ solve input
