import Control.Applicative
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

mulP :: Parser Int
mulP = stringP "mul(" *> mul <* charP ')'
    where mul = (*) <$> num <* charP ',' <*> num

ignore :: Parser Int
ignore = Parser $ \input -> case input of
    []     -> Nothing
    (_:xs) -> Just (xs, 0)


parse :: Parser [Int]
parse = many (mulP <|> ignore)

solve :: String -> Int
solve input = 
    case runParser parse input of
        Just (rest, final) -> sum final
        Nothing -> 0

main :: IO ()
main = do
    input <- readFile "input.txt"
    print $ solve input
