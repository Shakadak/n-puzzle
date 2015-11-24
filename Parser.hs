module Parser where
import Text.Parsec
import Control.Applicative
import Text.Parsec.String

parseGrid :: SourceName -> String -> Either ParseError [Int]
parseGrid = parse seqGrid

seqGrid :: Parser [Int]
seqGrid = do
    skipMany comment
    size <- lexeme' natural
    grid <- count size (skipMany comment *> gridLine size)
    lexeme' (skipMany comment)
    pure $ concat grid

gridLine :: Int -> Parser [Int]
gridLine n = count n (lexeme' natural)

natural :: Parser Int
natural = read <$> many1 digit

lexeme :: Parser a -> Parser a
lexeme p = do
    x <- p
    spaces
    return x

lexeme' :: Parser a -> Parser a
lexeme' p = p <* spaces

parseWithWSEof :: Parser a -> SourceName -> String -> Either ParseError a
parseWithWSEof p = parseWithEof (spaces *> p)

parseWithEof :: Parser a -> SourceName -> String -> Either ParseError a
parseWithEof p = parse (p <* eof)

comment :: Parser String
comment = (:) <$> char '#' <*> manyTill anyChar newline
