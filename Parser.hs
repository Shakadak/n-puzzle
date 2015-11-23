module Parser where
import Text.Parsec
import Control.Applicative
import Text.Parsec.String

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
