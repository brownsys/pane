module Lexer where

import Prelude hiding (lex)
import Text.ParserCombinators.Parsec
import qualified Text.ParserCombinators.Parsec.Token as T

identifierStart = (letter <|> oneOf "$_")

javascriptDef =
  T.LanguageDef "/*"
                "*/"
                "//"
                False -- no nested comments
                identifierStart
                (alphaNum <|> oneOf "$_") -- identifier rest
                (oneOf "{}<>()~.,?:|&^=!+-*/%!") -- operator start
                (oneOf "=<>|&+") -- operator rest
                [ "AddUser", "DropUser", "NewShare", "AddNetwork", "DropNetwork", "on"
                ]
                [ "*", "(", ")", ",", ":" ]
                 True -- case-sensitive
            
lex :: T.TokenParser st
lex = T.makeTokenParser javascriptDef

-- everything but commaSep and semiSep
identifier = T.identifier	 lex
reserved = T.reserved	 lex
operator = T.operator	 lex
reservedOp = T.reservedOp lex	
charLiteral = T.charLiteral lex	
stringLiteral = T.stringLiteral lex	
natural = T.natural lex	
integer = T.integer lex	
naturalOrFloat = T.naturalOrFloat lex	
decimal = T.decimal lex	
hexadecimal = T.hexadecimal lex	
octal = T.octal lex	
symbol = T.symbol lex	
whiteSpace = T.whiteSpace lex	
parens = T.parens	 lex
braces = T.braces	 lex
squares = T.squares lex	
semi = T.semi	 lex
comma = T.comma	 lex
colon = T.colon lex	
dot = T.dot lex
brackets = T.brackets lex
lexeme = T.lexeme lex

