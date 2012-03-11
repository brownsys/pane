module Lexer where

import Prelude hiding (lex)
import Text.Parsec
import Text.Parsec.Token (GenLanguageDef (..), GenTokenParser)
import qualified Text.Parsec.Token as T
import Control.Monad.Identity

javascriptDef :: Monad m
              => GenLanguageDef String u m
javascriptDef =
  T.LanguageDef "/*"
                "*/"
                "//"
                False -- no nested comments
                (alphaNum <|> oneOf "$_")
                -- eventually, will want to handle : in IPv6 addresses
                (alphaNum <|> oneOf "$_") -- identifier rest
                (oneOf "{}<>()~.,?:|&^=!+-*/%!") -- operator start
                (oneOf "=<>|&+") -- operator rest
                [ "Tick", "AddUser", "NewShare", "Grant", "on", "True", "False",
                  "from", "to", "forever", "reserve", "allow", "deny",
                  "reserveTBCapacity", "reserveTBFill",
                  "GrantDefault", "for", "strict", "partial", "GetSchedule",
                  "ListShares", "ListSharesByFlowGroup"
                -- "DropUser", "AddNetwork", "DropNetwork"
                ]
                [ "*", "(", ")", ",", ":" ]
                 True -- case-sensitive

lex :: Monad m
    => GenTokenParser String u m
lex = T.makeTokenParser javascriptDef

identifier :: Monad m => ParsecT String u m String
identifier = T.identifier	 lex

reserved :: Monad m => String -> ParsecT String u m ()
reserved = T.reserved	 lex

reservedOp :: Monad m => String -> ParsecT String u m ()
reservedOp = T.reservedOp lex	

charLiteral :: Monad m => ParsecT String u m Char
charLiteral = T.charLiteral lex	

integer :: Monad m => ParsecT String u m Integer
integer = T.integer lex	

parens :: Monad m => ParsecT String u m a -> ParsecT String u m a
parens = T.parens	 lex

braces :: Monad m => ParsecT String u m a -> ParsecT String u m a
braces = T.braces	 lex

brackets :: Monad m => ParsecT String u m a -> ParsecT String u m a
brackets = T.brackets lex

semi :: Monad m => ParsecT String u m String
semi = T.semi	 lex

comma :: Monad m => ParsecT String u m String
comma = T.comma	 lex

colon:: Monad m => ParsecT String u m String
colon = T.colon lex	

dot :: Monad m => ParsecT String u m String
dot = T.dot lex
