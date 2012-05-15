module Lexer where

import Prelude hiding (lex)
import Text.Parsec
import qualified Text.Parsec.Token as T

paneDef = T.LanguageDef {
    T.commentStart = "/*",
    T.commentEnd = "*/",
    T.commentLine = "//",
    T.nestedComments = False,
    T.identStart = alphaNum <|> oneOf "$_",
    T.identLetter = alphaNum <|> oneOf "$_",
    T.opStart = oneOf "{}<>()~.,?:|&^=!+-*/%!",
    T.opLetter = oneOf "=<>|&+",
    T.reservedNames = [ "AddUser", "NewShare", "Grant", "on", "True", "False",
                      "from", "to", "forever", "reserve", "allow", "deny",
                      "reserveTBCapacity", "reserveTBFill",
                      "GrantDefault", "for", "strict", "partial", "GetSchedule",
                      "ListShares", "ListSharesByFlowGroup"
                   -- "DropUser", "AddNetwork", "DropNetwork"
                   ],
    T.reservedOpNames = [ "*", "(", ")", ",", ":" ],
    T.caseSensitive = True
}

lex = T.makeTokenParser paneDef

identifier = T.identifier	 lex

reserved = T.reserved	 lex

reservedOp = T.reservedOp lex	

charLiteral = T.charLiteral lex	

integer = T.integer lex	

parens = T.parens	 lex

braces = T.braces	 lex

brackets = T.brackets lex

semi = T.semi	 lex

comma = T.comma	 lex

colon = T.colon lex	

dot = T.dot lex
