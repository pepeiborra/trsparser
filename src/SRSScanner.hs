-----------------------------------------------------------------------------------------
{-| Module      : Scanner
    Copyright   : 
    License     : All Rights Reserved

    Maintainer  : Pepe Iborra
    Stability   : 
    Portability : 
-}
-----------------------------------------------------------------------------------------

module SRSScanner where
import Text.ParserCombinators.Parsec
import qualified Text.ParserCombinators.Parsec.Token as P
import Text.ParserCombinators.Parsec.Language( haskellStyle, emptyDef )

srsDef = emptyDef  { 
     P.commentStart   = ""
   , P.commentEnd     = ""
   , P.commentLine    = ""
   , P.nestedComments = True
   , P.identStart     = alphaNum <|> oneOf "<>~!\\·$%&/.:;-_{}[]^*+ç¡'¿?=#@|" -- noneOf " ()\","
   , P.identLetter    = P.identStart srsDef
   , P.opStart        = oneOf ")(\"-"
   , P.opLetter       = oneOf ")(\",>="
   , P.reservedNames= ["RULES","STRATEGY","LEFTMOST","RIGHTMOST"]
   , P.reservedOpNames  = ["->","->="]
   , P.caseSensitive  = True
   }
   
lexer :: P.TokenParser ()
lexer  = P.makeTokenParser srsDef
         
   
whiteSpace= P.whiteSpace lexer
lexeme    = P.lexeme lexer
symbol    = P.symbol lexer
natural   = P.natural lexer
parens    = P.parens lexer
comma     = P.comma lexer
semi      = P.semi lexer
identifier= P.identifier lexer
reserved  = P.reserved lexer
reservedOp= P.reservedOp lexer
commaSep  = P.commaSep lexer
stringLiteral = P.stringLiteral lexer
