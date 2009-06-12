-----------------------------------------------------------------------------------------
{-| Module      : TRSScanner
    Copyright   : 
    License     : All Rights Reserved

    Maintainer  : 
    Stability   : 
    Portability : 
-}
-----------------------------------------------------------------------------------------

module TRSScanner where
import Text.ParserCombinators.Parsec
import qualified Text.ParserCombinators.Parsec.Token as P
import Text.ParserCombinators.Parsec.Language( haskellStyle, emptyDef )
import SRSScanner hiding (lexer)

--lexer :: P.TokenParser ()
lexer  = P.makeTokenParser $
         srsDef  { 
     P.opLetter       = oneOf ")(\",><="
   , P.reservedNames= ["RULES"
                      ,"PAIRS"
                      ,"STRATEGY"
                      ,"INNERMOST"
                      ,"OUTERMOST"
                      ,"NARROWING"
                      ,"BASICNARROWING"
                      ,"CONSTRUCTORNARROWING"
                      ,"INNERMOSTNARROWING"
                      ,"NARROWINGM"
                      ,"BASICNARROWINGM"
                      ,"CONSTRUCTORNARROWINGM"
                      ,"INNERMOSTNARROWINGM"
                      ,"CONTEXTSENSITIVE"
                      ,"EQUATIONS"
                      ,"THEORY"
                      ,"VAR"]
   , P.reservedOpNames  = ["->","->=","==","-><-","|"]
   , P.caseSensitive  = True
   }
   
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

