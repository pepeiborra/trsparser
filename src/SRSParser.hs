-----------------------------------------------------------------------------------------
{-| Module      : SRSParser
    Copyright   : 
    License     : All Rights Reserved

    Maintainer  : Pepe Iborra
    Stability   : 
    Portability : 
-}
-----------------------------------------------------------------------------------------

module SRSParser(srsParser) where
import Text.ParserCombinators.Parsec
import SRS
import SRSScanner
import Control.Monad

srsParser :: Parser Spec
srsParser = liftM Spec (many (parens decl))

decl, declRules, declStrategy, declAny :: Parser Decl
decl = declRules <|> declStrategy <|> declAny
    
declRules = 
 do reserved "RULES" >> liftM Rules (commaSep' rule)

declStrategy = 
 do reserved "STRATEGY" >> liftM Strat (leftMost <|> rightMost)

declAny  = 
 do name <- identifier 
    decls <- many anyContent
    return$ Any (Just name) decls

anyContent, anyI, anyS, anyA :: Parser AnyContent
anyContent = anyI <|> anyS <|> anyA <|> (comma >> anyContent)
		    
rule :: Parser Rule
rule = liftM Single word `chainl1` ruleOps

ruleOps :: Parser (Rule -> Rule -> Rule)
ruleOps =    (reservedOp "->"  >> return (:->))
	  <|> 
             (reservedOp "->=" >> return (:->=))

word :: Parser [Id]
word = many1$ identifier

leftMost  = reserved "LEFTMOST"  >> return LeftMost
rightMost = reserved "RIGHTMOST" >> return RightMost

anyI = liftM AnyI identifier
anyS = liftM AnyS stringLiteral
anyA = liftM AnyA (parens$ many anyContent)

commaSep' = (`sepEndBy` comma)
