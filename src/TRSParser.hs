-----------------------------------------------------------------------------------------
{-| Module      : TRSParser
    Copyright   : 
    License     : All Rights Reserved

    Maintainer  : 
    Stability   : 
    Portability : 
-}
-----------------------------------------------------------------------------------------

module TRSParser (trsParser, term) where
import Text.ParserCombinators.Parsec
import TRSTypes
import TRSScanner
import Control.Monad

trsParser :: Parser Spec
trsParser = liftM Spec (many1 (whiteSpace >> parens decl))

decl, declVar, declTheory, declRules, declStrategy :: Parser Decl
decl = (declVar <|> declTheory <|> declRules <|> declStrategy <|> declAny)

declRules  = reserved "RULES" >> liftM Rules (many rule)
declVar    = reserved "VAR" >> liftM Var phrase
declTheory = reserved "THEORY" >> liftM Theory (many$ parens theory)
		   
theory =     (phrase >>= return.TVar)
	 <|> 
	     (many equation >>= return.Equations)

equation :: Parser Equation
equation = 
 do t1 <- term
    reservedOp "=="
    t2 <- term
    return (t1 :==: t2)

term :: Parser Term
term = 
 do n <- identifier
    terms <- option [] (parens (commaSep' term))
    return (T n terms)
          
declStrategy = 
 do reserved "STRATEGY" >> liftM Strategy (inn <|> out <|> ctx)
 
inn, out, ctx :: Parser Strategy
inn = reserved "INNERMOST"  >> return InnerMost
out = reserved "OUTERMOST"  >> return OuterMost
ctx = 
 do reserved "CONTEXTSENSITIVE" 
    strats <- many$ parens (do a <- identifier
		               b <- many natural
		               return (a, map fromInteger b))
    return$ Context strats  
         
rule :: Parser Rule
rule = 
 do sr <- simpleRule
    conds <- option [] (reservedOp "|" >> commaSep' cond)
    return (Rule sr conds)

simpleRule = 
 do t1 <- term
    op <- ruleOps
    t2 <- term
    return (op t1 t2)

ruleOps = (reservedOp "->"  >> return (:->))
      <|> (reservedOp "->=" >> return (:->=))

cond = 
 do t1 <- term
    op <- condOps
    t2 <- term
    return (op t1 t2)
    
condOps = (reservedOp "->"  >> return (:->:))
      <|> (reservedOp "-><-" >> return (:-><-:))
        
declAny  = 
 do name <- identifier 
    decls <- many anyContent
    return$ Any (Just name) decls

anyContent, anyI, anyS, anyA :: Parser AnyContent
anyContent = anyI <|> anyS <|> anyA <|> (comma >> anyContent)

anyI = liftM AnyI identifier
anyS = liftM AnyS stringLiteral
anyA = liftM AnyA (parens$ many anyContent)
        
        
phrase = many identifier
commaSep' = (`sepEndBy` comma)
