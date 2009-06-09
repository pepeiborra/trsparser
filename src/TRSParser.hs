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
import Data.Set (Set)
import qualified Data.Set as Set

type Vars = Set Id
type TRSParser a = GenParser Char Vars a

trsParser :: TRSParser Spec
trsParser = liftM Spec (many1 (whiteSpace >> parens decl))

decl, declVar, declTheory, declRules, declStrategy :: TRSParser Decl
decl = (declVar <|> declTheory <|> declRules <|> declStrategy <|> declAny)

declRules  = reserved "RULES" >> liftM Rules (many rule)
declVar    = do
  reserved "VAR"
  vv <- phrase
  setState (Set.fromList vv)
  return (Var vv)

declTheory = reserved "THEORY" >> liftM Theory (many$ parens theory)

theory =     (phrase >>= return.TVar)
	 <|>
	     (many equation >>= return.Equations)

equation :: TRSParser Equation
equation =
 do t1 <- term
    reservedOp "=="
    t2 <- term
    return (t1 :==: t2)

term :: TRSParser Term
term =
 do n <- identifier
    env <- getState
    terms <- option [] (parens (commaSep' term))
    return $ if n `Set.member` env && null terms
             then var n
             else mkT n terms

declStrategy =
 do reserved "STRATEGY" >> liftM Strategy (inn <|> out <|> ctx)

inn, out, ctx :: TRSParser Strategy
inn = reserved "INNERMOST"  >> return InnerMost
out = reserved "OUTERMOST"  >> return OuterMost
ctx = 
 do reserved "CONTEXTSENSITIVE" 
    strats <- many$ parens (do a <- identifier
		               b <- many natural
		               return (a, map fromInteger b))
    return$ Context strats  
         
rule :: TRSParser Rule
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

anyContent, anyI, anyS, anyA :: TRSParser AnyContent
anyContent = anyI <|> anyS <|> anyA <|> (comma >> anyContent)

anyI = liftM AnyI identifier
anyS = liftM AnyS stringLiteral
anyA = liftM AnyA (parens$ many anyContent)
        
        
phrase = many identifier
commaSep' = (`sepEndBy` comma)
