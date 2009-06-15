-----------------------------------------------------------------------------------------
{-| Module      : TRSParser
    Copyright   : 
    License     : All Rights Reserved

    Maintainer  : 
    Stability   : 
    Portability : 
-}
-----------------------------------------------------------------------------------------

module TRSParser (TRSParser, trsParser
                 , decl, term, identifier, modes, goal
                 , whiteSpace) where
import Text.ParserCombinators.Parsec
import TRSTypes
import TRSScanner
import Control.Monad
import Data.Term.Simple (TermF(..))
import Data.Set (Set)
import qualified Data.Set as Set

type Vars = Set Id
type TRSParser a = GenParser Char Vars a
type SParser s   = GenParser Char s

trsParser :: TRSParser Spec
trsParser = liftM Spec (many1 (whiteSpace >> parens decl))

decl, declVar, declTheory, declRules, declStrategy :: TRSParser Decl
decl = (declVar <|> declTheory <|> declRules <|> declStrategy <|> declAny)

declRules  = reserved "RULES" >> liftM Rules (many rule)
declPairs  = reserved "PAIRS" >> liftM Pairs (many rule)
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

modes :: SParser a [Mode]
modes = parens (mode `sepBy` char ',') where parens= between (char '(') (char ')')
mode  :: SParser a Mode
mode = (oneOf "gbi" >> return G) <|> (oneOf "vof" >> return V)

goal :: SParser a Goal
goal = return Term `ap` identifier `ap` modes

declStrategy = do
    reserved "STRATEGY"
    Strategy `liftM` msumP [ reserved "INNERMOST"  >> return InnerMost  -- probably needs to add try
                           , reserved "OUTERMOST"  >> return OuterMost
                           , ctx
                           , reserved "NARROWING"            >> maybe Narrowing            NarrowingG            <$> option Nothing (Just <$> goal)
                           , reserved "BASICNARROWING"       >> maybe BasicNarrowing       BasicNarrowingG       <$> option Nothing (Just <$> goal)
                           , reserved "INNERMOSTNARROWING"   >> maybe InnermostNarrowing   InnermostNarrowingG   <$> option Nothing (Just <$> goal)
                           , reserved "CONSTRUCTORNARROWING" >> maybe ConstructorNarrowing ConstructorNarrowingG <$> option Nothing (Just <$> goal)
                           , Other `liftM` identifier
                           ]

msumP = foldr (<|>) pzero
infixr 2 <$>
(<$>) = liftM
ctx :: TRSParser Strategy
ctx = do
    reserved "CONTEXTSENSITIVE" 
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
