-----------------------------------------------------------------------------------------
{-| Module      : SRS
    Copyright   : 
    License     : All Rights Reserved

    Maintainer  : Pepe Iborra
    Stability   : 
    Portability : 
-}
-----------------------------------------------------------------------------------------

module SRSTypes where
import Text.PrettyPrint
import Data.Maybe

data Spec = Spec [Decl] 
 deriving (Eq, Show)
 
data Decl = Rules [Rule]
          | Strat StratDecl
          | Any (Maybe String) [AnyContent]
 deriving (Eq, Show)

data Rule = Rule :->  Rule
          | Rule :->= Rule
          | Single [Id]
          
 deriving (Eq, Show)

data StratDecl = LeftMost | RightMost
 deriving (Eq, Show)

data AnyContent = AnyI Id 
                | AnyS String 
                | AnyA [AnyContent]
 deriving (Eq, Show)                

type Id = String     

---------------- INSTANCES -----------------
--------------------------------------------
 
{-instance Show Spec where
 showsPrec _ s rest = (render$ pprintSpec s) ++ rest
  -}
  
pprintSpec (Spec dd)     = vcat (map (parens.pprintDecl) dd)
pprintDecl (Rules rr)    = text "RULES" <+> (sep$ punctuate comma (map pprintRule rr))
pprintDecl (Strat x)     = text "STRATEGY" <+> text (show x)
pprintDecl (Any name cc) = text (maybe "" id name) <+> fsep (map pprintAnyC cc)
pprintRule (r1 :-> r2)   = pprintRule r1 <+> ptext ":->" <+> pprintRule r2 
pprintRule (r1 :->= r2)  = pprintRule r1 <+> ptext ":->=" <+> pprintRule r2
pprintRule (Single w1)   = text (unwords w1)
pprintAnyC (AnyI i)      = text i
pprintAnyC (AnyS s)      = text s
pprintAnyC (AnyA aa)     = fsep (map pprintAnyC aa)