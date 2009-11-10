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
import Text.PrettyPrint.HughesPJClass
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
 
instance Pretty Spec where
  pPrint (Spec dd)     = vcat (map (parens.pPrint) dd)

instance Pretty Decl where
  pPrint (Rules rr)    = text "RULES" <+> (sep$ punctuate comma (map pPrint rr))
  pPrint (Strat x)     = text "STRATEGY" <+> text (show x)
  pPrint (Any name cc) = text (maybe "" id name) <+> fsep (map pPrint cc)

instance Pretty Rule where
  pPrint (r1 :-> r2)   = pPrint r1 <+> ptext ":->" <+> pPrint r2
  pPrint (r1 :->= r2)  = pPrint r1 <+> ptext ":->=" <+> pPrint r2
  pPrint (Single w1)   = text (unwords w1)

instance Pretty AnyContent where
  pPrint (AnyI i)      = text i
  pPrint (AnyS s)      = text s
  pPrint (AnyA aa)     = fsep (map pPrint aa)