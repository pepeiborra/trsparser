-----------------------------------------------------------------------------------------
{-| Module      : TRS
    Copyright   : 
    License     : All Rights Reserved

    Maintainer  : Pepe Iborra
    Stability   : 
    Portability : 
-}
-----------------------------------------------------------------------------------------

module TRS where


data Spec = Spec [Decl]
 deriving (Eq, Show)
 
data Decl = Var [Id] 
	  | Theory [TheoryDecl]
	  | Rules [Rule]
	  | Strategy Strategy
	  | Any (Maybe String) [AnyContent]
 deriving (Eq, Show)

data TheoryDecl = TVar [Id] 
		| Equations [Equation]
 deriving (Eq, Show)
 
data Equation = Term :==: Term
 deriving (Eq, Show)

data SimpleRule = Term :->  Term	
	        | Term :->= Term
 deriving (Eq, Show)

data Rule = Rule SimpleRule [Cond]
 deriving (Eq, Show)

data Term = T Id [Term]
 deriving (Eq, Show)

data Cond = Term :-><-: Term
	  | Term  :->:  Term
 deriving (Eq, Show)
	  
data Strategy = InnerMost | OuterMost | Context [(Id, [Int])]
 deriving (Eq, Show)
	  
data AnyContent = AnyI Id 
                | AnyS String 
                | AnyA [AnyContent]
 deriving (Eq, Show)
                
type Id = String	  
{-
instance Functor Decl where
 fmap f (Var ii)          = Var (map f ii)
 fmap f (Theory ttdd)     = Theory (map f ttdd)
 fmap f (Rules rr)	  = Rules (map f rr)
 fmap f Strategy st	  = Strategy (f st)
 fmap f (Any ms aa)	  = Any ms (map f aa)
-}