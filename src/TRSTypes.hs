-----------------------------------------------------------------------------------------
{-| Module      : TRS
    Copyright   : 
    License     : All Rights Reserved

    Maintainer  : Pepe Iborra
    Stability   : 
    Portability : 
-}
-----------------------------------------------------------------------------------------

module TRSTypes where

import Data.Typeable
import Data.Generics

data Spec = Spec [Decl]
 deriving (Eq, Show, Data, Typeable)
 
data Decl = Var [Id] 
	  | Theory [TheoryDecl]
	  | Rules [Rule]
	  | Strategy Strategy
	  | Any (Maybe String) [AnyContent]
 deriving (Eq, Show, Data, Typeable)

data TheoryDecl = TVar [Id] 
		| Equations [Equation]
 deriving (Eq, Show, Data, Typeable)
 
data Equation = Term :==: Term
 deriving (Eq, Show, Data, Typeable)

data SimpleRule = Term :->  Term	
	        | Term :->= Term
 deriving (Eq, Show, Data, Typeable)

data Rule = Rule SimpleRule [Cond]
 deriving (Eq, Show, Data, Typeable)

data Term = T Id [Term]
 deriving (Eq, Show, Data, Typeable)

data Cond = Term :-><-: Term
	  | Term  :->:  Term
 deriving (Eq, Show, Data, Typeable)
	  
data Strategy = InnerMost | OuterMost | Context [(Id, [Int])]
 deriving (Eq, Show, Data, Typeable)
	  
data AnyContent = AnyI Id 
                | AnyS String 
                | AnyA [AnyContent]
 deriving (Eq, Show, Data, Typeable)
                
type Id = String	  
{-
instance Functor Decl where
 fmap f (Var ii)          = Var (map f ii)
 fmap f (Theory ttdd)     = Theory (map f ttdd)
 fmap f (Rules rr)	  = Rules (map f rr)
 fmap f Strategy st	  = Strategy (f st)
 fmap f (Any ms aa)	  = Any ms (map f aa)
-}