-----------------------------------------------------------------------------------------
{-| Module      : TRS
    Copyright   :
    License     : All Rights Reserved

    Maintainer  : Pepe Iborra
    Stability   :
    Portability :
-}
-----------------------------------------------------------------------------------------

module TRSTypes (module TRSTypes, var) where

import Data.Term hiding (Term)
import Data.Term.Simple
import Data.Term.Var

type Spec = SpecF Decl
data SpecF a = Spec [a]
 deriving (Eq, Show)

type Decl = DeclF Rule
data DeclF a = Var [Id]
	     | Theory [TheoryDecl]
             | Pairs [a]
	     | Rules [a]
	     | Strategy Strategy
	     | Any (Maybe String) [AnyContent]
 deriving (Eq, Show)

data TheoryDecl = TVar [Id]
		| Equations [Equation]
 deriving (Eq, Show)

type Term = Term1 String Var
mkT = term

type Equation = EquationF Term
data EquationF a = a :==: a
 deriving (Eq, Show)

type SimpleRule = SimpleRuleF Term
data SimpleRuleF a = a :->  a
	           | a :->= a
 deriving (Eq, Show)

type Rule = RuleF Term
data RuleF a = Rule (SimpleRuleF a) [CondF a]
 deriving (Eq, Show)

type Cond = CondF Term
data CondF a = a :-><-: a
	     | a  :->:  a
 deriving (Eq, Show)

data Strategy = InnerMost | OuterMost | Context [(Id, [Int])]
 deriving (Eq, Show)

data AnyContent = AnyI Id
                | AnyS String
                | AnyA [AnyContent]
 deriving (Eq, Show)

type Id = String

instance Functor SpecF where fmap f (Spec aa) = Spec (map f aa)
instance Functor DeclF where
    fmap _ (Var vv)     = Var vv
    fmap _ (Theory tt)  = Theory tt
    fmap f (Rules rr)   = Rules (map f rr)
    fmap f (Pairs rr)   = Pairs (map f rr)
    fmap _ (Strategy s) = Strategy s
    fmap _ (Any ms c)   = Any ms c

instance Functor RuleF where fmap f (Rule sr c) = Rule (fmap f sr) ((fmap.fmap) f c)

instance Functor EquationF where fmap f (a :==: b) = f a :==: f b
instance Functor SimpleRuleF where
    fmap f (a :->  b) = f a :->  f b
    fmap f (a :->= b) = f a :->= f b
instance Functor CondF where
    fmap f (a :->: b)   = f a :->: f b
    fmap f (a :-><-: b) = f a :-><-: f b