-----------------------------------------------------------------------------------------
{-| Module      : TRS
    Copyright   :
    License     : All Rights Reserved

    Maintainer  : Pepe Iborra
    Stability   :
    Portability :
-}
-----------------------------------------------------------------------------------------

module TRSTypes (module TRSTypes, TermF(..)) where

import Data.Foldable
import Data.Monoid (Monoid(..))
import Data.Term hiding (Term)
import Data.Term.Simple

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

type Term = Term1 String String
mkT = term
var :: String -> Term
var = return

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


data Strategy = InnerMost
              | OuterMost
              | Context [(Id, [Int])]
              | Narrowing
              | NarrowingG Goal
              | ConstructorNarrowing
              | ConstructorNarrowingG Goal
              | BasicNarrowing
              | BasicNarrowingG Goal
              | InnermostNarrowing
              | InnermostNarrowingG Goal
              | Other String
 deriving (Eq, Show)

type Goal = TermF String Mode
data Mode = G | V deriving (Eq, Bounded, Show)

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


instance Foldable SpecF where foldMap f (Spec aa) = foldMap f aa
instance Foldable DeclF where
    foldMap _ (Var vv)     = mempty
    foldMap _ (Theory tt)  = mempty
    foldMap f (Rules rr)   = (foldMap f rr)
    foldMap f (Pairs rr)   = (foldMap f rr)
    foldMap _ (Strategy s) = mempty
    foldMap _ (Any ms c)   = mempty

instance Foldable RuleF where foldMap f (Rule sr c) = foldMap f sr `mappend` (foldMap.foldMap) f c

instance Foldable EquationF where foldMap f (a :==: b) = f a `mappend` f b
instance Foldable SimpleRuleF where
    foldMap f (a :->  b) = f a `mappend` f b
    foldMap f (a :->= b) = f a `mappend` f b
instance Foldable CondF where
    foldMap f (a :->: b)   = f a `mappend` f b
    foldMap f (a :-><-: b) = f a `mappend` f b