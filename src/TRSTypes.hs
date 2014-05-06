
{-# LANGUAGE TypeFamilies #-}
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

import Control.Applicative
import Data.Foldable
import Data.Monoid (Monoid(..))
import Data.Term hiding (Term, TermF, Id)
import Data.Term.Simple
import Data.Term.Variables
import qualified Data.Term.Family as Family
import Data.Traversable
import Text.PrettyPrint.HughesPJClass hiding (Mode)

type Spec = SpecF Decl
data SpecF a = Spec [a]
 deriving (Eq, Show)

type Decl = DeclF Term
data DeclF a = Var [Id]
	     | Theory [TheoryDecl]
             | Pairs [RuleF a]
	     | Rules [RuleF a]
	     | Strategy (Strategy a)
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


data Strategy a = GoalStrategy a
                | InnerMost
                | OuterMost
                | Context [(Id, [Int])]
                | Narrowing
                | ConstructorNarrowing
                | BasicNarrowing
                | InnermostNarrowing
                | Other String
 deriving (Eq, Ord, Show)

data AnyContent = AnyI Id
                | AnyS String
                | AnyA [AnyContent]
 deriving (Eq, Show)

type Id = String


-- --------------------------------------
-- Family instances for TRS syntax types
-- --------------------------------------

type instance Family.Var (DeclF a) = Family.Var a
type instance Family.TermF (DeclF a) = Family.TermF a
type instance Family.Id (DeclF a) = Family.Id a

instance GetVars a => GetVars (DeclF a) where getVars = foldMap getVars

-- -------------------
-- Functor instances
-- -------------------

instance Functor SpecF where fmap f (Spec aa) = Spec (map f aa)
instance Functor DeclF where
    fmap _ (Var vv)     = Var vv
    fmap _ (Theory tt)  = Theory tt
    fmap f (Rules rr)   = Rules ((fmap.fmap) f rr)
    fmap f (Pairs rr)   = Pairs ((fmap.fmap) f rr)
    fmap f (Strategy s) = Strategy (fmap f s)
    fmap _ (Any ms c)   = Any ms c

instance Functor RuleF where fmap f (Rule sr c) = Rule (fmap f sr) ((fmap.fmap) f c)

instance Functor EquationF where fmap f (a :==: b) = f a :==: f b
instance Functor SimpleRuleF where
    fmap f (a :->  b) = f a :->  f b
    fmap f (a :->= b) = f a :->= f b
instance Functor CondF where
    fmap f (a :->: b)   = f a :->: f b
    fmap f (a :-><-: b) = f a :-><-: f b

instance Functor Strategy where fmap = fmapDefault
instance Foldable Strategy where foldMap = foldMapDefault

instance Traversable Strategy where
    traverse f (GoalStrategy a) = GoalStrategy <$> f a
    traverse _ InnerMost = pure InnerMost
    traverse _ OuterMost = pure OuterMost
    traverse _ (Context ct) = pure (Context ct)
    traverse _ Narrowing = pure Narrowing
    traverse _ ConstructorNarrowing = pure ConstructorNarrowing
    traverse _ BasicNarrowing = pure BasicNarrowing
    traverse _ InnermostNarrowing = pure InnermostNarrowing

instance Foldable SpecF where foldMap f (Spec aa) = foldMap f aa
instance Foldable DeclF where
    foldMap _ (Var vv)     = mempty
    foldMap _ (Theory tt)  = mempty
    foldMap f (Rules rr)   = ((foldMap.foldMap) f rr)
    foldMap f (Pairs rr)   = ((foldMap.foldMap) f rr)
    foldMap f (Strategy s) = foldMap f s
    foldMap _ (Any ms c)   = mempty

instance Foldable RuleF where foldMap f (Rule sr c) = foldMap f sr `mappend` (foldMap.foldMap) f c

instance Foldable EquationF where foldMap f (a :==: b) = f a `mappend` f b
instance Foldable SimpleRuleF where
    foldMap f (a :->  b) = f a `mappend` f b
    foldMap f (a :->= b) = f a `mappend` f b
instance Foldable CondF where
    foldMap f (a :->: b)   = f a `mappend` f b
    foldMap f (a :-><-: b) = f a `mappend` f b

instance Pretty a => Pretty (RuleF a) where
  pPrint (Rule sr c) = pPrint sr $$ nest 4 (text "if" <+> pPrint c)

instance Pretty a => Pretty (SimpleRuleF a) where
  pPrint (a :-> b) = pPrint a <+> text "->" <+> pPrint b
  pPrint (a :->= b) = pPrint a <+> text "->=" <+> pPrint b

instance Pretty a => Pretty (CondF a) where
  pPrint (a :->: b) = pPrint a <+> text "->" <+> pPrint b
  pPrint (a :-><-: b) = pPrint a <+> text "-><-" <+> pPrint b

