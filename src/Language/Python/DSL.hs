{-|
Module      : Language.Python.DSL
Copyright   : (C) CSIRO 2017-2019
License     : BSD3
Maintainer  : Isaac Elliott <isaace71295@gmail.com>
Stability   : experimental
Portability : non-portable

Passing @[]@ to a function which expects a @['Raw' 'Line']@ is the same as
passing @['line_' 'pass_']@
-}


{-# language DataKinds #-}
{-# language FlexibleContexts #-}
{-# language MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances #-}
{-# language LambdaCase #-}
{-# language RankNTypes #-}
{-# language RecordWildCards #-}
{-# language TemplateHaskell #-}
{-# language TypeApplications #-}
{-# language TypeFamilies #-}
{-# language UndecidableInstances #-}
module Language.Python.DSL
  ( (&)
  , Raw
  , Module
  , Statement
  , Expr
    -- * Modules
  , module_
    -- * Lines of code
  , blank_
  , AsLine(..)
  , Line(..)
    -- * Identifiers
  , id_
  , Ident(..)
    -- ** Lenses
  , identAnn
  , identValue
  , identWhitespace
    -- * Starred values
  , StarSyntax(..)
  , star_
    -- * Double-starred values
  , DoubleStarSyntax(..)
    -- * @as@ syntax
  , As(..)
    -- * @if@ syntax
  , IfSyntax(..)
    -- * @for@ syntax
  , ForSyntax(..)
    -- * @in@ syntax
  , InSyntax(..), In(..), InList(..)
    -- * @:@ syntax
  , ColonSyntax(..)
    -- * Comprehensions
  , comp_
  , Guard(..)
    -- * Parameters and arguments
    -- ** Parameters
  , Param(..)
  , ParametersSyntax(..)
    -- ** Arguments
  , Arg(..)
  , ArgumentsSyntax(..)
    -- ** Positional
  , PositionalSyntax(..)
  , PositionalParam(..)
  , _PositionalParam
    -- *** Lenses
  , ppAnn
  , ppName
  , ppType
    -- ** Keyword
  , KeywordSyntax(..)
  , KeywordParam(..)
  , _KeywordParam
    -- *** Lenses
  , kpAnn
  , kpName
  , kpType
  , kpEquals
  , kpExpr
    -- * Decorators
  , decorated_
  , DecoratorsSyntax(..)
    -- * Statements
    -- ** @async@
  , AsyncSyntax(..)
    -- ** Block bodies
  , BodySyntax(..)
    -- ** Function definitions
  , def_
  , Fundef(..)
  , mkFundef
    -- *** Lenses
  , fdAnn
  , fdDecorators
  , fdIndents
  , fdAsync
  , fdDefSpaces
  , fdName
  , fdLeftParenSpaces
  , fdParameters
  , fdRightParenSpaces
  , fdReturnType
  , fdBody
    -- ** Class definitions
  , class_
  , ClassDef(..)
  , mkClassDef
    -- *** Lenses
  , cdAnn
  , cdDecorators
  , cdIndents
  , cdClass
  , cdName
  , cdArguments
  , cdBody
    -- ** Assignment
  , chainEq
  , (.=)
  , (.+=)
  , (.-=)
  , (.*=)
  , (.@=)
  , (./=)
  , (.%=)
  , (.&=)
  , (.|=)
  , (.^=)
  , (.<<=)
  , (.>>=)
  , (.**=)
  , (.//=)
    -- ** Exceptions
  , tryE_
  , tryF_
  , ExceptSyntax(..)
  , FinallySyntax(..)
  , TryExcept(..)
  , mkTryExcept
  , TryFinally(..)
  , mkTryFinally
  , ExceptAs(..)
  , AsExceptAs(..)
  , Except(..)
  , mkExcept
  , Finally(..)
  , mkFinally
    -- *** Lenses
  , teAnn
  , teIndents
  , teTry
  , teBody
  , teExcepts
  , teElse
  , teFinally
  , exceptIndents
  , exceptExcept
  , exceptExceptAs
  , exceptBody
  , finallyIndents
  , finallyFinally
  , finallyBody
    -- ** With statements
  , with_
  , withItem_
  , With(..)
  , mkWith
  , AsWithItem(..)
  , WithItem(..)
    -- *** Lenses
  , withAnn
  , withIndents
  , withAsync
  , withWith
  , withItems
  , withBody
    -- ** Flow control
    -- *** 'Else' clauses
    -- | 'If', 'While', 'For', and 'TryExcept' statements can have an 'Else'
    -- component.
    --
    -- 'else_' is considered to be a modifier on these structures.
    --
    -- \-\-\-
    --
    -- 'If' ... 'Else':
    --
    -- >>> if_ false_ [line_ pass_] & else_ [line_ pass_]
    -- if False:
    --     pass
    -- else:
    --     pass
    --
    -- \-\-\-
    --
    -- 'While' ... 'Else':
    --
    -- >>> while_ false_ [line_ pass_] & else_ [line_ pass_]
    -- while False:
    --     pass
    -- else:
    --     pass
    --
    -- \-\-\-
    --
    -- 'For' ... 'Else':
    --
    -- >>> for_ (var_ "a" `in_` [var_ b]) [line_ pass_] & else_ [line_ pass_]
    -- for a in b:
    --     pass
    -- else:
    --     pass
    --
    -- \-\-\-
    --
    -- 'TryExcept' ... 'Else':
    --
    -- >>> tryE_ [line_ pass_] & except_ [line_ pass_] & else_ [line_ pass_]
    -- try:
    --     pass
    -- except:
    --     pass
    -- else:
    --     pass
  , else_
  , ElseSyntax(..)
    -- *** Break
  , break_
    -- *** For loops
    -- | 'For' loops are built using 'for_' syntax:
    --
    -- >>> for_ (var_ "a" `in_` [1, 2, 3]) [line_ (call_ "print" [var_ "a"])]
    -- for a in 1, 2, 3:
    --     print(a)
    --
    -- See also: 'ForSyntax'
  , forSt_
  , For(..)
  , _For
  , mkFor
    -- *** If statements
  , ifThen_
  , elif_
  , If(..)
  , mkIf
  , Elif(..)
  , mkElif
  , Else(..)
  , mkElse
    -- **** Lenses
  , ifAnn
  , ifIndents
  , ifIf
  , ifCond
  , ifBody
  , ifElifs
  , ifElse
  , elifIndents
  , elifElif
  , elifCond
  , elifBody
  , elseIndents
  , elseElse
  , elseBody
    -- *** Pass
  , pass_
    -- *** Return
  , return_
    -- *** While loops
  , while_
  , While(..)
  , mkWhile
    -- **** Lenses
  , whileAnn
  , whileIndents
  , whileWhile
  , whileCond
  , whileBody
    -- * Expressions
  , expr_
  , var_
    -- ** @await@
  , await_
    -- ** @... if ... else ...@
  , ifThenElse_
    -- ** Generators
  , gen_
    -- ** @yield@
  , yield_
    -- ** @yield from ...@
  , yieldFrom_
    -- ** Tuples
  , tuple_
  , Tuple(..)
  , AsTupleItem(..)
  , TupleItem()
    -- ** Function calls
  , call_
  , Call(..)
  , mkCall
    -- *** Lenses
  , callAnn
  , callFunction
  , callLeftParen
  , callArguments
  , callRightParen
    -- ** Literals
    -- *** @None@
  , none_
  , None(..)
  , _None
    -- **** Lenses
  , noneAnn
  , noneWhitespace
    -- *** Strings
  , str_
  , str'_
  , longStr_
  , longStr'_
    -- *** Integers
  , int_
    -- *** Booleans
  , true_
  , false_
    -- *** Ellipses
  , ellipsis_
    -- ** Lists
  , AsList(..)
  , AsListItem(..)
  , ListItem()
    -- ** Dictionaries
  , AsDict(..)
  , DictItem()
    -- ** Sets
  , AsSet(..)
  , AsSetItem(..)
  , SetItem()
    -- ** Lambdas
  , lambda_
    -- ** Subscripting
  , subs_
    -- *** Slicing
  , sliceF_
  , sliceFS_
  , sliceT_
  , sliceTS_
  , sliceFT_
  , sliceFTS_
  , sliceS_
  , fullSlice_
  , slice_
    -- ** Dereferencing
  , (/>)
    -- ** Unary operators
  , not_
  , neg_
  , pos_
  , compl_
    -- ** Binary operators
    -- | Comparison, bitwise, and arithmetic operators have precedences that are
    -- consistent with their Python counterparts. This meansPython expressions can
    -- be translated to kellSyntax with minimal parentheses.
    --
    -- Note: this doesn't apply to unary operators (because kellSyntax doesn't have
    -- unary operators), or the boolean operations 'and_' and 'or_' (because we ran
    -- out of precedence levels)

    -- *** Boolean operations
  , or_
  , and_

    -- *** Comparison operations
  , is_
  , isNot_
  , notIn_
  , (.==)
  , (.>)
  , (.>=)
  , (.<)
  , (.<=)
  , (.!=)
    -- *** Bitwise operations
  , (.|)
  , (.^)
  , (.&)
  , (.<<)
  , (.>>)
    -- *** Arithmetic operations
  , (.-)
  , (.+)
  , (.*)
  , (.@)
  , (./)
  , (.//)
  , (.%)
  , (.**)
    -- * Miscellaneous
  , linesToBlock
  , blockToLines
  )
where

import Control.Applicative ((<|>))
import Control.Lens.Fold ((^..), (^?), folded, lengthOf)
import Control.Lens.Getter ((^.), to)
import Control.Lens.Iso (from)
import Control.Lens.Lens (Lens')
import Control.Lens.Prism (_Right, _Just)
import Control.Lens.Review ((#))
import Control.Lens.Setter ((.~), (<>~), (?~), (%~), Setter', set, over, mapped)
import Control.Lens.TH (makeWrapped)
import Control.Lens.Traversal (Traversal', traverseOf)
import Control.Lens.Tuple (_2)
import Control.Lens.Wrapped (_Wrapped)
import Data.Foldable (toList)
import Data.Function ((&))
import Data.String (fromString)
import Data.List.NonEmpty (NonEmpty(..))
import Data.Maybe (fromMaybe)
import Data.Semigroup ((<>))

import Language.Python.Optics
import Language.Python.Syntax.Ann
import Language.Python.Syntax.AugAssign
import Language.Python.Syntax.CommaSep
import Language.Python.Syntax.Expr
import Language.Python.Syntax.Ident
import Language.Python.Syntax.Module
import Language.Python.Syntax.Operator.Binary
import Language.Python.Syntax.Operator.Unary
import Language.Python.Syntax.Punctuation
import Language.Python.Syntax.Raw
import Language.Python.Syntax.Statement
import Language.Python.Syntax.Strings
import Language.Python.Syntax.Types
import Language.Python.Syntax.Whitespace

-- | 'Ident' has an 'Data.String.IsString' instance, but when a type class dispatches on
-- an 'Ident' we will run into ambiguity if we try to use @OverloadedStrings@. In these
-- cases we can use 'id_' to provide the extra type information
id_ :: String -> Raw Ident
id_ = fromString

-- | Create a 'Module'
--
-- >>> module_
-- >>> [ line_ $ def_ "a" [] [line_ pass_]
-- >>> , blank_
-- >>> , line_ $ def_ "b" [] [line_ pass_]
-- >>> ]
-- def a():
--     pass
-- <BLANKLINE>
-- def b():
--     pass
module_ :: [Raw Line] -> Raw Module
module_ [] = ModuleEmpty
module_ (a:as) =
  case unLine a of
    Left (bl, nl) -> ModuleBlank bl nl $ module_ as
    Right a -> ModuleStatement a $ module_ as

-- | One or more lines of Python code
newtype Line v a
  = Line
  { unLine :: Either (Blank a, Newline) (Statement v a)
  } deriving (Eq, Show)
makeWrapped ''Line

-- | Create a blank 'Line'
blank_ :: Raw Line
blank_ = Line $ Left (Blank (Ann ()) [] Nothing, LF)

-- | Convert some data to a 'Line'
class AsLine s where
  line_ :: Raw s -> Raw Line

instance AsLine SmallStatement where
  line_ ss =
    Line . Right $ SmallStatement (Indents [] (Ann ())) ss

instance AsLine SimpleStatement where
  line_ ss =
    Line . Right . SmallStatement (Indents [] (Ann ())) $
    MkSmallStatement ss [] Nothing Nothing (Just LF)

instance AsLine CompoundStatement where
  line_ = Line . Right . CompoundStatement

instance AsLine ClassDef where
  line_ = line_ @Statement . (_ClassDef #)

instance AsLine Fundef where
  line_ = line_ @Statement . (_Fundef #)

instance AsLine If where
  line_ = line_ @Statement . (_If #)

instance AsLine While where
  line_ = line_ @Statement . (_While #)

instance AsLine With where
  line_ = line_ @Statement . (_With #)

instance AsLine Statement where
  line_ = Line . Right

instance AsLine Expr where
  line_ e = line_ $ Expr (e ^. annot) e

instance HasExprs Line where
  _Exprs f (Line a) = Line <$> (_Right._Exprs) f a

instance HasStatements Line where
  _Statements f (Line a) = Line <$> _Right f a

class BodySyntax s where
  -- | A faux-Lens that targets lines in the body of some statement-piece, but
  -- does so \'around\' indentation.
  --
  -- >>> def_ "a" [] [ line_ pass_, line_ pass_ ]
  -- def a ():
  --     pass
  --     pass
  --
  -- >>> def_ "a" [] [ line_ pass_, line_ pass_ ] ^. body_
  -- pass
  -- pass
  --
  -- >>> def_ "a" [] [ line_ pass_, line_ pass_ ] & body_ .~ [ line_ $ var_ "b" += 1 ]
  -- def a():
  --     b += 1
  --
  -- >>> def_ "a" [] [ line_ pass_, line_ pass_ ] & body_ <>~ [ line_ $ var_ "b" += 1 ]
  -- def a():
  --     pass
  --     pass
  --     b += 1
  --
  -- >>> def_ "a" [] [ line_ pass_, line_ pass_ ] & body_ .~ []
  -- def a():
  --     pass
  --
  -- \-\-\-
  --
  -- It's a fake 'Lens' because it violates some of the laws. The most obvious violation is
  -- that setting the 'body_' to the empty list actually sets it to a singleton list containing
  -- 'pass_'. (This is because blocks must contain one or more statements)
  body_ :: Functor f => ([Raw Line] -> f [Raw Line]) -> Raw s -> f (Raw s)
  body :: Lens' (Raw s) (Raw Suite)

class ColonSyntax s t | s -> t, t -> s where
  (.:) :: Raw s -> Raw Expr -> Raw t

infix 0 .:

-- | Constructing dictionary items
--
-- @('.:') :: 'Raw' 'Expr' -> 'Raw' 'Expr' -> 'Raw' 'DictItem'@
instance ColonSyntax Expr DictItem where
  (.:) a = DictItem (Ann ()) a (MkColon [Space])

-- | Function parameter type annotations
--
-- @('.:') :: 'Raw' 'Param' -> 'Raw' 'Expr' -> 'Raw' 'Param'@
--
-- 'star_' can be annotated using '.:', but it will have no effect on the output program,
-- as unnamed starred parameters cannot have type annotations.
--
-- See 'def_'
instance ColonSyntax Param Param where
  (.:) p t = p & paramType_ ?~ (MkColon [Space], t)

-- | Positional parameters/arguments
--
-- @
-- p_ :: 'Raw' 'Expr' -> 'Raw' 'Arg'
-- @
--
-- @
-- p_ :: 'Raw' 'Ident' -> 'Raw' 'Param'
-- @
class PositionalSyntax p v | p -> v, v -> p where
  p_ :: Raw v -> Raw p

-- | See 'def_'
instance StarSyntax Ident Param where
  s_ i = StarParam (Ann ()) [] i Nothing

-- | See 'def_'
instance DoubleStarSyntax Ident Param where
  ss_ i = DoubleStarParam (Ann ()) [] i Nothing

class StarSyntax s t | t -> s where
  s_ :: Raw s -> Raw t

-- | See 'call_'
instance StarSyntax Expr Arg where
  s_ = StarArg (Ann ()) []

-- | See 'call_'
instance DoubleStarSyntax Expr Arg where
  ss_ = DoubleStarArg (Ann ()) []

-- | Keyword parameters/arguments
--
-- @
-- p_ :: 'Raw' 'Expr' -> 'Raw' 'Expr' -> 'Raw' 'Arg'
-- @
--
-- @
-- p_ :: 'Raw' 'Ident' -> 'Raw' 'Expr' -> 'Raw' 'Param'
-- @
class KeywordSyntax p where
  k_ :: Raw Ident -> Raw Expr -> Raw p

-- | Unnamed starred parameter
--
-- >>> def_ "a" [ p_ "b", star_ ] [ line_ pass_ ]
-- def a(b, *):
--     pass
star_ :: Raw Param
star_ = UnnamedStarParam (Ann ()) []

class DoubleStarSyntax s t | t -> s where
  ss_ :: Raw s -> Raw t

-- | See 'dict_'
instance DoubleStarSyntax Expr DictItem where
  ss_ = DictUnpack (Ann ()) []

-- | See 'def_'
instance PositionalSyntax Param Ident where
  p_ i = PositionalParam (Ann ()) i Nothing

-- | See 'def_'
instance KeywordSyntax Param where
  k_ a = KeywordParam (Ann ()) a Nothing []

-- | See 'call_'
instance PositionalSyntax Arg Expr where; p_ = PositionalArg (Ann ())

-- | See 'call_'
instance KeywordSyntax Arg where; k_ a = KeywordArg (Ann ()) a []

class ParametersSyntax s where
  -- | A faux-Lens that allows targeting 'Param's in-between existing formatting,
  -- and adding appropriate formatting when extra parameters are introduced.
  --
  -- >>> showStatement myStatement
  -- "def a(b ,  c   ):\n    pass"
  --
  -- >>> showStatement (myStatement & _Fundef.parameters_ .~ [p_ "d", p_ "e"]
  -- "def a(d ,  e   ):\n    pass"
  --
  -- >>> showStatement (myStatement & _Fundef.parameters_ .~ [p_ "d", p_ "e", p_ "f"]
  -- "def a(d ,  e   , f):\n    pass"
  --
  -- \-\-\-
  --
  -- It's not a 'Lens' because repeated 'set's can drop trailing commas, violating
  -- the 'Lens' laws. For example:
  --
  -- >>> someFunction
  -- def a(b, c,):
  --     pass
  --
  -- >>> set parameters_ [var_ "d", var_ "e"] someFunction
  -- def a(d, e,):
  --     pass
  --
  -- >>> set parameters_ [] someFunction
  -- def a():
  --     pass
  --
  -- >>> set parameters_ [var_ "d", var_ "e"] (set parameters_ [] someFunction)
  -- def a(d, e):
  --     pass
  parameters_ :: Functor f => ([Raw Param] -> f [Raw Param]) -> Raw s -> f (Raw s)
  parameters :: Lens' (Raw s) (CommaSep (Raw Param))

class ArgumentsSyntax s where
  setArguments :: [Raw Arg] -> Raw s -> Raw s
  getArguments :: Raw s -> [Raw Arg]

class DecoratorsSyntax s where
  setDecorators :: [Raw Expr] -> Raw s -> Raw s
  getDecorators :: Raw s -> [Raw Expr]
  decorators :: Lens' (Raw s) [Raw Decorator]

decorated_ :: DecoratorsSyntax s => [Raw Expr] -> Raw s -> Raw s
decorated_ = setDecorators

exprsToDecorators :: Indents () -> [Raw Expr] -> [Raw Decorator]
exprsToDecorators is = fmap (\e -> Decorator (Ann ()) is (MkAt []) e Nothing LF [])

instance DecoratorsSyntax Fundef where
  decorators = fdDecorators

  setDecorators new code =
    code
    { _fdDecorators = exprsToDecorators (_fdIndents code) new
    }

  getDecorators code = code ^.. fdDecorators.folded._Exprs

blockToLines :: Raw Block -> [Raw Line]
blockToLines (Block x y z) = fmap (Line . Left) x <> (Line (Right y) : fmap Line z)

mkBody_
  :: Traversal' (Raw s) (Indents ())
  -> Lens' (Raw s) (Raw Suite)
  -> forall f. Functor f => ([Raw Line] -> f [Raw Line]) -> Raw s -> f (Raw s)
mkBody_ gIndents gBody f e =
  (\ls -> e & gBody._Blocks .~ mkNewBlock allIndents ls id) <$> blLines'
  where
    -- | The default indent amount is the indentation level of the first statement
    -- in a block. If the first statement has no indentation, it defaults to 4
    -- spaces.
    defaultIndent =
      fromMaybe
        (Indents [replicate 4 Space ^. from indentWhitespaces] (Ann ()))
        (e ^? gIndents)

    -- | The number of indentation chunks that precede the lines we're focusing on.
    --
    -- It's one more than @defaultIndent@.
    --
    -- For example, if we're looking at this code, which is inside some larger
    -- context:
    --
    -- @
    --     def a():
    --       pass
    -- @
    --
    -- @defaultIndent@ refers to this part:
    --
    -- @
    --      def a():
    --  ^^^^
    -- @
    --
    -- It's a single chunk. The code body has 2 (= one + 1) chunks:
    --
    -- @
    --     def a():
    --       pass
    -- ^^^^
    -- @
    --
    -- and
    --
    -- @
    --     def a():
    --       pass
    --     ^^
    -- @
    --
    -- So we will need to drop/take two chunks from the beginning of each line in
    -- the body.
    numChunks = lengthOf (indentsValue.folded) defaultIndent + 1

    -- | The lines of the block
    blLines = e ^.. gBody._Blocks.to blockToLines.folded

    -- | The lines of the block, with leading indentation chopped off appropriately
    --
    -- For example:
    --
    -- @
    --   def a():
    --      pass
    --      pass
    -- @
    --
    -- the unprocessed lines are:
    --
    -- @
    --      pass
    --      pass
    -- @
    --
    -- so the processed lines should be:
    --
    -- @
    --   pass
    --   pass
    -- @
    blLines' =
      f $
      over
        (mapped._Wrapped._Right._Indents.indentsValue)
        (drop numChunks)
        blLines

    -- | @defaultNewIndent@ is the amount of indentation that 'new' lines should get.
    -- 'New' lines are only introduced when we set the @[Raw Line]@ to a list longer
    -- than its original value.
    --
    -- @allIndents@ is a list of indentation corresponding to the indents of the old
    -- @[Raw Line]@
    defaultNewIndent :: Indents (); allIndents :: [Indents ()]
    (defaultNewIndent, allIndents) =
      foldr
        (\a (di, as) ->
           maybe
             (di, di : as)
             (\x -> (x, x : as))
             (a ^? to unLine._Right._Indents.to (indentsValue %~ take numChunks)))
        (defaultIndent, [])
        blLines

    -- | @mkNewBlock@ zips the old indentation with the new lines, but if the new
    -- list of lines is longer than the old one then the extra lines at the end
    -- are indented by @defaultNewIndent@
    mkNewBlock
      :: [Indents ()]
      -> [Raw Line]
      -> (Raw Block -> Raw Block)
      -> Raw Block
    mkNewBlock [] [] k =
      k $ Block [] (pass_ & _Indents %~ (defaultNewIndent <>)) []
    mkNewBlock (a:_) [] k =
      k $ Block [] (pass_ & _Indents %~ (a <>)) []
    mkNewBlock [] [b] k =
      k $
      either
        (\w -> Block [w] (pass_ & _Indents %~ (defaultNewIndent <>)) [])
        (\w -> Block [] (w & _Indents %~ (defaultNewIndent <>)) [])
        (unLine b)
    mkNewBlock (a:_) [b] k =
      k $
      either
        (\w -> Block [w] (pass_ & _Indents %~ (a <>)) [])
        (\w -> Block [] (w & _Indents %~ (a <>)) [])
        (unLine b)
    mkNewBlock [] (b:bs) k =
      mkNewBlock [] bs $
      \(Block x y z) ->
        k $
        either
          (\w -> Block (w:x) y z)
          (\w ->
             Block []
               (w & _Indents %~ (defaultNewIndent <>))
               ((Left <$> x) <> (Right y:z)))
          (unLine b)
    mkNewBlock (a:as) (b:bs) k =
      mkNewBlock as bs $
      \(Block x y z) ->
        k $
        either
          (\w -> Block (w:x) y z)
          (\w ->
              Block []
                (w & _Indents %~ (a <>))
                ((Left <$> x) <> (Right y:z)))
          (unLine b)

instance BodySyntax Fundef where
  body = fdBody
  body_ = mkBody_ fdIndents fdBody

instance ParametersSyntax Fundef where
  parameters_ f e = flip (set fdParameters) e . go ps <$> ps'
    where
      ps = e ^. fdParameters
      ps' = f $ toList ps

      go :: CommaSep (Raw Param) -> [Raw Param] -> CommaSep (Raw Param)
      go CommaSepNone [] = CommaSepNone
      go CommaSepNone (x:xs) = listToCommaSep $ x:xs
      go CommaSepOne{} [] = CommaSepNone
      go (CommaSepOne a) [x] =
        CommaSepOne $ x & trailingWhitespace .~ (a ^. trailingWhitespace)
      go (CommaSepOne a) (x:xs) =
        listToCommaSep $ (x & trailingWhitespace .~ (a ^. trailingWhitespace)) :xs
      go CommaSepMany{} [] = CommaSepNone
      go (CommaSepMany a b c) (x:xs) =
        CommaSepMany (x & trailingWhitespace .~ (a ^. trailingWhitespace)) b $ go c xs

  parameters = fdParameters

-- | Create a minimal valid function definition
mkFundef :: Raw Ident -> [Raw Line] -> Raw Fundef
mkFundef name body =
  MkFundef
  { _fdAnn = Ann ()
  , _fdDecorators = []
  , _fdIndents = Indents [] (Ann ())
  , _fdAsync = Nothing
  , _fdDefSpaces = pure Space
  , _fdName = name
  , _fdLeftParenSpaces = []
  , _fdParameters = CommaSepNone
  , _fdRightParenSpaces = []
  , _fdReturnType = Nothing
  , _fdBody = SuiteMany (Ann ()) (MkColon []) Nothing LF $ linesToBlockIndented body
  }

-- |
-- >>> def_ "f" [p_ "x"] [line_ $ return_ "x"]
-- def f(x):
--     return x
--
-- >>> def_ "f" [p_ "x", k_ "y" 2] [line_ $ return_ "x"]
-- def f(x, y=2):
--     return x
--
-- >>> def_ "f" [p_ "x", k_ "y" 2, s_ "z"] [line_ $ return_ "x"]
-- def f(x, y=2, *z):
--     return x
--
-- >>> def_ "f" [p_ "x", k_ "y" 2, s_ "z", ss_ "w"] [line_ $ return_ "x"]
-- def f(x, y=2, *z, **w)
--     return x
--
-- >>> def_ "f" [p_ "x" .: "String"] [line_ $ return_ "x"]
-- def f(x: String):
--     return x
def_ :: Raw Ident -> [Raw Param] -> [Raw Line] -> Raw Fundef
def_ name params body = (mkFundef name body) { _fdParameters = listToCommaSep params }

-- | Create a minimal valid 'Call'
mkCall :: Raw Expr -> Raw Call
mkCall e =
  MkCall
  { _callAnn = Ann ()
  , _callFunction = e
  , _callLeftParen = []
  , _callArguments = Nothing
  , _callRightParen = []
  }

instance ArgumentsSyntax Call where
  setArguments args code =
    code
    { _callArguments =
        case args of
          [] -> Nothing
          a:as -> Just $ (a, zip (repeat (MkComma [Space])) as, Nothing) ^. _CommaSep1'
    }

  getArguments code = _callArguments code ^.. folded.folded

-- |
-- >>> call_ "f" [p_ $ var_ "x"]
-- f(x)
--
-- >>> call_ "f" [p_ $ var_ "x", k_ "y" 2]
-- f(x, y=2)
--
-- >>> call_ "f" [p_ $ var_ "x", k_ "y" 2, s_ "z"]
-- f(x, y=2, *z)
--
-- >>> call_ "f" [p_ $ var_ "x", k_ "y" 2, s_ "z", ss_ "w"]
-- f(x, y=2, *z, **w)
call_ :: Raw Expr -> [Raw Arg] -> Raw Expr
call_ expr args =
  _Call #
  (mkCall expr)
  { _callArguments = 
    case args of
      [] -> Nothing
      a:as -> Just $ (a, zip (repeat (MkComma [Space])) as, Nothing) ^. _CommaSep1'
  }

-- |
-- >>> return_ (var_ "a")
-- return a
return_ :: Raw Expr -> Raw Statement
return_ e =
  SmallStatement
    (Indents [] (Ann ()))
    (MkSmallStatement (Return (Ann ()) [Space] $ Just e) [] Nothing Nothing (Just LF))

-- | Turns an 'Expr' into a 'Statement'
--
-- >>> expr_ (int_ 3)
-- 3
expr_ :: Raw Expr -> Raw Statement
expr_ e =
  SmallStatement
    (Indents [] (Ann ()))
    (MkSmallStatement (Expr (Ann ()) e) [] Nothing Nothing (Just LF))

-- |
-- >>> list_ [li_ $ var_ "a"]
-- [a]
--
-- >>> list_ [s_ $ var_ "a"]
-- [*a]
--
-- >>> list_ [li_ $ var_ "a", s_ $ var_ "b"]
-- [a, *b]
--
-- >>> list_ $ comp_ (var_ "a") (for_ $ var_ "a" `in_` list_ [li_ $ int_ 1, li_ $ int_ 2, li_ $ int_ 3]) [if_ $ var_ "a" .== 2]
-- [a for a in [1, 2, 3] if a == 2]
class AsList s where
  list_ :: s -> Raw Expr

class AsListItem s where
  -- | Create a 'ListItem'
  li_ :: Raw s -> Raw ListItem

instance AsListItem ListItem where
  li_ = id

instance AsListItem Expr where
  li_ = ListItem (Ann ())

-- | See 'list_'
instance StarSyntax Expr ListItem where
  s_ = ListUnpack (Ann ()) [] []

instance e ~ Raw ListItem => AsList [e] where
  list_ es = List (Ann ()) [] (listToCommaSep1' es) []

instance e ~ Comprehension Expr => AsList (Raw e) where
  list_ c = ListComp (Ann ()) [] c []

newtype Guard v a = MkGuard { unGuard :: Either (CompFor v a) (CompIf v a) }

class ForSyntax a x | a -> x where
  for_ :: Raw x -> a

-- |
-- @'for_' :: 'Raw' 'In' -> 'Raw' 'CompFor'@
--
-- >>> comp_ (var_ "a") (for_ $ var_ "a" `in_` var_ "b") []
-- a for a in b
instance ForSyntax (Raw CompFor) In where
  for_ (MkIn a b) = CompFor (Ann ()) [Space] a [Space] b

-- |
-- @'for_' :: 'Raw' 'In' -> 'Raw' 'Guard'@
--
-- >>> comp_ (var_ "a") (for_ $ var_ "a" `in_` var_ "b") [for_ $ var_ "c" `in_` var_ "d"]
-- a for a in b for c in d
instance ForSyntax (Raw Guard) In where
  for_ (MkIn a b) = MkGuard . Left $ CompFor (Ann ()) [Space] a [Space] b

class IfSyntax a where
  if_ :: Raw Expr -> a

-- |
-- @'if_' :: 'Raw' 'Expr' -> 'Raw' 'Guard'@
--
-- >>> comp_ (var_ "a") (for_ $ var_ "a" `in_` var_ "b") [if_ $ var_ "c" .== var_ "d"]
-- a for a in b if c == d
instance IfSyntax (Raw Guard) where
  if_ = MkGuard . Right . CompIf (Ann ()) [Space]

-- |
-- >>> set_ []
-- set()
--
-- >>> set_ [si_ $ var_ "a"]
-- {a}
--
-- >>> set_ [s_ $ var_ "a"]
-- {*a}
--
-- >>> set_ [si_ $ var_ "a", s_ $ var_ "b"]
-- {a, *b}
--
-- >>> set_ $ comp_ (var_ "a") (for_ $ var_ "a" `in_` set_ [si_ $ int_ 1, si_ $ int_ 2, si_ $ int_ 3]) [if_ $ var_ "a" .== 2]
-- {a for a in [1, 2, 3] if a == 2}
class AsSet s where
  set_ :: s -> Raw Expr

class AsSetItem s where
  -- | Create a 'SetItem'
  si_ :: Raw s -> Raw SetItem

instance AsSetItem SetItem where
  si_ = id

instance AsSetItem Expr where
  si_ = SetItem (Ann ())

-- | See 'set_'
instance StarSyntax Expr SetItem where
  s_ = SetUnpack (Ann ()) [] []

instance e ~ Raw SetItem => AsSet [e] where
  set_ es =
    case es of
      [] -> call_ (var_ "set") []
      a:as -> Set (Ann ()) [] ((a, zip (repeat (MkComma [Space])) as, Nothing) ^. _CommaSep1') []

instance e ~ Comprehension SetItem => AsSet (Raw e) where
  set_ c = SetComp (Ann ()) [] c []

comp_ :: Raw e -> Raw CompFor -> [Raw Guard] -> Raw (Comprehension e)
comp_ val cfor guards =
  Comprehension (Ann ())
    val
    (if null guards
     then cfor
     else cfor & trailingWhitespace .~ [Space])
    (unGuard <$> guards)

-- |
-- >>> gen_ $ comp_ (var_ "a") (for_ $ var_ "a" `in_` list_ [li_ $ int_ 1, li_ $ int_ 2, li_ $ int_ 3]) [if_ $ var_ "a" .== 2]
-- (a for a in [1, 2, 3] if a == 2)
gen_ :: Raw (Comprehension Expr) -> Raw Expr
gen_ = Generator (Ann ())

-- |
-- >>> dict_ [var_ "a" .: 1]
-- {a: 1}
--
-- >>> dict_ [ss_ $ var_ "a"]
-- {**a}
--
-- >>> dict_ [var_ "a" .: 1, ss_ $ var_ "b"]
-- {a: 1, **b}
--
-- >>> dict_ $ comp_ (var_ "a" .: 1) (for_ $ var_ "a" `in_` list_ [li_ $ int_ 1, li_ $ int_ 2, li_ $ int_ 3]) [if_ $ var_ "a" .== 2]
-- {a: 1 for a in [1, 2, 3] if a == 2}
class AsDict s where
  dict_ :: s -> Raw Expr

-- |
-- @'dict_' :: ['Raw' 'DictItem'] -> 'Raw' 'Expr'@
instance e ~ Raw DictItem => AsDict [e] where
  dict_ ds =
    Dict (Ann ())
    []
    (case ds of
       [] -> Nothing
       a:as -> Just $ (a, zip (repeat (MkComma [Space])) as, Nothing) ^. _CommaSep1')
    []

-- |
-- @'dict_' :: 'Raw' ('Comprehension' 'DictItem') -> 'Raw' 'Expr'@
instance e ~ Comprehension DictItem => AsDict (Raw e) where
  dict_ comp = DictComp (Ann ()) [] comp []

mkBinOp :: ([Whitespace] -> BinOp ()) -> Raw Expr -> Raw Expr -> Raw Expr
mkBinOp bop a = BinOp (Ann ()) (a & trailingWhitespace .~ [Space]) (bop [Space])

-- | @a is b@
is_ :: Raw Expr -> Raw Expr -> Raw Expr
is_ = mkBinOp $ Is (Ann ())
infixl 1 `is_`

-- |
-- >>> var_ "a" `in_` var_ "b"
-- a in b
data In v a = MkIn (Expr v a) (Expr v a)

-- |
-- >>> var_ "a" `in_` [var_ "b", var_ "c"]
-- a in b, c
data InList v a = MkInList (Expr v a) [Expr v a]

class InSyntax a x | a -> x, x -> a where
  in_ :: Raw Expr -> x -> Raw a
infixl 1 `in_`

-- | @a and b@
--
-- Does not have a precedence
and_ :: Raw Expr -> Raw Expr -> Raw Expr
and_ a = BinOp (Ann ()) (a & trailingWhitespace .~ [Space]) (BoolAnd (Ann ()) [Space])

-- | @a or b@
--
-- Does not have a precedence
or_ :: Raw Expr -> Raw Expr -> Raw Expr
or_ a = BinOp (Ann ()) (a & trailingWhitespace .~ [Space]) (BoolOr (Ann ()) [Space])

-- |
-- >>> var_ "a" `in_` var_ "b"
-- a in b
instance InSyntax Expr (Raw Expr) where
  in_ = mkBinOp $ In (Ann ())

-- | See 'for_'
instance e ~ Raw Expr => InSyntax InList [e] where
  in_ = MkInList

-- | @a not in b@
notIn_ :: Raw Expr -> Raw Expr -> Raw Expr
notIn_ = mkBinOp $ NotIn (Ann ()) [Space]
infixl 1 `notIn_`

-- | @a is not b@
isNot_ :: Raw Expr -> Raw Expr -> Raw Expr
isNot_ = mkBinOp $ IsNot (Ann ()) [Space]
infixl 1 `isNot_`

-- | @not a@
not_ :: Raw Expr -> Raw Expr
not_ = Not (Ann ()) [Space]

-- | @a == b@
(.==) :: Raw Expr -> Raw Expr -> Raw Expr
(.==) = mkBinOp $ Eq (Ann ())
infixl 1 .==

-- | @a < b@
(.<) :: Raw Expr -> Raw Expr -> Raw Expr
(.<) = mkBinOp $ Lt (Ann ())
infixl 1 .<

-- | @a <= b@
(.<=) :: Raw Expr -> Raw Expr -> Raw Expr
(.<=) = mkBinOp $ LtEq (Ann ())
infixl 1 .<=

-- | @a > b@
(.>) :: Raw Expr -> Raw Expr -> Raw Expr
(.>) = mkBinOp $ Gt (Ann ())
infixl 1 .>

-- | @a >= b@
(.>=) :: Raw Expr -> Raw Expr -> Raw Expr
(.>=) = mkBinOp $ GtEq (Ann ())
infixl 1 .>=

-- | @a != b@
(.!=) :: Raw Expr -> Raw Expr -> Raw Expr
(.!=) = mkBinOp $ NotEq (Ann ())
infixl 1 .!=

-- | @a | b@
(.|) :: Raw Expr -> Raw Expr -> Raw Expr
(.|) = mkBinOp $ BitOr (Ann ())
infixl 2 .|

-- | @a ^ b@
(.^) :: Raw Expr -> Raw Expr -> Raw Expr
(.^) = mkBinOp $ BitXor (Ann ())
infixl 3 .^

-- | @a & b@
(.&) :: Raw Expr -> Raw Expr -> Raw Expr 
(.&) = mkBinOp $ BitAnd (Ann ())
infixl 4 .&

-- | @a << b@
(.<<) :: Raw Expr -> Raw Expr -> Raw Expr 
(.<<) = mkBinOp $ ShiftLeft (Ann ())
infixl 5 .<<

-- | @a >> b@
(.>>) :: Raw Expr -> Raw Expr -> Raw Expr 
(.>>) = mkBinOp $ ShiftRight (Ann ())
infixl 5 .>>

-- | @a + b@
(.+) :: Raw Expr -> Raw Expr -> Raw Expr 
(.+) = (+)
infixl 6 .+

-- | @a - b@
(.-) :: Raw Expr -> Raw Expr -> Raw Expr 
(.-) = (-)
infixl 6 .-

-- | @a * b@
(.*) :: Raw Expr -> Raw Expr -> Raw Expr 
(.*) = (*)
infixl 7 .*

-- | @a \@ b@
(.@) :: Raw Expr -> Raw Expr -> Raw Expr
(.@) = mkBinOp $ At (Ann ())
infixl 7 .@

-- | @a / b@
(./) :: Raw Expr -> Raw Expr -> Raw Expr
(./) = mkBinOp $ Divide (Ann ())
infixl 7 ./

-- | @a // b@
(.//) :: Raw Expr -> Raw Expr -> Raw Expr
(.//) = mkBinOp $ FloorDivide (Ann ())
infixl 7 .//

-- | @a % b@
(.%) :: Raw Expr -> Raw Expr -> Raw Expr
(.%) = mkBinOp $ Percent (Ann ())
infixl 7 .%

-- | @a ** b@
(.**) :: Raw Expr -> Raw Expr -> Raw Expr
(.**) = mkBinOp $ Exp (Ann ())
infixr 8 .**

-- |
-- >>> var_ "a" /> var_ "b"
-- a.b
(/>) :: Raw Expr -> Raw Ident -> Raw Expr
(/>) a = Deref (Ann ()) a []
infixl 9 />

-- | @-a@
neg_ :: Raw Expr -> Raw Expr
neg_ = negate

-- | @+a@
pos_ :: Raw Expr -> Raw Expr
pos_ = UnOp (Ann ()) (Positive (Ann ()) [])

-- | @~a@
compl_ :: Raw Expr -> Raw Expr
compl_ = UnOp (Ann ()) (Complement (Ann ()) [])

-- | Convert a list of 'Line's to a 'Block', giving them 4 spaces of indentation
linesToBlockIndented :: [Raw Line] -> Raw Block
linesToBlockIndented = over _Indents (indentIt $ replicate 4 Space) . linesToBlock

-- | Convert a list of 'Line's to a 'Block', without indenting them
linesToBlock :: [Raw Line] -> Raw Block
linesToBlock = go
  where
    go [] = Block [] pass_ []
    go [y] =
      case unLine y of
        Left l -> Block [l] pass_ []
        Right st -> Block [] st []
    go (y:ys) =
      case unLine y of
        Left l ->
          case go ys of
            Block a b c -> Block (l:a) b c
        Right st -> Block [] st (unLine <$> ys)

instance BodySyntax While where
  body = whileBody
  body_ = mkBody_ whileIndents whileBody

instance ElseSyntax While where
  getElse = mkGetElse _whileIndents _whileElse
  setElse = mkSetElse _whileIndents whileElse

-- | Create a minimal valid 'While'
mkWhile :: Raw Expr -> [Raw Line] -> Raw While
mkWhile cond body =
  MkWhile
  { _whileAnn = Ann ()
  , _whileIndents = Indents [] (Ann ())
  , _whileWhile = [Space]
  , _whileCond = cond
  , _whileBody = SuiteMany (Ann ()) (MkColon []) Nothing LF $ linesToBlockIndented body
  , _whileElse = Nothing
  }

while_ :: Raw Expr -> [Raw Line] -> Raw While
while_ = mkWhile

-- | Create a minimal valid 'If'
mkIf :: Raw Expr -> [Raw Line] -> Raw If
mkIf cond body =
  MkIf
  { _ifAnn = Ann ()
  , _ifIndents = Indents [] (Ann ())
  , _ifIf = [Space]
  , _ifCond = cond
  , _ifBody = SuiteMany (Ann ()) (MkColon []) Nothing LF $ linesToBlockIndented body
  , _ifElifs = []
  , _ifElse = Nothing
  }

instance BodySyntax Elif where
  body = elifBody
  body_ = mkBody_ elifIndents elifBody 

instance BodySyntax Else where
  body = elseBody
  body_ = mkBody_ elseIndents elseBody 

instance BodySyntax If where
  body = ifBody
  body_ = mkBody_ ifIndents ifBody 

-- |
-- @'if_' :: 'Raw' 'Expr' -> ['Raw' 'Line'] -> 'Raw' 'If'@
--
-- >>> if_ (var_ "a" .< 10) [var_ "a" .+= 1]
-- if a < 10:
--     a += 1
instance (l ~ Raw Line, s ~ Raw If) => IfSyntax ([l] -> s) where
  if_ = mkIf

ifThen_ :: Raw Expr -> [Raw Line] -> Raw If
ifThen_ = mkIf

var_ :: String -> Raw Expr
var_ s = Ident (Ann ()) $ MkIdent (Ann ()) s []

-- |
-- >>> none_
-- None
none_ :: Raw Expr
none_ = None (Ann ()) []

-- | @'Raw' 'Expr'@ has a 'Num' instance, but sometimes we need to name integers
-- explicitly
--
-- >>> int_ 10
-- 10
int_ :: Integer -> Raw Expr
int_ = fromInteger

-- |
-- >>> pass_
-- pass
pass_ :: Raw Statement
pass_ =
  SmallStatement
    (Indents [] (Ann ()))
    (MkSmallStatement (Pass (Ann ()) []) [] Nothing Nothing (Just LF))

-- | Create a minimal valid 'Elif'
mkElif :: Raw Expr -> [Raw Line] -> Raw Elif
mkElif cond body =
  MkElif
  { _elifIndents = Indents [] (Ann ())
  , _elifElif = [Space]
  , _elifCond = cond
  , _elifBody = SuiteMany (Ann ()) (MkColon []) Nothing LF $ linesToBlockIndented body
  }

elif_ :: Raw Expr -> [Raw Line] -> Raw If -> Raw If
elif_ cond body code = code & ifElifs <>~ [mkElif cond body]

-- | Create a minimal valid 'Else'
mkElse :: [Raw Line] -> Raw Else
mkElse body =
  MkElse
  { _elseIndents = Indents [] (Ann ())
  , _elseElse = []
  , _elseBody = SuiteMany (Ann ()) (MkColon []) Nothing LF $ linesToBlockIndented body
  }

class ElseSyntax s where
  getElse :: Raw s -> Maybe (Raw Else)
  setElse :: [Whitespace] -> Maybe (Raw Else) -> Raw s -> Raw s

else_ :: ElseSyntax s => [Raw Line] -> Raw s -> Raw s
else_ body = setElse (replicate 4 Space) $ Just (mkElse body)

mkGetElse
  :: (Raw s -> Indents ())
  -> (Raw s -> Maybe (Raw Else))
  -> Raw s
  -> Maybe (Raw Else)
mkGetElse indentLevel elseField code =
  fromMaybe
    (error "malformed indentation in else block")
    (traverseOf
        (traverse._Indents)
        (subtractStart (indentLevel code))
        (elseField code))

mkSetElse
  :: (Raw s -> Indents ())
  -> Setter' (Raw s) (Maybe (Raw Else))
  -> [Whitespace]
  -> Maybe (Raw Else)
  -> Raw s
  -> Raw s
mkSetElse indentLevel elseField _ new code =
  code &
  elseField .~
    fmap (elseIndents .~ indentLevel code)
    (over
       (traverse._Indents.indentsValue)
       (indentLevel code ^. indentsValue <>)
       new)

instance ElseSyntax For where
  getElse = mkGetElse _forIndents _forElse
  setElse = mkSetElse _forIndents forElse

instance ElseSyntax If where
  getElse = mkGetElse _ifIndents _ifElse
  setElse = mkSetElse _ifIndents ifElse

instance ElseSyntax TryExcept where
  getElse = mkGetElse _teIndents _teElse
  setElse = mkSetElse _teIndents teElse

break_ :: Raw Statement
break_ =
  SmallStatement
    (Indents [] (Ann ()))
    (MkSmallStatement (Break (Ann ()) []) [] Nothing Nothing (Just LF))

-- |
-- >>> true_
-- True
true_ :: Raw Expr
true_ = Bool (Ann ()) True []

-- |
-- >>> false_
-- False
false_ :: Raw Expr
false_ = Bool (Ann ()) False []

-- | Double-quoted string
--
-- >>> str_ "asdf"
-- "asdf"
str_ :: String -> Raw Expr
str_ s =
  String (Ann ()) . pure $
  StringLiteral (Ann ()) Nothing ShortString DoubleQuote (Char_lit <$> s) []

-- | Single-quoted string
--
-- >>> str_ "asdf"
-- 'asdf'
str'_ :: String -> Raw Expr
str'_ s =
  String (Ann ()) . pure $
  StringLiteral (Ann ()) Nothing ShortString SingleQuote (Char_lit <$> s) []

-- | Long double-quoted string
--
-- >>> longStr_ "asdf"
-- """asdf"""
longStr_ :: String -> Raw Expr
longStr_ s =
  String (Ann ()) . pure $
  StringLiteral (Ann ()) Nothing LongString DoubleQuote (Char_lit <$> s) []

-- | Long single-quoted string
--
-- >>> longStr'_ "asdf"
-- '''asdf'''
longStr'_ :: String -> Raw Expr
longStr'_ s =
  String (Ann ()) . pure $
  StringLiteral (Ann ()) Nothing LongString SingleQuote (Char_lit <$> s) []

mkAugAssign :: AugAssignOp -> Raw Expr -> Raw Expr -> Raw Statement
mkAugAssign at a b =
  SmallStatement
    (Indents [] (Ann ()))
    (MkSmallStatement
       (AugAssign
          (Ann ())
          (a & trailingWhitespace .~ [Space])
          (MkAugAssign (Ann ()) at [Space]) b)
       []
       Nothing
       Nothing
       (Just LF))

-- | Chained assignment
--
-- >>> chainEq (var_ "a") []
-- a
--
-- >>> chainEq (var_ "a") [var_ "b", var_ "c"]
-- a = b = c
chainEq :: Raw Expr -> [Raw Expr] -> Raw Statement
chainEq t [] = expr_ t
chainEq t (a:as) =
  SmallStatement
    (Indents [] (Ann ()))
    (MkSmallStatement
       (Assign (Ann ()) t $ (,) (MkEquals [Space]) <$> (a :| as))
       []
       Nothing
       Nothing
       (Just LF))

-- | @a = b@
(.=) :: Raw Expr -> Raw Expr -> Raw Statement
(.=) a b =
  SmallStatement
    (Indents [] (Ann ()))
    (MkSmallStatement
       (Assign (Ann ()) (a & trailingWhitespace .~ [Space]) $ pure (MkEquals [Space], b))
       []
       Nothing
       Nothing
       (Just LF))
infix 0 .=

-- | @a += b@
(.+=) :: Raw Expr -> Raw Expr -> Raw Statement
(.+=) = mkAugAssign PlusEq
infix 0 .+=

-- | @a -= b@
(.-=) :: Raw Expr -> Raw Expr -> Raw Statement
(.-=) = mkAugAssign MinusEq
infix 0 .-=

-- | @a *= b@
(.*=) :: Raw Expr -> Raw Expr -> Raw Statement
(.*=) = mkAugAssign StarEq
infix 0 .*=

-- | @a @= b@
(.@=) :: Raw Expr -> Raw Expr -> Raw Statement
(.@=) = mkAugAssign AtEq
infix 0 .@=

-- | @a /= b@
(./=) :: Raw Expr -> Raw Expr -> Raw Statement
(./=) = mkAugAssign SlashEq
infix 0 ./=

-- | @a %= b@
(.%=) :: Raw Expr -> Raw Expr -> Raw Statement
(.%=) = mkAugAssign PercentEq
infix 0 .%=

-- | @a &= b@
(.&=) :: Raw Expr -> Raw Expr -> Raw Statement
(.&=) = mkAugAssign AmpersandEq
infix 0 .&=

-- | @a |= b@
(.|=) :: Raw Expr -> Raw Expr -> Raw Statement
(.|=) = mkAugAssign PipeEq
infix 0 .|=

-- | @a ^= b@
(.^=) :: Raw Expr -> Raw Expr -> Raw Statement
(.^=) = mkAugAssign CaretEq
infix 0 .^=

-- | @a <<= b@
(.<<=) :: Raw Expr -> Raw Expr -> Raw Statement
(.<<=) = mkAugAssign ShiftLeftEq
infix 0 .<<=

-- | @a >>= b@
(.>>=) :: Raw Expr -> Raw Expr -> Raw Statement
(.>>=) = mkAugAssign ShiftRightEq
infix 0 .>>=

-- | @a **= b@
(.**=) :: Raw Expr -> Raw Expr -> Raw Statement
(.**=) = mkAugAssign DoubleStarEq
infix 0 .**=

-- | @a //= b@
(.//=) :: Raw Expr -> Raw Expr -> Raw Statement
(.//=) = mkAugAssign DoubleSlashEq
infix 0 .//=

mkFor :: Raw Expr -> [Raw Expr] -> [Raw Line] -> Raw For
mkFor binder collection body =
  MkFor
  { _forAnn = Ann ()
  , _forIndents = Indents [] (Ann ())
  , _forAsync = Nothing
  , _forFor = [Space]
  , _forBinder = binder & trailingWhitespace .~ [Space]
  , _forIn = [Space]
  , _forCollection =
      fromMaybe
        (CommaSepOne1' (Unit (Ann ()) [] []) Nothing)
        (listToCommaSep1' collection)
  , _forBody = SuiteMany (Ann ()) (MkColon []) Nothing LF $ linesToBlockIndented body
  , _forElse = Nothing
  }

-- |
-- @'for_' :: 'Raw' 'InList' -> ['Raw' 'Line'] -> 'Raw' 'Statement'@
--
-- >>> for_ (var_ "a" `in_` [var_ "b"]) [line_ (var_ "c" .+= var_ "a")]
-- for a in b:
--     c += a
instance (l ~ [Raw Line], s ~ Raw For) => ForSyntax (l -> s) InList where
  for_ (MkInList a b) = mkFor a b

forSt_ :: Raw Expr -> [Raw Expr] -> [Raw Line] -> Raw For
forSt_ = mkFor

instance BodySyntax For where
  body = forBody
  body_ = mkBody_ forIndents forBody

instance AsLine For where
  line_ = line_ @Statement . (_For #)

class AsyncSyntax s where
  async_ :: Raw s -> Raw s

instance AsyncSyntax Fundef where
  async_ = fdAsync ?~ pure Space

instance AsyncSyntax For where
  async_ = forAsync ?~ pure Space

-- | Create a minimal valid 'Finally'
mkFinally :: [Raw Line] -> Raw Finally
mkFinally body =
  MkFinally
  { _finallyIndents = Indents [] (Ann ())
  , _finallyFinally = []
  , _finallyBody = SuiteMany (Ann ()) (MkColon []) Nothing LF $ linesToBlockIndented body
  }

-- | Create a minimal valid 'Except'
mkExcept :: [Raw Line] -> Raw Except
mkExcept body =
  MkExcept
  { _exceptIndents = Indents [] (Ann ())
  , _exceptExcept = []
  , _exceptExceptAs = Nothing
  , _exceptBody = SuiteMany (Ann ()) (MkColon []) Nothing LF $ linesToBlockIndented body
  }

-- | Create a minimal valid 'TryExcept'
mkTryExcept :: [Raw Line] -> Raw Except -> Raw TryExcept
mkTryExcept body except =
  MkTryExcept
  { _teAnn = Ann ()
  , _teIndents = Indents [] (Ann ())
  , _teTry = [Space]
  , _teBody = SuiteMany (Ann ()) (MkColon []) Nothing LF $ linesToBlockIndented body
  , _teExcepts = pure except
  , _teElse = Nothing
  , _teFinally = Nothing
  }

-- | Create a minimal valid 'TryFinally'
mkTryFinally :: [Raw Line] -> [Raw Line] -> Raw TryFinally
mkTryFinally body fBody =
  MkTryFinally
  { _tfAnn = Ann ()
  , _tfIndents = Indents [] (Ann ())
  , _tfTry = [Space]
  , _tfBody = SuiteMany (Ann ()) (MkColon []) Nothing LF $ linesToBlockIndented body
  , _tfFinally = mkFinally fBody
  }

class FinallySyntax s t | s -> t where
  finally_ :: [Raw Line] -> s -> Raw t

-- |
-- >>> tryE_ [line_ pass_] & finally_ [line_ pass_]
-- try:
--     pass
-- finally:
--     pass
--
-- >>> tryF_ [line_ pass_] [line_ (a .+= 1)] & finally_ [line_ pass_]
-- try:
--     pass
-- finally:
--     pass
--
-- >>> tryF_ [line_ pass_] & finally_ [line_ pass_]
-- try:
--     pass
-- finally:
--     pass
instance FinallySyntax (Raw TryExcept) TryExcept where
  finally_ body = teFinally ?~ mkFinally body

instance FinallySyntax (Raw TryFinally) TryFinally where
  finally_ body = tfFinally .~ mkFinally body

instance (a ~ [Raw Line], b ~ Raw TryFinally) => FinallySyntax (a -> b) TryFinally where
  finally_ body f = f body

instance BodySyntax TryExcept where
  body = teBody
  body_ = mkBody_ teIndents teBody

-- | @try ... except@ with optional @else@ and optional @finally@
--
-- >>> tryE_ [line_ pass_] [line_ ("a" .+= 1)]
-- try:
--     pass
-- except
--     a += 1
tryE_ :: [Raw Line] -> Raw Except -> Raw TryExcept
tryE_ = mkTryExcept

instance BodySyntax TryFinally where
  body = tfBody
  body_ = mkBody_ tfIndents tfBody 

-- |
-- @try ... finally@
--
-- >>> tryF_ [line_ pass_] [line_ ("a" .+= 1)]
-- try:
--     pass
-- finally:
--     a += 1
tryF_ :: [Raw Line] -> [Raw Line] -> Raw TryFinally
tryF_ = mkTryFinally

class AsExceptAs s where
  toExceptAs :: Raw s -> Raw ExceptAs

instance AsExceptAs ExceptAs where
  toExceptAs = id

instance AsExceptAs Expr where
  toExceptAs e = ExceptAs (Ann ()) e Nothing

class ExceptSyntax s where
  except_ :: [Raw Line] -> s -> Raw TryExcept
  -- | You can use 'exceptAs_' without a binder:
  --
  -- @'exceptAs_' :: 'Raw' 'Expr' -> ['Raw' 'Line'] -> 'Raw' s -> 'Raw' 'TryExcept'@
  --
  -- @
  -- 'exceptAs_' ('var_' \"Exception\") body
  -- @
  --
  -- or with a binder:
  --
  -- @'exceptAs_' :: 'Raw' 'ExceptAs' -> ['Raw' 'Line'] -> 'Raw' s -> 'Raw' 'TryExcept'@
  --
  -- @
  -- 'exceptAs_' ('var_' \"Exception\" \``as_`\` 'id_' "a") body
  -- @
  exceptAs_ :: AsExceptAs e => Raw e -> [Raw Line] -> s -> Raw TryExcept

-- |
-- @'except_' :: ['Raw' 'Line'] -> ('Raw' 'Except' -> 'Raw' 'TryExcept') -> 'Raw' 'TryExcept'@
--
-- @'exceptAs_' :: 'AsExceptAs' e => 'Raw' e -> ['Raw' 'Line'] -> ('Raw' 'Except' -> 'Raw' 'TryExcept') -> 'Raw' 'TryExcept'@
--
-- >>> tryE_ [var_ "a" .= 2] & except_ [var_ "a" .= 3]
-- try:
--     a = 2
-- except:
--     a = 3
--
-- >>> tryE_ [var_ "a" .= 2] & exceptAs_ (var_ "Exception" `as_` id_ "b") [var_ "a" .= 3]
-- try:
--     a = 2
-- except Exception as b:
--     a = 3
instance (e ~ Raw Except, s ~ Raw TryExcept) => ExceptSyntax (e -> s) where
  except_ body f = f $ mkExcept body
  exceptAs_ ea body f = f $ mkExcept body & exceptExceptAs ?~ toExceptAs ea

-- |
-- @'except_' :: ['Raw' 'Line'] -> 'Raw' 'TryExcept' -> 'Raw' 'TryExcept'@
--
-- @'exceptAs_' :: AsExceptAs => 'Raw' e -> ['Raw' 'Line'] -> 'Raw' 'TryExcept' -> 'Raw' 'TryExcept'@
--
-- @
-- (someTryStatement :: 'Raw' 'TryExcept') '&'
--   'except_' ['line_' 'pass_']
-- @
--
-- @
-- (someTryStatement :: 'Raw' 'TryExcept') '&'
--   'exceptAs_' ('var_' \"Exception\" \``as_`\` 'id_' "b") ['line_' 'pass_']
-- @
instance ExceptSyntax (Raw TryExcept) where
  except_ body = teExcepts %~ (<> pure (mkExcept body))
  exceptAs_ ea body =
    teExcepts %~ (<> pure (mkExcept body & exceptExceptAs ?~ toExceptAs ea))

-- |
-- @'except_' :: ['Raw' 'Line'] -> 'Raw' 'TryFinally' -> 'Raw' 'TryExcept'@
--
-- @'exceptAs_' :: AsExceptAs => 'Raw' e -> ['Raw' 'Line'] -> 'Raw' 'TryFinally' -> 'Raw' 'TryExcept'@
--
-- @
-- (someTryStatement :: 'Raw' 'TryFinally') '&'
--   'except_' ['line_' 'pass_']
-- @
--
-- @
-- (someTryStatement :: 'Raw' 'TryFinally') '&'
--   'exceptAs_' ('var_' \"Exception\" \``as_`\` 'id_' "b") ['line_' 'pass_']
-- @
instance ExceptSyntax (Raw TryFinally) where
  except_ body MkTryFinally{..} =
    MkTryExcept
    { _teAnn = _tfAnn
    , _teIndents = _tfIndents
    , _teTry = _tfTry
    , _teBody = _tfBody
    , _teExcepts = pure $ mkExcept body
    , _teElse = Nothing
    , _teFinally = Just _tfFinally
    }

  exceptAs_ ea body MkTryFinally{..} =
    MkTryExcept
    { _teAnn = _tfAnn
    , _teIndents = _tfIndents
    , _teTry = _tfTry
    , _teBody = _tfBody
    , _teExcepts = pure $ mkExcept body & exceptExceptAs ?~ toExceptAs ea
    , _teElse = Nothing
    , _teFinally = Just _tfFinally
    }

instance AsLine TryExcept where
  line_ = line_ @Statement . (_TryExcept #)

instance AsLine TryFinally where
  line_ = line_ @Statement . (_TryFinally #)

class As s t u | s t -> u, u -> s t where
  as_ :: Raw s -> Raw t -> Raw u

-- | See 'exceptAs_'
instance As Expr Ident ExceptAs where
  as_ e name = ExceptAs (Ann ()) e $ Just ([Space], name)

-- |
-- >>> class_ "A" [] [line_ pass_]
-- class A:
--     pass
class_ :: Raw Ident -> [Raw Arg] -> [Raw Line] -> Raw ClassDef
class_ name args body =
  (mkClassDef name body) {
    _cdArguments =
      case args of
        [] -> Nothing
        a:as -> Just ([], Just $ (a, zip (repeat (MkComma [Space])) as, Nothing) ^. _CommaSep1', [])
  }

-- | Create a minimal 'ClassDef'
mkClassDef :: Raw Ident -> [Raw Line] -> Raw ClassDef
mkClassDef name body =
  MkClassDef
  { _cdAnn = Ann ()
  , _cdDecorators = []
  , _cdIndents = Indents [] (Ann ())
  , _cdClass = Space :| []
  , _cdName = name
  , _cdArguments = Nothing
  , _cdBody = SuiteMany (Ann ()) (MkColon []) Nothing LF $ linesToBlockIndented body
  }

instance BodySyntax ClassDef where
  body = cdBody
  body_ = mkBody_ cdIndents cdBody 

instance DecoratorsSyntax ClassDef where
  decorators = cdDecorators

  setDecorators new code =
    code
    { _cdDecorators = exprsToDecorators (_cdIndents code) new
    }

  getDecorators code = code ^.. cdDecorators.folded._Exprs

instance ArgumentsSyntax ClassDef where
  setArguments args code =
    code
    { _cdArguments =
        case args of
          [] -> Nothing
          a:as -> Just ([], Just $ (a, zip (repeat (MkComma [Space])) as, Nothing) ^. _CommaSep1', [])
    }

  getArguments code = _cdArguments code ^.. folded._2.folded.folded

-- | Create a minimal valid 'With'
mkWith :: NonEmpty (Raw WithItem) -> [Raw Line] -> Raw With
mkWith items body =
  MkWith
  { _withAnn = Ann ()
  , _withIndents = Indents [] (Ann ())
  , _withAsync = Nothing
  , _withWith = [Space]
  , _withItems = listToCommaSep1 items
  , _withBody = SuiteMany (Ann ()) (MkColon []) Nothing LF $ linesToBlockIndented body
  }

-- |
--
-- @
-- with_ :: 'NonEmpty' ('Raw' 'Expr') -> ['Raw' 'Line'] -> 'Raw' 'Statement'
-- with_ :: 'NonEmpty' ('Raw' 'WithItem') -> ['Raw' 'Line'] -> 'Raw' 'Statement'
-- @
--
-- >>> with_ [var_ "a"] [line_ $ var_ "b"]
-- with a:
--     b
--
-- >>> with_ [var_ "a" `as_` id_ "name"] [line_ $ var_ "b"]
-- with a as name:
--     b
--
-- >>> with_ [withItem_ e Nothing] [line_ $ var_ "b"]
-- with a:
--     b
with_ :: AsWithItem e => NonEmpty (Raw e) -> [Raw Line] -> Raw With
with_ items = mkWith (toWithItem <$> items)

withItem_ :: Raw Expr -> Maybe (Raw Expr) -> Raw WithItem
withItem_ a b = WithItem (Ann ()) a ((,) [Space] <$> b)

-- | See 'with_'
instance As Expr Expr WithItem where
  as_ a b = WithItem (Ann ()) a $ Just ([Space], b)

class AsWithItem s where
  toWithItem :: Raw s -> Raw WithItem

instance AsWithItem Expr where
  toWithItem e = WithItem (Ann ()) e Nothing

instance AsWithItem WithItem where
  toWithItem = id

instance BodySyntax With where
  body = withBody
  body_ = mkBody_ withIndents withBody 

instance AsyncSyntax With where
  async_ = withAsync ?~ pure Space

-- |
-- >>> ellipsis_
-- ...
ellipsis_ :: Raw Expr
ellipsis_ = Ellipsis (Ann ()) []

class AsTupleItem e where
  -- | Create a 'TupleItem'
  ti_ :: Raw e -> Raw TupleItem

-- | See 'tuple_'
instance StarSyntax Expr TupleItem where
  s_ = TupleUnpack (Ann ()) [] []

instance AsTupleItem Expr where
  ti_ = TupleItem (Ann ())

instance AsTupleItem TupleItem where
  ti_ = id

-- |
-- >>> tuple_ []
-- ()
--
-- >>> tuple_ [ti_ $ var_ "a"]
-- a,
--
-- >>> tuple_ [s_ $ var_ "a"]
-- (*a),
--
-- >>> tuple_ [ti_ $ var_ "a", ti_ $ var_ "b"]
-- a, b
--
-- >>> tuple_ [ti_ $ var_ "a", s_ $ var_ "b"]
-- a, *b
tuple_ :: [Raw TupleItem] -> Raw Expr
tuple_ [] = Unit (Ann ()) [] []
tuple_ (a:as) =
  case as of
    [] -> Tuple (Ann ()) (ti_ a) (MkComma []) Nothing
    b:bs ->
      Tuple (Ann ()) a (MkComma [Space]) . Just $
      (b, zip (repeat (MkComma [Space])) bs, Nothing) ^. _CommaSep1'

-- |
-- >>> await (var_ "a")
-- await a
await_ :: Raw Expr -> Raw Expr
await_ = Await (Ann ()) [Space]

-- |
-- >>> ifThenElse_ (var_ "a") (var_ "b") (var_ "c")
-- a if b else c
ifThenElse_ :: Raw Expr -> Raw Expr -> Raw Expr -> Raw Expr
ifThenElse_ a b = Ternary (Ann ()) a [Space] b [Space]

-- |
-- >>> lambda_ [p_ "x"] "x"
-- lambda x: x
--
-- >>> lambda_ [p_ "x", k_ "y" 2] ("x" .+ "y")
-- lambda x, y=2: x + y
--
-- >>> lambda_ [p_ "x", k_ "y" 2, s_ "z"] "a"
-- lambda x, y=2, *z: a
--
-- >>> lambda_ [p_ "x", k_ "y" 2, s_ "z", ss_ "w"] "a"
-- lambda x, y=2, *z, **w: a
lambda_ :: [Raw Param] -> Raw Expr -> Raw Expr
lambda_ params =
  Lambda (Ann ())
    (if null params then [] else [Space])
    (listToCommaSep params)
    (MkColon [Space])

-- |
-- >>> yield_ []
-- yield
--
-- >>> yield_ [var_ "a"]
-- yield a
--
-- >>> yield_ [var_ "a", var_ "b"]
-- yield a, b
yield_ :: [Raw Expr] -> Raw Expr
yield_ as = Yield (Ann ()) (foldr (\_ _ -> [Space]) [] as) (listToCommaSep as)

-- |
-- >>> yieldFrom_ (var_ "a")
-- yield from a
yieldFrom_ :: Raw Expr -> Raw Expr
yieldFrom_ = YieldFrom (Ann ()) [Space] [Space]

-- | The slice with no bounds
--
-- >>> subs_ (var_ "a") fullSlice_
-- a[:]
--
-- >>> fullSlice_
-- slice(None, None, None)
fullSlice_ :: Raw Expr
fullSlice_ = slice_ Nothing Nothing Nothing

-- | Slice with *step* @x@
--
-- >>> subs_ (var_ "a") (sliceS_ $ int_ (-1))
-- a[::-1]
--
-- >>> sliceS_ $ int_ (-1)
-- slice(None, None, -1)
sliceS_ :: Raw Expr -> Raw Expr
sliceS_ x = slice_ Nothing Nothing (Just x)

-- | Slice *from* @x@
--
-- >>> subs_ (var_ "a") (sliceF_ $ int_ 0)
-- a[1:]
--
-- >>> sliceF_ $ int_ 0
-- slice(1, None, None)
sliceF_ :: Raw Expr -> Raw Expr
sliceF_ x = slice_ (Just x) Nothing Nothing

-- | Slice *from* @x@, with *step* @y@
--
-- >>> subs_ (var_ "a") (sliceFS_ (int_ 0) (int_ 2))
-- a[1::2]
--
-- >>> sliceFS_ (int_ 0) (int_ 2)
-- slice(1, None, 2)
sliceFS_ :: Raw Expr -> Raw Expr -> Raw Expr
sliceFS_ x y = slice_ (Just x) Nothing (Just y)

-- | Slice /To/ @x@
--
-- >>> subs_ (var_ "a") (sliceT_ $ int_ 10)
-- a[:10]
--
-- >>> sliceT_ $ int_ 10
-- slice(None, 10, None)
sliceT_ :: Raw Expr -> Raw Expr
sliceT_ x = slice_ Nothing (Just x) Nothing

-- | Slice /To/ @x@, with /Step/ @y@
--
-- >>> subs_ (var_ "a") (sliceTS_ (int_ 10) (int_ 2))
-- a[:10:2]
--
-- >>> sliceTS_ (int_ 10) (int_ 2)
-- slice(None, 10, 2)
sliceTS_ :: Raw Expr -> Raw Expr -> Raw Expr
sliceTS_ x y = slice_ Nothing (Just x) (Just y)

-- | Slice /From/ @x@ /To/ @y@
--
-- >>> subs_ (var_ "a") (sliceFT_ (int_ 1) (int_ 10))
-- a[1:10]
--
-- >>> sliceFT_ (int_ 1) (int_ 10)
-- slice(1, 10, None)
sliceFT_ :: Raw Expr -> Raw Expr -> Raw Expr
sliceFT_ x y = slice_ (Just x) (Just y) Nothing

-- | Slice /From/ @x@ /To/ @y@, with /Step/ @z@
--
-- >>> subs_ (var_ "a") (sliceFTS_ (int_ 1) (int_ 10) (int_ 2))
-- a[1:10:2]
--
-- >>> sliceFTS_ (int_ 1) (int_ 10) (int_ 2)
-- slice(1, 10, 2)
sliceFTS_ :: Raw Expr -> Raw Expr -> Raw Expr -> Raw Expr
sliceFTS_ x y z = slice_ (Just x) (Just y) (Just z)

-- | A slice object
--
-- Represents a call to a function named @slice@, with 3 arguments.
-- If an argument is a 'Nothing' then it becomes 'None', and if the argument is a
-- 'Just' then the contents are extracted.
slice_ :: Maybe (Raw Expr) -> Maybe (Raw Expr) -> Maybe (Raw Expr) -> Raw Expr
slice_ a b c =
  call_ (var_ "slice")
    [ p_ $ fromMaybe none_ a
    , p_ $ fromMaybe none_ b
    , p_ $ fromMaybe none_ c
    ]

-- |
-- >>> subs_ (var_ "a") (int_ 1)
-- a[1]
--
-- >>> subs_ (var_ "a") (tuple_ [ti_ $ int_ 1])
-- a[1,]
--
-- >>> subs_ (var_ "a") (tuple_ [ti_ $ int_ 1, ti_ $ int_ 2])
-- a[1, 2]
--
-- >>> subs_ (var_ "a") (tuple_ [s_ $ var_ "b"])
-- a[((*b),)]
--
-- >>> subs_ (var_ "a") (tuple_ [ti_ $ int_ 1, s_ $ var_ "b"])
-- a[(1, *b)]
subs_ :: Raw Expr -> Raw Expr -> Raw Expr
subs_ a e =
  Subscript (Ann ()) a
    []
    (exprToSubscript e ^. _CommaSep1')
    []
  where
    exprToSubscript
      :: Raw Expr
      -> (Raw Subscript, [(Comma, Raw Subscript)], Maybe Comma)
    exprToSubscript e =
      let
        notSlice :: (Raw Subscript, [(Comma, Raw Subscript)], Maybe Comma)
        notSlice =
          case e ^? _Tuple of
            Nothing -> (SubscriptExpr e, [], Nothing)
            Just tup ->
              let
                h = tup ^. tupleHead
                comma = tup ^. tupleComma
                t = tup ^? tupleTail._Just.from _CommaSep1'
                res =
                  case t of
                    Just (a, bs, c) ->
                      (,,) <$>
                      fromTupleItem h <*>
                      traverseOf (traverse._2) fromTupleItem ((comma, a) : bs) <*>
                      pure c
                    Nothing -> (\a -> (a, [], Just comma)) <$> fromTupleItem h
              in
                fromMaybe (SubscriptExpr e, [], Nothing) res
      in
        maybe notSlice (\a -> (a, [], Nothing)) $ mkSlice e
      where
        mkSlice
          :: Raw Expr
          -> Maybe (Raw Subscript)
        mkSlice e = do
          c <- e ^? _Call
          case c ^? callFunction._Ident.identValue of
            Just "slice" ->
              pure $ case c ^.. callArguments.folded.folded of
                [PositionalArg _ x] ->
                  SubscriptSlice Nothing (MkColon []) (Just x) Nothing
                [PositionalArg _ x, PositionalArg _ y] ->
                  SubscriptSlice
                    (noneToMaybe x)
                    (MkColon [])
                    (noneToMaybe y)
                    Nothing
                [PositionalArg _ x, PositionalArg _ y, PositionalArg _ z] ->
                  SubscriptSlice
                    (noneToMaybe x)
                    (MkColon [])
                    (noneToMaybe y)
                    ((,) (MkColon []) . Just <$> noneToMaybe z)
                _ -> SubscriptExpr e
            _ -> Nothing

        noneToMaybe x = fromMaybe (Just x) $ Nothing <$ (x ^? _None)

        fromTupleItem
          :: Raw TupleItem
          -> Maybe (Raw Subscript)
        fromTupleItem (TupleItem _ a) = mkSlice a <|> pure (SubscriptExpr a)
        fromTupleItem _ = Nothing
