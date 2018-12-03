{-|
Module      : Language.Python.DSL
Copyright   : (C) CSIRO 2017-2018
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
{-# language TypeFamilies #-}
{-# language UndecidableInstances #-}
module Language.Python.DSL
  ( (&)
  , Raw
  , Statement
  , Expr
    -- * Identifiers
  , id_
  , Ident(..)
    -- ** Lenses
  , identAnn
  , identValue
  , identWhitespace
    -- * Starred values
  , HasStar(..)
    -- * Double-starred values
  , HasDoubleStar(..)
    -- * @as@ syntax
  , As(..)
    -- * @if@ syntax
  , HasIf(..)
    -- * @for@ syntax
  , HasFor(..)
    -- * @in@ syntax
  , HasIn(..)
  , In(..)
    -- * @:@ syntax
  , HasColon(..)
    -- * Comprehensions
  , comp_
  , Guard(..)
    -- * Parameters and arguments
    -- ** Parameters
  , HasParameters(..)
    -- ** Arguments
  , HasArguments(..)
    -- ** Positional
  , HasPositional(..)
  , PositionalParam(..)
  , _PositionalParam
    -- *** Lenses
  , ppAnn
  , ppName
  , ppType
    -- ** Keyword
  , HasKeyword(..)
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
  , HasDecorators(..)
    -- * Statements
    -- ** @async@
  , HasAsync(..)
    -- ** Lines of code
  , blank_
  , AsLine(..)
  , Line(..)
    -- ** Block bodies
  , HasBody(..)
    -- ** Function definitions
  , def_
  , Fundef(..)
  , _Fundef
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
  , _ClassDef
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
  , (.>)
  , (.>=)
  , (.<)
  , (.<=)
  , (.!=)
    -- ** Exceptions
  , tryE_
  , tryF_
  , HasExcept(..)
  , HasFinally(..)
  , AsTry(..)
  , TryExcept(..)
  , _TryExcept
  , mkTryExcept
  , TryFinally(..)
  , _TryFinally
  , mkTryFinally
  , ExceptAs(..)
  , AsExceptAs(..)
  , Except(..)
  , _Except
  , mkExcept
  , Finally(..)
  , _Finally
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
  , _With
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
  , else_
  , HasElse(..)
    -- *** Break
  , break_
    -- *** For loops
  , forSt_
  , For(..)
  , _For
  , mkFor
    -- *** If statements
  , ifThen_
  , elif_
  , If(..)
  , _If
  , mkIf
  , Elif(..)
  , _Elif
  , mkElif
  , Else(..)
  , _Else
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
  , _While
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
  , and_
  , or_
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
  , _Tuple
  , AsTupleItem(..)
  , TupleItem()
    -- ** Function calls
  , call_
  , Call(..)
  , _Call
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
  , longStr_
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
  , is_
  , isNot_
  , notIn_
  , (.*)
  , (.-)
  , (.+)
  , (.==)
  , (.|)
  , (.^)
  , (.&)
  , (.<<)
  , (.>>)
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
import Language.Python.Syntax.AugAssign
import Language.Python.Syntax.CommaSep
import Language.Python.Syntax.Expr
import Language.Python.Syntax.Ident
import Language.Python.Syntax.Operator.Binary
import Language.Python.Syntax.Operator.Unary
import Language.Python.Syntax.Punctuation
import Language.Python.Syntax.Statement
import Language.Python.Syntax.Strings
import Language.Python.Syntax.Types
import Language.Python.Syntax.Whitespace

type Raw f = f '[] ()

-- | 'Ident' has an 'Data.String.IsString' instance, but when a type class dispatches on
-- an 'Ident' we will run into ambiguity if we try to use @OverloadedStrings@. In these
-- cases we can use 'id_' to provide the extra type information
id_ :: String -> Raw Ident
id_ = fromString

-- | One or more lines of Python code
newtype Line v a
  = Line
  { unLine :: Either (Blank a, Newline) (Statement v a)
  } deriving (Eq, Show)
makeWrapped ''Line

-- | Create a blank 'Line'
blank_ :: Raw Line
blank_ = Line $ Left (Blank () [] Nothing, LF)

-- | Convert some data to a 'Line'
class AsLine s where
  line_ :: Raw s -> Raw Line

instance AsLine SimpleStatement where
  line_ ss =
    Line . Right $ SimpleStatement (Indents [] ()) ss

instance AsLine SmallStatement where
  line_ ss =
    Line . Right . SimpleStatement (Indents [] ()) $
    MkSimpleStatement ss [] Nothing Nothing (Just LF)

instance AsLine CompoundStatement where
  line_ = Line . Right . CompoundStatement

instance AsLine If where
  line_ = line_ . (_If #)

instance AsLine While where
  line_ = line_ . (_While #)

instance AsLine With where
  line_ = line_ . (_With #)

instance AsLine Statement where
  line_ = Line . Right

instance AsLine Expr where
  line_ e = line_ $ Expr (e ^. exprAnn) e

instance HasExprs Line where
  _Exprs f (Line a) = Line <$> (_Right._Exprs) f a

instance HasStatements Line where
  _Statements f (Line a) = Line <$> _Right f a

class HasBody s where
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
  body_ :: Functor f => ([Raw Line] -> f [Raw Line]) -> Raw s -> f (Raw s)
  body :: Lens' (Raw s) (Raw Suite)

class HasColon s t | s -> t, t -> s where
  (.:) :: Raw s -> Raw Expr -> Raw t

infix 0 .:

-- | Constructing dictionary items
--
-- @('.:') :: 'Raw' 'Expr' -> 'Raw' 'Expr' -> 'Raw' 'DictItem'@
instance HasColon Expr DictItem where
  (.:) a = DictItem () a (Colon [Space])

-- | Function parameter type annotations
--
-- @('.:') :: 'Raw' 'Param' -> 'Raw' 'Expr' -> 'Raw' 'Param'@
--
-- See 'def_'
instance HasColon Param Param where
  (.:) p t = p { _paramType = Just (Colon [Space], t) }

-- | Positional parameters/arguments
--
-- @
-- p_ :: 'Raw' 'Expr' -> 'Raw' 'Arg'
-- @
--
-- @
-- p_ :: 'Raw' 'Ident' -> 'Raw' 'Param'
-- @
class HasPositional p v | p -> v, v -> p where
  p_ :: Raw v -> Raw p

-- | See 'def_'
instance HasStar Ident Param where
  s_ i = StarParam () [] (Just i) Nothing

-- | See 'def_'
instance HasDoubleStar Ident Param where
  ss_ i = DoubleStarParam () [] i Nothing

-- | See 'call_'
instance HasStar Expr Arg where
  s_ = StarArg () []

-- | See 'call_'
instance HasDoubleStar Expr Arg where
  ss_ = DoubleStarArg () []

-- | Keyword parameters/arguments
class HasKeyword p where
  k_ :: Raw Ident -> Raw Expr -> Raw p

class HasStar s t | t -> s where
  s_ :: Raw s -> Raw t

class HasDoubleStar s t | t -> s where
  ss_ :: Raw s -> Raw t

-- | See 'dict_'
instance HasDoubleStar Expr DictItem where
  ss_ = DictUnpack () []

-- | See 'def_'
instance HasPositional Param Ident where
  p_ i = PositionalParam () i Nothing

-- | See 'def_'
instance HasKeyword Param where
  k_ a = KeywordParam () a Nothing []

-- | See 'call_'
instance HasPositional Arg Expr where; p_ = PositionalArg ()

-- | See 'call_'
instance HasKeyword Arg where; k_ a = KeywordArg () a []

class HasParameters s where
  -- | A faux-Lens that allows targeting 'Param's in-between existing formatting,
  -- and adding appropriate formatting when extra parameters are introduced.
  --
  -- >>> 'Language.Python.Render.showStatement' myStatement
  -- \"def a(b ,  c   ):\\n    pass\"
  --
  -- >>> 'Language.Python.Render.showStatement' (myStatement '&' '_Fundef' '.' 'parameters_' '.~' ['p_' \"d\", 'p_' \"e\"]
  -- \"def a(d ,  e   ):\\n    pass\"
  --
  -- >>> 'Language.Python.Render.showStatement' (myStatement '&' '_Fundef' '.' 'parameters_' '.~' ['p_' \"d\", 'p_' \"e\", 'p_' \"f\"]
  -- \"def a(d ,  e   , f):\\n    pass\"
  parameters_ :: Functor f => ([Raw Param] -> f [Raw Param]) -> Raw s -> f (Raw s)
  parameters :: Lens' (Raw s) (CommaSep (Raw Param))

class HasArguments s where
  setArguments :: [Raw Arg] -> Raw s -> Raw s
  getArguments :: Raw s -> [Raw Arg]

class HasDecorators s where
  setDecorators :: [Raw Expr] -> Raw s -> Raw s
  getDecorators :: Raw s -> [Raw Expr]
  decorators :: Lens' (Raw s) [Raw Decorator]

decorated_ :: HasDecorators s => [Raw Expr] -> Raw s -> Raw s
decorated_ = setDecorators

exprsToDecorators :: Indents () -> [Raw Expr] -> [Raw Decorator]
exprsToDecorators is = fmap (\e -> Decorator () is [] e Nothing LF [])

instance HasDecorators Fundef where
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
        (Indents [replicate 4 Space ^. from indentWhitespaces] ())
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

instance HasBody Fundef where
  body = fdBody
  body_ = mkBody_ fdIndents fdBody

instance HasParameters Fundef where
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
      go (CommaSepMany a b CommaSepNone) [x] =
        CommaSepMany
          (x & trailingWhitespace .~ (a ^. trailingWhitespace))
          b
          CommaSepNone
      go (CommaSepMany a _ _) [x] =
        CommaSepOne (x & trailingWhitespace .~ (a ^. trailingWhitespace))
      go (CommaSepMany a b c) (x:xs) =
        CommaSepMany (x & trailingWhitespace .~ (a ^. trailingWhitespace)) b $ go c xs

  parameters = fdParameters

-- | Create a minimal valid function definition
mkFundef :: Raw Ident -> [Raw Line] -> Raw Fundef
mkFundef name body =
  MkFundef
  { _fdAnn = ()
  , _fdDecorators = []
  , _fdIndents = Indents [] ()
  , _fdAsync = Nothing
  , _fdDefSpaces = pure Space
  , _fdName = name
  , _fdLeftParenSpaces = []
  , _fdParameters = CommaSepNone
  , _fdRightParenSpaces = []
  , _fdReturnType = Nothing
  , _fdBody = SuiteMany () (Colon []) Nothing LF $ linesToBlockIndented body
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
def_ :: Raw Ident -> [Raw Param] -> [Raw Line] -> Raw Statement
def_ name params body =
  _Fundef # (mkFundef name body) { _fdParameters = listToCommaSep params }

-- | Create a minimal valid 'Call'
mkCall :: Raw Expr -> Raw Call
mkCall e =
  MkCall
  { _callAnn = ()
  , _callFunction = e
  , _callLeftParen = []
  , _callArguments = Nothing
  , _callRightParen = []
  }

instance HasArguments Call where
  setArguments args code =
    code
    { _callArguments =
        case args of
          [] -> Nothing
          a:as -> Just $ (a, zip (repeat (Comma [Space])) as, Nothing) ^. _CommaSep1'
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
      a:as -> Just $ (a, zip (repeat (Comma [Space])) as, Nothing) ^. _CommaSep1'
  }

return_ :: Raw Expr -> Raw Statement
return_ e =
  SimpleStatement
    (Indents [] ())
    (MkSimpleStatement (Return () [Space] $ Just e) [] Nothing Nothing (Just LF))

expr_ :: Raw Expr -> Raw Statement
expr_ e =
  SimpleStatement
    (Indents [] ())
    (MkSimpleStatement (Expr () e) [] Nothing Nothing (Just LF))

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
  li_ :: Raw s -> Raw ListItem

instance AsListItem ListItem where
  li_ = id

instance AsListItem Expr where
  li_ = ListItem ()

-- | See 'list_'
instance HasStar Expr ListItem where
  s_ = ListUnpack () [] []

instance e ~ Raw ListItem => AsList [e] where
  list_ es = List () [] (listToCommaSep1' es) []

instance e ~ Comprehension Expr => AsList (Raw e) where
  list_ c = ListComp () [] c []

newtype Guard v a = MkGuard { unGuard :: Either (CompFor v a) (CompIf v a) }

class HasFor a x | a -> x where
  for_ :: Raw x -> a

-- |
-- @'for_' :: 'Raw' 'In' -> 'Raw' 'CompFor'@
--
-- >>> comp_ (var_ "a") (for_ $ var_ "a" `in_` var_ "b") []
-- a for a in b
instance HasFor (Raw CompFor) In where
  for_ (MkIn a b) = CompFor () [Space] a [Space] b

-- |
-- @'for_' :: 'Raw' 'In' -> 'Raw' 'Guard'@
--
-- >>> comp_ (var_ "a") (for_ $ var_ "a" `in_` var_ "b") [for_ $ var_ "c" \`in_\` var_ "d"]
-- a for a in b for c in d
instance HasFor (Raw Guard) In where
  for_ (MkIn a b) = MkGuard . Left $ CompFor () [Space] a [Space] b

class HasIf a where
  if_ :: Raw Expr -> a

-- |
-- @'if_' :: 'Raw' 'Expr' -> 'Raw' 'Guard'@
--
-- >>> comp_ (var_ "a") (for_ $ var_ "a" `in_` var_ "b") [if_ $ var_ "c" .== var_ "d"]
-- a for a in b if c == d
instance HasIf (Raw Guard) where
  if_ = MkGuard . Right . CompIf () [Space]

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
  si_ :: Raw s -> Raw SetItem

instance AsSetItem SetItem where
  si_ = id

instance AsSetItem Expr where
  si_ = SetItem ()

-- | See 'set_'
instance HasStar Expr SetItem where
  s_ = SetUnpack () [] []

instance e ~ Raw SetItem => AsSet [e] where
  set_ es =
    case es of
      [] -> call_ (var_ "set") []
      a:as -> Set () [] ((a, zip (repeat (Comma [Space])) as, Nothing) ^. _CommaSep1') []

instance e ~ Comprehension SetItem => AsSet (Raw e) where
  set_ c = SetComp () [] c []

comp_ :: Raw e -> Raw CompFor -> [Raw Guard] -> Raw (Comprehension e)
comp_ val cfor guards =
  Comprehension ()
    val
    (if null guards
     then cfor
     else cfor & trailingWhitespace .~ [Space])
    (unGuard <$> guards)

-- |
-- >>> gen_ $ comp_ (var_ "a") (for_ $ var_ "a" `in_` list_ [li_ $ int_ 1, li_ $ int_ 2, li_ $ int_ 3]) [if_ $ var_ "a" .== 2]
-- (a for a in [1, 2, 3] if a == 2)
gen_ :: Raw (Comprehension Expr) -> Raw Expr
gen_ = Generator ()

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
    Dict ()
    []
    (case ds of
       [] -> Nothing
       a:as -> Just $ (a, zip (repeat (Comma [Space])) as, Nothing) ^. _CommaSep1')
    []

-- |
-- @'dict_' :: 'Raw' ('Comprehension' 'DictItem') -> 'Raw' 'Expr'@
instance e ~ Comprehension DictItem => AsDict (Raw e) where
  dict_ comp = DictComp () [] comp []

mkBinOp :: ([Whitespace] -> BinOp ()) -> Raw Expr -> Raw Expr -> Raw Expr
mkBinOp bop a = BinOp () (a & trailingWhitespace .~ [Space]) (bop [Space])

-- | @a is b@
is_ :: Raw Expr -> Raw Expr -> Raw Expr
is_ = mkBinOp $ Is ()
infixl 1 `is_`

data In v a = MkIn (Expr v a) (Expr v a)
data InList v a = MkInList (Expr v a) [Expr v a]

class HasIn a x | a -> x where
  in_ :: Raw Expr -> x -> Raw a

infixl 1 `in_`

-- |
-- >>> var_ "a" `in_` var_ "b"
-- a in b
instance HasIn Expr (Raw Expr) where
  in_ = mkBinOp $ In ()

-- | See 'for_'
instance e ~ Raw Expr => HasIn InList [e] where
  in_ = MkInList

-- | @a not in b@
notIn_ :: Raw Expr -> Raw Expr -> Raw Expr
notIn_ = mkBinOp $ NotIn () [Space]
infixl 1 `notIn_`

-- | @a is not b@
isNot_ :: Raw Expr -> Raw Expr -> Raw Expr
isNot_ = mkBinOp $ IsNot () [Space]
infixl 1 `isNot_`

-- | @not a@
not_ :: Raw Expr -> Raw Expr
not_ = Not () [Space]

-- | @a == b@
(.==) :: Raw Expr -> Raw Expr -> Raw Expr
(.==) = mkBinOp $ Equals ()
infixl 1 .==

-- | @a < b@
(.<) :: Raw Expr -> Raw Expr -> Raw Expr
(.<) = mkBinOp $ Lt ()
infixl 1 .<

-- | @a <= b@
(.<=) :: Raw Expr -> Raw Expr -> Raw Expr
(.<=) = mkBinOp $ LtEquals ()
infixl 1 .<=

-- | @a > b@
(.>) :: Raw Expr -> Raw Expr -> Raw Expr
(.>) = mkBinOp $ Gt ()
infixl 1 .>

-- | @a >= b@
(.>=) :: Raw Expr -> Raw Expr -> Raw Expr
(.>=) = mkBinOp $ GtEquals ()
infixl 1 .>=

-- | @a != b@
(.!=) :: Raw Expr -> Raw Expr -> Raw Expr
(.!=) = mkBinOp $ NotEquals ()
infixl 1 .!=

-- | @a | b@
(.|) :: Raw Expr -> Raw Expr -> Raw Expr
(.|) = mkBinOp $ BitOr ()
infixl 2 .|

-- | @a ^ b@
(.^) :: Raw Expr -> Raw Expr -> Raw Expr
(.^) = mkBinOp $ BitXor ()
infixl 3 .^

-- | @a & b@
(.&) :: Raw Expr -> Raw Expr -> Raw Expr 
(.&) = mkBinOp $ BitAnd ()
infixl 4 .&

-- | @a << b@
(.<<) :: Raw Expr -> Raw Expr -> Raw Expr 
(.<<) = mkBinOp $ ShiftLeft ()
infixl 5 .<<

-- | @a >> b@
(.>>) :: Raw Expr -> Raw Expr -> Raw Expr 
(.>>) = mkBinOp $ ShiftRight ()
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
(.@) = mkBinOp $ At ()
infixl 7 .@

-- | @a / b@
(./) :: Raw Expr -> Raw Expr -> Raw Expr
(./) = mkBinOp $ Divide ()
infixl 7 ./

-- | @a // b@
(.//) :: Raw Expr -> Raw Expr -> Raw Expr
(.//) = mkBinOp $ FloorDivide ()
infixl 7 .//

-- | @a % b@
(.%) :: Raw Expr -> Raw Expr -> Raw Expr
(.%) = mkBinOp $ Percent ()
infixl 7 .%

-- | @a ** b@
(.**) :: Raw Expr -> Raw Expr -> Raw Expr
(.**) = mkBinOp $ Exp ()
infixr 8 .**

-- | @a.b@
(/>) :: Raw Expr -> Raw Ident -> Raw Expr
(/>) a = Deref () a []
infixl 9 />

-- | @-a@
neg_ :: Raw Expr -> Raw Expr
neg_ = negate

-- | @+a@
pos_ :: Raw Expr -> Raw Expr
pos_ = UnOp () (Positive () [])

-- | @~a@
compl_ :: Raw Expr -> Raw Expr
compl_ = UnOp () (Complement () [])

-- | Convert a list of 'Line's to a 'Block', giving them 4 spaces of indentation
linesToBlockIndented :: [Raw Line] -> Block '[] ()
linesToBlockIndented = over _Indents (indentIt $ replicate 4 Space) . linesToBlock

-- | Convert a list of 'Line's to a 'Block', without indenting them
linesToBlock :: [Raw Line] -> Block '[] ()
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

instance HasBody While where
  body = whileBody
  body_ = mkBody_ whileIndents whileBody

instance HasElse While where
  getElse = mkGetElse _whileIndents _whileElse
  setElse = mkSetElse _whileIndents whileElse

-- | Create a minimal valid 'While'
mkWhile :: Raw Expr -> [Raw Line] -> Raw While
mkWhile cond body =
  MkWhile
  { _whileAnn = ()
  , _whileIndents = Indents [] ()
  , _whileWhile = [Space]
  , _whileCond = cond
  , _whileBody = SuiteMany () (Colon []) Nothing LF $ linesToBlockIndented body
  , _whileElse = Nothing
  }

while_ :: Raw Expr -> [Raw Line] -> Raw Statement
while_ e sts = _While # mkWhile e sts

-- | Create a minimal valid 'If'
mkIf :: Raw Expr -> [Raw Line] -> Raw If
mkIf cond body =
  MkIf
  { _ifAnn = ()
  , _ifIndents = Indents [] ()
  , _ifIf = [Space]
  , _ifCond = cond
  , _ifBody = SuiteMany () (Colon []) Nothing LF $ linesToBlockIndented body
  , _ifElifs = []
  , _ifElse = Nothing
  }

instance HasBody Elif where
  body = elifBody
  body_ = mkBody_ elifIndents elifBody 

instance HasBody Else where
  body = elseBody
  body_ = mkBody_ elseIndents elseBody 

instance HasBody If where
  body = ifBody
  body_ = mkBody_ ifIndents ifBody 

-- |
-- @'if_' :: 'Raw' 'Expr' -> ['Raw' 'Line'] -> 'Raw' 'If'@
--
-- >>> if_ (var_ "a" .< 10) [var_ "a" .+= 1]
-- if a < 10:
--     a += 1
instance (l ~ Raw Line, s ~ Raw If) => HasIf ([l] -> s) where
  if_ = mkIf

ifThen_ :: Raw Expr -> [Raw Line] -> Raw If
ifThen_ = mkIf

var_ :: String -> Raw Expr
var_ s = Ident $ MkIdent () s []

none_ :: Raw Expr
none_ = None () []

int_ :: Integer -> Raw Expr
int_ = fromInteger

pass_ :: Raw Statement
pass_ =
  SimpleStatement
    (Indents [] ())
    (MkSimpleStatement (Pass () []) [] Nothing Nothing (Just LF))

-- | Create a minimal valid 'Elif'
mkElif :: Raw Expr -> [Raw Line] -> Raw Elif
mkElif cond body =
  MkElif
  { _elifIndents = Indents [] ()
  , _elifElif = [Space]
  , _elifCond = cond
  , _elifBody = SuiteMany () (Colon []) Nothing LF $ linesToBlockIndented body
  }

elif_ :: Raw Expr -> [Raw Line] -> Raw If -> Raw If
elif_ cond body code = code & ifElifs <>~ [mkElif cond body]

-- | Create a minimal valid 'Else'
mkElse :: [Raw Line] -> Raw Else
mkElse body =
  MkElse
  { _elseIndents = Indents [] ()
  , _elseElse = []
  , _elseBody = SuiteMany () (Colon []) Nothing LF $ linesToBlockIndented body
  }

class HasElse s where
  getElse :: Raw s -> Maybe (Raw Else)
  setElse :: [Whitespace] -> Maybe (Raw Else) -> Raw s -> Raw s

else_ :: HasElse s => [Raw Line] -> Raw s -> Raw s
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

instance HasElse For where
  getElse = mkGetElse _forIndents _forElse
  setElse = mkSetElse _forIndents forElse

instance HasElse If where
  getElse = mkGetElse _ifIndents _ifElse
  setElse = mkSetElse _ifIndents ifElse

break_ :: Raw Statement
break_ =
  SimpleStatement
    (Indents [] ())
    (MkSimpleStatement (Break () []) [] Nothing Nothing (Just LF))

true_ :: Raw Expr
true_ = Bool () True []

false_ :: Raw Expr
false_ = Bool () False []

and_ :: Raw Expr -> Raw Expr -> Raw Expr
and_ a = BinOp () (a & trailingWhitespace .~ [Space]) (BoolAnd () [Space])

or_ :: Raw Expr -> Raw Expr -> Raw Expr
or_ a = BinOp () (a & trailingWhitespace .~ [Space]) (BoolOr () [Space])

str_ :: String -> Raw Expr
str_ s =
  String () . pure $
  StringLiteral () Nothing ShortString DoubleQuote (Char_lit <$> s) []

longStr_ :: String -> Raw Expr
longStr_ s =
  String () . pure $
  StringLiteral () Nothing LongString DoubleQuote (Char_lit <$> s) []

mkAugAssign :: AugAssignOp -> Raw Expr -> Raw Expr -> Raw Statement
mkAugAssign at a b =
  SimpleStatement
    (Indents [] ())
    (MkSimpleStatement
       (AugAssign () (a & trailingWhitespace .~ [Space]) (MkAugAssign at () [Space]) b)
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
  SimpleStatement
    (Indents [] ())
    (MkSimpleStatement
       (Assign () t $ (,) [Space] <$> (a :| as))
       []
       Nothing
       Nothing
       (Just LF))

(.=) :: Raw Expr -> Raw Expr -> Raw Statement
(.=) a b =
  SimpleStatement
    (Indents [] ())
    (MkSimpleStatement
       (Assign () (a & trailingWhitespace .~ [Space]) $ pure ([Space], b))
       []
       Nothing
       Nothing
       (Just LF))
infix 0 .=

(.+=) :: Raw Expr -> Raw Expr -> Raw Statement
(.+=) = mkAugAssign PlusEq
infix 0 .+=

(.-=) :: Raw Expr -> Raw Expr -> Raw Statement
(.-=) = mkAugAssign MinusEq
infix 0 .-=

(.*=) :: Raw Expr -> Raw Expr -> Raw Statement
(.*=) = mkAugAssign StarEq
infix 0 .*=

(.@=) :: Raw Expr -> Raw Expr -> Raw Statement
(.@=) = mkAugAssign AtEq
infix 0 .@=

(./=) :: Raw Expr -> Raw Expr -> Raw Statement
(./=) = mkAugAssign SlashEq
infix 0 ./=

(.%=) :: Raw Expr -> Raw Expr -> Raw Statement
(.%=) = mkAugAssign PercentEq
infix 0 .%=

(.&=) :: Raw Expr -> Raw Expr -> Raw Statement
(.&=) = mkAugAssign AmpersandEq
infix 0 .&=

(.|=) :: Raw Expr -> Raw Expr -> Raw Statement
(.|=) = mkAugAssign PipeEq
infix 0 .|=

(.^=) :: Raw Expr -> Raw Expr -> Raw Statement
(.^=) = mkAugAssign CaretEq
infix 0 .^=

(.<<=) :: Raw Expr -> Raw Expr -> Raw Statement
(.<<=) = mkAugAssign ShiftLeftEq
infix 0 .<<=

(.>>=) :: Raw Expr -> Raw Expr -> Raw Statement
(.>>=) = mkAugAssign ShiftRightEq
infix 0 .>>=

(.**=) :: Raw Expr -> Raw Expr -> Raw Statement
(.**=) = mkAugAssign DoubleStarEq
infix 0 .**=

(.//=) :: Raw Expr -> Raw Expr -> Raw Statement
(.//=) = mkAugAssign DoubleSlashEq
infix 0 .//=

mkFor :: Raw Expr -> [Raw Expr] -> [Raw Line] -> Raw For
mkFor binder collection body =
  MkFor
  { _forAnn = ()
  , _forIndents = Indents [] ()
  , _forAsync = Nothing
  , _forFor = [Space]
  , _forBinder = binder & trailingWhitespace .~ [Space]
  , _forIn = [Space]
  , _forCollection =
      fromMaybe
        (CommaSepOne1' (Unit () [] []) Nothing)
        (listToCommaSep1' collection)
  , _forBody = SuiteMany () (Colon []) Nothing LF $ linesToBlockIndented body
  , _forElse = Nothing
  }

-- |
-- @'for_' :: 'Raw' 'In' -> ['Raw' 'Line'] -> 'Raw' 'Statement'@
--
-- >>> for_ (var_ "a" `in_` var_ "b") [line_ (var_ "c" .+= var_ "a")]
-- for a in b:
--     c += a
instance (l ~ Raw Line, s ~ Raw Statement) => HasFor ([l] -> s) InList where
  for_ (MkInList a b) = forSt_ a b

forSt_ :: Raw Expr -> [Raw Expr] -> [Raw Line] -> Raw Statement
forSt_ val vals block = _For # mkFor val vals block

instance HasBody For where
  body = forBody
  body_ = mkBody_ forIndents forBody 

instance AsLine For where
  line_ = line_ . (_For #)

class HasAsync s where
  async_ :: Raw s -> Raw s

instance HasAsync Fundef where
  async_ = fdAsync ?~ pure Space

instance HasAsync For where
  async_ = forAsync ?~ pure Space

-- | Create a minimal valid 'Finally'
mkFinally :: [Raw Line] -> Raw Finally
mkFinally body =
  MkFinally
  { _finallyIndents = Indents [] ()
  , _finallyFinally = []
  , _finallyBody = SuiteMany () (Colon []) Nothing LF $ linesToBlockIndented body
  }

-- | Create a minimal valid 'Except'
mkExcept :: [Raw Line] -> Raw Except
mkExcept body =
  MkExcept
  { _exceptIndents = Indents [] ()
  , _exceptExcept = []
  , _exceptExceptAs = Nothing
  , _exceptBody = SuiteMany () (Colon []) Nothing LF $ linesToBlockIndented body
  }

-- | Create a minimal valid 'TryExcept'
mkTryExcept :: [Raw Line] -> Raw Except -> Raw TryExcept
mkTryExcept body except =
  MkTryExcept
  { _teAnn = ()
  , _teIndents = Indents [] ()
  , _teTry = [Space]
  , _teBody = SuiteMany () (Colon []) Nothing LF $ linesToBlockIndented body
  , _teExcepts = pure except
  , _teElse = Nothing
  , _teFinally = Nothing
  }

-- | Create a minimal valid 'TryFinally'
mkTryFinally :: [Raw Line] -> [Raw Line] -> Raw TryFinally
mkTryFinally body fBody =
  MkTryFinally
  { _tfAnn = ()
  , _tfIndents = Indents [] ()
  , _tfTry = [Space]
  , _tfBody = SuiteMany () (Colon []) Nothing LF $ linesToBlockIndented body
  , _tfFinally = mkFinally fBody
  }

class HasFinally s where
  finally_ :: [Raw Line] -> Raw s -> Raw s

instance HasFinally TryExcept where
  finally_ body = teFinally ?~ mkFinally body

instance HasFinally TryFinally where
  finally_ body = tfFinally .~ mkFinally body

instance HasBody TryExcept where
  body = teBody
  body_ = mkBody_ teIndents teBody 

-- | @try ... except@ with optional @else@ and optional @finally@
tryE_ :: [Raw Line] -> Raw Except -> Raw TryExcept
tryE_ = mkTryExcept

instance HasBody TryFinally where
  body = tfBody
  body_ = mkBody_ tfIndents tfBody 

-- |
-- @
-- try:
--     ...
-- finally:
--     ...
-- @
tryF_ :: [Raw Line] -> [Raw Line] -> Raw TryFinally
tryF_ = mkTryFinally

class AsExceptAs s where
  toExceptAs :: Raw s -> Raw ExceptAs

instance AsExceptAs ExceptAs where
  toExceptAs = id

instance AsExceptAs Expr where
  toExceptAs e = ExceptAs () e Nothing

class HasExcept s where
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
-- @'exceptAs_' :: AsExceptAs => 'Raw' e -> ['Raw' 'Line'] -> ('Raw' 'Except' -> 'Raw' 'TryExcept') -> 'Raw' 'TryExcept'@
--
-- >>> _Try # (tryE_ [var_ "a" .= 2] & except_ [var_ "a" .= 3])
-- try:
--     a = 2
-- except:
--     a = 3
--
-- >>> _Try # (tryE_ [var_ "a" .= 2] & exceptAs_ (var_ "Exception" `as_` id_ "b") [var_ "a" .= 3]
-- try:
--     a = 2
-- except Exception as b:
--     a = 3
instance (e ~ Raw Except, s ~ Raw TryExcept) => HasExcept (e -> s) where
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
instance HasExcept (Raw TryExcept) where
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
instance HasExcept (Raw TryFinally) where
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
  line_ = line_ . (_TryExcept #)

instance AsLine TryFinally where
  line_ = line_ . (_TryFinally #)

class As s t u | s t -> u, u -> s t where
  as_ :: Raw s -> Raw t -> Raw u

-- | See 'exceptAs_'
instance As Expr Ident ExceptAs where
  as_ e name = ExceptAs () e $ Just ([Space], name)

class_ :: Raw Ident -> [Raw Arg] -> [Raw Line] -> Raw Statement
class_ name args body =
  _ClassDef #
  (mkClassDef name body) {
    _cdArguments =
      case args of
        [] -> Nothing
        a:as -> Just ([], Just $ (a, zip (repeat (Comma [Space])) as, Nothing) ^. _CommaSep1', [])
  }

-- | Create a minimal 'ClassDef'
mkClassDef :: Raw Ident -> [Raw Line] -> Raw ClassDef
mkClassDef name body =
  MkClassDef
  { _cdAnn = ()
  , _cdDecorators = []
  , _cdIndents = Indents [] ()
  , _cdClass = Space :| []
  , _cdName = name
  , _cdArguments = Nothing
  , _cdBody = SuiteMany () (Colon []) Nothing LF $ linesToBlockIndented body
  }

instance HasBody ClassDef where
  body = cdBody
  body_ = mkBody_ cdIndents cdBody 

instance HasDecorators ClassDef where
  decorators = cdDecorators

  setDecorators new code =
    code
    { _cdDecorators = exprsToDecorators (_cdIndents code) new
    }

  getDecorators code = code ^.. cdDecorators.folded._Exprs

instance HasArguments ClassDef where
  setArguments args code =
    code
    { _cdArguments =
        case args of
          [] -> Nothing
          a:as -> Just ([], Just $ (a, zip (repeat (Comma [Space])) as, Nothing) ^. _CommaSep1', [])
    }

  getArguments code = _cdArguments code ^.. folded._2.folded.folded

-- | Create a minimal valid 'With'
mkWith :: NonEmpty (Raw WithItem) -> [Raw Line] -> Raw With
mkWith items body =
  MkWith
  { _withAnn = ()
  , _withIndents = Indents [] ()
  , _withAsync = Nothing
  , _withWith = [Space]
  , _withItems = listToCommaSep1 items
  , _withBody = SuiteMany () (Colon []) Nothing LF $ linesToBlockIndented body
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
with_ :: AsWithItem e => NonEmpty (Raw e) -> [Raw Line] -> Raw Statement
with_ items body = _With # mkWith (toWithItem <$> items) body

withItem_ :: Raw Expr -> Maybe (Raw Expr) -> Raw WithItem
withItem_ a b = WithItem () a ((,) [Space] <$> b)

-- | See 'with_'
instance As Expr Expr WithItem where
  as_ a b = WithItem () a $ Just ([Space], b)

class AsWithItem s where
  toWithItem :: Raw s -> Raw WithItem

instance AsWithItem Expr where
  toWithItem e = WithItem () e Nothing

instance AsWithItem WithItem where
  toWithItem = id

instance HasBody With where
  body = withBody
  body_ = mkBody_ withIndents withBody 

instance HasAsync With where
  async_ = withAsync ?~ pure Space

ellipsis_ :: Raw Expr
ellipsis_ = Ellipsis () []

class AsTupleItem e where
  ti_ :: Raw e -> Raw TupleItem

-- | See 'tuple_'
instance HasStar Expr TupleItem where
  s_ = TupleUnpack () [] []

instance AsTupleItem Expr where
  ti_ = TupleItem ()

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
tuple_ [] = Unit () [] []
tuple_ (a:as) =
  case as of
    [] -> Tuple () (ti_ a) (Comma []) Nothing
    b:bs ->
      Tuple () a (Comma [Space]) . Just $
      (b, zip (repeat (Comma [Space])) bs, Nothing) ^. _CommaSep1'

await_ :: Raw Expr -> Raw Expr
await_ = Await () [Space]

-- |
-- >>> ifThenElse_ (var_ "a") (var_ "b") (var_ "c")
-- a if b else c
ifThenElse_ :: Raw Expr -> Raw Expr -> Raw Expr -> Raw Expr
ifThenElse_ a b = Ternary () a [Space] b [Space]

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
  Lambda ()
    (if null params then [] else [Space])
    (listToCommaSep params)
    (Colon [Space])

yield_ :: Maybe (Raw Expr) -> Raw Expr
yield_ a = Yield () (maybe [] (const [Space]) a) a

yieldFrom_ :: Raw Expr -> Raw Expr
yieldFrom_ = YieldFrom () [Space] [Space]

-- | The slice with no bounds
--
-- >>> subs_ (var_ "a") fullSlice_
-- a[:]
--
-- >>> fullSlice_
-- slice(None, None, None)
fullSlice_ :: Raw Expr
fullSlice_ = slice_ Nothing Nothing Nothing

-- | Slice with *step* x
--
-- >>> subs_ (var_ "a") (sliceS_ $ int_ (-1))
-- a[::-1]
--
-- >>> sliceS_ $ int_ (-1)
-- slice(None, None, -1)
sliceS_ :: Raw Expr -> Raw Expr
sliceS_ x = slice_ Nothing Nothing (Just x)

-- | Slice *from* x
--
-- >>> subs_ (var_ "a") (sliceF_ $ int_ 0)
-- a[1:]
--
-- >>> sliceF_ $ int_ 0
-- slice(1, None, None)
sliceF_ :: Raw Expr -> Raw Expr
sliceF_ x = slice_ (Just x) Nothing Nothing

-- | Slice *from* x, with *step* y
--
-- >>> subs_ (var_ "a") (sliceFS_ (int_ 0) (int_ 2))
-- a[1::2]
--
-- >>> sliceFS_ (int_ 0) (int_ 2)
-- slice(1, None, 2)
sliceFS_ :: Raw Expr -> Raw Expr -> Raw Expr
sliceFS_ x y = slice_ (Just x) Nothing (Just y)

-- | Slice *to* x
--
-- >>> subs_ (var_ "a") (sliceT_ $ int_ 10)
-- a[:10]
--
-- >>> sliceT_ $ int_ 10
-- slice(None, 10, None)
sliceT_ :: Raw Expr -> Raw Expr
sliceT_ x = slice_ Nothing (Just x) Nothing

-- | Slice *to* x, with *step* y
--
-- >>> subs_ (var_ "a") (sliceTS_ (int_ 10) (int_ 2))
-- a[:10:2]
--
-- >>> sliceTS_ (int_ 10) (int_ 2)
-- slice(None, 10, 2)
sliceTS_ :: Raw Expr -> Raw Expr -> Raw Expr
sliceTS_ x y = slice_ Nothing (Just x) (Just y)

-- | Slice *from* x *to* y
--
-- >>> subs_ (var_ "a") (sliceFT_ (int_ 1) (int_ 10))
-- a[1:10]
--
-- >>> sliceFT_ (int_ 1) (int_ 10)
-- slice(1, 10, None)
sliceFT_ :: Raw Expr -> Raw Expr -> Raw Expr
sliceFT_ x y = slice_ (Just x) (Just y) Nothing

-- | Slice *from* x *to* y, with *step* z
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
-- Represents a call to a functionc named \"slice\", with 3 arguments.
-- If an argument is a 'Nothing' then it becomes a @None@, and if the argument is a
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
  Subscript () a
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
                  SubscriptSlice Nothing (Colon []) (Just x) Nothing
                [PositionalArg _ x, PositionalArg _ y] ->
                  SubscriptSlice
                    (noneToMaybe x)
                    (Colon [])
                    (noneToMaybe y)
                    Nothing
                [PositionalArg _ x, PositionalArg _ y, PositionalArg _ z] ->
                  SubscriptSlice
                    (noneToMaybe x)
                    (Colon [])
                    (noneToMaybe y)
                    ((,) (Colon []) . Just <$> noneToMaybe z)
                _ -> SubscriptExpr e
            _ -> Nothing

        noneToMaybe x = fromMaybe (Just x) $ Nothing <$ (x ^? _None)

        fromTupleItem
          :: Raw TupleItem
          -> Maybe (Raw Subscript)
        fromTupleItem (TupleItem _ a) = mkSlice a <|> pure (SubscriptExpr a)
        fromTupleItem _ = Nothing
