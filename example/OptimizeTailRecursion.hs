{-# language OverloadedLists, OverloadedStrings #-}
{-# language DataKinds #-}
{-# language BangPatterns #-}
module OptimizeTailRecursion where

import Control.Applicative ((<|>))
import Control.Lens.Cons (_last, _init)
import Control.Lens.Fold ((^..), (^?), (^?!), allOf, anyOf, folded, foldrOf, toListOf)
import Control.Lens.Getter ((^.))
import Control.Lens.Plated (cosmos, transform, transformOn)
import Control.Lens.Prism (_Just)
import Control.Lens.Setter ((%~))
import Control.Lens.Tuple (_2, _3)
import Data.Foldable (toList)
import Data.Function ((&))
import Data.Semigroup ((<>))

import qualified Data.List.NonEmpty as NonEmpty

import Language.Python.Internal.Optics
import Language.Python.Internal.Syntax hiding (Expr(), Statement())
import Language.Python.Syntax

optimizeTailRecursion :: Raw Statement -> Maybe (Raw Statement)
optimizeTailRecursion st = do
  function <- st ^? _Fundef
  let functionBody = toList $ getBody function
  bodyLast <- lastStatement functionBody

  let
    functionName = function ^. fdName.identValue
    bodyInit = functionBody ^?! _init
    paramNames = function ^.. fdParameters.folded.paramName.identValue

  if not $ hasTC functionName bodyLast
    then Nothing
    else
      Just $
      fundef
        (function &
         setBody
           (flip (foldr NonEmpty.cons)
              (zipWith (\a b -> st_ $ var_ (a <> "__tr") .= var_ b) paramNames paramNames)
              [ st_ $ "__res__tr" .= none_
              , st_ . while_ true_ .
                NonEmpty.fromList . transformOn (traverse._Exprs) (renameIn paramNames "__tr") $
                  bodyInit <>
                  fmap st_ (looped functionName paramNames bodyLast)
              , st_ $ return_ "__res__tr"
              ]))

  where
    lastStatement :: [Raw Line] -> Maybe (Raw Statement)
    lastStatement = go Nothing
      where
        go !res [] = res
        go !res (a:as) = go (res <|> a ^? _Statements) as

    isTailCall :: String -> Raw Expr -> Bool
    isTailCall name e
      | anyOf (cosmos._Call._2._Ident._2.identValue) (== name) e
      = (e ^? _Call._2._Ident._2.identValue) == Just name
      | otherwise = False

    hasTC :: String -> Raw Statement -> Bool
    hasTC name st =
      case st of
        CompoundStatement (If _ _ _ e sts [] sts') ->
          allOf _last (hasTC name) (sts ^.. _Statements) ||
          allOf _last (hasTC name) (sts' ^.. _Just._3._Statements)
        SmallStatements _ s ss _ _ ->
          case last (s : fmap (^. _2) ss) of
            Return _ _ (Just e) -> isTailCall name e
            -- Return _ _ Nothing -> True
            Expr _ e -> isTailCall name e
            _ -> False
        _ -> False

    renameIn :: [String] -> String -> Raw Expr -> Raw Expr
    renameIn params suffix =
      transform
        (_Ident._2.identValue %~ (\a -> if a `elem` params then a <> suffix else a))

    looped :: String -> [String] -> Raw Statement -> [Raw Statement]
    looped name params st =
      case st of
        CompoundStatement c ->
          case c of
            If _ _ _ e sts [] sts'
              | hasTC name st ->
                  case sts' of
                    Nothing ->
                      [ if_ e
                          (fmap st_ . NonEmpty.fromList $
                           (toListOf _Statements sts ^?! _init) <>
                           looped name params (toListOf _Statements sts ^?! _last))
                      ]
                    Just (_, _, sts'') ->
                      [ ifElse_ e
                          (fmap st_ . NonEmpty.fromList $
                           (toListOf _Statements sts ^?! _init) <>
                           looped name params (toListOf _Statements sts ^?! _last))
                          (fmap st_ . NonEmpty.fromList $
                           (toListOf _Statements sts'' ^?! _init) <>
                           looped name params (toListOf _Statements sts'' ^?! _last))
                      ]
            _ -> [st]
        SmallStatements idnts s ss sc cmtnl ->
          let
            initExps = foldr (\_ _ -> init ss) [] ss
            lastExp =
              foldrOf (folded._2) (\_ _ -> last ss ^. _2) s ss
            newSts =
              case initExps of
                [] -> []
                first : rest ->
                  let
                    lss = last ss
                  in
                    [SmallStatements idnts (first ^. _2) rest sc cmtnl]
          in
            case lastExp of
              Return _ _ e ->
                case e ^? _Just._Call of
                  Just (_, f, _, args, _)
                    | Just name' <- f ^? _Ident._2.identValue
                    , name' == name ->
                        newSts <>
                        fmap (\a -> var_ (a <> "__tr__old") .= (var_ $ a <> "__tr")) params <>
                        zipWith
                          (\a b -> var_ (a <> "__tr") .= b)
                          params
                          (transformOn
                             traverse
                             (renameIn params "__tr__old")
                             (args ^.. folded.folded.argExpr))
                  _ -> newSts <> maybe [] (\e' -> [ "__res__tr" .= e' ]) e <> [ break_ ]
              Expr _ e
                | isTailCall name e -> newSts <> [pass_]
              _ -> [st]
