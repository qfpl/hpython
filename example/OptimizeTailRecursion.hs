{-# language OverloadedStrings #-}
{-# language DataKinds #-}
{-# language BangPatterns #-}
module OptimizeTailRecursion where

import Control.Applicative ((<|>))
import Control.Lens.Cons (_last, _init)
import Control.Lens.Fold ((^..), (^?), (^?!), allOf, anyOf, folded, foldrOf)
import Control.Lens.Getter ((^.), to)
import Control.Lens.Plated (cosmos, transform, transformOn)
import Control.Lens.Prism (_Just)
import Control.Lens.Review ((#))
import Control.Lens.Setter ((%~))
import Control.Lens.Tuple (_2, _3)
import Data.Foldable (toList)
import Data.Function ((&))
import Data.Semigroup ((<>))


import Language.Python.Optics
import Language.Python.Internal.Syntax (CompoundStatement (..), Expr (..), Statement (..), SimpleStatement (..), SmallStatement (..), _Exprs, _Statements, argExpr, paramName)
import Language.Python.DSL
import Language.Python.Syntax.Whitespace

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
      _Fundef #
        (function &
         setBody (replicate 4 Space)
           (zipWith (\a b -> line_ (var_ (a <> "__tr") .= var_ b)) paramNames paramNames <>
            [ line_ ("__res__tr" .= none_)
            , line_ . while_ true_ .
              transformOn (traverse._Exprs) (renameIn paramNames "__tr") $
                bodyInit <>
                looped functionName paramNames bodyLast
            , line_ $ return_ "__res__tr"
            ]))

  where
    lastStatement :: [Raw Line] -> Maybe (Raw Statement)
    lastStatement = go Nothing
      where
        go !res [] = res
        go !res (a:as) = go (a ^? _Statements <|> res) as

    isTailCall :: String -> Raw Expr -> Bool
    isTailCall name e
      | anyOf (cosmos._Call.callFunction._Ident.identValue) (== name) e
      = (e ^? _Call.callFunction._Ident.identValue) == Just name
      | otherwise = False

    hasTC :: String -> Raw Statement -> Bool
    hasTC name st =
      case st of
        CompoundStatement (If _ _ _ e sts [] sts') ->
          allOf _last (hasTC name) (sts ^.. _Statements) ||
          allOf _last (hasTC name) (sts' ^.. _Just._3._Statements)
        SimpleStatement _ (MkSimpleStatement s ss _ _ _) ->
          case last (s : fmap (^. _2) ss) of
            Return _ _ (Just e) -> isTailCall name e
            -- Return _ _ Nothing -> True
            Expr _ e -> isTailCall name e
            _ -> False
        _ -> False

    renameIn :: [String] -> String -> Raw Expr -> Raw Expr
    renameIn params suffix =
      transform
        (_Ident.identValue %~ (\a -> if a `elem` params then a <> suffix else a))

    looped :: String -> [String] -> Raw Statement -> [Raw Line]
    looped name params st
      | Just ifSt <- st ^? _If
      , hasTC name st =
          let
            ifBodyLines = toList $ getBody ifSt
          in
            case ifSt ^? to getElse._Just.to getBody of
              Nothing ->
                [ line_ $
                  if_ (ifSt ^. ifCond)
                    ((ifBodyLines ^?! _init) <>
                     looped name params (ifBodyLines ^?! _last._Statements))
                ]
              Just sts'' ->
                [ line_ $
                  if_ (ifSt ^. ifCond)
                    ((toList (getBody ifSt) ^?! _init) <>
                     looped name params (ifBodyLines ^?! _last._Statements)) &
                  else_
                    ((toList sts'' ^?! _init) <>
                     looped name params (toList sts'' ^?! _last._Statements))
                ]
      | otherwise =
          case st of
            CompoundStatement{} -> [line_ st]
            SimpleStatement idnts (MkSimpleStatement s ss sc cmt nl) ->
              let
                initExps = foldr (\_ _ -> init ss) [] ss
                lastExp = foldrOf (folded._2) (\_ _ -> last ss ^. _2) s ss
                newSts =
                  case initExps of
                    [] -> []
                    first : rest ->
                      let
                        lss = last ss
                      in
                        [ line_ $
                          SimpleStatement idnts
                          (MkSimpleStatement (first ^. _2) rest sc cmt nl)
                        ]
              in
                case lastExp of
                  Return _ _ e ->
                    case e ^? _Just._Call of
                      Just call
                        | Just name' <- call ^? callFunction._Ident.identValue
                        , name' == name ->
                            newSts <>
                            fmap
                              (\a -> line_ (var_ (a <> "__tr__old") .= (var_ $ a <> "__tr")))
                              params <>
                            zipWith
                              (\a b -> line_ (var_ (a <> "__tr") .= b))
                              params
                              (transformOn
                                traverse
                                (renameIn params "__tr__old")
                                (call ^.. callArguments.folded.folded.argExpr))
                      _ ->
                        newSts <>
                        maybe [] (\e' -> [ line_ ("__res__tr" .= e') ]) e <> [ line_ break_ ]
                  Expr _ e
                    | isTailCall name e -> newSts <> [line_ pass_]
                  _ -> [line_ st]
