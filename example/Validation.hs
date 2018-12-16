{-# language OverloadedStrings #-}
{-# language TypeApplications #-}
module Validation where

import qualified Data.Text.IO as Text

import Language.Python.DSL
import Language.Python.Render
import Language.Python.Validate

good_program :: Raw Module
good_program =
  module_
  [ line_ $
    def_ "a" [p_ "b", p_ "c"]
    [ line_ $ return_ ("b" .+ "c")
    ]
  ]

bad_program :: Raw Module
bad_program =
  module_
  [ line_ $
    def_ "a" [p_ "b", p_ "c"]
    [ line_ $ return_ ("b" .+ "d")
    ]
  ]

doValidating :: IO ()
doValidating = do
  putStrLn "Validating good program:\n"

  -- We can render unvalidated programs
  Text.putStrLn $ showModule good_program

  -- Validate the module for indentation, syntax, and scope correctness
  --
  -- We use the type application specify the error type so that we can Show the
  -- result
  --
  -- On success, we get back the same program we put in, but it has a slightly
  -- different type to indicate that it has been validated
  print $ validateModuleAll @(ValidationError ()) good_program

  putStrLn ""

  putStrLn "Validating bad program:\n"
  Text.putStrLn $ showModule bad_program

  -- On failure, we get back a non-empty list of errors that occurred
  print $ validateModuleAll @(ValidationError ()) bad_program