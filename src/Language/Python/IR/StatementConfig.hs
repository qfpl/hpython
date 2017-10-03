{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
module Language.Python.IR.StatementConfig where

import Papa
import Data.Singletons.TH

data LoopContext = InLoop | NotInLoop
  deriving (Eq, Show)

genSingletons [''LoopContext]
promoteEqInstances [''LoopContext]

data StatementConfig loopContext
  = StatementConfig
  { _loopContext :: loopContext
  }

makeLenses ''StatementConfig
