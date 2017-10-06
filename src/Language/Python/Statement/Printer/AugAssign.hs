module Language.Python.Statement.Printer.AugAssign where

import Text.PrettyPrint
import Language.Python.Statement.AST.AugAssign

augAssign :: AugAssign -> Doc
augAssign a =
  case a of
    PlusEquals -> text "+="
    MinusEquals -> text "-="
    StarEquals -> text "*="
    AtEquals -> text "@="
    SlashEquals -> text "/="
    PercentEquals -> text "%="
    AmphersandEquals -> text "&="
    PipeEquals -> text "||="
    CaretEquals -> text "^^="
    ShiftLeftEquals -> text "<<="
    ShiftRightEquals -> text ">>="
    DoubleStarEquals -> text "**="
    DoubleSlashEquals -> text "//="
