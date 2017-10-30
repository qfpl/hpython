module Language.Python.Statement.Parser.AugAssign where

import Papa
import Text.Trifecta hiding (Unspaced)
import Language.Python.Statement.AST.AugAssign

import Text.Parser.Unspaced

augAssign :: DeltaParsing m => Unspaced m AugAssign
augAssign =
  (string "+=" $> PlusEquals) <|>
  (string "-=" $> MinusEquals) <|>
  (try (string "*=") $> StarEquals) <|>
  (string "@=" $> AtEquals) <|>
  (try (string "/=") $> SlashEquals) <|>
  (string "&=" $> AmphersandEquals) <|>
  (string "|=" $> PipeEquals) <|>
  (string "^=" $> CaretEquals) <|>
  (string "<<=" $> ShiftLeftEquals) <|>
  (string ">>=" $> ShiftRightEquals) <|>
  (string "**=" $> DoubleStarEquals) <|>
  (string "//=" $> DoubleSlashEquals)
