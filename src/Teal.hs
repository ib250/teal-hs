
module Teal where

import qualified Teal.Expr as T
import           Text.Parsec.Error
    ( ParseError
    )

import qualified Text.ParserCombinators.Parsec as P


parseFromStdIn :: String -> Either ParseError [T.TealExpr]
parseFromStdIn = P.parse T.parseSource "stdin"
