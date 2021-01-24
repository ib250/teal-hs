
module Teal (parseFromStdIn, toForest) where

import Data.List (intersperse)
import qualified Data.Tree as Tree
import qualified Teal.Expr as T
import           Text.Parsec.Error
    ( ParseError
    )

import qualified Text.ParserCombinators.Parsec as P


parseFromStdIn :: String -> Either ParseError [T.TealExpr]
parseFromStdIn = P.parse T.parseSource "stdin"


toForest :: [T.TealExpr] -> Tree.Forest String
toForest expr = Tree.unfoldForest alg expr
    where alg :: T.TealExpr -> (String, [T.TealExpr])
          alg (T.TealUnaryOp op expr) = (show op, [expr])
          alg (T.TealBinaryOp op lhs rhs) = (show op, [lhs, rhs])
          alg (T.TealIf c e1 e2) =
            ( "IF"
            , [c, T.TealList e1, T.TealList e2]
            )

          alg (T.TealApply f es) =
            ( "Apply"
            , [T.TealString f, T.TealList es]
            )

          alg (T.TealFunctionDecl (T.TealFunction name args es)) =
            ( unwords [ "Declare"
                      , name
                      , "("
                      , unwords $ intersperse "," args
                      , ")"
                      ]
            , es
            )
          alg (T.TealImport s) = (unwords s, [])

          -- simple case wrapped haskell values, containers etc...
          alg xs = (show xs, [])
