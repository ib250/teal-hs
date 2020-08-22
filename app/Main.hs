module Main where

import qualified Teal


parseAst :: String -> String
parseAst teal = case Teal.parseFromStdIn teal of
    Right expr -> "success...\n" ++ (show expr)
    Left err -> "failure!!!\n" ++ (show err)


main :: IO ()
main = interact parseAst
