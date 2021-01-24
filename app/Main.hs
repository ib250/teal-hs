module Main where

--import qualified Data.Map as Map
import qualified Data.Tree as Tree
import qualified Teal


parseAst :: String -> String
parseAst teal = case Teal.parseFromStdIn teal of
    Right expr -> "success...\n" ++ (showTree expr)
    Left err -> "failure!!!\n" ++ (show err)
    where showTree = Tree.drawForest . Teal.toForest



main :: IO ()
main = interact parseAst
