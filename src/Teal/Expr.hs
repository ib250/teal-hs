module Teal.Expr where


import           Control.Applicative
import           Data.Functor.Identity
    ( Identity
    )
import qualified Data.List                     as DL
import qualified Data.Map                      as DM
import qualified Data.String                   as DS
import           Text.Parsec
    ( Parsec
    , eof
    )
import qualified Text.Parsec.Expr              as Expr
import qualified Text.Parsec.Language          as Lang
import qualified Text.Parsec.Token             as Token
import qualified Text.ParserCombinators.Parsec as P


tealDef :: Lang.LanguageDef st
tealDef = Lang.emptyDef
    { Token.commentStart    = "/*"
    , Token.commentEnd      = "*/"
    , Token.commentLine     = "//"
    , Token.identStart      = P.letter
    , Token.identLetter     = P.alphaNum <|> specialIdentChars
    , Token.reservedNames   = reservedNames
    , Token.reservedOpNames = reservedOpNames
    }
  where reservedOpNames  = DL.words "+ - * / = < <= > >= == and or not async await <->"
        specialIdentChars = P.oneOf "_"
        reservedNames    = DL.words $
            ("if else true false and or not async await fn import " ++
             "python cplusplus haskell")


tealLexer :: Token.TokenParser st
tealLexer = Token.makeTokenParser tealDef


---------------------------------------
-- aliases
---------------------------------------
identifier = Token.identifier tealLexer
reserved   = Token.reserved tealLexer
reservedOp = Token.reservedOp tealLexer
---------------------------------------
parens     = Token.parens tealLexer
braces     = Token.braces tealLexer
brackets   = Token.brackets tealLexer
comma      = Token.comma tealLexer
commaSep   = Token.commaSep tealLexer
colon      = Token.colon tealLexer
dot        = Token.dot tealLexer
whiteSpace = Token.whiteSpace tealLexer
---------------------------------------------
integer       = Token.integer tealLexer
floats        = Token.float tealLexer
stringLiteral = Token.stringLiteral tealLexer
---------------------------------------------


data TealFunction a = TealFunction String [String] a
    deriving (Eq, Show)

parseFunction' :: P.Parser a -> P.Parser (TealFunction a)
parseFunction' p = do
    reserved "fn"
    functionName <- identifier
    args <- parens $ commaSep identifier
    body <- braces p
    return $ TealFunction functionName args body


data BinOp = TEq
    | TNEq
    | TGt
    | TLt
    | TLte
    | TGte
    | TNGe
    | TAnd
    | TOr
    | TUpdate
    | TPlus
    | TMinus
    | TMul
    | TDiv
    | TAssign
    deriving (Eq, Show)


data UnaryOp = TNot
    | TNeg
    | TAsync
    | TAwait
    deriving (Eq, Show)


data TealFFILang = Python
    | CPlusPlus
    | Haskell
    deriving (Eq, Show)


data TealExpr = TealBool Bool
    | TealVar String
    | TealInt Integer
    | TealFloat Double
    | TealString String
    | TealList [TealExpr]
    | TealMap (DM.Map String TealExpr)
    | TealUnaryOp UnaryOp TealExpr
    | TealBinaryOp BinOp TealExpr TealExpr
    | TealIf TealExpr [TealExpr] [TealExpr]
    | TealApply String [TealExpr]
    | TealFunctionDecl (TealFunction [TealExpr])
    | TealImport [String]
    deriving (Eq, Show)



uOp :: String -> UnaryOp -> Parsec String u (TealExpr -> TealExpr)
uOp stringRep op = reservedOp stringRep >> uOp'
    where uOp' = pure $ TealUnaryOp op


bOp :: String -> BinOp -> Parsec String u (TealExpr -> TealExpr -> TealExpr)
bOp stringRep op = reservedOp stringRep >> bOp'
    where bOp' = pure $ TealBinaryOp op


------------------------------------------------
-- auxilliary parsers
------------------------------------------------
parseTerm' :: P.Parser TealExpr
parseTerm' =  parens parseTerm
          <|> (P.choice $ fmap P.try parsers)
    where parsers = [ parseString
                    , parseApply
                    , parseFunction
                    , parseBool
                    , parseFloat
                    , parseInt
                    , parseList
                    , parseMap
                    , parseIf
                    , parseVar
                    ]

parseIf = liftA3 TealIf cond parseBlock elseBlock
    where cond = reserved "if" >> parseTerm
          parseBlock = braces $ P.many1 parseTerm
          elseBlock = reserved "else" >> parseBlock

parseFunction = fmap TealFunctionDecl decl
    where decl = parseFunction' $ P.many parseTerm

parseApply    = P.try $ do
    functionName <- identifier
    args <- parens $ commaSep parseTerm
    return $ TealApply functionName args

parseBool = fmap TealBool $ parseTrue <|> parseFalse
    where parseTrue = reserved "true" >> pure True
          parseFalse = reserved "false" >> pure False

parseInt = fmap TealInt $ integer

parseFloat = fmap TealFloat $ floats

parseString = fmap TealString $ stringLiteral

parseList = fmap TealList (brackets $ commaSep parseTerm)

parseVar = fmap TealVar identifier

parseMap = fmap toTealMap (braces $ commaSep pairs)
    where toTealMap = TealMap . DM.fromList
          pairs = do
            key <- stringLiteral
            colon
            value <- parseTerm
            return $ (key, value)

-- TODO implement this guy
parseImport = reserved "import" >> pure TealImport


----------------------------------
-- main parser
----------------------------------
parseSource :: P.Parser [TealExpr]
parseSource = do
    whiteSpace
    contents <- P.many parseTerm
    eof
    return contents

parseTerm :: P.Parser TealExpr
parseTerm = Expr.buildExpressionParser precedenceTable parseTerm'
    where precedenceTable =
            [ [ Expr.Prefix ("-"     `uOp` TNeg)
              , Expr.Prefix ("not"   `uOp` TNot)
              , Expr.Prefix ("async" `uOp` TAsync)
              , Expr.Prefix ("await" `uOp` TAwait)
              ]
            , [ Expr.Infix  ("*" `bOp` TMul) Expr.AssocLeft
              , Expr.Infix  ("/" `bOp` TDiv) Expr.AssocLeft
              ]
            , [ Expr.Infix  ("+"   `bOp` TPlus)   Expr.AssocLeft
              , Expr.Infix  ("-"   `bOp` TMinus) Expr.AssocLeft
              ]
            , [ Expr.Infix  ("=="  `bOp` TEq)  Expr.AssocLeft
              , Expr.Infix  (">"   `bOp` TGt)  Expr.AssocLeft
              , Expr.Infix  ("<"   `bOp` TLt)  Expr.AssocLeft
              , Expr.Infix  (">="  `bOp` TGte) Expr.AssocLeft
              , Expr.Infix  ("<="  `bOp` TLte) Expr.AssocLeft
              , Expr.Infix  ("and" `bOp` TAnd) Expr.AssocLeft
              , Expr.Infix  ("or"  `bOp` TOr)  Expr.AssocLeft
              ]
            , [ Expr.Infix ("<->" `bOp` TUpdate) Expr.AssocNone
              , Expr.Infix ("="   `bOp` TAssign) Expr.AssocNone
              ]
            ]

