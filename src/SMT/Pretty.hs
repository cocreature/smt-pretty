module SMT.Pretty
  (
  ) where

import           SMT.Pretty.Prelude

import           Control.Monad.Fail (fail)
import qualified Data.Set as Set
import qualified Data.Text as Text
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

data Model =
  Model [DefineFun]
  deriving (Show)

data Type =
  Float16
  deriving (Show)

data TypedVariable = TypedVariable
  { varName :: !Text
  , varType :: !Type
  } deriving (Show)

data DefineFun = DefineFun
  { funName :: !Text
  , funParameters :: [TypedVariable]
  , funReturnType :: !Type
  , funBody :: Expr
  } deriving (Show)

data FPConstant = FPConstant
  { fpSign :: !Text
  , fpExp :: !Text
  , fpMantissa :: !Text
  } deriving (Show)

fp :: Text -> Text -> Text -> FPConstant
fp s e m = FPConstant s e m

data Expr =
  FP !FPConstant
  deriving (Show)

type Parser = Parsec Void Text

modelTest :: Text
modelTest =
  Text.unlines
    [ "(model"
    , "  (define-fun x () Float16"
    , "    (fp #b0 #b11011 #b0010010000))"
    , ")"
    ]

spaceConsumer :: Parser ()
spaceConsumer = L.space space1 (L.skipLineComment ";") (fail "No block comments")

lexeme :: Parser a -> Parser a
lexeme = L.lexeme spaceConsumer

symbol :: Text -> Parser Text
symbol = L.symbol spaceConsumer

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

reservedWords :: Set Text
reservedWords = mempty

identifier :: Parser Text
identifier = (lexeme . try) (p >>= check)
  where
    p = toS <$> ((:) <$> letterChar <*> many alphaNumChar)
    check x =
      if x `Set.member` reservedWords
        then fail $ "keyword " ++ show x ++ " cannot be an identifier"
        else return x

modelParser :: Parser Model
modelParser = parens $ do
  _ <- symbol "model"
  Model <$> many defineFunParser

defineFunParser :: Parser DefineFun
defineFunParser = parens $ do
  _ <- symbol "define-fun"
  name <- identifier
  parameters <- parens (many typedVariableParser)
  returnType <- typeParser
  body <- exprParser
  pure (DefineFun name parameters returnType body)

typeParser :: Parser Type
typeParser = Float16 <$ symbol "Float16"

typedVariableParser :: Parser TypedVariable
typedVariableParser = parens (TypedVariable <$> identifier <*> typeParser)

exprParser :: Parser Expr
exprParser = choice [fpConstant]
  where
    fpConstant =
      parens
        (do symbol "fp"
            FP <$> (fp <$> binaryLiteral <*> binaryLiteral <*> binaryLiteral))

binaryLiteral :: Parser Text
binaryLiteral = lexeme $ do
  _ <- string "#b"
  toS <$> takeWhile1P (Just "binary literal") (\c -> c `elem` ['0','1'])
