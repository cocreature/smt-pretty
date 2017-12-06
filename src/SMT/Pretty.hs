{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module SMT.Pretty
  ( resultParser
  , ppResult
  , runDocM
  , Options(..)
  , FPOptions(..)
  , BVOptions(..)
  , Signed(..)
  ) where

import           SMT.Pretty.Prelude

import           Control.Monad.Fail (fail)
import           Data.Char (intToDigit, isHexDigit)
import           Data.Scientific
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Data.Text.Prettyprint.Doc as PP
import           Data.Text.Prettyprint.Doc hiding (parens, (<>))
import           Numeric (readHex, showIntAtBase)
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import           Text.ParserCombinators.ReadP (ReadS)

data Result = Sat !Model | Unsat
  deriving (Show)

data Model =
  Model [DefineFun]
  deriving (Show)

data Type
  = Float16
  | BitVec !Word
  | ADTType !Text [Type]
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

data FPConstant
  = FPConstant { fpSign :: !Text
               , fpExp :: !Text
               , fpMantissa :: !Text }
  | ZeroPos !Word
            !Word
  | ZeroNeg !Word
            !Word
  deriving (Show)

data BVConstant =
  Hex !Text
  deriving (Show)

data Expr
  = FP !FPConstant
  | BV !BVConstant
  | App !Expr [Expr]
  | Var !Text
  deriving (Show)

data Signed = Signed | Unsigned

data BVOptions = Binary | Hexadecimal | Decimal Signed

data FPOptions = FPBinary | FPDecimal

data Options = Options { bvOpts :: !BVOptions, fpOpts :: !FPOptions }

newtype DocM a = DocM (Reader Options a)
  deriving (Functor, Applicative, Monad, MonadReader Options)

runDocM :: Options -> DocM a -> a
runDocM o (DocM r) = runReader r o

readBin :: Num a => Text -> a
readBin t =
  sum $
  zipWith
    (\c i ->
       case c of
         '0' -> 0
         '1' -> 2 ^ i)
    (reverse (toS t))
    [0 ..]

pow :: Scientific -> Int -> Scientific
pow x e
  | e >= 0 = x ^ e
  | otherwise = 1 / (1 ^ e)

fpToScientific :: Text -> Text -> Text -> Scientific
fpToScientific s e m
  | 16 == Text.length s + Text.length e + Text.length m =
    let e' = readBin e :: Int
        s' = (-1) ^ (readBin s :: Int) :: Scientific
    in if e' == 0
         then panic "denormalized"
         else let m' = readBin ("1" <> m)
              in s' * pow 2 (e' - 15) * m' / 2 ^ (10 :: Int)

ppFPConstant :: FPConstant -> DocM (Doc a)
ppFPConstant (FPConstant s e m) = do
  opts <- fpOpts <$> ask
  case opts of
    FPBinary ->
      pure
        (PP.parens
           ("fp" <+> "#b" <> pretty s <+> "#b" <> pretty e <+> "#b" <> pretty m))
    FPDecimal -> pure (pretty (formatScientific Fixed Nothing (fpToScientific s e m)))
ppFPConstant (ZeroPos e m) = do
  opts <- fpOpts <$> ask
  case opts of
    FPBinary -> pure (PP.parens ("_" <+> "+zero" <+> pretty e <+> pretty m))
    FPDecimal -> pure "+0"
ppFPConstant (ZeroNeg e m) = do
  opts <- fpOpts <$> ask
  case opts of
    FPBinary -> pure (PP.parens ("_" <+> "-zero" <+> pretty e <+> pretty m))
    FPDecimal -> pure "-0"

unsafeReadS :: ReadS a -> Text -> a
unsafeReadS r t =
  case r (toS t) of
    [(x, [])] -> x
    _ -> panic "readS failed"

ppBVConstant :: BVConstant -> DocM (Doc a)
ppBVConstant (Hex x) = do
  opts <- bvOpts <$> ask
  case opts of
    Hexadecimal -> pure ("#x" <> pretty x)
    Binary -> pure ("#b" <> pretty (concatMap hexDigitToBin (toS x :: [Char])))
    Decimal s -> do
      let w = unsafeReadS readHex x :: Word32
      case s of
        Signed -> pure (pretty (fromIntegral w :: Int32))
        Unsigned -> pure (pretty w)

hexDigitToBin :: Char -> [Char]
hexDigitToBin c =
  leftPad
    4
    '0'
    (showIntAtBase 2 intToDigit (unsafeReadS readHex (toS $ c : [])) "")

leftPad :: Int -> a -> [a] -> [a]
leftPad l x xs
  | l > length xs = replicate (l - length xs) x ++ xs
  | otherwise = xs

ppExpr :: Expr -> DocM (Doc a)
ppExpr (FP fp) = ppFPConstant fp
ppExpr (BV bv) = ppBVConstant bv
ppExpr (App f xs) = do
  ppF <- ppExpr f
  ppXs <- mapM ppExpr xs
  pure (PP.parens (ppF <+> align (vcat ppXs)))
ppExpr (Var n) = pure (pretty n)

ppResult :: Result -> DocM (Doc a)
ppResult (Sat m) = do
  ppM <- ppModel m
  pure (vcat ["sat", ppM] <> PP.line)
ppResult Unsat = pure ("unsat" <> PP.line)

ppModel :: Model -> DocM (Doc a)
ppModel (Model defs) = do
  ppDefs <- mapM ppDefineFun defs
  pure $ PP.parens (vsep ["model", indent 4 (vsep ppDefs)])

ppTypedVariable :: TypedVariable -> DocM (Doc a)
ppTypedVariable (TypedVariable name ty) = do
  ppTy <- ppType ty
  pure (PP.parens (pretty name <+> ppTy))

ppType :: Type -> DocM (Doc a)
ppType Float16 = pure "Float16"
ppType (BitVec w) = pure (PP.parens ("_" <+> "BitVec" <+> pretty w))
ppType (ADTType name params) = do
  ppParams <- mapM ppType params
  pure (PP.parens (pretty name <+> sep ppParams))

ppDefineFun :: DefineFun -> DocM (Doc a)
ppDefineFun (DefineFun name params retTy body) = do
  ppParams <- mapM ppTypedVariable params
  ppRetTy <- ppType retTy
  ppBody <- ppExpr body
  pure $ PP.parens (vcat ["define-fun" <+> pretty name <+> PP.parens (sep ppParams) <+> ppRetTy, indent 4 ppBody])

type Parser = Parsec Void Text

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
    cs :: [Char]
    cs = "~!@$%^&*_-+=<>.?/"
    p = toS <$> ((:) <$> (letterChar <|> oneOf cs) <*> many (alphaNumChar <|> oneOf cs))
    check x =
      if x `Set.member` reservedWords
        then fail $ "keyword " ++ show x ++ " cannot be an identifier"
        else return x

resultParser :: Parser Result
resultParser = unsat <|> sat
  where unsat = Unsat <$ symbol "unsat"
        sat = do
          _ <- symbol "sat"
          Sat <$> modelParser

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
typeParser = choice [Float16 <$ symbol "Float16", try bitvec <?> "BitVec type", adt <?> "ADT type"]
  where
    bitvec = BitVec <$> parens (symbol "_" *> symbol "BitVec" *> L.decimal)
    adt =
      parens (ADTType <$> identifier <*> many typeParser) <|>
      ADTType <$> identifier <*> pure []

typedVariableParser :: Parser TypedVariable
typedVariableParser = parens (TypedVariable <$> identifier <*> typeParser)

fpConstant :: Parser FPConstant
fpConstant =
  choice
    [ try $
      parens
        (symbol "fp" *>
         (FPConstant <$> binaryLiteral <*> binaryLiteral <*> binaryLiteral))
    , try $
      parens
        (symbol "_" *> symbol "+zero" *>
         (ZeroPos <$> lexeme L.decimal <*> lexeme L.decimal))
    , parens
        (symbol "_" *> symbol "-zero" *>
         (ZeroNeg <$> lexeme L.decimal <*> lexeme L.decimal))
    ]

exprParser :: Parser Expr
exprParser =
  choice
    [ try (FP <$> fpConstant) <?> "fp constant"
    , try bvConstant <?> "bv constant"
    , app <?> "app"
    , var <?> "var"
    ]
  where
    bvConstant = BV . Hex <$> hexLiteral
    app = parens (App <$> exprParser <*> many exprParser)
    var = Var <$> identifier

hexLiteral :: Parser Text
hexLiteral = lexeme $ do
  _ <- string "#x"
  toS <$> takeWhile1P (Just "hex literal") isHexDigit

binaryLiteral :: Parser Text
binaryLiteral = lexeme $ do
  _ <- string "#b"
  toS <$> takeWhile1P (Just "binary literal") (\c -> c `elem` ['0','1'])
