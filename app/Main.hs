module Main where

import           SMT.Pretty.Prelude hiding (option)

import qualified Data.Text.IO as Text
import           Options.Applicative
import           Text.Megaparsec hiding (option)

import           SMT.Pretty

bvOptions :: Parser BVOptions
bvOptions = option bv (long "bv" <> value Hexadecimal)
  where
    bv =
      eitherReader
        (\s ->
           case s of
             "hex" -> Right Hexadecimal
             "bin" -> Right Binary
             "udec" -> Right (Decimal Unsigned)
             "dec" -> Right (Decimal Signed)
             _ -> Left ("Unknown bitvector option " <> show s))

fpOptions :: Parser FPOptions
fpOptions = option fp (long "fp" <> value FPDecimal)
  where
    fp =
      eitherReader
        (\s ->
           case s of
             "bin" -> Right FPBinary
             "dec" -> Right FPDecimal
             _ -> Left ("Unknown floating point option " <> show s))

options :: Parser Options
options = Options <$> bvOptions <*> fpOptions

main :: IO ()
main = do
  opts <- execParser (info (options <**> helper) mempty)
  t <- Text.getContents
  case parse resultParser "" t of
    Left err -> do
      hPutStrLn stderr (parseErrorPretty' t err)
      exitFailure
    Right r -> print (runDocM opts (ppResult r))
