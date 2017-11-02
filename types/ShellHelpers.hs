module ShellHelpers where

import Language.Haskell.Exts.Simple
import Turtle
import Filesystem.Path.CurrentOS (fromText)

import Data.Char (toLower)
import qualified Data.Text as T
import Data.Text.Encoding as E
import Crypto.Hash


(|>) = (&)


loadFileAst :: Turtle.FilePath -> IO Module
loadFileAst filepath = fromParseResult <$> parseFile (asString filepath)


fileAstSha :: Turtle.FilePath -> IO Text
fileAstSha file = do
  ast <- loadFileAst file
  pure $ astSha ast


astSha :: Module -> Text
astSha ast = tShow $ sha1 $ tShow ast


sha1 :: Text -> Digest SHA1
sha1 = hash . E.encodeUtf8


asText :: Turtle.FilePath -> Text
asText = format fp

asString :: Turtle.FilePath -> String
asString = T.unpack . asText

tShow :: Show a => a -> Text
tShow = T.pack . show


doNothing :: IO ()
doNothing = pure ()


gs :: MonadIO io => io ()
gs = stdout $ inshell "git status" empty


lowercase :: String -> String
lowercase = map toLower


shellExec :: Text -> IO Text
shellExec cmd = strict $ inshell cmd empty
