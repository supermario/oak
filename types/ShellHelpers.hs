module ShellHelpers where

import Language.Haskell.Exts.Simple
import Turtle
import Filesystem.Path.CurrentOS (fromText)
import qualified Text.PrettyPrint.ANSI.Leijen as A

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

-- Shell ANSI coloring

colorTest :: IO ()
colorTest =
  A.putDoc
    $     A.text "All the colors are: "
    <>    A.comma
    A.<+> A.black (A.text "black")
    A.<+> A.red (A.text "red")
    A.<+> A.green (A.text "green")
    A.<+> A.yellow (A.text "yellow")
    A.<+> A.blue (A.text "blue")
    A.<+> A.magenta (A.text "magenta")
    A.<+> A.cyan (A.text "cyan")
    A.<+> A.white (A.text "white")
    A.<+> A.dullblack (A.text "dullblack")
    A.<+> A.dullred (A.text "dullred")
    A.<+> A.dullgreen (A.text "dullgreen")
    A.<+> A.dullyellow (A.text "dullyellow")
    A.<+> A.dullblue (A.text "dullblue")
    A.<+> A.dullmagenta (A.text "dullmagenta")
    A.<+> A.dullcyan (A.text "dullcyan")
    A.<+> A.dullwhite (A.text "dullwhite")
    <>    A.char '!'
    <>    A.linebreak


putColored :: A.Doc -> IO ()
putColored = A.putDoc


red :: String -> A.Doc
red = A.dullred . A.text


green :: String -> A.Doc
green = A.dullgreen . A.text


yellow :: String -> A.Doc
yellow = A.dullyellow . A.text


blue :: String -> A.Doc
blue = A.dullblue . A.text


magenta :: String -> A.Doc
magenta = A.dullmagenta . A.text


cyan :: String -> A.Doc
cyan = A.dullcyan . A.text


white :: String -> A.Doc
white = A.dullwhite . A.text
