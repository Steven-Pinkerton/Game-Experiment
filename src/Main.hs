module Main where
import qualified Main.Utf8 as Utf8

main :: IO ()
main = do
  -- For withUtf8, see https://serokell.io/blog/haskell-with-utf8
  Utf8.withUtf8 $ do
    putTextLn "Hello 🌎"
