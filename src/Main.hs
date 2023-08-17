module Main () where

import Data.Text (Text)
import qualified Data.Text as T
import Text.XML
import Text.XML.Cursor

-- Our COLLADA parsing modules
import AssetLoader.AssetLoader
import AssetLoader.Animations
import AssetLoader.Camaras
import AssetLoader.Effects
import AssetLoader.Lights
import AssetLoader.Materials
import AssetLoader.Meshes
import AssetLoader.Textures



main = do
  -- Load asset
  asset <- loadAndParseDAE "sample.dae"

  -- Print effects
  putStrLn "Effects:"
  forM_ (assetEffects asset) $ \effect -> do
    putStrLn $ "Effect: " ++ effectId effect

    forM_ (effectParams effect) $ \param -> do
      putStrLn $ "  Param: " ++ paramSid param
      print $ paramType param

    putStrLn ""