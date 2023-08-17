module Main () where

import Data.Text (Text)
import qualified Data.Text as T
import Text.XML
import Text.XML.Cursor

-- Our COLLADA parsing modules
import AssetLoader.AssetLoader
import AssetLoader.Animation
import AssetLoader.Camera
import AssetLoader.Effect
import AssetLoader.Light
import AssetLoader.Material
import AssetLoader.Mesh
import AssetLoader.Textures

-- Utilities

import Utils.Color
import Utils.Matrix
import Utils.Vector


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