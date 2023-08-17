module AssetLoader.AssetLoader where


import Data.Text qualified as T
import Text.XML ( parseLBS_, def, Document )
import Text.XML.Cursor
    ( attribute,
      content,
      element,
      fromDocument,
      ($//),
      (&/),
      (&|),
      Cursor )
import AssetLoader.Datatypes

-- Parse transform, children nodes, extras etc

parseAssetVisualScenes :: Cursor -> Asset
parseAssetVisualScenes c =
  let scenes = parseVisualScenes c
   in asset {assetScenes = scenes}


loadDAEFile :: FilePath -> IO Document
loadDAEFile path = do
  fileContent <- readFile path
  return $ parseLBS_ def fileContent

parseFloatArray :: Cursor -> [Float]
parseFloatArray c =
  let values = c &/ content
      individualValues = splitOn " " values
   in map read individualValues

interpolateKeys :: AnimationKey -> AnimationKey -> Float -> Transform
interpolateKeys key1 key2 t =
  Transform
    { transTranslation = // interpolate translation
    , transRotation = // interpolate rotation
    , transScale = // interpolate scale
    }

parseAsset :: Cursor -> Asset
parseAsset cursor =
  Asset
    { assetMeshes = parseMeshes cursor
    , assetCameras = parseCameras cursor
    , assetLights = parseLights cursor
    , assetMaterials = parseMaterials cursor
    , assetAnimations = parseAnimations cursor
    , assetImages = parseImages cursor 
    , assetTextures = parseTextures cursor
    , assetEffects = parseEffects cursor
    , assetControllers = parseControllers cursor
    , assetvisualScenes = parseVisualScenes cursor
    }

-- Do something with asset, e.g., print it out, render it, etc.