{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
module AssetLoader where


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


parseVisualScenes :: Cursor -> [VisualScene]
parseVisualScenes cursor =
  cursor
    $// element "{DAE_NAMESPACE}library_visual_scenes"
    $// element "{DAE_NAMESPACE}visual_scene"
    &| parseVisualScene

parseVisualScene :: Cursor -> VisualScene
parseVisualScene c =
  VisualScene
    { sceneId = attribute "id" c
    , sceneName = attribute "name" c
    , sceneNodes = parseNodes c
    }

parseNodes :: Cursor -> [Node]
parseNodes c =
  c $// element "{DAE_NAMESPACE}node" &| parseNode

parseNode :: Cursor -> Node
parseNode c =
  Node
    { nodeId = attribute "id" c
    , nodeName = attribute "name" c
    , nodeSid = attribute "sid" c
    , nodeType = attribute "type" c
    , nodeTransform = parseMatrix c
    , nodeChildren = parseNodes c
    , nodeExtras = parseExtras c
    }

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
    , parseVisualScenes = parseVisualScenes cursor
    , assetAnimations = parseAnimations cursor
    , assetImages = parseImages cursor 
    , assetTextures = parseTextures cursor
    , assetEffects = parseEffects cursor
    , assetControllers = parseControllers cursor
    , assetvisualScenes = parseVisualScenes cursor
    }

main :: IO ()
main = do
  doc <- loadDAEFile "path_to_your_file.dae"
  let cursor = fromDocument doc
  let asset = parseAsset cursor
  undefined

-- Do something with asset, e.g., print it out, render it, etc.