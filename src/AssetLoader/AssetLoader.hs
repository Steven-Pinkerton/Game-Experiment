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

-- Let's add parsing for Camera
parseCameras :: Cursor -> [Camera]
parseCameras cursor = cursor $// element "{DAE_NAMESPACE}camera" &| parseCamera

interpolateKeys :: AnimationKey -> AnimationKey -> Float -> Transform
interpolateKeys key1 key2 t =
  Transform
    { transTranslation = // interpolate translation
    , transRotation = // interpolate rotation
    , transScale = // interpolate scale
    }


parseCamera :: Cursor -> Camera
parseCamera c =
  Camera
    { cameraName = toString $ attribute "name" c
    , cameraPosition = parseVector3 (c $// element "{DAE_NAMESPACE}position")
    , cameraTarget = parseVector3 (c $// element "{DAE_NAMESPACE}target")
    , cameraUpVector = parseVector3 (c $// element "{DAE_NAMESPACE}up_vector")
    , cameraFov = read $ toString $ head (c $// element "{DAE_NAMESPACE}fov" &/ content)
    , cameraNearClip = read $ toString $ head (c $// element "{DAE_NAMESPACE}near_clip" &/ content)
    , cameraFarClip = read $ toString $ head (c $// element "{DAE_NAMESPACE}far_clip" &/ content)
    }

-- And likewise for Light
parseLights :: Cursor -> [Light]
parseLights cursor = cursor $// element "{DAE_NAMESPACE}light" &| parseLight

parseLight :: Cursor -> Light
parseLight c =
  Light
    { lightName = toString $ attribute "name" c
    , lightType = parseLightType c
    , lightPosition = parseVector3 (c $// element "{DAE_NAMESPACE}position")
    , lightDirection = parseVector3 (c $// element "{DAE_NAMESPACE}direction")
    , lightColor = parseColor (c $// element "{DAE_NAMESPACE}color")
    , lightIntensity = read $ toString $ head (c $// element "{DAE_NAMESPACE}intensity" &/ content)
    }

-- Helper functions to parse Vector3, Color, and LightType
parseVector3 :: Cursor -> Vector3
parseVector3 c =
  let coords = T.splitOn "," $ head (c &/ content)
   in Vector3 (read $ toString (head coords)) (read $ toString (coords !! 1)) (read $ toString (coords !! 2))

parseColor :: Cursor -> Color
parseColor c =
  let rgba = T.splitOn "," $ head (c &/ content)
   in Color (read $ toString (head rgba)) (read $ toString (rgba !! 1)) (read $ toString (rgba !! 2)) (read $ toString (rgba !! 3))

parseLightType :: Cursor -> LightType
parseLightType c
  | typeStr == "point" = PointLight
  | typeStr == "directional" = DirectionalLight
  | typeStr == "spot" = SpotLight
  where
    typeStr = toString $ head (c $// element "{DAE_NAMESPACE}type" &/ content)

parseAsset :: Cursor -> Asset
parseAsset cursor =
  Asset
    { assetMeshes = parseMeshes cursor
    , assetCameras = parseCameras cursor
    , assetLights = parseLights cursor
    , assetMaterials = parseMaterials cursor
    , parseVisualScenes = parseVisualScenes cursor
    , assetAnimations = parseAnimations cursor
    }


parseMaterials :: Cursor -> [Material]
parseMaterials cursor = cursor $// element "{DAE_NAMESPACE}material" &| parseMaterial

parseMaterial :: Cursor -> Material
parseMaterial c =
  Material
    { materialDiffuse = parseColor (c $// element "{DAE_NAMESPACE}diffuse" &/ content)
    , materialSpecular = parseColor (c $// element "{DAE_NAMESPACE}specular" &/ content)
    }

main :: IO ()
main = do
  doc <- loadDAEFile "path_to_your_file.dae"
  let cursor = fromDocument doc
  let asset = parseAsset cursor
  undefined

-- Do something with asset, e.g., print it out, render it, etc.