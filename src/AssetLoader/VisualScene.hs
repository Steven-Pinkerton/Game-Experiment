{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use 'listToMaybe' from Relude" #-}
module AssetLoader.VisualScene where
import Text.XML.Cursor ( attribute, element, ($//), (&|), Cursor, (&//), content, ($/) )
import AssetLoader.Datatypes ( VisualScene(..), Node(..), Property (Property, propSid, propType, propValue), Extra (Extra), Technique (Technique, techniqueProfile, techniqueProperties), Matrix4x4 (Matrix4x4) )
import Data.Maybe ( fromJust, listToMaybe, fromMaybe, Maybe (Nothing) )
import Text.Read ( read )
import Data.Text (splitOn)
import Prelude (head, ($), Monad (return), viaNonEmpty, Semigroup ((<>)), ToString (toString), (.), (<$>), Text, fromMaybe)
import Data.List ( concatMap, (!!) )
import Text.XML ( Name(nameLocalName, Name, nameNamespace, namePrefix) )


toName :: Text -> Name
toName txt = Name {nameLocalName = txt, nameNamespace = Nothing, namePrefix = Nothing}


parseVisualScenes :: Cursor -> [VisualScene]
parseVisualScenes cursor =
  let daeNs = "{DAE_NAMESPACE}" :: Text
   in concatMap
        (\c -> c $// element (toName $ daeNs <> "visual_scene") &| parseVisualScene)
        (cursor $// element (toName $ daeNs <> "library_visual_scenes"))

parseVisualScene :: Cursor -> VisualScene
parseVisualScene c =
  VisualScene
    { sceneId = fromJust $ listToMaybe $ attribute "id" c
    , sceneName = fromJust $ listToMaybe $ attribute "name" c
    , sceneNodes = parseNodes c
    }

parseNodes :: Cursor -> [AssetLoader.Datatypes.Node]
parseNodes c =
  c $// element "{DAE_NAMESPACE}node" &| parseNode

parseNode :: Cursor -> AssetLoader.Datatypes.Node
parseNode c =
  Node
    { nodeId = fromJust $ listToMaybe $ attribute "id" c
    , nodeName = fromJust $ listToMaybe $ attribute "name" c
    , nodeSid = fromJust $ listToMaybe $ attribute "sid" c
    , nodeType = fromJust $ listToMaybe $ attribute "type" c
    , nodeTransform = parseMatrix c
    , nodeChildren = parseNodes c
    , nodeExtras = parseExtras c
    }


parseExtras :: Cursor -> [Extra]
parseExtras c = do
  techniques <- parseTechniques c
  return $ Extra techniques

parseProperties :: Cursor -> [Property]
parseProperties c =
  c $// element "technique"
    &// element "value"
    &| parseProperty

parseProperty :: Cursor -> Property
parseProperty c =
  Property
    { propSid = fromMaybe "" $ viaNonEmpty Prelude.head (attribute "sid" c)
    , propType = fromMaybe "" $ viaNonEmpty Prelude.head (attribute "type" c)
    , propValue = fromMaybe "" $ viaNonEmpty Prelude.head (c $/ content)
    }

parseTechnique :: Cursor -> Technique
parseTechnique c =
  Technique
    { techniqueProfile = "Custom"
    , techniqueProperties = parseProperties c
    }

parseTechniques :: Cursor -> [Technique]
parseTechniques c =
  [ Technique
      { techniqueProfile = "Custom"
      , techniqueProperties = parseProperties c
      }
  ]

parseMatrix :: Cursor -> Matrix4x4
parseMatrix c =
  let maybeContent = listToMaybe $ content c
      contentText = Prelude.fromMaybe "" maybeContent
      values = read . toString <$> splitOn " " contentText
    in Matrix4x4
      (values !! 0) (values !! 1) (values !! 2) (values !! 3)
      (values !! 4) (values !! 5) (values !! 6) (values !! 7)
      (values !! 8) (values !! 9) (values !! 10) (values !! 11)
      (values !! 12) (values !! 13) (values !! 14) (values !! 15)