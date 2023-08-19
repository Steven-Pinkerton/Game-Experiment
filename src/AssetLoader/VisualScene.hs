{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use 'listToMaybe' from Relude" #-}
module AssetLoader.VisualScene where
import Text.XML.Cursor ( attribute, element, ($//), (&|), Cursor, (&//), content, (&/), ($/) )
import AssetLoader.Datatypes ( VisualScene(..), Node(..), Property (Property, propSid, propType, propValue) )
import Data.Maybe ( fromJust )


parseVisualScenes :: Cursor -> [VisualScene]
parseVisualScenes cursor =
  let daeNs = "{DAE_NAMESPACE}"
   in concatMap
        (\c -> c $// element (daeNs <> "visual_scene") &| parseVisualScene)
        (cursor $// element (daeNs <> "library_visual_scenes"))

parseVisualScene :: Cursor -> VisualScene
parseVisualScene c =
  VisualScene
    { sceneId = fromJust $ listToMaybe $ attribute "id" c
    , sceneName = fromJust $ listToMaybe $ attribute "name" c
    , sceneNodes = parseNodes c
    }

parseNodes :: Cursor -> [Node]
parseNodes c =
  c $// element "{DAE_NAMESPACE}node" &| parseNode

parseNode :: Cursor -> Node
parseNode c =
  Node
    { nodeId = fromJust $ listToMaybe $ attribute "id" c
    , nodeName = fromJust $ listToMaybe $ attribute "name" c
    , nodeSid = fromJust $ listToMaybe $ attribute "sid" c
    , nodeType = fromJust $ listToMaybe $ attribute "type" c
    , nodeTransform = fromJust $ listToMaybe $ parseMatrix c
    , nodeChildren = parseNodes c
    , nodeExtras = fromJust $ listToMaybe $ parseExtras c
    }

newtype Extra = Extra
  {extraValues :: [Property]}

parseExtras :: Cursor -> [Extra]
parseExtras c = do
  extra <- c $// element "extra"
  return $ Extra (parseProperties extra)

parseProperties :: Cursor -> [Property]
parseProperties c =
  c $// element "technique"
    &// element "value"
    &| parseProperty

parseProperty :: Cursor -> Property
parseProperty c =
  Property
    { propSid = fromMaybe "" $ viaNonEmpty head (attribute "sid" c)
    , propType = fromMaybe "" $ viaNonEmpty head (attribute "type" c)
    , propValue = fromMaybe "" $ viaNonEmpty head (c $/ content)
    }