module AssetLoader.VisualScene where
import Text.XML.Cursor ( attribute, element, ($//), (&|), Cursor )
import AssetLoader.Datatypes ( VisualScene(..), Node(..) )




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