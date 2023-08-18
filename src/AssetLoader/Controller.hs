module AssetLoader.Controller where

import AssetLoader.Datatypes
    ( Joint(..), Controller(..), Transform(..), VertexWeight (weightJointIndex, weightInfluence, VertexWeight), JointLimits (limitMin, limitMax, JointLimits), Vector3 (Vector3) )
import Text.XML.Cursor ( attribute, element, ($//), (&|), Cursor, (&/), ($/) )
import AssetLoader.Camaras ( parseVector3 )
import AssetLoader.Animations ( parseQDouble )


parseControllers :: Cursor -> [Controller]
parseControllers c =
  c $// element "controller" &| parseController

parseController :: Cursor -> Controller
parseController c =
  Controller
    { controllerName = fromMaybe "" $ listToMaybe $ attribute "id" c
    , controllerRoot = parseJoint c -- root joint
    , controllerJoints = parseJoints c
    , controllerWeights = parseWeights c
    }

parseTransform :: Cursor -> Transform
parseTransform c =
  Transform
    { transTranslation = maybeParse parseVector3 (viaNonEmpty Prelude.head (c $// element "translate"))
    , transRotation = maybeParse parseQDouble (viaNonEmpty Prelude.head (c $// element "rotate"))
    , transScale = maybeParse parseVector3 (viaNonEmpty Prelude.head (c $// element "scale"))
    }

maybeParse :: (Cursor -> a) -> Maybe Cursor -> a
maybeParse parser mc =
  case mc of
    Just cursor -> parser cursor
    Nothing -> error "Failed to parse" -- Handle this appropriately

parseJoint :: Cursor -> Joint
parseJoint c =
  Joint
    { jointName = fromMaybe "" $ listToMaybe (attribute "id" c)
    , jointParent = Nothing
    , jointBindTransform = parseTransform c
    , jointInverseBindTransform = parseTransform c
    , jointLimits = parseJointLimits c
    }

parseWeight :: Cursor -> VertexWeight
parseWeight c =
  VertexWeight
    { weightJointIndex = fromMaybe 0 $ maybeReadInt . Prelude.toList $ attribute "joint" c
    , weightInfluence = fromMaybe 0.0 $ maybeRead . Prelude.toList $ attribute "value" c
    }


parseJoints :: Cursor -> [Joint]
parseJoints c =
  c $// element "joint" &| parseJoint

maybeRead :: [Text] -> Maybe Float
maybeRead [] = Nothing
maybeRead (t : _) = readMaybe . toString $ t

maybeReadInt :: [Text] -> Maybe Int
maybeReadInt [] = Nothing
maybeReadInt (t : _) = readMaybe . toString $ t

parseWeights :: Cursor -> [VertexWeight]
parseWeights c = c $// element "weights" &/ element "weight" &| parseWeight


parseJointLimits :: Cursor -> JointLimits
parseJointLimits c =
  JointLimits
    { limitMin = parseLimit "min"
    , limitMax = parseLimit "max"
    }
  where
    parseLimit tag =
      let limitCursors = c $/ element "limits" &/ element tag &| Prelude.id
          limitCursor = viaNonEmpty Prelude.head limitCursors
          x = (maybeRead . attribute "x") =<< limitCursor
          y = (maybeRead . attribute "y") =<< limitCursor
          z = (maybeRead . attribute "z") =<< limitCursor
      in case (x, y, z) of
          (Just xVal, Just yVal, Just zVal) -> Vector3 xVal yVal zVal
          _ -> error "Failed to parse limits" -- or handle this more gracefully

toList :: Maybe Text -> [Text]
toList Nothing = []
toList (Just t) = [t]