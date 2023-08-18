module AssetLoader.Controller where
  
import AssetLoader.Datatypes
    ( Joint(..), Controller(..), Transform(..), VertexWeight (weightJointIndex, weightInfluence, VertexWeight) )
import Text.XML.Cursor ( attribute, element, ($//), (&|), Cursor )
import AssetLoader.Camaras ( parseVector3 )
import AssetLoader.Animations ( parseQDouble )
import Text.Read


parseControllers :: Cursor -> [Controller]
parseControllers c =
  c $// element "controller" &| parseController

parseController :: Cursor -> Controller
parseController c =
  Controller
    { controllerName = attribute "id" c
    , controllerRoot = parseJoint c -- root joint
    , controllerJoints = parseJoints c
    , controllerWeights = parseWeights c
    }

parseTransform :: Cursor -> Transform
parseTransform c =
  Transform
    { transTranslation = parseVector3 $ head $ c $// element "translate"
    , transRotation = parseQDouble $ head $ c $// element "rotate"
    , transScale = parseVector3 (c $// element "scale")
    }

parseJoint :: Cursor -> Joint
parseJoint c =
  Joint
    { jointName = attribute "id" c
    , jointParent = Nothing -- populate later
    , jointBindTransform = parseTransform c
    , jointInverseBindTransform = parseTransform c
    , jointLimits = parseJointLimits c
    }

parseWeights :: Cursor -> [VertexWeight]
parseWeights c =
  c $// element "weight" &| parseWeight

parseWeight :: Cursor -> VertexWeight
parseWeight c =
  VertexWeight
    { weightJointIndex = read $ attribute "joint" c
    , weightInfluence = read $ attribute "value" c
    }

parseJoints :: Cursor -> [Joint]
parseJoints c =
  c $// element "joint" &| parseJoint