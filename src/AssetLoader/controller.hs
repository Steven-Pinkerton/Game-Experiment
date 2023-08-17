module AssetLoader.Controller () where
  
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

parseJoint :: Cursor -> Joint
parseJoint c =
  Joint
    { jointName = attribute "id" c
    , jointParent = Nothing -- populate later
    , jointBindTransform = parseTransform c
    , jointInverseBindTransform = parseTransform c
    , jointLimits = parseJointLimits c
    }