module AssetLoader.Datatypes where

import Numeric.Quaternion (QDouble)

data Vector3 = Vector3 Float Float Float
data Vector2 = Vector2 Float Float
data Vector4 = Vector4 Float Float Float Float
data Matrix4x4 = Matrix4x4 -- Define as you see fit, depending on your math library
data LightType = PointLight | DirectionalLight | SpotLight
data Color = Color Float Float Float Float

newtype FloatParam = FloatParam Float
newtype Vector2Param = Vector2Param Vector2
newtype Vector3Param = Vector3Param Vector3
newtype Vector4Param = Vector4Param Vector4
newtype Matrix4x4Param = Matrix4x4Param Matrix4x4
newtype BoolParam = BoolParam Bool
newtype IntParam = IntParam Int

data Asset = Asset
  { assetMeshes :: [Mesh]
  , assetMaterials :: [Material]
  , assetAnimations :: [Animation]
  , assetCameras :: [Camera]
  , assetLights :: [Light]
  , assetImages :: [Image]
  , assetTextures :: [Texture]
  , assetEffects :: [Effect]
  , assetControllers :: [Controller]
  , assetVisualScenes :: [VisualScene]
  }

data Mesh = Mesh
  { meshVertices :: [Vertex]
  , meshNormals :: [Vector3] -- One normal per face
  , meshTexcoords :: [Vector2] -- One texcoord per vertex
  , meshIndices :: [Int]
  , meshMaterial :: Maybe Material
  , meshVertexWeights :: [VertexSkin]
  }

data Vertex = Vertex
  { vertexPosition :: Vector3
  , vertexNormal :: Vector3
  , vertexTexCoord :: Vector2
  }

data TextureOrColor
  = TextureValue Texture
  | ColorValue Color

data Texture = Texture
  { textureSampler :: Text
  , textureTexcoord :: Text
  }

data Material = Material
  { materialName :: Text
  , materialDiffuse :: TextureOrColor
  , materialSpecular :: TextureOrColor
  , materialAmbient :: TextureOrColor
  , materialEmissive :: TextureOrColor
  , materialTransparent :: TextureOrColor
  , materialOpacity :: Float
  , materialShininess :: Float
  , materialReflective :: TextureOrColor
  }


data Animation = Animation
  { animationName :: Text
  , animationClips :: [AnimationClip]
  }

data AnimationClip = AnimationClip
  { clipName :: Text
  , clipKeys :: [AnimationKey]
  , clipTargets :: [Text] -- nodes animated
  }

data AnimationKey = AnimationKey
  { keyTime :: Float
  , keyTranslation :: Vector3
  , keyRotation :: QDouble
  , keyScale :: Vector3
  }


data Transform = Transform
  { transTranslation :: Vector3
  , transRotation :: QDouble
  , transScale :: Vector3
  }

data VertexSkin = VertexSkin
  { vertexJointIndices :: [Int] -- Joint indices affecting vertex
  , vertexWeights :: [Float] -- Weight values
  }

data Camera = Camera
  { cameraName :: Text
  , cameraNode :: Maybe Node -- scene graph node
  , cameraProjections :: [Projection] -- multiple projections
  , cameraLenses :: [CameraLens] -- lens attributes
  }

data Projection =
    PerspectiveProjection Float -- fov
  | OrthographicProjection Float -- ortho size

data CameraLens = CameraLens
  { lensFov :: Float
  , lensOffset :: Vector2
  }

data Light = Light
  { lightName :: Text
  , lightType :: LightType
  , lightNode :: Maybe Node
  , lightColor :: Color
  , lightIntensity :: Float
  , lightAttenuation :: Attenuation
  , lightSpotAngles :: (Float, Float)
  , lightAreaSize :: (Float, Float)
  }

data Attenuation = Attenuation
  { attenConst :: Float
  , attenLinear :: Float
  , attenQuad :: Float
  }

data Surface = Surface
  { surfaceType :: Text
  , surfaceInitFrom :: Text
  }

newtype Sampler = Sampler
  { samplerSource :: Text
  }

data Lambert = Lambert
  { lambertEmission :: Color
  , lambertDiffuse :: Texture
  , lambertIor :: Float
  }

data Image = Image
  { imageId :: Text
  , imagePath :: Text
  , imageDimensions :: (Int, Int)
  , imageData :: ByteString
  }

data Effect = Effect
  { effectId :: Text
  , effectParams :: [EffectParam]
  }

data EffectParam = EffectParam
  { paramSid :: Text
  , paramType :: ParamType
  }

data Controller = Controller
  { controllerName :: Text
  , controllerRoot :: Joint
  , controllerJoints :: [Joint]
  , controllerWeights :: [VertexWeight]
  }

data Joint = Joint
  { jointName :: Text
  , jointParent :: Maybe Joint -- Parent joint
  , jointBindTransform :: Transform
  , jointInverseBindTransform :: Transform
  , jointLimits :: JointLimits -- joint rotation limits
  }

data JointLimits = JointLimits
  { limitMin :: Vector3 -- min rotation
  , limitMax :: Vector3 -- max rotation
  }

data VertexWeight = VertexWeight
  { weightJointIndex :: Int -- joint affecting vertex
  , weightInfluence :: Float -- influence amount
  }

data ParamType
  = SurfaceParam Surface
  | SamplerParam Sampler
  | LambertParam Lambert

data Node = Node
  { nodeId :: Text
  , nodeName :: Text
  , nodeSid :: Text
  , nodeType :: Text
  , nodeTransform :: Matrix4x4
  , nodeChildren :: [Node]
  , nodeExtras :: [Extra]
  }

newtype Extra = Extra
  { extraTechnique :: Technique
  }

data Technique = Technique
  { techniqueProfile :: Text
  , techniqueProperties :: [Property]
  }

data Property = Property
  { propSid :: Text
  , propType :: Text
  , propValue :: Text
  }

data VisualScene = VisualScene
  { sceneId :: Text
  , sceneName :: Text
  , sceneNodes :: [Node]
  }