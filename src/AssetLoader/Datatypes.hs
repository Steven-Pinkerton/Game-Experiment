module AssetLoader.Datatypes where


data Vector3 = Vector3 Float Float Float
data Vector2 = Vector2 Float Float
data Color = Color Float Float Float Float -- RGBA
data Matrix4x4 = Matrix4x4 -- Define as you see fit, depending on your math library
data LightType = PointLight | DirectionalLight | SpotLight

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
  = Texture Texture
  | Color Color

data Material = Material
  { materialName :: String
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
  { animationName :: String
  , animationClips :: [AnimationClip]
  }

data AnimationClip = AnimationClip
  { clipName :: String
  , clipKeys :: [AnimationKey]
  , clipTargets :: [String] -- nodes animated
  }

data AnimationKey = AnimationKey
  { keyTime :: Float
  , keyTranslation :: Vector3
  , keyRotation :: Quaternion
  , keyScale :: Vector3
  }


data Transform = Transform
  { transTranslation :: Vector3
  , transRotation :: Quaternion
  , transScale :: Vector3
  }

data VertexSkin = VertexSkin
  { vertexJointIndices :: [Int] -- Joint indices affecting vertex
  , vertexWeights :: [Float] -- Weight values
  }

data Camera = Camera
  { cameraName :: String
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
  { lightName :: String
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
  { surfaceType :: String
  , surfaceInitFrom :: String
  }

newtype Sampler = Sampler
  { samplerSource :: String
  }

data Lambert = Lambert
  { lambertEmission :: Color
  , lambertDiffuse :: Texture
  , lambertIor :: Float
  }

data Texture = Texture
  { textureSampler :: String
  , textureTexcoord :: String
  }

data Image = Image
  { imageId :: String
  , imagePath :: String
  , imageDimensions :: (Int, Int)
  , imageData :: ByteString
  }

data Effect = Effect
  { effectId :: String
  , effectParams :: [EffectParam]
  }

data Controller = Controller
  { controllerName :: String
  , controllerRoot :: Joint
  , controllerJoints :: [Joint]
  , controllerWeights :: [VertexWeight]
  }

data Joint = Joint
  { jointName :: String
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
  { nodeId :: String
  , nodeName :: String
  , nodeSid :: String
  , nodeType :: String
  , nodeTransform :: Matrix4x4
  , nodeChildren :: [Node]
  , nodeExtras :: [Extra]
  }

newtype Extra = Extra
  { extraTechnique :: Technique
  }

data Technique = Technique
  { techniqueProfile :: String
  , techniqueProperties :: [Property]
  }

data Property = Property
  { propSid :: String
  , propType :: String
  , propValue :: String
  }

data VisualScene = VisualScene
  { sceneId :: String
  , sceneName :: String
  , sceneNodes :: [Node]
  }