module Datatypes where


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
  , animationKeys :: [AnimationKey]
  }

data AnimationKey = AnimationKey
  { keyTime :: Float
  , keyMatrix :: Transform
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
  , cameraPosition :: Vector3
  , cameraTarget :: Vector3 -- Where the camera is looking
  , cameraUpVector :: Vector3 -- Usually (0, 1, 0) to represent the "up" direction
  , cameraFov :: Float -- Field of view
  , cameraNearClip :: Float -- Near clipping plane
  , cameraFarClip :: Float -- Far clipping plane
  -- ... other camera properties
  }

data Light = Light
  { lightName :: String
  , lightType :: LightType
  , lightPosition :: Vector3
  , lightDirection :: Vector3 -- Relevant for directional and spotlights
  , lightColor :: Color -- RGB value of the light
  , lightIntensity :: Float -- Overall strength/magnitude of the light
  -- ... other light properties like attenuation, spot angle, etc.
  }

data Surface = Surface
  { surfaceType :: String
  , surfaceInitFrom :: String
  }

data Sampler = Sampler
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
  }

data Effect = Effect
  { effectId :: String
  , effectParams :: [EffectParam]
  }

data EffectParam = EffectParam
  { paramSid :: String
  , paramType :: ParamType
  }

data Controller = Controller
  { controllerName :: String
  , controllerJoints :: [Joint]
  }

data Joint = Joint
  { jointName :: String
  , jointBindTransform :: Matrix4x4
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

data Extra = Extra
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