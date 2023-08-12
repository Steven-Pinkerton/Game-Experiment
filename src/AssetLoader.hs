{-# LANGUAGE ForeignFunctionInterface #-}

module AssetLoader where

import Foreign
import Foreign.C.String
import Foreign.C.Types
import Foreign.Ptr

data Vector3D = Vector3D Float Float Float deriving (Show, Eq)

type Triangle = (Int, Int, Int)

data Asset = Asset
  { assetVertices :: [Vector3D]
  , assetTriangles :: [Triangle]
  , assetNormals :: [Vector3D]
  , assetMaterial :: Material
  } deriving (Show, Eq)

data Color3D = Color3D Float Float Float deriving (Show, Eq)

data Material = Material
  { materialDiffuse :: Color3D
  , materialAmbient :: Color3D
  , materialSpecular :: Color3D
  }
  deriving (Show, Eq)

type Matrix4x4 =
  ( (Float, Float, Float, Float)
  , (Float, Float, Float, Float)
  , (Float, Float, Float, Float)
  , (Float, Float, Float, Float)
  )

data HNode = HNode
  { nodeName :: String
  , nodeTransform :: Matrix4x4
  , nodeChildren :: [HNode]
  }
  deriving (Show, Eq)

-- Opaque data structures representing Assimp structures
data CScene

data CMaterial

data CNode

data CMatrix4x4



-- FFI declarations for relevant Assimp functions
foreign import ccall unsafe "assimp/cimport.h aiImportFile"
  c_aiImportFile :: CString -> CUInt -> IO (Ptr CScene)

foreign import ccall unsafe "assimp/cimport.h aiReleaseImport"
  c_aiReleaseImport :: Ptr CScene -> IO ()

foreign import ccall unsafe "assimp/cimport.h aiGetErrorString"
  c_aiGetErrorString :: IO CString

foreign import ccall unsafe "assimp/material.h aiGetMaterialColor"
  c_aiGetMaterialColor :: Ptr CMaterial -> CString -> CUInt -> CUInt -> Ptr Color3D -> IO CInt

  -- Assuming definitions for C types like CFloat
data CVector3D = CVector3D CFloat CFloat CFloat

data CMesh
foreign import ccall unsafe "assimp/mesh.h aiMesh_mNumVertices"
  c_aiMesh_mNumVertices :: Ptr CMesh -> IO CUInt

foreign import ccall unsafe "assimp/mesh.h aiMesh_mVertices"
  c_aiMesh_mVertices :: Ptr CMesh -> IO (Ptr CVector3D)

foreign import ccall unsafe "nodeName"
  c_nodeName :: Ptr CNode -> IO CString

foreign import ccall unsafe "nodeTransformation"
  c_nodeTransformation :: Ptr CNode -> IO (Ptr CMatrix4x4)

foreign import ccall unsafe "nodeNumChildren"
  c_nodeNumChildren :: Ptr CNode -> IO CUInt

foreign import ccall unsafe "nodeChildren"
  c_nodeChildren :: Ptr CNode -> IO (Ptr (Ptr CNode))



-- Function to retrieve the last error message from Assimp
getErrorString :: IO String
getErrorString = do
  cstr <- c_aiGetErrorString
  peekCString cstr

-- Function to release resources associated with a loaded asset
releaseAsset :: Ptr CScene -> IO ()
releaseAsset = c_aiReleaseImport


getVertices :: Ptr CMesh -> IO [Vector3D]
getVertices meshPtr = do
  numVertices <- c_aiMesh_mNumVertices meshPtr
  vertexArrayPtr <- c_aiMesh_mVertices meshPtr
  vertexList <- forM [0 .. (fromIntegral numVertices - 1)] $ \i -> do
    let vertexPtr = advancePtr vertexArrayPtr i
    CVector3D x y z <- peek vertexPtr
    return (Vector3D x y z)
  return vertexList

getDiffuseColor :: Ptr CMaterial -> IO (Maybe Color3D)
getDiffuseColor materialPtr = do
  let key = "AI_MATKEY_COLOR_DIFFUSE" -- This might be a predefined key in Assimp or similar.
  alloca $ \colorPtr -> do
    result <- withCString key $ \c_key ->
      c_aiGetMaterialColor materialPtr c_key 0 0 colorPtr
    if result == aiSuccess -- `aiSuccess` is an assumed success code. Refer to Assimp docs for actual value.
      then do
        color <- peek colorPtr
        return $ Just color
      else return Nothing

constructMaterial :: Ptr CMaterial -> IO Material
constructMaterial materialPtr = do
  Just diffuse <- getDiffuseColor materialPtr
  -- Similarly for ambient and specular...
  -- Just ambient <- getAmbientColor materialPtr
  -- Just specular <- getSpecularColor materialPtr
  return $ Material diffuse -- ambient specular

getHNode :: Ptr CNode -> IO HNode
getHNode cNodePtr = do
  name <- c_nodeName cNodePtr >>= peekCString
  matrix <- c_nodeTransformation cNodePtr >>= peekMatrix
  numChildren <- c_nodeNumChildren cNodePtr
  childrenPtrs <- c_nodeChildren cNodePtr >>= peekArray (fromIntegral numChildren)
  children <- mapM getHNode childrenPtrs
  return $ HNode name matrix children

peekMatrix :: Ptr CMatrix4x4 -> IO Matrix4x4
peekMatrix ptr = do
  -- Extract and construct your Matrix4x4 from CMatrix4x4
  -- ... (Implementation depends on exact layout of CMatrix4x4)
  return matrix

-- Function to load an asset and return either an error message or a pointer to the loaded scene
loadAsset :: String -> IO (Either String Asset)
loadAsset filepath = do
    scenePtr <- withCString filepath $ \c_filepath ->
      c_aiImportFile c_filepath someFlags
    if scenePtr == nullPtr
      then do
          errorMsg <- getErrorString
          return $ Left errorMsg
      else do
          -- Assuming just one mesh in the scene for simplicity.
          let meshPtr = -- Retrieve mesh pointer from the scene using some Assimp function
          vertices <- getVertices meshPtr
          -- Similarly, extract other data like triangle indices, normals, and materials...
          material <- constructMaterial -- Assuming you have a pointer to the material
          let asset = Asset vertices -- ... other data ... material
          return $ Right asset