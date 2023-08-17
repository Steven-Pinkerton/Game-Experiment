module AssetLoader.Meshes where
   
parseMeshes :: Cursor -> [Mesh]
parseMeshes cursor = cursor $// element "{DAE_NAMESPACE}mesh" &| parseMesh

parseMesh :: Cursor -> Mesh
parseMesh c =
  Mesh
    { meshVertices = parseVertices c
    , meshNormals = parseNormals c
    , meshTexcoords = parseTexcoords c
    , meshIndices = parseIndices c
    , meshMaterial = Nothing
    }

parseVertices :: Cursor -> [Float]
parseVertices c =
  let source = c $// element "{DAE_NAMESPACE}source"
      sourceWithId = source & attribute "id" @= "*-mesh-positions"
      floatArray = sourceWithId $// element "{DAE_NAMESPACE}float_array"
   in parseFloatArray floatArray

parseNormals :: Cursor -> [Float]
parseNormals c =
  let source = c $// element "{DAE_NAMESPACE}source"
      sourceWithId = source & attribute "id" @= "*-mesh-normals"
      floatArray = sourceWithId $// element "{DAE_NAMESPACE}float_array"
   in parseFloatArray floatArray

parseTexcoords :: Cursor -> [Float]
parseTexcoords c =
  let source = c $// element "{DAE_NAMESPACE}source"
      sourceWithId = source & attribute "id" @= "*-mesh-map-*"
      floatArray = sourceWithId $// element "{DAE_NAMESPACE}float_array"
   in parseFloatArray floatArray

parseIndices :: Cursor -> [Int]
parseIndices c =
  let source = c $// element "{DAE_NAMESPACE}source"
      sourceId = source & attribute "id" @= "*-mesh-map-0"
      intArray = sourceId $// element "{DAE_NAMESPACE}p"
   in map read $ splitOn " " $ toString $ intArray &/ content
