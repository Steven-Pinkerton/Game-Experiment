module AssetLoader.Cameras where
  
-- Let's add parsing for Camera
parseCameras :: Cursor -> [Camera]
parseCameras cursor = cursor $// element "{DAE_NAMESPACE}camera" &| parseCamera


parseCamera :: Cursor -> Camera
parseCamera c = Camera
  { cameraName = attribute "name" c 
    , cameraProjections = parseCameraProjections c
    , cameraLenses = parseCameraLenses c
    , cameraPosition = parseVector3 (c $// element "{DAE_NAMESPACE}position")
    , cameraTarget = parseVector3 (c $// element "{DAE_NAMESPACE}target")
    , cameraUpVector = parseVector3 (c $// element "{DAE_NAMESPACE}up_vector")
    , cameraFov = read $ toString $ head (c $// element "{DAE_NAMESPACE}fov" &/ content)
    , cameraNearClip = read $ toString $ head (c $// element "{DAE_NAMESPACE}near_clip" &/ content)
    , cameraFarClip = read $ toString $ head (c $// element "{DAE_NAMESPACE}far_clip" &/ content)
    }

parseVector3 :: Cursor -> Vector3
parseVector3 c =
  let coords = T.splitOn "," $ head (c &/ content)
   in Vector3 (read $ toString (head coords)) (read $ toString (coords !! 1)) (read $ toString (coords !! 2))

parseCameraProjections :: Cursor -> [Projection]
parseCameraProjections c =
  let perspectives = c $// element "{DAE_NAMESPACE}perspective" &| parsePerspective
      orthographics = c $// element "{DAE_NAMESPACE}orthographic" &| parseOrthographic
   in perspectives ++ orthographics

parsePerspective :: Cursor -> Projection 
parsePerspective c = 
   PerspectiveProjection $ read $ attribute "fov" c

parseOrthographic :: Cursor -> Projection
parseOrthographic c =
   OrthographicProjection $ read $ attribute "size" c
   
parseCameraLenses :: Cursor -> [CameraLens]  
parseCameraLenses c =
  c $// element "{DAE_NAMESPACE}lens" &| parseCameraLens

parseCameraLens :: Cursor -> CameraLens
parseCameraLens c =
  CameraLens
    { lensFov = read $ attribute "fov" c
    , lensOffset = parseVector2 $ c $// element "{DAE_NAMESPACE}offset"
    } 

parseVector2 :: Cursor -> Vector2
parseVector2 c =
  let coords = T.splitOn "," $ head (c &/ content)
   in Vector2
        (read $ T.unpack $ head coords)
        (read $ T.unpack $ coords !! 1)