module AssetLoader.Camaras where
import Text.XML.Cursor
    ( attribute, content, element, ($//), (&|), Cursor )
import AssetLoader.Datatypes
    ( CameraLens(..),
      Projection(..),
      Camera(..),
      Vector2(..),
      Vector3(..) )
import Text.Read ( read )
import Data.List ( head, (!!) )
import Data.Text ( splitOn )

-- Let's add parsing for Camera
parseCameras :: Cursor -> [Camera]
parseCameras cursor = cursor $// element "{DAE_NAMESPACE}camera" &| parseCamera

parseCamera :: Cursor -> Camera
parseCamera c =
  Camera
    { cameraName = Data.List.head $ attribute "name" c
    , cameraNode = Nothing
    , cameraProjections = parseCameraProjections c
    , cameraLenses = parseCameraLenses c
    }
    
parseVector3 :: Cursor -> Vector3
parseVector3 c =
  let coords = splitOn "," $ Data.List.head $ content c
   in Vector3
        (read $ toString (Data.List.head coords))
        (read $ toString (coords !! 1))
        (read $ toString (coords !! 2))

parseCameraProjections :: Cursor -> [Projection]
parseCameraProjections c =
  let perspectives = c $// element "{DAE_NAMESPACE}perspective" &| parsePerspective
      orthographics = c $// element "{DAE_NAMESPACE}orthographic" &| parseOrthographic
   in perspectives ++ orthographics

parsePerspective :: Cursor -> Projection
parsePerspective c =
   PerspectiveProjection $ read $ toString $ Data.List.head $ attribute "fov" c

parseOrthographic :: Cursor -> Projection
parseOrthographic c =
  OrthographicProjection $ read $ toString $ Data.List.head $ attribute "size" c


parseCameraLenses :: Cursor -> [CameraLens]
parseCameraLenses c =
  c $// element "{DAE_NAMESPACE}lens" &| parseCameraLens

parseCameraLens :: Cursor -> CameraLens
parseCameraLens c =
  CameraLens
    { lensFov = read $ toString $ Data.List.head $ attribute "fov" c
    , lensOffset = parseVector2 $ Data.List.head $ c $// element "{DAE_NAMESPACE}offset"
    }

parseVector2 :: Cursor -> Vector2
parseVector2 c =
  let coords = splitOn "," $ Data.List.head $ content c
   in Vector2
        (read $ toString $ Data.List.head coords)
        (read $ toString $ coords !! 1)