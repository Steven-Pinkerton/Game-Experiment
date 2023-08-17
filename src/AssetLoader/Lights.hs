module AssetLoader.Lights () where

-- And likewise for Light
parseLights :: Cursor -> [Light]
parseLights cursor = cursor $// element "{DAE_NAMESPACE}light" &| parseLight

parseLight :: Cursor -> Light
parseLight c =
  Light
    { lightName = attribute "name" c
    , lightType = parseLightType c
    , lightNode = Nothing
    , lightColor = parseColor (c $// element "color")
    , lightIntensity = read $ extractField "intensity" c
    , lightAttenuation = parseAttenuation c
    , lightSpotAngles = parseSpotAngles c
    , lightAreaSize = parseLightAreaSize c
    }

-- Helper functions to parse Vector3, Color, and LightType
parseVector3 :: Cursor -> Vector3
parseVector3 c =
  let coords = T.splitOn "," $ head (c &/ content)
   in Vector3 (read $ toString (head coords)) (read $ toString (coords !! 1)) (read $ toString (coords !! 2))

parseColor :: Cursor -> Color
parseColor c =
  let rgba = T.splitOn "," $ head (c &/ content)
   in Color (read $ toString (head rgba)) (read $ toString (rgba !! 1)) (read $ toString (rgba !! 2)) (read $ toString (rgba !! 3))

parseLightType :: Cursor -> LightType
parseLightType c
  | typeStr == "point" = PointLight
  | typeStr == "directional" = DirectionalLight
  | typeStr == "spot" = SpotLight
  where
    typeStr = toString $ head (c $// element "{DAE_NAMESPACE}type" &/ content)

parseAttenuation :: Cursor -> Attenuation
parseAttenuation c =
  Attenuation
    { attenConst = read $ extractField "constant" c
    , attenLinear = read $ extractField "linear" c
    , attenQuad = read $ extractField "quadratic" c
    }

parseSpotAngles :: Cursor -> (Float, Float)
parseSpotAngles c =
  ( read $ extractField "inner" c
  , read $ extractField "outer" c
  )

parseLightNode :: Cursor -> Maybe Node
parseLightNode c =
  let id = extractField "node" c
   in lookupNode id sceneNodes

-- sceneNodes being all nodes parsed earlier

parseLightShadow :: Cursor -> Bool
parseLightShadow c =
  read $ extractField "shadow" c == "1"

parseLightAreaSize :: Cursor -> (Float, Float)
parseLightAreaSize c =
  ( read $ extractField "width" c
  , read $ extractField "height" c
  )