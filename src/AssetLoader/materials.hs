module Materials where

parseMaterials :: Cursor -> [Material]
parseMaterials cursor = cursor $// element "{DAE_NAMESPACE}material" &| parseMaterial

parseMaterial :: Cursor -> Material
parseMaterial c =
  Material
    { materialName = attribute "name" c
    , materialDiffuse = parseColorOrTexture "diffuse" c
    , materialSpecular = parseColorOrTexture "specular" c
    , materialAmbient = parseColorOrTexture "ambient" c
    , materialEmissive = parseColorOrTexture "emissive" c
    , materialTransparent = parseColorOrTexture "transparent" c
    , materialOpacity = read =<< extractField "opacity" c
    , materialShininess = read =<< extractField "shininess" c
    , materialReflective = parseColorOrTexture "reflective" c
    }

parseColorOrTexture :: String -> Cursor -> TextureOrColor
parseColorOrTexture name c =
  let color = c $// element name &| parseColor
      texture = c $// element (name ++ "-map") &| parseTexture
   in if isJust color
        then Color $ fromJust color
        else Texture $ fromJust texture

extractField :: String -> Cursor -> Maybe String
extractField name c =
  listToMaybe $ c $// element name &/ content