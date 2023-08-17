module AssetLoader.Textures () where

parseTextures :: Cursor -> [Texture]
parseTextures c =
  c
    $// element "{DAE_NAMESPACE}library_textures"
    $// element "{DAE_NAMESPACE}texture"
    &| parseTexture

parseTexture :: Cursor -> Texture
parseTexture c =
  Texture
    { textureSampler = attribute "texcoord" c
    , textureTexcoord = attribute "texcoord" c
    }