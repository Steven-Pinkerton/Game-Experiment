module AssetLoader.Textures where

import AssetLoader.Datatypes ( Texture(..) )
import Text.XML.Cursor ( attribute, element, ($//), (&|), Cursor, (&//) )

parseTextures :: Cursor -> [Texture]
parseTextures c =
  c $// element "{DAE_NAMESPACE}library_textures"
    &// element "{DAE_NAMESPACE}texture"
    &| parseTexture

parseTexture :: Cursor -> Texture
parseTexture c =
  Texture
    { textureSampler = fromMaybe "" $ listToMaybe $ attribute "sampler" c
    , textureTexcoord = fromMaybe "" $ listToMaybe $ attribute "texcoord" c
    }
