module Images where
  
parseImages :: Cursor -> [Image]
parseImages c =
  c
    $// element "{DAE_NAMESPACE}library_images"
    $// element "{DAE_NAMESPACE}image"
    &| parseImage

parseImage :: Cursor -> Image
parseImage c =
  Image
    { imageId = attribute "id" c
    , imagePath = attribute "source" c
    , imageDimensions = parseImageDimensions c
    , imageData = parseImageData c
    }

parseImageData :: Cursor -> ByteString
parseImageData c =
  case decode $ extractField "data" c of
    Left err -> error "Invalid image data"
    Right pngData -> pngData