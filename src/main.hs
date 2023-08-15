module Main where


main = do
  doc <- loadDAEFile "sample.dae"
  let cursor = fromDocument doc
  asset <- parseAsset cursor

  -- Print number of items loaded
  print $ length $ assetMeshes asset
  print $ length $ assetCameras asset

  -- Check some values
  print $ head $ assetCameras asset
  print $ animationName $ head $ assetAnimations asset
