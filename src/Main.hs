module Main () where


main = do
  -- Load asset
  asset <- loadAndParseDAE "sample.dae"

  -- Print effects
  putStrLn "Effects:"
  forM_ (assetEffects asset) $ \effect -> do
    putStrLn $ "Effect: " ++ effectId effect

    forM_ (effectParams effect) $ \param -> do
      putStrLn $ "  Param: " ++ paramSid param
      print $ paramType param

    putStrLn ""