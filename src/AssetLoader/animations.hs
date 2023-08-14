parseAnimations :: Cursor -> [Animation]
parseAnimations cursor = cursor $// element "{DAE_NAMESPACE}animation" &| parseAnimation

parseAnimation :: Cursor -> Animation
parseAnimation c =
  Animation
    { animationName = attribute "name" c
    , animationKeys = parseAnimationKeys (c $// element "{DAE_NAMESPACE}animation_clip")
    }

parseAnimationKeys :: Cursor -> [AnimationKey]
parseAnimationKeys c = c $// element "{DAE_NAMESPACE}key" &| parseAnimationKey

parseAnimationKey :: Cursor -> AnimationKey
parseAnimationKey c =
  AnimationKey
    { keyTime = read $ toString $ attribute "time" c
    , keyMatrix = parseMatrix4x4 (c $// element "{DAE_NAMESPACE}transform")
    }