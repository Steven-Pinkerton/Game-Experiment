module AssetLoader.Animations where

import AssetLoader.Datatypes
    ( AnimationKey(..),
      AnimationClip(AnimationClip, clipName, clipKeys, clipTargets),
      Animation(..) )
import Text.XML.Cursor ( attribute, element, ($//), (&|), Cursor, content )
import Text.Read (read)
import AssetLoader.Camaras ( parseVector3 )
import Numeric.Quaternion ( Quater(Quater), QDouble )
import Data.Text ( splitOn )
import Data.Maybe ( fromJust )

parseAnimations :: Cursor -> [Animation]
parseAnimations cursor = cursor $// element "{DAE_NAMESPACE}animation" &| parseAnimation

parseAnimation :: Cursor -> Animation
parseAnimation c =
  Animation
    { animationName = fromJust $ listToMaybe $ attribute "name" c
    , animationClips = parseAnimationClips c
    }

parseAnimationClip :: Cursor -> AnimationClip
parseAnimationClip c =
  AnimationClip
    { clipName = fromJust $ listToMaybe $ attribute "name" c
    , clipKeys = parseAnimationKeys c
    , clipTargets = parseClipTargets c
    }

parseClipTargets :: Cursor -> [Text]
parseClipTargets c =
  map toText $ attribute "targets" c

parseAnimationClips :: Cursor -> [AnimationClip]
parseAnimationClips c = c $// element "{DAE_NAMESPACE}animation_clip" &| parseAnimationClip

parseAnimationKeys :: Cursor -> [AnimationKey]
parseAnimationKeys c = c $// element "{DAE_NAMESPACE}key" &| parseAnimationKey

parseQDouble :: Cursor -> QDouble
parseQDouble c =
  let txtContent = fromMaybe "" $ viaNonEmpty Prelude.head $ content c
      textComponents = Data.Text.splitOn "," txtContent
      components = fmap (read . toString) textComponents
   in case components of
        [x, y, z, w] -> Quater x y z w
        _ -> error "Unexpected number of components in quaternion data"

parseAnimationKey :: Cursor -> AnimationKey
parseAnimationKey c =
  AnimationKey
    { keyTime = read $ toString $ fromJust $ listToMaybe $ attribute "time" c
    , keyTranslation = parseVector3 $ fromJust $ listToMaybe $ c $// element "translate"
    , keyRotation = parseQDouble $ fromJust $ listToMaybe $ c $// element "rotate"
    , keyScale = parseVector3 $ fromJust $ listToMaybe $ c $// element "scale"
    }