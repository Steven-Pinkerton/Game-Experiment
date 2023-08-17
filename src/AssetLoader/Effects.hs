module AssetLoader.Effects where

import AssetLoader.Datatypes
  
parseEffects :: Cursor -> [Effect]
parseEffects c =
  c
    $// element "{DAE_NAMESPACE}library_effects"
    $// element "{DAE_NAMESPACE}effect"
    &| parseEffect

parseEffect :: Cursor -> Effect
parseEffect c =
  Effect
    { effectId = attribute "id" c
    , effectParams = parseEffectParams c
    }

parseEffectParams :: Cursor -> [EffectParam]
parseEffectParams c =
  c $// element "{DAE_NAMESPACE}newparam" &| parseEffectParam

parseEffectParam :: Cursor -> EffectParam
parseEffectParam c =
  EffectParam
    { paramSid = attribute "sid" c
    , paramType = parseEffectParamType c
    }

parseEffectParamType :: Cursor -> ParamType
parseEffectParamType c =
  case lookupParamType (attribute "type" c) of
    Just paramType -> paramType
    Nothing -> error "Unknown effect param type"

lookupParamType :: String -> Maybe ParamType
lookupParamType "SURFACE" = Just (SurfaceParam defaultSurface)
lookupParamType "SAMPLER_STATE" = Just (SamplerParam defaultSampler)
lookupParamType "LAMBERT" = Just (LambertParam defaultLambert)
lookupParamType "CONSTANT" = Just (FloatParam 0)
lookupParamType "FLOAT" = Just (FloatParam 0)
lookupParamType "FLOAT2" = Just (Vector2Param zeroVector2)
lookupParamType "FLOAT3" = Just (Vector3Param zeroVector3)
lookupParamType "FLOAT4" = Just (Vector4Param zeroVector4)
lookupParamType "FLOAT4x4" = Just (Matrix4x4Param identMatrix4x4)
lookupParamType "BOOL" = Just (BoolParam False)
lookupParamType "INT" = Just (IntParam 0)
lookupParamType _ = Nothing

newtype FloatParam = FloatParam Float
newtype Vector2Param = Vector2Param Vector2
newtype Vector3Param = Vector3Param Vector3
newtype Vector4Param = Vector4Param Vector4
newtype Matrix4x4Param = Matrix4x4Param Matrix4x4
newtype BoolParam = BoolParam Bool
newtype IntParam = IntParam Int

defaultSurface = Surface "" ""
defaultSampler = Sampler ""
defaultLambert = Lambert blackColor noTexture 0