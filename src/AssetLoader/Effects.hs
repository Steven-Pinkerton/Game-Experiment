module AssetLoader.Effects where

import AssetLoader.Datatypes
    ( ParamType(..),
      Effect(..),
      Lambert(Lambert),
      Sampler(Sampler),
      Surface(Surface),
      IntParam(IntParam),
      BoolParam(BoolParam),
      Matrix4x4Param(Matrix4x4Param),
      Vector4Param(Vector4Param),
      Vector3Param(Vector3Param),
      Vector2Param(Vector2Param),
      FloatParam(FloatParam),
      EffectParam(..) )
import Text.XML.Cursor ( attribute, element, ($//), (&|), Cursor, (&//) )
  
parseEffects :: Cursor -> [Effect]
parseEffects c =
  c
    $// element "{DAE_NAMESPACE}library_effects"
    &// element "{DAE_NAMESPACE}effect"
    &| parseEffect

parseEffect :: Cursor -> Effect
parseEffect c =
  Effect
    { effectId = case attribute "id" c of
        [idValue] -> idValue
        _ -> error "Unexpected number of id attributes"
    , effectParams = parseEffectParams c
    }

parseEffectParams :: Cursor -> [EffectParam]
parseEffectParams c =
  c $// element "{DAE_NAMESPACE}newparam" &| parseEffectParam

parseEffectParam :: Cursor -> EffectParam
parseEffectParam c =
  EffectParam
    { paramSid = case attribute "sid" c of
        [sidValue] -> sidValue
        _ -> error "Unexpected number of sid attributes"
    , paramType = parseEffectParamType c
    }

parseEffectParamType :: Cursor -> ParamType
parseEffectParamType c =
  case attribute "type" c of
    [typeValue] -> case lookupParamType (toString typeValue) of
      Just paramType' -> paramType'
      Nothing -> error "Unknown effect param type"
    _ -> error "Unexpected number of type attributes"


lookupParamType :: String -> Maybe ParamType
lookupParamType "SURFACE" = Just (SurfaceParam defaultSurface)
lookupParamType "SAMPLER_STATE" = Just (SamplerParam defaultSampler)
lookupParamType "LAMBERT" = Just (LambertParam defaultLambert)
lookupParamType "CONSTANT" = Just (FloatParam 0)
lookupParamType "FLOAT" = FloatParam (FloatParam 0)
lookupParamType "FLOAT2" = Vector2Param (Vector2Param zeroVector2)
lookupParamType "FLOAT3" = Vector3Param (Vector3Param zeroVector3)
lookupParamType "FLOAT4" = Just (Vector4Param zeroVector4)
lookupParamType "FLOAT4x4" = Just (Matrix4x4Param identMatrix4x4)
lookupParamType "BOOL" = Just (BoolParam False)
lookupParamType "INT" = Just (IntParam 0)
lookupParamType _ = Nothing

defaultSurface = Surface "" ""
defaultSampler = Sampler ""
defaultLambert = Lambert blackColor noTexture 0