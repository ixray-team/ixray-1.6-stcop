void EvaluateMaterial(
    in MaterialContext Context,
    in MaterialParameters Parameters,
    out MaterialInputs Result)
{
    const float Checker = fmod(floor(Context.WorldPosition.x * 2.0f) + floor(Context.WorldPosition.z * 2.0f), 2.0f);
    Result.BaseColor = lerp(float3(0.02f, 0.02f, 0.02f), float3(1.0f, 0.0f, 1.0f), Checker);
    Result.Normal = Context.WorldNormal;
    Result.Roughness = 0.5f;
    Result.Metallic = 0.0f;
    Result.AmbientOcclusion = 1.0f;
    Result.Emissive = Result.BaseColor;
    Result.Opacity = 1.0f;
    Result.OpacityMask = 1.0f;
    Result.WorldPositionOffset = float3(0.0f, 0.0f, 0.0f);
}
