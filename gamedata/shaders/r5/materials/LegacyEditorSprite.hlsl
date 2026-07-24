void EvaluateMaterial(
    in MaterialContext Context,
    in MaterialParameters Parameters,
    out MaterialInputs Result)
{
    const float4 SurfaceColor = SampleMaterialTexture2D(
        Parameters.P_2136fd1d_29bd_48e9_9f4c_9ebedb470774,
        Parameters.MaterialSamplerIndex,
        Context.TexCoord0) *
        Parameters.P_915ce004_8c2f_47ce_87c7_b4af787b835e *
        Context.VertexColor;

    Result.BaseColor = SurfaceColor.rgb;
    Result.Normal = Context.WorldNormal;
    Result.Roughness = 1.0f;
    Result.Metallic = 0.0f;
    Result.AmbientOcclusion = 1.0f;
    Result.Emissive = 0.0f.xxx;
    Result.Opacity = SurfaceColor.a;
    Result.OpacityMask = SurfaceColor.a;
    Result.WorldPositionOffset = 0.0f.xxx;
}
