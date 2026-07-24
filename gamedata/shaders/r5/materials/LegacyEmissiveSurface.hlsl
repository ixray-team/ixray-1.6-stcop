void EvaluateMaterial(
    in MaterialContext Context,
    in MaterialParameters Parameters,
    out MaterialInputs Result)
{
    const float4 BaseSample = SampleMaterialTexture2D(
        Parameters.P_2136fd1d_29bd_48e9_9f4c_9ebedb470774,
        Parameters.MaterialSamplerIndex,
        Context.TexCoord0);
    const float4 SurfaceColor = BaseSample * Parameters.P_915ce004_8c2f_47ce_87c7_b4af787b835e;
    Result.BaseColor = 0.0f.xxx;
    Result.Normal = Context.WorldNormal;
    Result.Roughness = 1.0f;
    Result.Metallic = 0.0f;
    Result.AmbientOcclusion = 1.0f;
    Result.Emissive = SurfaceColor.rgb * Parameters.P_37863262_0b9b_4bfd_a13d_1386c399e151;
    Result.Opacity = SurfaceColor.a;
    Result.OpacityMask = SurfaceColor.a;
    Result.WorldPositionOffset = 0.0f.xxx;
}
