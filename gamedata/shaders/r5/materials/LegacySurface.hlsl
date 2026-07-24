void EvaluateMaterial(
    in MaterialContext Context,
    in MaterialParameters Parameters,
    out MaterialInputs Result)
{
    const float4 BaseSample = SampleMaterialTexture2D(
        Parameters.P_2136fd1d_29bd_48e9_9f4c_9ebedb470774,
        Parameters.MaterialSamplerIndex,
        Context.TexCoord0);
    float4 SurfaceColor = BaseSample * Parameters.P_915ce004_8c2f_47ce_87c7_b4af787b835e;

#if MATERIAL_STATIC_P_0d987e11_951d_43f3_b2a8_5d47d3b10ba2
    SurfaceColor *= Context.VertexColor;
#endif

#if MATERIAL_STATIC_P_e5d7660f_f7d5_4ccd_9be2_602174fa12aa
    const float3 Lightmap = SampleMaterialTexture2D(
        Parameters.P_7449f07b_e2f1_48cf_879d_d7e84ecb97b2,
        Parameters.MaterialSamplerIndex,
        Context.TexCoord1).rgb;
    SurfaceColor.rgb *= Lightmap * 2.0f;
#endif

    Result.BaseColor = SurfaceColor.rgb;
    Result.Normal = Context.WorldNormal;
    Result.Roughness = Parameters.P_a274b611_7391_4d5f_b08d_d9ce8255fdaf;
    Result.Metallic = Parameters.P_d601334d_b60a_4632_b631_9c8bd421c71e;
    Result.AmbientOcclusion = 1.0f;
    Result.Emissive = 0.0f.xxx;
    Result.Opacity = SurfaceColor.a;
    Result.OpacityMask = SurfaceColor.a;
    Result.WorldPositionOffset = 0.0f.xxx;
}
