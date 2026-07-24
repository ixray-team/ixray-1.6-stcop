void EvaluateMaterial(
    in MaterialContext Context,
    in MaterialParameters Parameters,
    out MaterialInputs Result)
{
    const float4 BaseSample = SampleMaterialTexture2D(
        Parameters.P_2136fd1d_29bd_48e9_9f4c_9ebedb470774,
        Parameters.MaterialSamplerIndex,
        Context.TexCoord0);

    Result.BaseColor = BaseSample.rgb * Parameters.P_915ce004_8c2f_47ce_87c7_b4af787b835e.rgb;
    Result.Normal = Context.WorldNormal;
    Result.Roughness = Parameters.P_a274b611_7391_4d5f_b08d_d9ce8255fdaf;
    Result.Metallic = Parameters.P_d601334d_b60a_4632_b631_9c8bd421c71e;
    Result.AmbientOcclusion = 1.0f;
    Result.Emissive = float3(0.0f, 0.0f, 0.0f);
    Result.Opacity = BaseSample.a;
    Result.OpacityMask = BaseSample.a;
    Result.WorldPositionOffset = float3(0.0f, 0.0f, 0.0f);
}
