#ifdef USE_PROCEDURAL_SKY_VIEW
    #include "common_sky.hlsli"
#else
    #include "common.hlsli"
#endif

struct v2p
{
    float4 factor : COLOR0;
    float3 p : TEXCOORD1;

    float4 hpos_curr : TEXCOORD2;
    float4 hpos_old : TEXCOORD3;

    float4 hpos : SV_POSITION;
#ifdef USE_PROCEDURAL_SKY_VIEW
    float3 world_direction : TEXCOORD4;
#endif
};

#ifdef USE_PROCEDURAL_SKY_VIEW
Texture2D<float4> s_sky_view_lut : register(t2);
Texture2D<float4> s_procedural_clouds : register(t3);
#else
TextureCube s_sky0 : register(t0);
TextureCube s_sky1 : register(t1);
#endif

struct sky
{
    float4 Color : SV_Target0;
    float2 Velocity : SV_Target1;
};

void main(in v2p I, out sky O)
{
#ifdef USE_PROCEDURAL_SKY_VIEW
    const float3 ray_direction = safe_normalize(I.world_direction);
    // X-Ray stores the sunlight propagation direction.
    // Atmospheric functions expect the direction towards the Sun.
    const float3 sun_direction = safe_normalize(-L_sun_dir_w);
    // Must match ComputeSkyView.cs.hlsl.
    const float camera_elevation = max(0.002f * eye_position.y + 0.2f, 0.0f);
    const float3 sky_color = sky_sample_view_lut(s_sky_view_lut, smp_rtlinear, ray_direction, sun_direction, camera_elevation).rgb;
    // Display-only centered radius-1 blur. It remains after temporal accumulation,
    // so it cannot contaminate history or shift the cloud silhouette by half a pixel.
    uint cloud_width, cloud_height;
    s_procedural_clouds.GetDimensions(cloud_width, cloud_height);
    const int2 cloud_offsets[5] = { int2(0, 0), int2(-1, 0), int2(1, 0), int2(0, -1), int2(0, 1) };
    const float cloud_weights[5] = { 0.5f, 0.125f, 0.125f, 0.125f, 0.125f };
    float4 clouds = 0.0f;
    [unroll]
    for (uint cloud_tap = 0u; cloud_tap < 5u; ++cloud_tap)
    {
        int2 cloud_pixel = clamp(int2(I.hpos.xy) + cloud_offsets[cloud_tap],
            int2(0, 0), int2(cloud_width, cloud_height) - 1);
        clouds += s_procedural_clouds.Load(int3(cloud_pixel, 0)) * cloud_weights[cloud_tap];
    }
    // Filter linear premultiplied radiance and transmittance with identical weights.
    float3 final_sky = clouds.rgb + sky_color * saturate(clouds.a);
    // SkyView is already stored in linear HDR.
    // Do not call GammaToLinear, LinearToGamma, detonemap or tonemap.
    O.Color = float4(max(final_sky, 0.0f), 0.0f);
#else // USE_PROCEDURAL_SKY_VIEW

    float3 TexCoord = I.p;

    #ifndef USE_FULL_SKY_SPHERE

        RemapVector(TexCoord);

    #endif

    const float3 s0 = s_sky0.SampleLevel(smp_rtlinear,TexCoord,0.0f).xyz;
    const float3 s1 = s_sky1.SampleLevel(smp_rtlinear,TexCoord,0.0f).xyz;
    float3 sky_color = lerp(s0, s1, I.factor.w);

    #ifdef USE_BGRA_SKYCOLOR

        sky_color *= L_sky_color.zyx;

    #else

        sky_color *= L_sky_color.xyz;

    #endif

    #ifdef USE_LEGACY_SKY_TONEMAP

        O.Color =float4(detonemap(sky_color * 0.66f), 0.0f);

    #else

        O.Color =float4(GammaToLinear(sky_color), 0.0f);

    #endif
#endif // USE_PROCEDURAL_SKY_VIEW

    O.Velocity = I.hpos_curr.xy / I.hpos_curr.w - I.hpos_old.xy / I.hpos_old.w;
}

