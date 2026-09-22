#ifndef USE_PROCEDURAL_SKY_VIEW
    #define USE_PROCEDURAL_SKY_VIEW
#endif

#ifdef USE_PROCEDURAL_SKY_VIEW
    #include "common_celestial.hlsli"
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
Texture2D<float4> s_celestial_transmittance_lut : register(t4);
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
    float3 ray_direction = safe_normalize(I.world_direction);
    // X-Ray stores the sunlight propagation direction.
    // Atmospheric functions expect the direction towards the Sun.
    float3 sun_direction = safe_normalize(-L_sun_dir_w);
    // Must match ComputeSkyView.cs.hlsl.
    float camera_elevation = sky_get_camera_elevation();
    float3 sky_color = sky_sample_view_lut(s_sky_view_lut, smp_rtlinear, ray_direction, sun_direction, camera_elevation).rgb;
    // Full internal-resolution history uses an unjittered grid. Invert the shift
    // applied by sky.vs exactly once, here at composition (also for FSR/DLSS).
    float2 cloud_uv = I.hpos.xy * pos_decompression_params2.zw - m_taa_jitter.xy * float2(0.5f, -0.5f);
    float4 clouds = s_procedural_clouds.SampleLevel(smp_rtlinear, cloud_uv, 0.0f);
    float3 sun_disk = sky_sun_disk(s_celestial_transmittance_lut, smp_rtlinear, ray_direction, sun_direction);
    float cloud_transmittance = saturate(clouds.a);
    float sun_visibility = sky_cloud_sun_visibility(cloud_transmittance);
    float3 final_sky = clouds.rgb + sky_color * cloud_transmittance + sun_disk * sun_visibility;
    // Keep extreme artistic disk settings finite in the RGBA16F scene target.
    // Preserve hue; this is an HDR storage limit, not exposure or tone mapping.
    float peak = max(max(final_sky.r, final_sky.g), final_sky.b);
    final_sky *= min(1.0f, 60000.0f / max(peak, 60000.0f));
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

