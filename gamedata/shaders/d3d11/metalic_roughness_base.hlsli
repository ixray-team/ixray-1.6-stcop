#ifndef metalic_roughness_base_h_ixray_included
#define metalic_roughness_base_h_ixray_included

#include "common.hlsli"

#define PI 3.141592653589793
#define F0 float3(0.04, 0.04, 0.04)

// Use legacy lighting pipeline
#define USE_LEGACY_LIGHT

struct IXrayMaterial
{
    float Metalness;
    float Roughness;

    float3 Normal;
    float3 Point;
    float4 Color;

    float Depth;

    float Hemi;
    float Sun;

    float SSS;
    float AO;
};

struct IXrayGbufferPack
{
    float4 Color : SV_Target1;

    float4 Normal : SV_Target0;
    float4 Material : SV_Target2;

    float2 Velocity : SV_Target3;
};

struct IXrayGbuffer
{
    float Metalness;
    float Roughness;

    float3 Normal;
    float3 Color;

    float Depth;
    float Hemi;

    float3 Point;
    float3 PointHud;
    float3 PointReal;

    float SSS;
};

float3 NormalEncode(float3 Normal)
{
    Normal.z = -Normal.z;
    return Normal.xyz * 0.5f + 0.5f;
}

float3 NormalDecode(float3 Normal)
{
    Normal -= 0.5f;
    Normal.z = -Normal.z;
    return normalize(Normal);
}

void GbufferPack(inout IXrayGbufferPack O, inout IXrayMaterial M)
{
    O.Normal.xyz = NormalEncode(M.Normal.xyz);
    O.Normal.w = M.Hemi;

    O.Color.xyz = M.Color.xyz * M.AO;
    O.Color.w = M.Roughness;

    O.Material.y = M.SSS;

#ifdef USE_R2_STATIC_SUN
    O.Material.y = M.Sun;
#endif

    O.Material.x = M.Metalness;
    O.Material.zw = 0.0f;

    // TODO: Pack like this (hack for wetness)
    // O.Material.x = M.Metalness;
    // O.Material.y = M.Roughness;
}

void GbufferUnpack(in float2 TexCoord, in float2 HPos, inout IXrayGbuffer O)
{
    HPos = HPos - m_taa_jitter.xy * float2(0.5f, -0.5f) * pos_decompression_params2.xy;

    float3 P = float3(HPos * pos_decompression_params.zw - pos_decompression_params.xy, 1.0f);
    float3 P_hud = float3(HPos * pos_decompression_params_hud.zw - pos_decompression_params_hud.xy, 1.0f);

    float4 NormalHemi = s_normal.Sample(smp_nofilter, TexCoord);
    float4 Material = s_surface.Sample(smp_nofilter, TexCoord);
    float4 ColorSSS = s_diffuse.Sample(smp_nofilter, TexCoord);

    O.Depth = s_position.Sample(smp_nofilter, TexCoord).x;

    O.Point = P * depth_unpack.x * rcp(O.Depth - depth_unpack.y);
    O.PointHud = P_hud * depth_unpack.z * rcp(min(1.0f, O.Depth * 50.0f) - depth_unpack.w);

    O.PointReal = O.Depth > 0.02f ? O.Point : O.PointHud;

    O.Normal.xyz = NormalDecode(NormalHemi.xyz);
    O.Hemi = NormalHemi.w;

    O.Color.xyz = ColorSSS.xyz;
    O.SSS = Material.y;

    O.Metalness = Material.x;
    O.Roughness = ColorSSS.w;
}

#endif
