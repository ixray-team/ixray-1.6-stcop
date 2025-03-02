#ifndef metalic_roughness_base_h_ixray_included
#define metalic_roughness_base_h_ixray_included

#include "common.hlsli"
#define PI 3.141592653589793f

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
	float3 F0;

	float Depth;
	float Hemi;

	float3 Point;
	float3 PointHud;
	float3 PointReal;

	float3 View;
	float ViewDist;

	float SSS;
	float AO;
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

    O.Color.xyz = M.Color.xyz;
    O.Color.w = M.Roughness;

    O.Material.y = M.SSS;

#ifdef USE_R2_STATIC_SUN
    O.Material.y = M.Sun;
#endif

    O.Material.x = M.Metalness;
    O.Material.z = M.AO;
	
#ifndef USE_PBR
    O.Material.w = 0.0f;
#else
    O.Material.w = 1.0f;
#endif
}

float4 GbufferGetPoint(in float2 HPos)
{
	float Depth = s_position.Load(int3(HPos, 0)).x;
	HPos = HPos - m_taa_jitter.xy * float2(0.5f, -0.5f) * pos_decompression_params2.xy;
	float3 Point = float3(HPos * pos_decompression_params.zw - pos_decompression_params.xy, 1.0f);
    Point *= depth_unpack.x * rcp(Depth - depth_unpack.y);
    return float4(Point, 1.0f);
}

float3 GbufferGetPointRealUnjitter(in float2 TexCoord, in float Depth)
{
	float3 Point = float3(TexCoord * 2.0f - 1.0f, 1.0f);
	
	if(Depth < 0.02f) {
		Point.z = depth_unpack.z * rcp(Depth * 50.0f - depth_unpack.w);
		Point.xy *= pos_decompression_params_hud.xy * Point.z;
	} else {
		Point.z = depth_unpack.x * rcp(Depth - depth_unpack.y);
		Point.xy *= pos_decompression_params.xy * Point.z;
	}
	
	return Point;
}

float3 GbufferGetPointRealUnjitter(in float2 TexCoord)
{
	float Depth = s_position.Load(int3(TexCoord * pos_decompression_params2.xy, 0)).x;
	return GbufferGetPointRealUnjitter(TexCoord, Depth);
}

void GbufferUnpack(in float2 TexCoord, in float2 HPos, inout IXrayGbuffer O)
{
    float4 NormalHemi = s_normal.Load(int3(HPos, 0));
	
    float4 Material = s_surface.Load(int3(HPos, 0));
    float4 ColorSSS = s_diffuse.Load(int3(HPos, 0));

    O.Depth = s_position.Load(int3(HPos, 0)).x;
	
    HPos = HPos - m_taa_jitter.xy * float2(0.5f, -0.5f) * pos_decompression_params2.xy;

    float3 P = float3(HPos * pos_decompression_params.zw - pos_decompression_params.xy, 1.0f);
    float3 P_hud = float3(HPos * pos_decompression_params_hud.zw - pos_decompression_params_hud.xy, 1.0f);

    O.Point = P * depth_unpack.x * rcp(O.Depth - depth_unpack.y);
    O.PointHud = P_hud * depth_unpack.z * rcp(O.Depth * 50.0f - depth_unpack.w);

    O.PointReal = O.Depth < 0.02f ? O.PointHud : O.Point;
	
	O.ViewDist = length(O.PointReal);
	O.View = O.PointReal * rcp(O.ViewDist);

    O.Normal.xyz = NormalDecode(NormalHemi.xyz);
    O.Hemi = NormalHemi.w;

    O.Color.xyz = PushGamma(ColorSSS.xyz);
    O.SSS = Material.y;

    O.Metalness = Material.x;
    O.Roughness = ColorSSS.w;
	
	O.AO = PushGamma(Material.z);
	O.F0 = 0.002f + 0.018f * Material.w;
}

void GbufferUnpack(in float2 TexCoord, inout IXrayGbuffer O)
{
	float2 HPos = TexCoord * pos_decompression_params2.xy;
	
    float4 NormalHemi = s_normal.SampleLevel(smp_rtlinear, TexCoord, 0);
	
    float4 Material = s_surface.SampleLevel(smp_rtlinear, TexCoord, 0);
    float4 ColorSSS = s_diffuse.SampleLevel(smp_rtlinear, TexCoord, 0);

    O.Depth = s_position.Load(int3(HPos, 0)).x;
	
    HPos = HPos - m_taa_jitter.xy * float2(0.5f, -0.5f) * pos_decompression_params2.xy;

    float3 P = float3(HPos * pos_decompression_params.zw - pos_decompression_params.xy, 1.0f);
    float3 P_hud = float3(HPos * pos_decompression_params_hud.zw - pos_decompression_params_hud.xy, 1.0f);

    O.Point = P * depth_unpack.x * rcp(O.Depth - depth_unpack.y);
    O.PointHud = P_hud * depth_unpack.z * rcp(O.Depth * 50.0f - depth_unpack.w);

    O.PointReal = O.Depth < 0.02f ? O.PointHud : O.Point;
	
	O.ViewDist = length(O.PointReal);
	O.View = O.PointReal * rcp(O.ViewDist);

    O.Normal.xyz = NormalDecode(NormalHemi.xyz);
    O.Hemi = NormalHemi.w;

    O.Color.xyz = PushGamma(ColorSSS.xyz);
    O.SSS = Material.y;

    O.Metalness = Material.x;
    O.Roughness = ColorSSS.w;
	
	O.AO = PushGamma(Material.z);
	O.F0 = 0.002f + 0.018f * Material.w;
}

#endif

