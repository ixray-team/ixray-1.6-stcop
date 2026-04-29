#ifndef metalic_roughness_base_h_ixray_included
#define metalic_roughness_base_h_ixray_included

#include "common.hlsli"
#define PI 3.141592653589793f

struct IXRayMaterial
{
#ifndef USE_LEGACY_LIGHT
	float Metalness;
	float Roughness;
	float Specular;
	
    float AO;
#else
	float Gloss;
	float Material;
#endif

    float SSS;
	
	float3 Normal;
	float3 Point;
	
	float4 Color;
	float Depth;
	
	float Hemi;
	float Sun;
	
	float SnowMask;
};

struct IXRayGbuffer
{
	float3 Normal;
	float3 Color;
	
#ifndef USE_LEGACY_LIGHT
	float Roughness;
	float3 Specular;
	
	float AO;
#else
	float Gloss;
	float Material;
#endif
	
	float Depth;
	float Hemi;
	
#ifdef USE_R2_STATIC_SUN
	float Sun;
#endif

	float SSS;
	
	float3 Point;
	float3 PointHud;
	float3 PointReal;
	
	float3 View;
	float ViewDist;
	
	float SnowMask;
};

struct IXRayGbufferPack
{
	float4 Color : SV_Target0;
	float4 Normal : SV_Target1;
	float4 Material : SV_Target2;
	
#ifndef DISABLE_MOTION_VECTORS
	float2 Velocity : SV_Target3;
#endif
};

struct IXRayForward
{
    float4 Color : SV_Target0;
    float4 Velocity : SV_Target1;
	
    float Reactive : SV_Target2;
};

struct IXRayVSLRGBuffer
{
    float3 Color : SV_Target0;
    float Length : SV_Target1;
};

inline float2 PackNormalVector(float3 Vector)
{
	float PackedZ = 0.5f + 0.5f * Vector.z;
	float Scale = rcp(dot(Vector.xy, Vector.xy));
	
	Vector.xy *= sqrt(PackedZ * Scale);
	
	return Vector.xy;
}

inline float3 UnPackNormalVector(float2 Packed)
{
	float PackedZ = dot(Packed, Packed);
	
	float3 Vector;
	Vector.z = PackedZ * 2.0f - 1.0f;
	Vector.xy = Packed * sqrt(1.0f - PackedZ) * 2.0f;
	
	return Vector;
}

inline float2 NormalEncode(float3 Normal)
{
	Normal.z = -Normal.z;
	
    Normal *= rcp(abs(Normal.x) + abs(Normal.y) + abs(Normal.z));
    float Shift = saturate(-Normal.z);
	
    Normal.xy += Normal.xy > 0.0f ? Shift : -Shift;
    return Normal.xy;
}

inline float3 NormalDecode(float2 InNormal)
{
    float3 Normal = float3(InNormal, 1.0f - abs(InNormal.x) - abs(InNormal.y));
    float Shift = saturate(-Normal.z);

    Normal.xy -= Normal.xy > 0.0f ? Shift : -Shift;
	Normal.z = -Normal.z;
	
    return normalize(Normal);
}

inline void GbufferPack(inout IXRayGbufferPack O, inout IXRayMaterial M)
{
    O.Color.xyz = M.Color.xyz;
	
#ifdef USE_LEGACY_LIGHT
	O.Color.w = M.Gloss;
	O.Material = 0.0f;
	
	#ifdef USE_R2_STATIC_SUN
		M.Material.x = M.Sun;
	#else
		O.Material.x = float(((uint(M.SSS) & 1) << 7) | (uint(M.Material * 0x7F) & 0x7F)) / 255.0f;
	#endif
#else
	O.Color.w = M.AO;

	O.Material.x = M.Roughness;
	O.Material.y = M.Metalness;
	O.Material.z = M.Specular;
	O.Material.w = M.SSS;
	
	#ifdef USE_R2_STATIC_SUN
		O.Color.xyz *= O.Color.w;
		O.Material.z *= O.Color.w;

		O.Color.w = M.Sun;
	#endif
#endif
	
    O.Normal.xy = NormalEncode(M.Normal.xyz) * 0.5f + 0.5f;
	
	O.Normal.w = M.SnowMask;
	O.Normal.z = M.Hemi;
}

inline void GbufferUnpackMaterial(inout IXRayGbufferPack O, inout IXRayMaterial M)
{
    M.Color.xyz = O.Color.xyz;
	M.Color.w = 1.0f;

#ifdef USE_LEGACY_LIGHT
    M.Gloss = O.Color.w;

	#ifdef USE_R2_STATIC_SUN
		M.Sun = 0.0f;
		M.Material = 0.0f;
	#else
		uint packed = uint(O.Material.x * 255.0f);
	
		M.SSS = float((packed >> 7) & 1);
		M.Material = float(packed & 0x7F) / 127.0f;
	#endif
#else
    M.AO = O.Color.w;

    M.Roughness = O.Material.x;
    M.Metalness = O.Material.y;
    M.Specular = O.Material.z;
    M.SSS = O.Material.w;

	#ifdef USE_R2_STATIC_SUN
		M.Sun = O.Color.w;
		M.AO  = 1.0f;
		M.Color.xyz = O.Color.xyz;
		M.Specular  = O.Material.z;
	#else
		M.Sun = 0.0f;
	#endif
#endif

    M.Normal.xyz = NormalDecode(O.Normal.xy * 2.0f - 1.0f);
    M.SnowMask = O.Normal.w;
    M.Hemi = O.Normal.z;
}

inline float4 GbufferGetPoint(in float2 HPos)
{
	float4 Point = float4
	(
		HPos * pos_decompression_params2.zw,
		s_position.Load(int3(HPos, 0)).x,
		1.0f
	);
	
	Point.x = Point.x * 2.0f - 1.0f;
	Point.y = 1.0f - Point.y * 2.0f;
	
	Point.xy -= m_taa_jitter.xy;
	
	Point = mul(m_invP, Point);
    return Point / Point.w;
}

inline float3 GbufferGetPointRealUnjitter(in float2 TexCoord, in float Depth)
{
	float4 Point = float4(TexCoord, Depth, 1.0f);
	
	Point.x = Point.x * 2.0f - 1.0f;
	Point.y = 1.0f - Point.y * 2.0f;
	
	if(Point.z < 0.02f)
	{
		Point.z *= 50.0f;
		Point = mul(m_invP_hud, Point);
	}
	else
	{
		Point = mul(m_invP, Point);
	}
	
    return Point.xyz / Point.w;
}

inline float3 GbufferGetPointRealUnjitter(in float2 TexCoord)
{
	float Depth = s_position[uint2(TexCoord * pos_decompression_params2.xy)].x;
	return GbufferGetPointRealUnjitter(TexCoord, Depth);
}

inline float3 GbufferGetPointRealJitter(in float2 TexCoord, in float Depth)
{
	float4 Point = float4(TexCoord, Depth, 1.0f);
	
	Point.x = Point.x * 2.0f - 1.0f;
	Point.y = 1.0f - Point.y * 2.0f;
	
	Point.xy -= m_taa_jitter.xy;
	
	if(Point.z < 0.02f)
	{
		Point.z *= 50.0f;
		Point = mul(m_invP_hud, Point);
	}
	else
	{
		Point = mul(m_invP, Point);
	}
	
    return Point.xyz / Point.w;
}

inline float3 GbufferGetPointRealJitter(in float2 TexCoord)
{
	float Depth = s_position.Load(int3(TexCoord * pos_decompression_params2.xy, 0)).x;
	return GbufferGetPointRealJitter(TexCoord, Depth);
}

inline void GbufferUnpackDepth(in uint2 HPos, inout IXRayGbuffer O)
{
    float2 TexCoord = HPos * pos_decompression_params2.zw;
    O.Depth = s_position.Load(uint3(HPos, 0)).x;
	
	float4 Point = float4(TexCoord, O.Depth, 1.0f);
	
	Point.x = Point.x * 2.0f - 1.0f;
	Point.y = 1.0f - Point.y * 2.0f;
	
	Point.xy -= m_taa_jitter.xy;
	
	float4 Proj = mul(m_invP, Point);
	O.Point = Proj.xyz / Proj.w;
	
	Point.z *= 50.0f;
	
	Proj = mul(m_invP_hud, Point);	
	O.PointHud = Proj.xyz / Proj.w;

    O.PointReal = O.Depth < 0.02f ? O.PointHud : O.Point;
	
	O.ViewDist = length(O.PointReal);
	O.View = O.PointReal * rcp(O.ViewDist);
}

inline void GbufferUnpackNormal(in uint2 TexCoord, inout IXRayGbuffer O)
{
    float4 Sample = s_normal.Load(uint3(TexCoord, 0));
	
	O.Normal.xyz = NormalDecode(Sample.xy * 2.0f - 1.0f);
	O.SnowMask = Sample.w;
	O.Hemi = Sample.z;
	
#ifdef USE_LEGACY_LIGHT
	float Surface = s_surface.Load(uint3(TexCoord, 0)).x;
	
	#ifdef USE_R2_STATIC_SUN
		O.Sun = Surface.x;
	#else
		uint data = Surface.x * 255u;
	
		O.SSS = float((data >> 7) & 1);
		O.Material = float(data & 0x7F) / 0x7F;
	#endif
#endif
}

inline void GbufferUnpackColor(in uint2 TexCoord, inout IXRayGbuffer O)
{
    float4 Sample = s_diffuse.Load(uint3(TexCoord, 0));
	
#ifdef USE_LEGACY_LIGHT
	O.Color = Sample.xyz;
	O.Gloss = Sample.w;
#else
	Sample.xyz = GammaToLinear(Sample.xyz);

	#ifdef USE_R2_STATIC_SUN
		O.Sun = Sample.w;
		O.AO = 1.0f;
	#else
		O.AO = GammaToLinear(Sample.w);
	#endif

	float4 Surface = s_surface.Load(uint3(TexCoord, 0));
	Surface.z = GammaToLinear(Surface.z);
	
	O.SSS = Surface.w;
	O.Roughness = Surface.x;
	
	O.Specular = lerp(Surface.z, Sample.xyz, Surface.y);
	O.Color = Sample.xyz * float(1.0f - Surface.y);
#endif
}

inline void GbufferUnpack(in uint2 HPos, inout IXRayGbuffer O)
{
	GbufferUnpackColor(HPos, O);
	GbufferUnpackDepth(HPos, O);
	GbufferUnpackNormal(HPos, O);
}

#endif

