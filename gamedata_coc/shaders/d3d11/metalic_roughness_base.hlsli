#ifndef metalic_roughness_base_h_ixray_included
#define metalic_roughness_base_h_ixray_included

#include "common.hlsli"
#ifndef PI
#define PI 3.141592653589793f
#endif

struct IXRayMaterial
{
#ifndef USE_LEGACY_LIGHT
	float Metalness;
	float Roughness;
	float Specular;
	
    float AO;
	float TerrainHeight;
	bool HasTerrainHeight;
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
	
	uint MaterialID;
};

IXRayMaterial IXRayMaterial_Lerp(in IXRayMaterial A, in IXRayMaterial B, float Factor)
{
	IXRayMaterial O = A;

#ifndef USE_LEGACY_LIGHT
	O.Metalness = lerp(A.Metalness, B.Metalness, Factor);
	O.Roughness = lerp(A.Roughness, B.Roughness, Factor);
	O.Specular = lerp(A.Specular, B.Specular, Factor);
	O.AO = lerp(A.AO, B.AO, Factor);
#else
	O.Gloss = lerp(A.Gloss, B.Gloss, Factor);
	O.Material = lerp(A.Material, B.Material, Factor);
#endif

	O.SSS = lerp(A.SSS, B.SSS, Factor);

	O.Normal = lerp(A.Normal, B.Normal, Factor);
	O.Normal = normalize(O.Normal);

	O.Color = lerp(A.Color, B.Color, Factor);

	O.Hemi = lerp(A.Hemi, B.Hemi, Factor);
	O.Sun = lerp(A.Sun, B.Sun, Factor);

	return O;
}

#define BASE_ID 0
#define OBJECT_ID 1
#define TERRAIN_ID 2
#define FOLIAGE_ID 3

#define MAX_ID 3.0f

struct IXRayGbuffer
{
	float3 Normal;
	float3 Color;
	
#ifndef USE_LEGACY_LIGHT
	float Roughness;
	float3 Specular;
	
	float AO;
	float TerrainHeight;
	bool HasTerrainHeight;
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
	
	uint MaterialID;
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
	
#ifndef DISABLE_MOTION_VECTORS
    float4 Velocity : SV_Target1;
	
    float Reactive : SV_Target2;
#else
    float Reactive : SV_Target1;
#endif
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

inline void WboitBufferPack(inout IXRayForward O, in float3 Point)
{
	O.Reactive = 1.0f - O.Color.w;
	
	O.Color.w = O.Color.w * O.Color.w * clamp(40.0f * rcp(1e-5 + dot(Point, Point)), 0.001, 3000);
	O.Color.xyz = O.Color.xzy * O.Color.w;
}

#ifndef USE_LEGACY_LIGHT
inline float PackTerrainSurface(in float SSS, in float TerrainHeight, in bool HasTerrainHeight)
{
	// Terrain stores either SSS or height, with bit 7 selecting height.
	bool HasHeight = HasTerrainHeight && SSS <= EPS;
	float Value = HasHeight ? TerrainHeight : SSS;
	uint Packed = uint(saturate(Value) * 127.0f + 0.5f);

	if (HasHeight)
	{
		Packed |= 0x80u;
	}

	return float(Packed) / 255.0f;
}

inline void UnpackTerrainSurface(in float Surface, out float SSS, out float TerrainHeight, out bool HasTerrainHeight)
{
	uint Packed = uint(saturate(Surface) * 255.0f + 0.5f);
	float Value = float(Packed & 0x7Fu) / 127.0f;

	HasTerrainHeight = (Packed & 0x80u) != 0;
	TerrainHeight = HasTerrainHeight ? Value : 0.0f;
	SSS = HasTerrainHeight ? 0.0f : Value;
}
#endif

inline void GbufferPack(inout IXRayGbufferPack O, inout IXRayMaterial M)
{
    O.Color.xyz = M.Color.xyz;
	
#ifdef USE_LEGACY_LIGHT
	O.Color.w = M.Gloss;
	O.Material = 0.0f;
	
	#ifdef USE_R2_STATIC_SUN
		O.Material.x = M.Sun;
	#else
		O.Material.x = float(((uint(M.SSS) & 1) << 7) | (uint(M.Material * 0x7F) & 0x7F)) / 255.0f;
	#endif
#else
	O.Color.w = M.AO;

	O.Material.x = M.Metalness;
	O.Material.y = M.Roughness;
	O.Material.z = M.Specular;
	O.Material.w = M.SSS;
	if (M.MaterialID == TERRAIN_ID)
	{
		O.Material.w = PackTerrainSurface(M.SSS, M.TerrainHeight, M.HasTerrainHeight);
	}
	
	#ifdef USE_R2_STATIC_SUN
		O.Color.xyz *= O.Color.w;
		O.Material.z *= O.Color.w;

		O.Color.w = M.Sun;
	#endif
#endif
	
    O.Normal.xy = NormalEncode(M.Normal.xyz) * 0.5f + 0.5f;
	
	O.Normal.w = float(M.MaterialID) / MAX_ID;
	O.Normal.z = M.Hemi;
	
	// float2 Jitter = Hash32(cos(M.Point * 124.0f) * 1245.0f);
	
	// Jitter = frac(Jitter + timers.x) - 0.5f;
	// Jitter *= rcp(256.0f);
	
	// O.Material += Jitter.yxyx;
	// O.Color += Jitter.yxyx;
	
	// O.Normal.xyz += Jitter.yxyx * 0.25f;
}

inline void GbufferUnpackMaterial(inout IXRayGbufferPack O, inout IXRayMaterial M)
{
    M.Color.xyz = O.Color.xyz;
	M.Color.w = 1.0f;

#ifdef USE_LEGACY_LIGHT
    M.Gloss = O.Color.w;

	#ifdef USE_R2_STATIC_SUN
		M.Sun = O.Material.x;
		M.Material = 0.25f;
	#else
		uint packed = uint(O.Material.x * 255.0f);
	
		M.SSS = float((packed >> 7) & 1);
		M.Material = float(packed & 0x7F) / 127.0f;
	#endif
#else
    M.AO = O.Color.w;

    M.Metalness = O.Material.x;
    M.Roughness = O.Material.y;
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
	
	M.MaterialID = uint(O.Normal.w * MAX_ID);
    M.Hemi = O.Normal.z;

#ifndef USE_LEGACY_LIGHT
	M.TerrainHeight = 0.0f;
	M.HasTerrainHeight = false;
	if (M.MaterialID == TERRAIN_ID)
	{
		UnpackTerrainSurface(O.Material.w, M.SSS, M.TerrainHeight, M.HasTerrainHeight);
	}
#endif
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
	O.MaterialID = uint(Sample.w * MAX_ID);
	
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
	Surface.z = Surface.z * Surface.z * 0.16f;
	
	O.SSS = Surface.w;
	O.TerrainHeight = 0.0f;
	O.HasTerrainHeight = false;
	if (O.MaterialID == TERRAIN_ID)
	{
		UnpackTerrainSurface(Surface.w, O.SSS, O.TerrainHeight, O.HasTerrainHeight);
	}
	O.Roughness = Surface.y;
	
	O.Specular = lerp(Surface.z, Sample.xyz, Surface.x);
	O.Color = Sample.xyz * float(1.0f - Surface.x);
#endif
}

inline void GbufferUnpack(in uint2 HPos, inout IXRayGbuffer O)
{
	GbufferUnpackNormal(HPos, O);
	GbufferUnpackColor(HPos, O);
	GbufferUnpackDepth(HPos, O);
}

#endif

