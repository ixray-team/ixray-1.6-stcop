#ifndef SHARED_MATERIAL_HLSLI
#define SHARED_MATERIAL_HLSLI

// A surface from the texels of its maps, and its packing into the three surface targets. No resources and no
// derivatives here: pixel shaders and the compute shading pass both include it and do their own sampling.

// As common\common_functions.hlsli has them, for a shader that includes this without it
#ifndef SRGB_GAMMA
	#define SRGB_GAMMA 2.2
#endif

#if !defined(USE_LEGACY_LIGHT) && !defined(GammaToLinear)
	#define GammaToLinear(x) pow(abs(x), SRGB_GAMMA)
#endif

#ifndef def_gloss
#define def_gloss float(2.f / 255.f)
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

#define BASE_ID 0
#define OBJECT_ID 1
#define TERRAIN_ID 2
#define FOLIAGE_ID 3

#define MAX_ID 3.0f

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

// Legacy lighting keeps gloss (specular intensity) instead of a PBR surface: reflect as a mirror scaled by gloss
float3 GbufferSpecular(IXRayGbuffer O)
{
#ifdef USE_LEGACY_LIGHT
	return O.Gloss * 0.04f;
#else
	return O.Specular;
#endif
}

float GbufferRoughness(IXRayGbuffer O)
{
#ifdef USE_LEGACY_LIGHT
	return 0.0f;
#else
	return (O.Roughness);
#endif
}

// What lighting reads of a surface that never went through the targets: GbufferUnpack without the packing.
// M.Normal and Point are in view space. GammaToLinear is the including shader's
inline IXRayGbuffer LitSurface(IXRayMaterial M, float3 Point)
{
	IXRayGbuffer O = (IXRayGbuffer)0;
#ifdef USE_LEGACY_LIGHT
	O.Color = M.Color.xyz;
	O.Gloss = M.Gloss;
	O.Material = M.Material;
	O.SSS = M.SSS;
#else
	float3 Albedo = GammaToLinear(M.Color.xyz);
	float F0 = M.Specular * M.Specular * 0.16f;

	O.Color = Albedo * (1.0f - M.Metalness);
	O.Specular = lerp(F0, Albedo, M.Metalness);
	O.Roughness = M.Roughness;
	O.AO = GammaToLinear(M.AO);
	O.SSS = M.SSS;
	O.TerrainHeight = M.TerrainHeight;
	O.HasTerrainHeight = M.HasTerrainHeight;
#endif
	O.Hemi = M.Hemi;
	O.MaterialID = M.MaterialID;

	O.Depth = 1.0f;
	O.Point = Point;
	O.PointHud = Point;
	O.PointReal = Point;
	O.ViewDist = length(Point);
	O.View = Point * rcp(O.ViewDist);

	O.Normal = M.Normal;

	if (O.MaterialID != FOLIAGE_ID)
	{
		O.Normal = normalize(O.Normal - O.View * max(0.0f, dot(O.Normal, O.View) + 0.01f));
	}

	return O;
}

// A surface without bump maps
inline void MaterialFlat(inout IXRayMaterial M)
{
	M.Normal = float3(0.0f, 0.0f, 1.0f);

#ifdef USE_LEGACY_LIGHT
	M.Gloss = def_gloss;
#else
	M.SSS = 0.0f;
	M.AO = 1.0f;

	M.Specular = def_gloss;
	M.Metalness = 0.0f;
#endif
}

// F0: the specular value of a PBR surface, which has no map for it here
inline void MaterialBump(inout IXRayMaterial M, float4 Bump, float4 BumpX, bool IsPbr, float F0)
{
	if (IsPbr)
	{
		M.Normal.xy = Bump.wy * 2.0f - 1.0f;
		M.Normal.z = sqrt(1.0f - saturate(dot(M.Normal.xy, M.Normal.xy)));

#ifdef USE_LEGACY_LIGHT
		M.Gloss = 1.0f - BumpX.y;
		M.Color.xyz *= BumpX.w;
#else
		M.Metalness = BumpX.x;
		M.Roughness = BumpX.y;
		M.Specular = F0;
		M.AO = BumpX.w;
#endif

		M.SSS = BumpX.z;
	}
	else
	{
		M.Normal = Bump.wzy + BumpX.xyz - 1.0f;

#ifdef USE_LEGACY_LIGHT
		M.Gloss = Bump.x * Bump.x;
#else
		M.Specular = Bump.x;
		M.Metalness = 0.0f;

		M.SSS = 0.0f;
		M.AO = 1.0f;
#endif
	}
}

// The detail map over the surface; its bump maps count only with HasBump
inline void MaterialDetail(inout IXRayMaterial M, float4 Detail, float4 DetailBump, float4 DetailBumpX, bool HasBump, bool IsPbr)
{
	M.Color.xyz *= Detail.xyz * 2.0f;

	if (!IsPbr || !HasBump)
	{
#ifdef USE_LEGACY_LIGHT
		M.Gloss *= Detail.w * 2.0f;
#else
		M.Specular *= Detail.w * 2.0f;
#endif
	}

	if (HasBump)
	{
		if (IsPbr)
		{
			float3 DetailNormal = DetailBump.wyy * 2.0f - 1.0f;
			DetailNormal.z = sqrt(saturate(1.0f - dot(DetailNormal.xy, DetailNormal.xy)));

			M.Normal += DetailNormal;

#ifdef USE_LEGACY_LIGHT
			M.Gloss *= DetailBumpX.x * 2.0f;
#else
			M.Metalness *= DetailBumpX.x * 2.0f;
			M.Roughness *= DetailBumpX.y * 2.0f;

			M.SSS *= DetailBumpX.z;
			M.AO *= DetailBumpX.w;
#endif
		}
		else
		{
			M.Normal += DetailBump.wzy + DetailBumpX.xyz - 1.0f;
		}
	}
}

// The last step for a surface that is not made of terrain layers. Kind: L_material.w
inline void MaterialFinish(inout IXRayMaterial M, bool IsPbr, float Kind)
{
	if (IsPbr)
	{
		M.Normal.y *= -1.0f;

#ifdef USE_LEGACY_LIGHT
		M.Gloss = 1.0f - M.Gloss;
#else
		M.Roughness = max(0.02f, M.Roughness);
#endif
	}
	else
	{
#ifndef USE_LEGACY_LIGHT
		M.Roughness = Kind * 0.25f + 0.15f;
		M.Specular = M.Specular * M.Specular;
#endif

		M.Normal.z *= 0.5f;

		// for some reason
		M.Normal.z = abs(M.Normal.z);
	}
}

// Four terrain layers, each already weighed by the mask and summed. The specular value and the height of a PBR
// surface stay with the caller
inline void MaterialLayers(inout IXRayMaterial M, float3 Tint, float4 Bumps, float4 Extras, bool IsPbr)
{
	M.Color.xyz *= Tint * 2.0f;

	if (IsPbr)
	{
		M.Normal.xy = Bumps.wy * 2.0f - 1.0f;
		M.Normal.z = sqrt(1.0f - saturate(dot(M.Normal.xy, M.Normal.xy)));

		M.Normal.y *= -1.0f;

#ifndef USE_LEGACY_LIGHT
		M.Metalness = Extras.x;
		M.Roughness = Extras.y;
		M.SSS = Extras.z;
		M.AO = Extras.w;
#endif
	}
	else
	{
		M.Normal = Bumps.wzy - 0.5f;
		M.Normal.z *= 0.5f;

#ifdef USE_LEGACY_LIGHT
		M.Gloss = min(1.0f, Bumps.x);
#else
		M.Specular = min(1.0f, Bumps.x);
#endif
	}
}

#ifndef USE_LEGACY_LIGHT
inline float PackTerrainSurface(in float SSS, in float TerrainHeight, in bool HasTerrainHeight)
{
	// Terrain stores either SSS or height, with bit 7 selecting height.
	bool HasHeight = HasTerrainHeight && SSS <= 0.00001f;
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

// M.Normal: in view space
inline void PackSurface(IXRayMaterial M, out float4 Color, out float4 Normal, out float4 Material)
{
	Color.xyz = M.Color.xyz;

#ifdef USE_LEGACY_LIGHT
	Color.w = M.Gloss;
	Material = 0.0f;

	#ifdef USE_R2_STATIC_SUN
		Material.x = M.Sun;
	#else
		Material.x = float(((uint(M.SSS) & 1) << 7) | (uint(M.Material * 0x7F) & 0x7F)) / 255.0f;
	#endif
#else
	Color.w = M.AO;

	Material.x = M.Metalness;
	Material.y = M.Roughness;
	Material.z = M.Specular;
	Material.w = M.SSS;

	if (M.MaterialID == TERRAIN_ID)
	{
		Material.w = PackTerrainSurface(M.SSS, M.TerrainHeight, M.HasTerrainHeight);
	}

	#ifdef USE_R2_STATIC_SUN
		Color.xyz *= Color.w;
		Material.z *= Color.w;

		Color.w = M.Sun;
	#endif
#endif

	Normal.xy = NormalEncode(M.Normal.xyz) * 0.5f + 0.5f;
	Normal.z = M.Hemi;
	Normal.w = float(M.MaterialID) / MAX_ID;
}

#endif
