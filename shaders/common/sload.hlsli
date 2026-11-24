#ifndef SLOAD_H
#define SLOAD_H
#include "common\common.hlsli"

static const float fParallaxStartFade = 8.0f;
static const float fParallaxStopFade = 12.0f;

#ifdef USE_IOR_TEXTURE
Texture2D s_specular : register(t25);
#endif

#ifdef USE_SNOW_TEXTURE
Texture2D s_snow : register(t26);
#endif

#ifndef PARALLAX_HEIGHT
	#ifdef USE_PBR
		#define PARALLAX_HEIGHT 0.02
	#else
		#define PARALLAX_HEIGHT 0.01
	#endif
#endif

#ifndef DYNAMIC_SNOW_COLOR
	#define DYNAMIC_SNOW_COLOR float3(0.75f, 0.75f, 0.75f)
#endif

#ifndef PLANE_CORRECTION_VAL
	#define PLANE_CORRECTION_VAL EPS
#endif

#ifndef F0_BASE
	#ifdef USE_PBR
		#define F0_BASE 0.5f // 0.5 -> 0.04
	#else
		#define F0_BASE 0.0f
	#endif
#endif

inline float2 UpdateTC(inout p_bumped_new I, in float2 texCoord, Texture2D heightMap, uint idx)
{
#ifdef ALLOW_STEEPPARALLAX
	float3x3 TBN = float3x3(I.M1, I.M2, I.M3);
	float3 viewDir = mul(transpose(TBN), -I.position.xyz);
	
	viewDir = normalize(viewDir);
	
	float2 currTexCoord = texCoord;
	float height = heightMap.Sample(smp_base, currTexCoord)[idx];
	
	float2 texcoordDelta = viewDir.xy * PARALLAX_HEIGHT;
		
#ifdef USE_PARALLAX_PLANE_CORRECTION
	texcoordDelta *= rcp(max(PLANE_CORRECTION_VAL, abs(viewDir.z)));
#endif

#ifdef USE_PBR
	texCoord -= texcoordDelta * (1.0f - height);
#else
	texCoord += texcoordDelta * (height - 0.5f);
#endif
	
#ifndef DISABLE_STEEPPARALLAX
    if (I.position.z < fParallaxStopFade)
    {
		const float minLayers = 8.0f;
		const float maxLayers = 20.0f;
		
		float numLayers = lerp(maxLayers, minLayers, viewDir.z * viewDir.z);
		float layerDepth = rcp(numLayers);
		
		texcoordDelta = viewDir.xy * layerDepth * PARALLAX_HEIGHT;
		
#ifdef USE_PARALLAX_PLANE_CORRECTION
		texcoordDelta *= rcp(max(PLANE_CORRECTION_VAL, abs(viewDir.z)));
#endif
	
		float currDepthMapVal = 1.0f - height;
		float currLayerDepth = 0.5f;
		
#ifdef USE_PBR
		currLayerDepth = 0.0f;
#endif

        //LVutner: Tuff shit, consider unrolling it in the future
		[loop] while(currLayerDepth < currDepthMapVal)
		{
			currLayerDepth += layerDepth;
			currTexCoord -= texcoordDelta;
			currDepthMapVal = 1.0f - heightMap.SampleLevel(smp_linear, currTexCoord, 0.0f)[idx];
		}
		
		float2 prevTexCoord = currTexCoord + texcoordDelta;

		float afterDepth  = currDepthMapVal - currLayerDepth;
		currDepthMapVal = 1.0f - heightMap.Sample(smp_base, prevTexCoord)[idx];
		
		float beforeDepth = currDepthMapVal - currLayerDepth + layerDepth;
		
		float weight = afterDepth * rcp(afterDepth - beforeDepth);
		currTexCoord = lerp(currTexCoord, prevTexCoord, weight);
		
        float fParallaxFade = smoothstep(fParallaxStopFade, fParallaxStartFade, I.position.z);	
		texCoord = lerp(texCoord, currTexCoord, fParallaxFade);
    }
#endif
#endif

	return texCoord;
}


inline void SloadNew(inout p_bumped_new I, inout IXRayMaterial M)
{
#if defined(USE_STEEPPARALLAX) && defined(USE_HIGH_QUALITY)
    #ifdef USE_PBR
		I.tcdh.xy = UpdateTC(I, I.tcdh.xy, s_bump, 0);
    #else
		I.tcdh.xy = UpdateTC(I, I.tcdh.xy, s_bumpX, 3);
	#endif
#endif

    M.Color = s_base.Sample(smp_base, I.tcdh.xy);

#ifdef USE_PBR
	const bool IsPbr = true;
#else
	const bool IsPbr = false;
#endif

#ifdef USE_BUMP
	float4 Bump = s_bump.Sample(smp_base, I.tcdh.xy);
	float4 BumpX = s_bumpX.Sample(smp_base, I.tcdh.xy);
	MaterialBump(M, Bump, BumpX, IsPbr, F0_BASE);

	#if defined(USE_PBR) && defined(USE_IOR_TEXTURE) && !defined(USE_LEGACY_LIGHT)
		M.Specular = s_specular.Sample(smp_base, I.tcdh.xy).x;
	#endif
#else
	MaterialFlat(M);
#endif

#ifdef USE_TDETAIL
	float2 tcdbump = I.tcdh.xy * dt_params.xy;
	float4 Detail = s_detail.Sample(smp_base, tcdbump);

	#ifdef USE_TDETAIL_BUMP
		MaterialDetail(M, Detail, s_detailBump.Sample(smp_base, tcdbump), s_detailBumpX.Sample(smp_base, tcdbump), true, IsPbr);
	#else
		MaterialDetail(M, Detail, 0.0f, 0.0f, false, IsPbr);
	#endif
#endif

#ifdef USE_SNOW_TEXTURE
	float4 Snow = s_snow.Sample(smp_base, I.tcdh.xy);
    Snow.y *= smoothstep(0.2f, 0.3f, hemi_cube_pos_faces.y);
	
	#ifndef USE_LEGACY_LIGHT
		M.Roughness = lerp(M.Roughness, Snow.x, Snow.y);
		M.Metalness = lerp(M.Metalness, Snow.z, Snow.y);
	#else
		M.Gloss = lerp(M.Gloss, Snow.x * Snow.x, Snow.y);
	#endif

	M.Color.xyz = lerp(M.Color.xyz, DYNAMIC_SNOW_COLOR, Snow.y);
#endif

	MaterialFinish(M, IsPbr, L_material.w);
}
#endif

