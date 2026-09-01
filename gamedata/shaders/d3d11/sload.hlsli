#ifndef SLOAD_H
#define SLOAD_H
#include "common.hlsli"

static const float fParallaxStartFade = 8.0f;
static const float fParallaxStopFade = 12.0f;

#ifdef USE_IOR_TEXTURE
Texture2D s_specular;
#endif

#ifdef USE_SNOW_TEXTURE
Texture2D s_snow;
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
		
#ifdef ENABLE_RELIEF_STEPS
		const uint reliefSteps = 5;
		
		texcoordDelta *= 0.5;
		layerDepth *= 0.5;
		
		currTexCoord += texcoordDelta;
		currLayerDepth -= layerDepth;
		
		[unroll]
		for(uint i = 0; i < reliefSteps; ++i)
		{
			currDepthMapVal = 1.0f - heightMap.SampleLevel(smp_linear, currTexCoord, 0.0f)[idx];
			
			texcoordDelta *= 0.5f;
			layerDepth *= 0.5f;
			
			if(currDepthMapVal > currLayerDepth)
			{
				currTexCoord -= texcoordDelta;
				currLayerDepth += layerDepth;
			}
			else
			{
				currTexCoord += texcoordDelta;
				currLayerDepth -= layerDepth;
			}
		}
#else
		float2 prevTexCoord = currTexCoord + texcoordDelta;

		float afterDepth  = currDepthMapVal - currLayerDepth;
		currDepthMapVal = 1.0f - heightMap.Sample(smp_base, prevTexCoord)[idx];
		
		float beforeDepth = currDepthMapVal - currLayerDepth + layerDepth;
		
		float weight = afterDepth * rcp(afterDepth - beforeDepth);
		currTexCoord = lerp(currTexCoord, prevTexCoord, weight);
#endif
		
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

#ifdef USE_BUMP
    float4 Bump = s_bump.Sample(smp_base, I.tcdh.xy);
    float4 BumpX = s_bumpX.Sample(smp_base, I.tcdh.xy);

    #ifdef USE_PBR
		M.Normal.xy = Bump.wy * 2.0 - 1.0;
		M.Normal.z = sqrt(1.0f - saturate(dot(M.Normal.xy, M.Normal.xy)));
		
		#ifdef USE_LEGACY_LIGHT
			M.Gloss = 1.0f - BumpX.y;
			M.Color.xyz *= BumpX.w;
		#else
			M.Metalness = BumpX.x;
			M.Roughness = BumpX.y;
			M.Specular = F0_BASE;
			M.AO = BumpX.w;
		
			#ifdef USE_IOR_TEXTURE
				M.Specular = s_specular.Sample(smp_base, I.tcdh.xy).x;
			#endif
		#endif

		M.SSS = BumpX.z;
    #else
		M.Normal = Bump.wzy + BumpX.xyz - 1.0f;

		#ifdef USE_LEGACY_LIGHT
			M.Gloss = Bump.x * Bump.x;
		#else
			M.Specular = Bump.x;
			M.Metalness = 0.0f;

			M.SSS = 0.0;
			M.AO = 1.0;
		#endif
    #endif
#else
    M.Normal = float3(0.0f, 0.0f, 1.0f);

	#ifdef USE_LEGACY_LIGHT
		M.Gloss = def_gloss;
	#else
		M.SSS = 0.0f;
		M.AO = 1.0f;

		M.Specular = def_gloss;
		M.Metalness = 0.0f;
	#endif
#endif

#ifdef USE_TDETAIL
    float2 tcdbump = I.tcdh.xy * dt_params.xy;
    float4 Detail = s_detail.Sample(smp_base, tcdbump);
	
    M.Color.xyz *= Detail.xyz * 2.0f;

    #ifndef USE_PBR
		#ifdef USE_LEGACY_LIGHT
			M.Gloss *= Detail.w * 2.0f;
		#else
			M.Specular *= Detail.w * 2.0f;
		#endif
		#ifdef USE_TDETAIL_BUMP
			float4 DetailBump = s_detailBump.Sample(smp_base, tcdbump);
			float4 DetailBumpX = s_detailBumpX.Sample(smp_base, tcdbump);
			M.Normal += DetailBump.wzy + DetailBumpX.xyz - 1.0f;
		#endif
    #else
        #ifdef USE_TDETAIL_BUMP
			float4 DetailBump = s_detailBump.Sample(smp_base, tcdbump);
			float4 DetailBumpX = s_detailBumpX.Sample(smp_base, tcdbump);

			float3 DetailNormal = DetailBump.wyy * 2.0 - 1.0;
			DetailNormal.z = sqrt(1.0f - dot(DetailNormal.xy, DetailNormal.xy));

			M.Normal += DetailNormal;

			#ifndef USE_LEGACY_LIGHT
				M.Metalness *= DetailBumpX.x * 2.0f;
				M.Roughness *= DetailBumpX.y * 2.0f;

				M.SSS *= DetailBumpX.z;
				M.AO *= DetailBumpX.w;
			#else
				M.Gloss *= DetailBumpX.x * 2.0f;
			#endif
        #else
			#ifdef USE_LEGACY_LIGHT
				M.Gloss *= Detail.w * 2.0f;
			#else
				M.Specular *= Detail.w * 2.0f;
			#endif
        #endif
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

#ifndef USE_PBR
	#ifndef USE_LEGACY_LIGHT
		M.Roughness = test_exp_to_shaders_1.x; //L_material.w * 0.50f + 0.25f;
		M.Specular = M.Specular * M.Specular * test_exp_to_shaders_2.x;
	#endif

	#ifndef USE_TRUE_NORMAL_MAP
		M.Normal.z *= 0.5f;
	#endif
	
	// for some reason
	M.Normal.z = abs(M.Normal.z);
#else
	#ifndef USE_DX_NORMAL_MAP
		M.Normal.y *= -1.0f;
	#endif
    #ifndef USE_LEGACY_LIGHT
		M.Roughness = max(0.02f, M.Roughness);
	#else
		M.Gloss = 1.0f - M.Gloss;
	#endif
#endif
}
#endif

