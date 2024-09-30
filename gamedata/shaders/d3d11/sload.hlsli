#ifndef SLOAD_H
#define SLOAD_H
#include "common.hlsli"

static const float fParallaxStartFade = 8.0f;
static const float fParallaxStopFade = 12.0f;

#ifndef PARALLAX_HEIGHT
#define PARALLAX_HEIGHT 0.01
#endif

#ifdef USE_STEEPPARALLAX
#ifdef ALLOW_STEEPPARALLAX

void UpdateTC(inout p_bumped_new I)
{
	float3x3 TBN = float3x3(I.M1, I.M2, I.M3);
	float3 viewDir = mul(transpose(TBN), -I.position.xyz);   // better
	
	viewDir = normalize(viewDir);
	
	float2 currTexCoord = I.tcdh.xy;
	float height = s_bumpX.Sample(smp_base, currTexCoord).w;
	I.tcdh.xy += viewDir.xy * PARALLAX_HEIGHT * (height - 0.5f);
	
    if (I.position.z < fParallaxStopFade)
    {
		const float minLayers = 8.0f;
		const float maxLayers = 20.0f;
		const uint reliefSteps = 5;
		
		float numLayers = lerp(maxLayers, minLayers, abs(viewDir.z));
		float layerDepth = rcp(numLayers);
		
		float2 texcoordDelta = viewDir.xy * layerDepth * PARALLAX_HEIGHT;
		
		float currDepthMapVal = 1.0f - height;
		float currLayerDepth = 0.5f;
		
		[loop] while(currLayerDepth < currDepthMapVal)
		{
			currLayerDepth += layerDepth;
			currTexCoord -= texcoordDelta;
			currDepthMapVal = 1.0f - s_bumpX.SampleLevel(smp_base, currTexCoord, 0.0f).w;
		}
		
		texcoordDelta *= 0.5;
		layerDepth *= 0.5;
		
		currTexCoord += texcoordDelta;
		currLayerDepth -= layerDepth;
		
		[unroll(reliefSteps)]
		for(uint i = 0; i < reliefSteps; ++i)
		{
			currDepthMapVal = 1.0f - s_bumpX.SampleLevel(smp_base, currTexCoord, 0.0f).w;
			
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
			
        float fParallaxFade = smoothstep(fParallaxStartFade, fParallaxStopFade, I.position.z);	
		I.tcdh.xy = lerp(currTexCoord, I.tcdh.xy, fParallaxFade);
    }
}

#else

void UpdateTC(inout p_bumped_new I)
{
    float3x3 TBN = float3x3(I.M1, I.M2, I.M3);
    float3 viewDir = mul(transpose(TBN), -I.position.xyz);
    viewDir = normalize(viewDir);

    float height = s_bumpX.Sample(smp_base, I.tcdh.xy).w - 0.5f;
    height *= PARALLAX_HEIGHT;
    I.tcdh.xy += height * viewDir.xy;
}

#endif
#endif

void SloadNew(inout p_bumped_new I, inout IXrayMaterial M)
{
#if defined(USE_STEEPPARALLAX) && defined(USE_HIGH_QUALITY)
    UpdateTC(I);
#endif

    M.Color = s_base.Sample(smp_base, I.tcdh.xy);

#ifdef USE_BUMP
    float4 Bump = s_bump.Sample(smp_base, I.tcdh.xy);
    float4 BumpX = s_bumpX.Sample(smp_base, I.tcdh.xy);

    #ifdef USE_PBR
		M.Normal.xy = Bump.wy - 128.0f / 255.0f;
		M.Normal.z = sqrt(1.0f - dot(M.Normal.xy, M.Normal.xy));

		M.Metalness = BumpX.x;
		M.Roughness = BumpX.y;

		M.SSS = BumpX.z;
		M.AO = BumpX.w;
    #else
		M.Normal = Bump.wzy + BumpX.xyz - 1.0f;

		M.Metalness = 0.0f;
		M.Roughness = Bump.x;

#ifdef USE_LEGACY_LIGHT
		M.Roughness *= M.Roughness;
#endif

		M.SSS = 0.0;
		M.AO = 1.0;
    #endif
#else
	
    M.Normal = float3(0.0f, 0.0f, 1.0f);

    M.Roughness = def_gloss;
    M.Metalness = 0.0f;

    M.SSS = 0.0f;
    M.AO = 1.0f;

    #ifdef USE_PBR
    M.Roughness = 1.0f - M.Roughness;
    #endif
#endif

#ifdef USE_TDETAIL
    float2 tcdbump = I.tcdh.xy * dt_params.xy;
    float4 Detail = s_detail.Sample(smp_base, tcdbump);
    M.Color.xyz *= Detail.xyz * 2.0f;

    #ifndef USE_PBR
		M.Roughness *= Detail.w * 2.0f;
		#ifdef USE_TDETAIL_BUMP
			float4 DetailBump = s_detailBump.Sample(smp_base, tcdbump);
			float4 DetailBumpX = s_detailBumpX.Sample(smp_base, tcdbump);
			M.Normal += DetailBump.wzy + DetailBumpX.xyz - 1.0f;
		#endif
    #else
        #ifdef USE_TDETAIL_BUMP
			float4 DetailBump = s_detailBump.Sample(smp_base, tcdbump);
			float4 DetailBumpX = s_detailBumpX.Sample(smp_base, tcdbump);

			float3 DetailNormal = DetailBump.wyy - 128.0f / 255.0f;
			DetailNormal.z = sqrt(1.0f - dot(DetailNormal.xy, DetailNormal.xy));

			M.Normal += DetailNormal;

			M.Metalness *= DetailBumpX.x * 2.0f;
			M.Roughness *= DetailBumpX.y * 2.0f;

			M.SSS *= DetailBumpX.z;
			M.AO *= DetailBumpX.w;
        #else
			M.Roughness *= Detail.w * 2.0f;
        #endif
    #endif
#endif

#ifndef USE_PBR
    M.Normal.z *= 0.5f;
    // Aprox GSC material to PBS
    #ifndef USE_LEGACY_LIGHT
		M.Roughness = max(0.0f, 1.0f - M.Roughness);
		M.Roughness *= M.Roughness * M.Roughness;
		M.Roughness = lerp(0.2, 0.9f, M.Roughness);
    #endif
#else
    M.Normal.y *= -1.0f;
    M.Roughness = max(0.02f, M.Roughness);
#endif
}

#endif

