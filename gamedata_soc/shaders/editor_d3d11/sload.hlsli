#ifndef SLOAD_H
#define SLOAD_H
#include "common.hlsli"

static const float fParallaxStartFade = 8.0f;
static const float fParallaxStopFade = 12.0f;

uniform sampler s_detailBumpX;
uniform sampler s_detailBump;

uniform sampler s_bumpX;
uniform sampler s_bump;

struct XrayMaterial
{
    float Metalness;
    float Roughness;

    float3 Normal;
    float3 Point;

    float4 Color;
    float Hemi;
    float Sun;

    float SSS;
    float AO;
};

void SloadNew(inout p_bumped_new I, inout XrayMaterial M)
{
    M.Color = tex2D(s_base, I.tcdh.xy);

#ifdef USE_BUMP
    float4 Bump = tex2D(s_bump, I.tcdh.xy);
    float4 BumpX = tex2D(s_bumpX, I.tcdh.xy);

    //	#ifndef USE_PBR
    M.Normal = Bump.wzy + BumpX.xyz - 1.0f;
    M.Normal.z *= 0.5f;

    M.Roughness = Bump.x * Bump.x;
    M.Metalness = 0.0f;

    M.SSS = 0.0;
    M.AO = 1.0;
//	#endif
#else
    M.Normal = float3(0.0f, 0.0f, 1.0f);

    M.Roughness = 0.04f;
    M.Metalness = 0.0f;

    M.SSS = 0.0f;
    M.AO = 1.0f;
#endif

#ifdef USE_TDETAIL
		float2 tcdbump = I.tcdh.xy * dt_params.xy;
		float4 Detail = tex2D(s_detail, tcdbump);
	#ifndef USE_PBR
			M.Color.xyz *= Detail.xyz * 2.0f;
			
		#ifdef USE_TDETAIL_BUMP
			float4 DetailBump = tex2D(s_detailBump, tcdbump);
			float4 DetailBumpX = tex2D(s_detailBumpX, tcdbump);
			M.Normal += DetailBump.wzy + DetailBumpX.xyz - 1.0f;
			M.Roughness *= DetailBump.x * 2.0f;
		#else
			M.Roughness *= Detail.w * 2.0f;
		#endif
	#endif
#endif

#ifndef USE_PBR
    M.Metalness = M.Roughness * 0.1f;
    M.Roughness = saturate(1.0f - M.Roughness);
#endif
}

#endif

