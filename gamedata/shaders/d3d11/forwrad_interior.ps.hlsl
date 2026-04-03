#include "common.hlsli"

#define DISABLE_STEEPPARALLAX

#include "sload.hlsli"
#include "shadow.hlsli"

float3 CubemapParralax(float3 PositionWS, float3 ReflDirectionWS)
{
	float3 BoxMax = 2.5f; BoxMax.z = 0.0f;
	float3 BoxMin = -2.5f; BoxMin.z = -3.0f;
	
	float3 FirstPlaneIntersect = (BoxMax - PositionWS) / ReflDirectionWS;
	float3 SecondPlaneIntersect = (BoxMin - PositionWS) / ReflDirectionWS;
	
	float3 FurthestPlane = max(FirstPlaneIntersect, SecondPlaneIntersect);
	float Distance = min(min(FurthestPlane.x, FurthestPlane.y), FurthestPlane.z);

	return PositionWS + ReflDirectionWS * Distance;
}

float3 ParallaxCorrect_SphereProxy_Full(
    float3 worldPos,
    float3 reflDir,       // normalized
    float3 probeCenter,
    float  radius
)
{
    float3 oc = worldPos - probeCenter;

    float b = dot(oc, reflDir);
    float c = dot(oc, oc) - radius * radius;

    float d = b * b - c;

    float s = sqrt(abs(d));
    float t = -b + sqrt(d);

    float3 hit = worldPos + t * reflDir;
    return normalize(hit - probeCenter);
}

void cotangent_frame(inout p_bumped_new O)
{
    // Get edge vectors of the pixel triangle
    float3 dp1 = ddx(O.position.xyz);
    float3 dp2 = ddy(O.position.xyz);
	
    float2 duv1 = ddx(O.tcdh.xy);
    float2 duv2 = ddy(O.tcdh.xy);

	float3 N = normalize(cross(dp1, dp2));

    // Solve the linear system
    float3 dp2perp = cross(dp2, N);
    float3 dp1perp = cross(N, dp1);
	
    float3 T = normalize(dp2perp * duv1.x + dp1perp * duv2.x);
    float3 B = normalize(dp2perp * duv1.y + dp1perp * duv2.y);
	
    float3x3 xform = float3x3(
        T.x, B.x, N.x,
        T.y, B.y, N.y,
        T.z, B.z, N.z
	);

    O.M1 = xform[0];
    O.M2 = xform[1];
    O.M3 = xform[2];
}

float3 CompureDiffuseIrradanceSimple(float3 LightDirection, float3 Hemi)
{
	LightDirection = normalize(LightDirection);
	
#ifdef IBL_REMAP_IRRADANCE
	RemapVector(LightDirection);
#endif

#ifdef USE_NORMAL_HEMI_DISTRIBUTION
	Hemi = min(Hemi, LightDirection.yyy * 0.375f + 0.375f);
#endif

	float3 SampleLast = env_s0.SampleLevel(smp_linear, LightDirection, 0.0f).xyz;
	float3 SampleNext = env_s1.SampleLevel(smp_linear, LightDirection, 0.0f).xyz;

#ifdef USE_CGIM_SKY_TWEAK
	float topToDownVec = saturate(LightDirection.y);
	topToDownVec *= topToDownVec;
	
	float Factor = SMALLSKY_TOP_VECTOR_POWER;
	Factor = saturate(Factor + (1.0 - Factor) * topToDownVec) + (1.0 - Factor) * 0.5f;
	
	Hemi *= Factor * Factor; float3 Irradance = 1.0f;
	Hemi *= lerp(SampleLast, SampleNext, L_hemi_color.w);
#else
	float3 Irradance = lerp(SampleLast, SampleNext, L_hemi_color.w);
#endif

#ifdef USE_DIFFUSE_SKY_COLOR
	#ifdef USE_BGRA_SKYCOLOR
		Irradance *= L_sky_color.zyx;
	#else
		Irradance *= L_sky_color.xyz;
	#endif
#else
	Irradance *= L_hemi_color.xyz;
#endif

#ifdef USE_LEGACY_LIGHT
	Irradance *= Irradance;
#endif

	return Irradance * Hemi;
}

void main(p_bumped_new I, out f_forward O)
{	
#if defined(USE_STEEPPARALLAX) && defined(USE_HIGH_QUALITY)
    #ifdef USE_PBR
		I.tcdh.xy = UpdateTC(I, I.tcdh.xy, s_bump, 0);
    #else
		I.tcdh.xy = UpdateTC(I, I.tcdh.xy, s_bumpX, 3);
	#endif
#endif

 //	cotangent_frame(I);

	float3x3 TBN = float3x3(I.M1, I.M2, I.M3);
	
	float3 viewDir = mul(transpose(TBN), I.position.xyz);
	float3 PositionWS = float3(I.tcdh.xy * 2.0f - 1.0f, 0);
	viewDir = normalize(viewDir);
	
	float3 dist = CubemapParralax(PositionWS, viewDir);	
	
	float3 LightDirE = mul((float3x3)m_V, L_sun_dir_w.xyz);
	float3 LightDir = normalize(mul(transpose(TBN), -LightDirE).xyz);
	
	float Hemi = I.tcdh.z;
	
#ifdef USE_LM_HEMI
    float4 hs = s_hemi.Sample(smp_rtlinear, I.tcdh.zw);
    Hemi = get_hemi(hs);
#endif
	
	float4 SampleRef = s_env.Sample(smp_base, -dist);
	float3 LightColor = CompureDiffuseIrradanceSimple(dist, Hemi) + L_ambient.xyz;
	
 	if(LightDir.z > 0.0f) 
	{		
		float3 dist1 = CubemapParralax(dist, LightDir);
		float3 PosD = I.position.xyz + length(dist - PositionWS) * normalize(I.position.xyz) - length(dist1 - dist) * LightDirE.xyz + normalize(float3(I.M1.z, I.M2.z, I.M3.z)) * 0.1f;
		
		PosD = mul(m_invV, float4(PosD.xyz, 1.0f)).xyz;

		float Shadow = 1.0;
		
		int cascade_index;
		float3 smap_texcoord;
		
		bool is_in_bounds = calc_cascades(PosD.xyz, m_shadow_sun, cascade_index, smap_texcoord);

		if(is_in_bounds)
		{
			Shadow = pcf_3x3(s_smap_sun, smp_smap, smap_texcoord, float2(SMAP_size, 1.0 / SMAP_size), 0.0, cascade_index);
		}
		
		LightColor += L_sun_color.xyz * min(Shadow, 1.0f - s_base.Sample(smp_base, saturate(dist1.xy * 0.5f + 0.5f)).w);
	}
	
    O.Color.xyz = GammaToLinear(SampleRef.xyz * LightColor);
    O.Color.w = 1.0f - s_base.Sample(smp_base, I.tcdh.xy).w;

    float Fog = GammaToLinear(saturate(length(I.position.xyz) * fog_params.w + fog_params.x));
    O.Color = lerp(O.Color, GammaToLinear(fog_color), Fog);

#ifdef USE_LENGTH_BUFFER
	O.Color.xyz = saturate(O.Color.xyz * rcp(1.0f + O.Color.xyz));
#else
	#ifndef DISABLE_MOTION_VECTORS
		O.Velocity.xy = I.hpos_curr.xy / I.hpos_curr.w - I.hpos_old.xy / I.hpos_old.w;
		O.Reactive = O.Color.w * 0.9f; O.Velocity.zw = 1.0f;
	#endif
#endif
}

