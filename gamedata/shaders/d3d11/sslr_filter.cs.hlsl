#include "common.hlsli"
#define IXR_COMPUTE
#include "reflections.hlsli"
#include "metalic_roughness_light.hlsli"
#include "metalic_roughness_ambient.hlsli"

// Half resolution sslr_render results are packed in the top-left quarter of its targets
#ifdef USE_SSLR_HQ
	#define SSLR_TRACE_UV 1.0f
#else
	#define SSLR_TRACE_UV 0.5f
#endif


#define mirror(x) saturate(1.0 - abs(abs(x) - 1.0))

#define DISK32_RADIUS8 2.443279f
#define DISK32_RADIUS16 1.48565f
#define DISK32_RADIUS32 1.0f

#define NUM_SAMPLES 16
#define DISK32_RADIUS DISK32_RADIUS16

static const float2 Disk32_Normalized[32] = {
	float2(0.408569f, 0.024217f),
	float2(0.162925f, 0.230704f),
	float2(-0.108248f, 0.367911f),
	float2(-0.329684f, 0.150003f),
	float2(-0.223398f, -0.167128f),
	float2(-0.067794f, -0.356288f),
	float2(0.136270f, -0.214864f),
	float2(0.512536f, -0.384894f),

	float2(0.597250f, 0.006447f),
	float2(0.464972f, 0.455376f),
	float2(0.054674f, 0.571788f),
	float2(-0.423541f, 0.423589f),
	float2(-0.657243f, -0.046063f),
	float2(-0.484844f, -0.466902f),
	float2(0.019780f, -0.556973f),
	float2(0.00000f, 0.000000f),

	float2(0.932249f, 0.011329f),
	float2(0.857066f, 0.402364f),
	float2(0.681793f, 0.580318f),
	float2(0.323008f, 0.880092f),
	float2(-0.016841f, 0.961073f),
	float2(-0.422076f, 0.906560f),
	float2(-0.676936f, 0.692191f),
	float2(-0.925246f, 0.292709f),

	float2(-0.893555f, -0.016208f),
	float2(-0.790589f, -0.380594f),
	float2(-0.677237f, -0.701563f),
	float2(-0.295770f, -0.880309f),
	float2(-0.002152f, -0.909661f),
	float2(0.336380f, -0.833836f),
	float2(0.637664f, -0.692579f),
	float2(0.895505f, -0.323214f),
};

float IntersectAABB(float3 Dir, float3 Org, float3 Box)
{
	float3 RcpDir = rcp(Dir);

	float3 TNeg = (Box - Org) * RcpDir;
	float3 TPos = -RcpDir * (Box + Org);

	return max(min(TNeg.x, TPos.x), max(min(TNeg.y, TPos.y), min(TNeg.z, TPos.z)));
}

float HistoryClamp(float3 History, float3 Filtered, float3 aabb_min, float3 aabb_max)
{
	float3 Min = min(Filtered, min(aabb_min, aabb_max));
	float3 Max = max(Filtered, max(aabb_min, aabb_max));

	float3 Avg2 = Max + Min;
	float3 Dir = Filtered - History;

	float3 Org = History - Avg2 * 0.5f;
	float3 Scale = Max - Avg2 * 0.5f;

	return saturate(IntersectAABB(Dir, Org, Scale));
}

RWTexture2D<float4> u_sslr : register(u0);

// Output of this pass last frame: phase_sslr swaps the two history targets every frame and binds the previous one here
Texture2D s_sslr_history : register(t26);

// History is clipped to mean +- gamma * sigma of the reused ray colors, unweighted: the BRDF weights of a smooth surface keep
// only its own ray and the box would collapse. Half resolution moves the traced pixel of every 2x2 quad each frame,
// so its result changes more between frames and keeps a wider box
#ifdef USE_SSLR_HQ
	#define SSLR_HISTORY_GAMMA 1.25f
#else
	#define SSLR_HISTORY_GAMMA 1.75f
#endif

[numthreads(8, 8, 1)]
void main(uint2 DTid : SV_DispatchThreadID, uint2 Gid : SV_GroupID, uint GI : SV_GroupIndex)
{
	//LVutner: Making my life easier.
	PSInputFullscreen I;
	I.hpos.xy = float2(DTid.xy) + 0.5; //half-pix
	I.hpos.zw = float2(0.0, 1.0);
	I.texcoord = I.hpos.xy * pos_decompression_params2.zw;

    IXRayGbuffer O = (IXRayGbuffer)NULL;
    GbufferUnpack((uint2)I.hpos.xy, O);

	float isHUDRender = O.Depth < 0.02f ? 1.0f : 0.0f;
	
	if(O.Depth >= 1.0f)
	{
		u_sslr[DTid.xy] = float4(s_image.SampleLevel(smp_nofilter, I.texcoord * SSLR_TRACE_UV, 0.0f).xyz, O.Depth);
		return;
	}

	// Combine uses the environment here, the depth keeps the history test valid when the pixel returns to SSLR
	[branch]
	if (SslrWeight(O.Normal, O.View, O.Specular, O.Roughness) <= 0.0f)
	{
		u_sslr[DTid.xy] = float4(0.0f, 0.0f, 0.0f, O.Depth);
		return;
	}

	float3 ReflectPoint = GbufferGetPointRealUnjitter(I.texcoord.xy, O.Depth);
	float3 View = normalize(ReflectPoint);
	
	float4 FinalColor = 0.0f;
	float FinalWeight = 0.0;

	float3 Moment1 = 0.0f;
	float3 Moment2 = 0.0f;
	
	float SampleRadius = 32.0f - 24.0f * GetBorderAtten(I.texcoord, 0.025f);

	// Stochastic SSR resolve (Stachowiak, SIGGRAPH 2015): ray reuse.
	// Every neighbour hit point is treated as a sample of this pixel's BRDF:
	// weight = BRDF(V, L, N) * NdotL / pdf, result = sum(Color * weight) / sum(weight).
	// The disk is rotated per pixel, the history below averages the pattern out.
	float Angle = s_blue_noise[uint3(DTid % 128, uint(m_taa_jitter.w) % 32)].x * 6.2831853f;
	float2x2 Rotation = float2x2(cos(Angle), -sin(Angle), sin(Angle), cos(Angle));
	
	float NdotV = max(EPS, dot(O.Normal, -View));

	[unroll]
	for(uint i = 0; i < NUM_SAMPLES; ++i)
	{
		float2 offset = mul(Rotation, Disk32_Normalized[i]) * scaled_screen_res.zw * DISK32_RADIUS;
		offset = mirror(I.texcoord.xy + offset * SampleRadius) * SSLR_TRACE_UV;
		
		float4 SSLR = s_refl.SampleLevel(smp_nofilter, offset, 0);
		
		float4 Color = s_image.SampleLevel(smp_nofilter, offset, 0.0f);
		float3 Light = ReflectPoint - SSLR.xyz;
		
		float Length = length(Light);
		Light *= Length > 0.0f ? rcp(Length) : 0.0f;
		
		float3 Half = normalize(Light + View);

		float NdotH = max(0.0f, dot(O.Normal, -Half));
		float NdotL = max(0.0f, dot(O.Normal, -Light));
		
#ifndef USE_LEGACY_LIGHT
		// Local BRDF for the reused direction, divided by the pdf of the ray that found it
		float D = DistributionGGX(NdotH, O.Roughness);
		float G = GeometrySmithD(NdotL, NdotV, O.Roughness);
		float SampleWeight = max(D * G * NdotL * SSLR.w, 1e-5);
#else
		float SampleWeight = rcp(NdotH + EPS);
#endif
		
		//HUD weight
		SampleWeight *= 1.0f - abs(Color.w - isHUDRender);

		// Inverse luminance weighting against fireflies
		SampleWeight *= rcp(1.0f + dot(Color.xyz, LUMINANCE_VECTOR));

		Moment1 += Color.xyz;
		Moment2 += Color.xyz * Color.xyz;

		Color.w = Length;
		FinalColor += Color * SampleWeight;
		
		FinalWeight += SampleWeight;
	}

	FinalColor *= rcp(FinalWeight);

	Moment1 *= 1.0f / NUM_SAMPLES;
	Moment2 *= 1.0f / NUM_SAMPLES;

	float3 Sigma = sqrt(max(Moment2 - Moment1 * Moment1, 0.0f)) * SSLR_HISTORY_GAMMA;
	float3 BoxMin = Moment1 - Sigma;
	float3 BoxMax = Moment1 + Sigma;

	float4 Current = float4(saturate(FinalColor.xyz), O.Depth);

	// Reflected point along the view ray at the mean hit distance, for the reprojection of mirror-like reflections
	ReflectPoint = View * (FinalColor.w + O.ViewDist);

	float2 PrevDiffuseUV = I.texcoord.xy + s_velocity.SampleLevel(smp_nofilter, I.texcoord.xy, 0).xy * float2(-0.5f, 0.5f);

	float4 History = s_sslr_history.SampleLevel(smp_rtlinear, PrevDiffuseUV.xy, 0.0f);
	History = lerp(Current, History, GetBorderAtten(PrevDiffuseUV));

#ifndef USE_LEGACY_LIGHT
	float4 PrevSpecularUV = mul(m_VP_old, float4(mul(m_invV, float4(ReflectPoint, 1.0f)).xyz, 1.0f));

	PrevSpecularUV.xy = PrevSpecularUV.xy / PrevSpecularUV.w * float2(0.5f, -0.5f) + 0.5f;
	PrevSpecularUV.xy = (O.Roughness > 0.1f || O.Depth < 0.02f) ? PrevDiffuseUV.xy : PrevSpecularUV.xy;

	float3 HistorySpecular = s_sslr_history.SampleLevel(smp_rtlinear, PrevSpecularUV.xy, 0.0f).xyz;
	History.xyz = lerp(History.xyz, HistorySpecular, GetBorderAtten(PrevSpecularUV.xy));
#endif

	History.xyz = lerp(History.xyz, Current.xyz, HistoryClamp(History.xyz, Current.xyz, BoxMin, BoxMax));

	float DepthClamp = 1.0f - saturate(50.0f * abs(History.w - O.Depth));
	Current.xyz = lerp(Current.xyz, History.xyz, DepthClamp * 0.98f);

	u_sslr[DTid.xy] = Current;
}
