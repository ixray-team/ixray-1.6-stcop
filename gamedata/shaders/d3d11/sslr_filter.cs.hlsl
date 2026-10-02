#include "common.hlsli"
#include "reflections.hlsli"
#include "metalic_roughness_light.hlsli"


#define mirror(x) saturate(1.0 - abs(abs(x) - 1.0))

#define DISK32_RADIUS8 2.443279f
#define DISK32_RADIUS16 1.48565f
#define DISK32_RADIUS32 1.0f

#define NUM_SAMPLES 16
#define FILTER_BATCH 8
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

RWTexture2D<float4> u_sslr_temp : register(u0);

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
		float4 FinalColor = s_image.SampleLevel(smp_nofilter, I.texcoord, 0.0f);
		FinalColor.w = O.ViewDist;
		
		u_sslr_temp[DTid.xy] = FinalColor;
		return;
	}

	float3 ReflectPoint = GbufferGetPointRealUnjitter(I.texcoord.xy, O.Depth);
	float3 View = normalize(ReflectPoint);
	
	float4 FinalColor = 0.0f;
	float FinalWeight = 0.0;
	
	float SampleRadius = 32.0f - 24.0f * GetBorderAtten(I.texcoord, 0.025f);
	uint TapBegin = 0;

#ifndef USE_LEGACY_LIGHT
	// A narrow GGX lobe gives almost every wide tap a near-zero weight, so shrink the kernel with roughness
	SampleRadius *= lerp(0.15f, 1.0f, saturate(O.Roughness * 2.0f));
	// The second half of the disk contains the centre tap, keep that one
	TapBegin = O.Roughness < 0.1f ? FILTER_BATCH : 0;
#endif

	// Each batch issues all of its fetches before any of them is consumed
	[loop]
	for(uint b = TapBegin; b < NUM_SAMPLES; b += FILTER_BATCH)
	{
		float4 SSLRs[FILTER_BATCH];
		float4 Colors[FILTER_BATCH];

		[unroll]
		for(uint f = 0; f < FILTER_BATCH; ++f)
		{
			float2 offset = Disk32_Normalized[b + f] * scaled_screen_res.zw * DISK32_RADIUS;
			offset = mirror(I.texcoord.xy + offset * SampleRadius);

			SSLRs[f] = s_refl.SampleLevel(smp_nofilter, offset, 0);
			Colors[f] = s_image.SampleLevel(smp_nofilter, offset, 0.0f);
		}

		[unroll]
		for(uint e = 0; e < FILTER_BATCH; ++e)
		{
			float4 SSLR = SSLRs[e];
			float4 Color = Colors[e];
			
			Color.w = SSLR.w < 0.0f ? 1.0f : 0.0f;
			SSLR.w = abs(SSLR.w);

			float3 Light = ReflectPoint - SSLR.xyz;

			float Length = length(Light);

#ifndef USE_LEGACY_LIGHT
			//LVutner: it just works.
			float SampleWeight = 1e-5;

			if(SSLR.w > 0.0f)
			{
				Light *= Length > 0.0f ? rcp(Length) : 0.0f;

				float3 Half = normalize(Light + View);
				float NdotH = max(0.0f, dot(O.Normal, -Half));

				float D = DistributionGGX(NdotH, O.Roughness);
				SampleWeight = max(D * NdotH * SSLR.w, 1e-5);
			}
#else
			Light *= Length > 0.0f ? rcp(Length) : 0.0f;

			float3 Half = normalize(Light + View);
			float NdotH = max(0.0f, dot(O.Normal, -Half));

			float SampleWeight = rcp(NdotH + EPS);
#endif

			//HUD weight
			SampleWeight *= 1.0f - abs(Color.w - isHUDRender);

			Color.w = Length;
			FinalColor += Color * SampleWeight;

			FinalWeight += SampleWeight;
		}
	}

	FinalColor *= rcp(FinalWeight);
	FinalColor.xyz = saturate(FinalColor.xyz);

	FinalColor.w += O.ViewDist;

	u_sslr_temp[DTid.xy] = FinalColor;
}