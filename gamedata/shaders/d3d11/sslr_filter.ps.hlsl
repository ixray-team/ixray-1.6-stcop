#include "common.hlsli"
#include "reflections.hlsli"
#include "metalic_roughness_light.hlsli"

#define mirror(x) saturate(1.0 - abs(abs(x) - 1.0))

#define DISK32_RADIUS8 2.443279f
#define DISK32_RADIUS16 1.48565f
#define DISK32_RADIUS32 1.0f

#define NUM_SAMPLES 32
#define DISK32_RADIUS DISK32_RADIUS32

static const float2 Disk32_Normalized[32] = {
	float2(-0.50000f, 0.100000f),
	float2(0.408569f, 0.024217f),
	float2(0.162925f, 0.230704f),
	float2(-0.108248f, 0.367911f),
	float2(-0.329684f, 0.150003f),
	float2(-0.223398f, -0.167128f),
	float2(-0.067794f, -0.356288f),
	float2(0.136270f, -0.214864f),

	float2(0.597250f, 0.006447f),
	float2(0.464972f, 0.455376f),
	float2(0.054674f, 0.571788f),
	float2(-0.423541f, 0.423589f),
	float2(-0.657243f, -0.046063f),
	float2(-0.484844f, -0.466902f),
	float2(0.019780f, -0.556973f),
	float2(0.512536f, -0.384894f),

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

float4 main(PSInputFullscreen I) : SV_Target
{
    IXrayGbuffer O;
    GbufferUnpack(I.texcoord.xy, I.hpos.xy, O);

	float4 SSLR = s_refl.SampleLevel(smp_nofilter, I.texcoord.xy, 0);
	float4 BaseColor = s_image.SampleLevel(smp_nofilter, I.texcoord.xy, 0.0f);
	
	if(O.Depth >= 1.0f)
	{		
		return 0.0f;
	}
		
	float3 ReflectPoint = GbufferGetPointRealUnjitter(I.texcoord.xy, O.Depth);
	float3 View = normalize(ReflectPoint);
	
	float4 FinalColor = BaseColor.xyzz;
 	FinalColor.w = length(ReflectPoint - SSLR.xyz);
	
	float FinalWeight = 1;
    float NdotV = max(0.0f, -dot(O.Normal, View));

	[unroll(NUM_SAMPLES)]
	for(uint i = 0; i < NUM_SAMPLES; ++i)
	{
		float2 offset = Disk32_Normalized[i] * scaled_screen_res.zw * DISK32_RADIUS;
		offset = mirror(I.texcoord.xy + offset * 16.0f);
		
		SSLR = s_refl.SampleLevel(smp_nofilter, offset, 0);
		
		float4 Color = s_image.SampleLevel(smp_nofilter, offset, 0.0f);
		float3 Light = ReflectPoint - SSLR.xyz;
		
		float Length = length(Light);
		Light *= rcp(max(EPS_S, Length));
		
		float3 Half = normalize(Light + View);

		float NdotL = max(0.0f, -dot(O.Normal, Light));
		float NdotH = max(0.0f, -dot(O.Normal, Half));
		
		float D = DistributionGGX(NdotH, O.Roughness);
		float G = NdotL * GeometrySmithD(NdotL, NdotV, O.Roughness);
		
		float SampleWeight = D * G * SSLR.w;
		SampleWeight *= 1.0f - abs(Color.w - BaseColor.w);

		Color.w = Length;
		FinalColor += Color * SampleWeight;
		
		FinalWeight += SampleWeight;
	}

	FinalColor *= rcp(FinalWeight);
	FinalColor.xyz = saturate(FinalColor.xyz);
	
	FinalColor.w += O.ViewDist;
	
	return FinalColor;
}

