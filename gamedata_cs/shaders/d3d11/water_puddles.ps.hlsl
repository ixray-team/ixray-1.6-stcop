#include "common.hlsli"
#include "reflections.hlsli"

struct PSInput
{
    float4 hpos	: SV_POSITION;
	float3 world_position : TEXCOORD0;	
};

uniform float3 water_intensity;

Texture2D s_nmap;
TextureCube s_env0;
TextureCube s_env1;

float3 SpecularPhong(float3 Point, float3 Normal, float3 Light)
{
	float3 LightColor = max(0.0f, L_sun_color.xyz * 4.0f - 1.0f);
	return LightColor * pow(dot(normalize(Point + Light), -Normal), 256.0);
}

// Pixel
float4 main(PSInput I) : SV_Target
{
	float2 tcdh = I.world_position.xz * 0.3f;
	float4 base = s_base.Sample(smp_base, tcdh);
	float3 normal = s_nmap.Sample(smp_base, tcdh).xyz * 2.0 - 1.0;

	//Build cotangent frame and transform our normal to world space
	float3x3 TBN = {float3(0.0, 0.0, 0.0), float3(0.0, 0.0, 0.0), float3(0.0, 1.0, 0.0)};
	
	build_contangent_frame(I.world_position.xyz, TBN[2], tcdh, TBN[0], TBN[1]);

    float3 Nw = normalize(mul(TBN, normal));

	float3 envd0 = env_s0.Sample(smp_rtlinear, Nw).xyz;
	float3 envd1 = env_s1.Sample(smp_rtlinear, Nw).xyz;
	
	float3 envd = lerp(envd0, envd1, L_ambient.w) * L_hemi_color.xyz;
	base.xyz *= envd * envd; //Ambient
	
	float3 v2point = normalize(I.world_position - eye_position);
	float3 vreflect = reflect(v2point, Nw);

	float fresnel = saturate(dot(vreflect, v2point));
	float3 WaterPos = mul(m_V, float4(I.world_position, 1.0)).xyz;

#ifdef USE_SSLR_ON_WATER
    float3 WaterPoint = WaterPos.z * float3(I.hpos.xy * pos_decompression_params.zw - pos_decompression_params.xy, 1.0f);
	float3 Reflect = mul((float3x3)m_V, vreflect);
	
    float4 sslr = ScreenSpaceLocalReflections(WaterPoint, Reflect);
	
	#ifdef USE_OFFSCREEN_REFLECTIONS
		float4 vslr = FastViewReflections(WaterPoint, Reflect);
		
		float Fog = saturate(length(vslr.xyz) * fog_params.w + fog_params.x);
		vslr.w *= 1.f - Fog * Fog;
		
		vslr.xyz = s_env.SampleLevel(smp_rtlinear, vslr.xyz, 0.0f);
		vslr.xyz *= rcp(1.00001f - vslr.xyz);
	#endif
#else
	#ifdef USE_OFFSCREEN_REFLECTIONS
		float3 Reflect = mul((float3x3)m_V, vreflect);
		float4 vslr = s_env.SampleLevel(smp_rtlinear, Reflect.xyz, 0.0f);
		vslr.xyz *= rcp(1.00001f - vslr.xyz);
		
		float Fog = saturate(vslr.w * fog_params.w + fog_params.x);
		vslr.w = 1.f - Fog * Fog;
	#endif
#endif

	float2 rotation = 0.0f;
	sincos(L_sky_color.w, rotation.x, rotation.y);
	vreflect.xz = float2(vreflect.x * rotation.y - vreflect.z * rotation.x, vreflect.x * rotation.x + vreflect.z * rotation.y);
	
#ifndef USE_FULL_SKY_SPHERE
	RemapVector(vreflect);
#endif

	float3 env0 = s_env0.Sample(smp_rtlinear, vreflect).xyz;
	float3 env1 = s_env1.Sample(smp_rtlinear, vreflect).xyz;
	
	float3 env = lerp(env0, env1, L_ambient.w) * L_sky_color.xyz;

#ifdef USE_OFFSCREEN_REFLECTIONS
	env.xyz = lerp(env, PopGamma(vslr.xyz), vslr.w);
#endif
	
#ifdef USE_SSLR_ON_WATER
	env = lerp(env, PopGamma(sslr.xyz), sslr.w);
#endif

    float power = pow(fresnel, 5.0f);
	float amount = 0.25f + 0.25f * power;

	float3 final = lerp(env * amount * 0.8f, base.xyz, base.w);
	float alpha = 0.25f + 0.65f * power;
	
	alpha = lerp(alpha, 1.0f, base.w);
	
	// Igor: additional depth test
#ifdef USE_SOFT_WATER
    float4 Point = GbufferGetPoint(I.hpos.xy);
	float waterDepth = length(WaterPos.xyz - Point.xyz) * 0.75f;

	alpha = min(alpha, saturate(waterDepth));
	alpha = max(1.0f - exp(-4.0f * waterDepth), alpha);

	float4 Light = s_accumulator.Sample(smp_nofilter, I.hpos.xy * pos_decompression_params2.zw);
	Light *= 1.0f - base.w;

	final += SpecularPhong(v2point, Nw, L_sun_dir_w.xyz) * Light.w;
#endif
	
	return PushGamma(lerp(float4(final, PopGamma(alpha)), fog_color, calc_fogging(I.world_position)));
}
