#include "common.hlsli"
#include "reflections.hlsli"

struct vf
{
	float2 tbase : TEXCOORD0;
	float2 tnorm0 : TEXCOORD1;
	float2 tnorm1 : TEXCOORD2;
	float3 M1 : TEXCOORD3;
	float3 M2 : TEXCOORD4;
	float3 M3 : TEXCOORD5;
	float3 v2point : TEXCOORD6;
	float4 tctexgen : TEXCOORD7;
	float3 pos : TEXCOORD8;
	float4 c0 : COLOR0;
	float4 hpos : SV_POSITION;
};

uniform float3 water_intensity;

Texture2D s_nmap;
TextureCube s_env0;
TextureCube s_env1;

Texture2D s_leaves;
Texture2D s_caustic;

float3 SpecularPhong(float3 Point, float3 Normal, float3 Light)
{
	float3 LightColor = max(0.0f, L_sun_color.xyz * 4.0f - 1.0f);
	return LightColor * pow(dot(normalize(Point + Light), -Normal), 256.0);
}

// Pixel
float4 main(vf I, float4 pos2d : SV_POSITION) : SV_Target
{
	float4 base = s_base.Sample(smp_base, I.tbase);
	
	float3 n0 = s_nmap.Sample(smp_base, I.tnorm0).xyz;
	float3 n1 = s_nmap.Sample(smp_base, I.tnorm1).xyz;
	float3 Navg = n0 + n1 - 1.0f;

    float3 Nw = normalize(mul(float3x3(I.M1, I.M2, I.M3), Navg).xyz);
	
	float3 envd0 = env_s0.SampleLevel(smp_rtlinear, Nw, 0).xyz;
	float3 envd1 = env_s1.SampleLevel(smp_rtlinear, Nw, 0).xyz;
	
	float3 envd = lerp(envd0, envd1, L_ambient.w) * L_hemi_color.xyz;
	float3 color = I.c0.xyz + envd * envd * I.c0.w;
	base.xyz *= color;
			
	float3 v2point = normalize(I.v2point);
	float3 vreflect = reflect(v2point, Nw);

	float fresnel = saturate(dot(vreflect, v2point));

#ifdef USE_SSLR_ON_WATER
	float4 sslr = calc_reflections(pos2d.xy, I.tctexgen.z, vreflect);
#endif

	float2 rotation = 0.0f;
	sincos(L_sky_color.w, rotation.x, rotation.y);
	vreflect.xz = float2(vreflect.x * rotation.y - vreflect.z * rotation.x, vreflect.x * rotation.x + vreflect.z * rotation.y);
	
#ifndef USE_FULL_SKY_SPHERE
	RemapVector(vreflect);
#endif

	float3 env0 = s_env0.SampleLevel(smp_rtlinear, vreflect, 0).xyz;
	float3 env1 = s_env1.SampleLevel(smp_rtlinear, vreflect, 0).xyz;
	
	float3 env = lerp(env0, env1, L_ambient.w) * L_sky_color.xyz;

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
    float4 Point = GbufferGetPoint(pos2d.xy);
	
	float3 waterPos = Point.xyz * rcp(Point.z) * I.tctexgen.z;
	float waterDepth = length(waterPos - Point.xyz) * 0.75f;

	//	water fog
	float3 Fc = 0.1f * water_intensity.xxx * color;
	final = lerp(Fc, final, alpha);

	alpha = min(alpha, saturate(waterDepth));
	alpha = max(1.0f - exp(-4.0f * waterDepth), alpha);

	float4 leaves = s_leaves.Sample(smp_base, I.tbase);
	leaves.xyz *= water_intensity.xxx * color;
	leaves.w *= 1.0f - base.w;
	
	float calc_cos = -dot(float3(I.M1.z, I.M2.z, I.M3.z), v2point);
	float calc_depth = saturate(waterDepth * calc_cos);
	
	float fLeavesFactor = smoothstep(0.025f, 0.05f, calc_depth);
	fLeavesFactor *= smoothstep(0.1f, 0.075f, calc_depth);
	float4 Light = s_accumulator.Load(int3(pos2d.xy, 0), 0);
	Light *= 1.0f - base.w;
	
	float2 CausticTexcoord = mul(m_invV, float4(Point.xyz, 1.0f)).xz * 0.45f;
	float3 Caustic = s_caustic.Sample(smp_base, CausticTexcoord).yyy;

	//LV: Subtle chromatic abberation effect
	//https://x.com/XorDev/status/1831738521935360079
	Caustic += ddx(Caustic) * float3(1.25, 0.0, -1.25); 
	Caustic += ddy(Caustic) * float3(1.25, 0.0, -1.25);

	final += SpecularPhong(v2point, Nw, L_sun_dir_w.xyz) * Light.w;
	final += Caustic * Light.xyz * 0.25f;
	
	final = lerp(final, leaves.xyz, leaves.w * fLeavesFactor);
	alpha = max(alpha, leaves.w * fLeavesFactor);
#endif
	
	return PushGamma(lerp(float4(final, PopGamma(alpha)), fog_color, calc_fogging(I.pos.xyz)));
}

