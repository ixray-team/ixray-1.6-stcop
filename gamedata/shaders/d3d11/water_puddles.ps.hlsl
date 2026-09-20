#include "common.hlsli"
#include "reflections.hlsli"
#include "shadow.hlsli"
#include "water_sky_ibl.hlsli"

struct PSInput
{
    float4 hpos	: SV_POSITION;
	float3 world_position : TEXCOORD0;	
};

uniform float3 water_intensity;

Texture2D s_nmap;

float3 SpecularPhong(float3 Point, float3 Normal, float3 Light)
{
	float3 LightColor = max(0.0f, L_sun_color.xyz * 4.0f - 1.0f);
	return LightColor * pow(saturate(dot(normalize(Point + Light), -Normal)), 256.0);
}

// Pixel
float4 main(PSInput I) : SV_Target
{
	float2 tcdh = I.world_position.xz * 0.3f;
	float4 base = s_base.Sample(smp_base, tcdh);
	base.xyz = GammaToLinear(base.xyz);
	float3 normal = s_nmap.Sample(smp_base, tcdh).xyz * 2.0 - 1.0;

	//Build cotangent frame and transform our normal to world space
	float3x3 TBN = {float3(0.0, 0.0, 0.0), float3(0.0, 0.0, 0.0), float3(0.0, 1.0, 0.0)};
	
	build_contangent_frame(I.world_position.xyz, TBN[2], tcdh, TBN[0], TBN[1]);

    float3 Nw = normalize(mul(TBN, normal));

	base.xyz *= water_sky_diffuse(Nw);
	
	float3 v2point = normalize(I.world_position - eye_position);
	float3 vreflect = reflect(v2point, Nw);

	float fresnel = saturate(dot(vreflect, v2point));
	float3 WaterPos = mul(m_V, float4(I.world_position, 1.0)).xyz;

#ifdef USE_SSLR_ON_WATER
	float3 Reflect = mul((float3x3)m_V, vreflect);
	float3 ReflectPoint = WaterPos * 0.99f + Reflect * 0.025f;
	
    float4 sslr = ScreenSpaceLocalReflections(ReflectPoint, Reflect);
	
	#ifdef USE_OFFSCREEN_REFLECTIONS
		float4 vslr = FastViewReflections(ReflectPoint, Reflect);
		
		float Fog = saturate(length(vslr.xyz) * fog_params.w + fog_params.x);
		vslr.w *= 1.f - Fog * Fog;
		
		vslr.xyz = s_env.SampleLevel(smp_rtlinear, vslr.xyz, 0.0f);
	#endif
#endif


	float3 env = water_sky_reflection(vreflect);

#ifdef USE_SSLR_ON_WATER
	#ifdef USE_OFFSCREEN_REFLECTIONS
		env.xyz = lerp(env, vslr.xyz, vslr.w);
	#endif
	
	env = lerp(env, sslr.xyz, sslr.w);
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

	float Shadow = 1.0f;
	
#ifndef USE_R2_STATIC_SUN
	int cascade_index;
	float3 smap_texcoord;
	
	bool is_in_bounds = calc_cascades(I.world_position.xyz, m_shadow_sun, cascade_index, smap_texcoord);
	
	if(is_in_bounds) 
	{
		Shadow = pcf_3x3(s_smap_sun, smp_smap, smap_texcoord, float2(SMAP_size, 1.0 / SMAP_size), 0.0, cascade_index);
	}

	if(cascade_index >= 2)
	{
		float3 Factor = smoothstep(0.499f, 0.498f, abs(smap_texcoord - 0.5f));
		float Fade = Factor.x * Factor.y * Factor.z;
		
		Shadow = lerp(1.0f, Shadow, Fade);
	}
#endif
	
	final += SpecularPhong(v2point, Nw, L_sun_dir_w.xyz) * water_sun_transmittance() * Shadow;
#endif
	
	return lerp(float4(final, alpha), float4(GammaToLinear(fog_color.rgb), fog_color.a), calc_fogging(I.world_position));
}

