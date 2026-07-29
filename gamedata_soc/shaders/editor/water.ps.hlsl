#include "common.hlsli"

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
};

uniform float3 water_intensity;

uniform sampler s_image;
uniform sampler s_nmap;
uniform sampler s_env0;
uniform sampler s_env1;
uniform sampler env_s0;
uniform sampler env_s1;
uniform sampler s_leaves;
uniform sampler s_caustic;
uniform float4 screen_res;

float3 SpecularPhong(float3 Point, float3 Normal, float3 Light)
{
	float3 LightColor = max(0.0f, L_sun_color.xyz * 4.0f - 1.0f);
	return LightColor * pow(dot(normalize(Point + Light), -Normal), 256.0);
}

float GetBorderAtten(float2 tc, float2 att)
{
    att.x *= screen_res.y * screen_res.z;
    float2 factors = saturate(min(1.0f - tc, tc) / att);
    return factors.x * factors.y;
}

float4 ScreenSpaceLocalReflections(float3 vreflect) //Initial beam length
{
	float3 refl_vec = mul((float3x3)m_V, vreflect);
	float4 new_pos_proj = mul(m_P,float4(refl_vec,1));//переводим её в скрин спейс
	new_pos_proj.xy /= new_pos_proj.w; //нормализуем
	new_pos_proj.xy = new_pos_proj.xy * float2(0.5h, -0.5h) + 0.5h;
	
	float2 reflPos = saturate(new_pos_proj); // We get the pixel position in Screen Space of the reflected geometry
	
	float canrefl = smoothstep(0.0f, 0.05f, refl_vec.z);
	float borderAtten = GetBorderAtten(reflPos, 0.125f); // Grease the edges of the reflections for a smoother transition
	
	float3 color = tex2D(s_image, reflPos);
	return float4(color, borderAtten * canrefl);
}

// Pixel
float4 main(vf I) : COLOR
{
	float4 base = tex2D(s_base, I.tbase);
	
	float3 n0 = tex2D(s_nmap, I.tnorm0);
	float3 n1 = tex2D(s_nmap, I.tnorm1);
	float3 Navg = n0 + n1 - 1.0f;

    float3 Nw = normalize(mul(float3x3(I.M1, I.M2, I.M3), Navg).xyz);
			
	float3 v2point = normalize(I.v2point);
	float3 vreflect = reflect(v2point, Nw);

	float fresnel = saturate(dot(vreflect, v2point));

    float power = pow(fresnel, 5.0f);
	float amount = 0.25f + 0.25f * power;

	float3 reflection = vreflect;

	float2 rotation = 0.0f;
	sincos(L_sky_color.w, rotation.x, rotation.y);
	vreflect.xz = float2(vreflect.x * rotation.y - vreflect.z * rotation.x, vreflect.x * rotation.x + vreflect.z * rotation.y);
	
	// true remapping. Slow.
	float3 vreflectabs = abs(vreflect);
	float vreflectmax = max(vreflectabs.x, max(vreflectabs.y, vreflectabs.z));
	
	vreflect /= vreflectmax;
	vreflect.y = vreflect.y * 2.0f - 1.0f;
	
	if(is_lighting_enable.x < 0.5f) {
		float3 env = texCUBE(s_env, vreflect) * amount * 0.8f;	
		
		env = lerp(env.xyz, base.xyz, base.w);
		return float4(env, 0.8f + 0.2f * base.w);
	}
	
	float3 envd0 = texCUBE(env_s0, Nw);
	float3 envd1 = texCUBE(env_s1, Nw);
	
	float3 envd = lerp(envd0, envd1, L_ambient.w) * L_hemi_color.xyz;
	float3 color = I.c0.xyz + envd * envd * I.c0.w;
	base.xyz *= color;

	float3 env0 = texCUBE(s_env0, vreflect);
	float3 env1 = texCUBE(s_env1, vreflect);
	
	float3 env = lerp(env0, env1, L_ambient.w) * L_sky_color.xyz;
	
	float4 sslr = ScreenSpaceLocalReflections(reflection);
	env = lerp(env, sslr.xyz, sslr.w);
	
	float3 final = lerp(env * amount * 0.8f, base.xyz, base.w);
	
	float alpha = 0.25f + 0.65f * power;
	alpha = lerp(alpha, 1.0f, base.w);
	float waterDepth = 0.75f;

	//	water fog
	float3 Fc = 0.1f * water_intensity.xxx * color;
	final = lerp(Fc, final, alpha);

	alpha = min(alpha, saturate(waterDepth));
	alpha = max(1.0f - exp(-4.0f * waterDepth), alpha);
	
	float4 Light = float4(L_sun_color.xyz * 0.66f, 1.0f);
	Light *= 1.0f - base.w;
	
	float2 CausticTexcoord = I.pos.xz * 0.45f + v2point.xz / (0.0001f + abs(v2point.y)) * 0.3f;
	float3 Caustic = tex2D(s_caustic, CausticTexcoord).yyy;
	
	final += SpecularPhong(v2point, Nw, L_sun_dir_w.xyz) * Light.w;
	final += Caustic * Light.xyz * 0.25f;
	
	return lerp(float4(final, alpha), fog_color, calc_fogging(I.pos));
}

