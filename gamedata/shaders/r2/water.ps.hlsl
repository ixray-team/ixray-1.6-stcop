#include "common.hlsli"

struct vf
{
    float4 hpos : POSITION;
    float2 tbase : TEXCOORD0; // base
    float2 tnorm0 : TEXCOORD1; // nm0
    float2 tnorm1 : TEXCOORD2; // nm1
    float3 M1 : TEXCOORD3;
    float3 M2 : TEXCOORD4;
    float3 M3 : TEXCOORD5;
    float3 v2point : TEXCOORD6;
    float4 tctexgen : TEXCOORD7;
    float4 c0 : COLOR0;
    float fog : FOG;
};

uniform sampler2D s_nmap;
uniform samplerCUBE s_env0;
uniform samplerCUBE s_env1;
uniform sampler2D s_leaves;
uniform float4 pos_decompression_params;

#if defined(USE_SOFT_WATER) && defined(NEED_SOFT_WATER)
float3 water_intensity;
#endif //	defined(USE_SOFT_WATER) && defined(NEED_SOFT_WATER)

float4 main(vf I) : COLOR
{
    float4 base = tex2D(s_base, I.tbase);
    float3 n0 = tex2D(s_nmap, I.tnorm0);
    float3 n1 = tex2D(s_nmap, I.tnorm1);
    float3 Navg = n0 + n1 - 1.0f;

    float3 Nw = mul(float3x3(I.M1, I.M2, I.M3), Navg);
    Nw = normalize(Nw);
    float3 v2point = normalize(I.v2point);
    float3 vreflect = reflect(v2point, Nw);

    float fresnel = saturate(dot(vreflect, v2point));

    //	true remapping. Slow.
    float3 vreflectabs = abs(vreflect);
    float vreflectmax = max(vreflectabs.x, max(vreflectabs.y, vreflectabs.z));
    vreflect /= vreflectmax;
    vreflect.y = vreflect.y * 2 - 1;

    float3 env0 = texCUBE(s_env0, vreflect);
    float3 env1 = texCUBE(s_env1, vreflect);
    float3 env = lerp(env0, env1, L_ambient.w);
    env *= env * L_sky_color.xyz;

    float power = pow(fresnel, 9.0f);
    float amount = 0.25f + 0.55f * power; // 1=full env, 0=no env

    float3 c_reflection = env * amount;
    float3 final = lerp(c_reflection, base.rgb, base.a);

#ifdef NEED_SOFT_WATER

    float alpha = 0.75f + 0.25f * power; // 1=full env, 0=no env

    #ifdef USE_SOFT_WATER
    //	Igor: additional depth test
    float4 _P = tex2Dproj(s_position, I.tctexgen);

    float2 PosTc = I.tctexgen.xy / I.tctexgen.w;
    float3 waterPos = float3((PosTc * 2 - 1) * pos_decompression_params.xy, 1) * I.tctexgen.z;
    float waterDepth = length(waterPos - _P) * 0.75f;

    //	water fog
    float fog_exp_intens = -4.0f;
    float fog = 1 - exp(fog_exp_intens * waterDepth);
    float3 Fc = float3(0.1f, 0.1f, 0.1f) * water_intensity.r;
	
    final = lerp(Fc, final, alpha);

    alpha = min(alpha, saturate(waterDepth));

    alpha = max(fog, alpha);

    //	Leaves
    float4 leaves = tex2D(s_leaves, I.tbase);
    leaves.rgb *= water_intensity.r;
    float calc_cos = -dot(float3(I.M1.z, I.M2.z, I.M3.z), normalize(v2point));
    float calc_depth = saturate(waterDepth * calc_cos);
    float fLeavesFactor = smoothstep(0.025f, 0.05f, calc_depth);
    fLeavesFactor *= smoothstep(0.1f, 0.075f, calc_depth);
    final = lerp(final, leaves, leaves.a * fLeavesFactor);
    alpha = lerp(alpha, leaves.a, leaves.a * fLeavesFactor);

    #endif //	USE_SOFT_WATER
    final *= I.c0;

    //	Fogging
    final = lerp(fog_color, final, I.fog);
    alpha *= I.fog * I.fog;

    return float4(final, alpha);

#else //	NEED_SOFT_WATER
    final *= I.c0;

    //	Fogging
    final = lerp(fog_color, final, I.fog);

    return float4(final, I.fog * I.fog);

#endif //	NEED_SOFT_WATER
}

