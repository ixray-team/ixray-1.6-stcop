#include "r1_water.hlsli"

Texture2D s_nmap;
TextureCube s_env0;
TextureCube s_env1;

float4 main(vf_water I) : SV_Target
{
    float4 base = s_base.Sample(smp_base, I.tbase);
    float3 n0 = s_nmap.Sample(smp_base, I.tnorm0).xyz;
    float3 n1 = s_nmap.Sample(smp_base, I.tnorm1).xyz;
    float3 Nw = normalize(mul(float3x3(I.M1, I.M2, I.M3), n0 + n1 - 1.0f));

    float3 v2point = normalize(I.v2point);
    float3 vreflect = reflect(v2point, Nw);
    float fresnel = saturate(dot(vreflect, v2point));
    float3 va = abs(vreflect);
    vreflect /= max(va.x, max(va.y, va.z));
    vreflect.y = vreflect.y * 2.0f - 1.0f;

    float3 env = lerp(s_env0.Sample(smp_rtlinear, vreflect).rgb, s_env1.Sample(smp_rtlinear, vreflect).rgb, L_ambient.w);
    float power = pow(fresnel, 9);
    float3 final = lerp(env * (0.55f + 0.25f * power), base.rgb, base.a) * I.c0.rgb * 2.0f;

    float fog = 1.0f - I.c0.w;
    return float4(lerp(final, fog_color.xyz, fog), (0.75f + 0.25f * power) * (1.0f - fog * fog));
}
