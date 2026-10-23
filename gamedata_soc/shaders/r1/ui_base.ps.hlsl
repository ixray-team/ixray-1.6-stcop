#include "common.hlsli"
#include "shared\ui_icon.hlsli"

uniform samplerCUBE env_s0;

struct v2p
{
    float2 tc0 : TEXCOORD0; // base
    float3 normal : TEXCOORD1; // view-space normal
};

float4 main(v2p I) : COLOR
{
    float4 Base = tex2D(s_base, I.tc0);
    float3 Normal = normalize(I.normal);

    float3 View = float3(0.0f, 0.0f, 1.0f);
    float3 Light = UIIconLightDirection();
    float3 Reflect = reflect(View, Normal);

#ifdef UI_ENV_MAP
    // Environment-mapped models blend the reflection by the base alpha.
    Base.xyz = lerp(texCUBE(s_env, Reflect).xyz, Base.xyz, Base.w);
#endif

    float NdotL = max(0.0f, -dot(Normal, Light));
    float3 Direct = ui_icon_light.xyz * NdotL * Base.xyz;
    float3 Diffuse = texCUBE(env_s0, Normal).xyz * Base.xyz;

    return UIIconFinalize(Diffuse * ui_icon_light.w + Direct, Base.w);
}
