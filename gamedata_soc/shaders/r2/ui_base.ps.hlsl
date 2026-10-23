#include "common.hlsli"
#include "sload.hlsli"
#include "shared\ui_icon.hlsli"

uniform samplerCUBE env_s0;

// The UI pass may bind G-buffer MRTs, so the icon color goes to every slot.
struct f_ui
{
    float4 C0 : COLOR0;
    float4 C1 : COLOR1;
    float4 C2 : COLOR2;
};

f_ui main(p_bumped_new I)
{
    XrayMaterial M;

    M.Sun = I.tcdh.w;
    M.Hemi = I.tcdh.z;
    M.Point = I.position.xyz;

    SloadNew(I, M);

#ifdef USE_AREF
    clip(M.Color.w - def_aref);
#endif

    M.Normal = normalize(mul(float3x3(I.M1, I.M2, I.M3), M.Normal));

    float Material = xmaterial;
    float Gloss = 1.0f - M.Roughness;

    float3 View = float3(0.0f, 0.0f, 1.0f);
    float3 Light = UIIconLightDirection();
    float3 Half = normalize(Light + View);

    float NdotL = max(0.0f, -dot(M.Normal, Light));
    float NdotH = max(0.0f, -dot(M.Normal, Half));
    float Surface = tex3D(s_material, float3(NdotL, NdotH, Material)).x;
    float3 Direct = ui_icon_light.xyz * Surface * M.Color.xyz;

    float3 Reflect = reflect(View, M.Normal);
    float HdotV = 0.5f - 0.5f * dot(View, Reflect);
    float2 Ambient = tex3D(s_material, float3(1.0f, HdotV, Material)).xy;
    float3 Diffuse = Ambient.x * texCUBE(env_s0, M.Normal).xyz * M.Color.xyz;
    float3 Specular = Ambient.y * texCUBE(env_s0, Reflect).xyz * Gloss;

#ifdef UI_ALPHA_BLEND
    float Alpha = M.Color.w;
#else
    float Alpha = 1.0f;
#endif

    f_ui O;
    O.C0 = UIIconFinalize((Diffuse + Specular) * ui_icon_light.w + Direct, Alpha);
    O.C1 = O.C0;
    O.C2 = O.C0;
    return O;
}
