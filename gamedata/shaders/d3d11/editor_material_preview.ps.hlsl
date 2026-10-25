#include "common.hlsli"

// Material preview of the Thumbnail View. Decodes the surface like SloadNew (sload.hlsli)
// and lights it with one directional light over a sky/ground ambient.
float4 preview_flags; // x = bump, y = steep parallax, z = diffuse detail, w = detail bump
float4 preview_material; // x = detail scale, y = material (type + weight), z = PBR, w = alpha test
float4 preview_light; // xyz = direction to the light, w = ambient
float4 preview_eye; // xyz = camera position, w = UV tiling

struct v2p_material_preview
{
    float4 hpos : SV_POSITION;
    float2 tc : TEXCOORD0;
    float3 wpos : TEXCOORD1;
    float3 N : TEXCOORD2;
    float3 T : TEXCOORD3;
    float3 B : TEXCOORD4;
};

static const float PreviewPI = 3.14159265f;

float2 PreviewParallax(float2 tc, float3 ViewTS, bool IsPBR)
{
    const float Height = IsPBR ? 0.02f : 0.01f;
    const uint Channel = IsPBR ? 0 : 3;

    float h = IsPBR ? s_bump.Sample(smp_base, tc)[Channel] : s_bumpX.Sample(smp_base, tc)[Channel];
    float2 Delta = ViewTS.xy * Height / max(0.05f, abs(ViewTS.z));

    if (IsPBR)
    {
        tc -= Delta * (1.0f - h);
    }
    else
    {
        tc += Delta * (h - 0.5f);
    }

    const float NumLayers = lerp(20.0f, 8.0f, ViewTS.z * ViewTS.z);
    const float LayerDepth = rcp(NumLayers);
    Delta *= LayerDepth;

    float2 Curr = tc;
    float CurrDepth = 1.0f - h;
    float CurrLayer = IsPBR ? 0.0f : 0.5f;

    [loop] for (uint i = 0; i < 32 && CurrLayer < CurrDepth; ++i)
    {
        CurrLayer += LayerDepth;
        Curr -= Delta;
        float4 Sample = IsPBR ? s_bump.SampleLevel(smp_linear, Curr, 0.0f) : s_bumpX.SampleLevel(smp_linear, Curr, 0.0f);
        CurrDepth = 1.0f - Sample[Channel];
    }

    float2 Prev = Curr + Delta;
    float After = CurrDepth - CurrLayer;
    float4 PrevSample = IsPBR ? s_bump.Sample(smp_base, Prev) : s_bumpX.Sample(smp_base, Prev);
    float Before = (1.0f - PrevSample[Channel]) - CurrLayer + LayerDepth;
    float Weight = After * rcp(After - Before);
    return lerp(Curr, Prev, saturate(Weight));
}

float4 main(v2p_material_preview I) : SV_Target
{
    const bool IsPBR = preview_material.z > 0.5f;

    float3 Nw = normalize(I.N);
    float3 Tw = normalize(I.T);
    float3 Bw = normalize(I.B);
    float3 V = normalize(preview_eye.xyz - I.wpos);

    float2 tc = I.tc * preview_eye.w;
    if (preview_flags.x > 0.5f && preview_flags.y > 0.5f)
    {
        float3 ViewTS = normalize(float3(dot(V, Tw), dot(V, Bw), dot(V, Nw)));
        tc = PreviewParallax(tc, ViewTS, IsPBR);
    }

    float4 Color = s_base.Sample(smp_base, tc);
    if (preview_material.w > 0.5f)
    {
        clip(Color.a - 0.5f);
    }

    float3 Normal = float3(0.0f, 0.0f, 1.0f);
    float Specular = def_gloss;
    float Metalness = 0.0f;
    float Roughness = 0.5f;
    float AO = 1.0f;

    if (preview_flags.x > 0.5f)
    {
        float4 Bump = s_bump.Sample(smp_base, tc);
        float4 BumpX = s_bumpX.Sample(smp_base, tc);

        if (IsPBR)
        {
            Normal.xy = Bump.wy * 2.0f - 1.0f;
            Normal.z = sqrt(1.0f - saturate(dot(Normal.xy, Normal.xy)));
            Metalness = BumpX.x;
            Roughness = BumpX.y;
            AO = BumpX.w;
        }
        else
        {
            Normal = Bump.wzy + BumpX.xyz - 1.0f;
            Specular = Bump.x;
        }
    }

    if (preview_flags.z > 0.5f)
    {
        float2 tcd = tc * preview_material.x;
        float4 Detail = s_detail.Sample(smp_base, tcd);
        Color.xyz *= Detail.xyz * 2.0f;

        if (preview_flags.w > 0.5f)
        {
            float4 DetailBump = s_detailBump.Sample(smp_base, tcd);
            float4 DetailBumpX = s_detailBumpX.Sample(smp_base, tcd);

            if (IsPBR)
            {
                float3 DetailNormal = DetailBump.wyy * 2.0f - 1.0f;
                DetailNormal.z = sqrt(saturate(1.0f - dot(DetailNormal.xy, DetailNormal.xy)));
                Normal += DetailNormal;
                Metalness *= DetailBumpX.x * 2.0f;
                Roughness *= DetailBumpX.y * 2.0f;
                AO *= DetailBumpX.w;
            }
            else
            {
                Normal += DetailBump.wzy + DetailBumpX.xyz - 1.0f;
                Specular *= Detail.w * 2.0f;
            }
        }
        else
        {
            Specular *= Detail.w * 2.0f;
        }
    }

    if (IsPBR)
    {
        Normal.y *= -1.0f;
        Roughness = max(0.02f, Roughness);
    }
    else
    {
        Roughness = preview_material.y * 0.25f + 0.15f;
        Specular = Specular * Specular;
        Normal.z *= 0.5f;
        Normal.z = abs(Normal.z);
    }

    float3 N = normalize(Tw * Normal.x + Bw * Normal.y + Nw * Normal.z);
    float3 L = normalize(preview_light.xyz);
    float3 H = normalize(L + V);

    float NdotL = saturate(dot(N, L));
    float NdotV = max(dot(N, V), 1e-3f);
    float NdotH = saturate(dot(N, H));
    float VdotH = saturate(dot(V, H));

    float3 Albedo = Color.xyz;
    float3 F0 = IsPBR ? lerp(0.04f, Albedo, saturate(Metalness)) : max(Specular, 0.02f).xxx;
    float3 Diffuse = Albedo * (IsPBR ? 1.0f - saturate(Metalness) : 1.0f);

    float a = saturate(Roughness) * saturate(Roughness);
    float a2 = max(a * a, 1e-4f);
    float d = NdotH * NdotH * (a2 - 1.0f) + 1.0f;
    float D = a2 / (PreviewPI * d * d);
    float k = a * 0.5f;
    float Vis = 0.25f / ((NdotL * (1.0f - k) + k) * (NdotV * (1.0f - k) + k));
    float3 F = F0 + (1.0f - F0) * pow(1.0f - VdotH, 5.0f);

    float3 Direct = (Diffuse / PreviewPI + D * Vis * F) * NdotL * PreviewPI;
    float3 Sky = lerp(float3(0.35f, 0.33f, 0.3f), float3(0.55f, 0.65f, 0.8f), N.y * 0.5f + 0.5f);
    float3 Ambient = Sky * preview_light.w * (Diffuse + F0 * 0.25f) * AO;

    return float4(Direct + Ambient, 1.0f);
}
