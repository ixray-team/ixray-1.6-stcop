Texture2D s_base : register(t0);
Texture2D s_blend_alt : register(t1);
Texture2D s_blend_mask : register(t2);
SamplerState smp_base;

uniform float4 ui_blend_base_rect; // normalized source origin and extent
uniform float4 ui_blend_alt_rect;
uniform float4 ui_blend_mask_rect; // center in pixels, inverse screen size

struct TextureBlendInput
{
    float2 Tex0 : TEXCOORD0;
    float4 Color : COLOR;
    float4 HPos : SV_POSITION;
};

float4 main(TextureBlendInput I) : SV_Target
{
    float2 MaskUV = (I.HPos.xy - ui_blend_mask_rect.xy) * ui_blend_mask_rect.zw + 0.5;
    float Inside = all(MaskUV >= 0.0) && all(MaskUV <= 1.0) ? 1.0 : 0.0;
    float Mask = saturate(s_blend_mask.Sample(smp_base, saturate(MaskUV)).r) * Inside;
    float2 LocalUV = (I.Tex0 - ui_blend_base_rect.xy) / max(ui_blend_base_rect.zw, 1e-8);
    uint Width, Height;
    s_blend_alt.GetDimensions(Width, Height);
    float2 HalfTexel = 0.5 / max(float2(Width, Height), 1.0);
    float2 AltUV = ui_blend_alt_rect.xy + LocalUV * ui_blend_alt_rect.zw;
    AltUV = clamp(AltUV, ui_blend_alt_rect.xy + HalfTexel,
                  ui_blend_alt_rect.xy + ui_blend_alt_rect.zw - HalfTexel);
    float4 Base = s_base.Sample(smp_base, I.Tex0);
    float4 Alt = s_blend_alt.Sample(smp_base, AltUV);
    // Interpolate premultiplied values, then return straight alpha for normal UI blending.
    float Alpha = lerp(Base.a, Alt.a, Mask);
    float3 RGB = lerp(Base.rgb * Base.a, Alt.rgb * Alt.a, Mask) / max(Alpha, 1e-8);
    return float4(RGB, Alpha) * I.Color;
}
