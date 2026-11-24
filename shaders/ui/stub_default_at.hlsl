#include "common\common.hlsli"
#include "common\common_iostructs.hlsli"

// Screen-space blender with alpha test (editor previews of B_SCREEN_SET)
float4 ps_main(p_TL I) : SV_Target
{
    float4 Color = s_base.Sample(smp_base, I.Tex0) * I.Color;

    // FFP alpha test, D3DCMP_GREATER
    clip(Color.a - m_AlphaRef - 0.5f / 255.0f);

    return Color;
}
