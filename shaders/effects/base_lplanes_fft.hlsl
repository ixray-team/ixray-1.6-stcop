#include "common\common.hlsli"

// returns the mark of a collimator sight, there only while the sight is raised
float4 ps_main(p_bumped_new input) : SV_Target
{
    float4 color = s_base.Sample(smp_base, input.tcdh.xy);
    return m_hud_params.w == 1.0f ? float4(color.rgb * color.a, 0.0f) : 0.0f;
}
