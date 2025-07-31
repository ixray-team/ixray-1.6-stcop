#include "common.hlsli"

struct ui_vert_in
{
    float4 P : POSITION;
    float4 color : COLOR0;
    float2 uv : TEXCOORD0;
};

struct ui_vert_out
{
    float2 tc0 : TEXCOORD0;
    float4 P : SV_POSITION;
};

ui_vert_out main(ui_vert_in v)
{
    ui_vert_out O;

    O.tc0 = v.uv;
    O.P = v.P;
    O.P.w = 1;
    O.P = mul(m_WVP, O.P);

    O.P.xy += m_taa_jitter.xy * O.P.w;
    return O;
}
