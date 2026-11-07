#include "common.hlsli"

struct ui_color_input
{
    float4 position : POSITION;
    float4 color : COLOR0;
};

struct ui_color_output
{
    float4 position : SV_POSITION;
    float4 color : COLOR0;
};

ui_color_output main(ui_color_input input)
{
    ui_color_output output;
    input.position.w = 1.0f;
    output.position = mul(m_WVP, input.position);
    output.color = input.color.bgra;
    return output;
}
