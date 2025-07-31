#include "common.hlsli"

struct v2p
{
    float4 tc0: TEXCOORD0;    // base
    float4 tc1: TEXCOORD1;    // environment
    float4 c0:  COLOR0;       // sun.(fog*fog)
};

//////////////////////////////////////////////////////////////////////////////////////////

// Pixel
uniform sampler2D s_vp2;
uniform sampler2D s_base2;

float4 main (v2p I) : COLOR
{
    // Производим выборку правой и левой половин текстуры с сеткой
    float2 coords = I.tc0;
    coords.x *= 0.5;
    float4 base2 = tex2D(s_base2, coords);
    coords.x += 0.5;
    float4 t_base = tex2D(s_base, coords);
    // Миксуем половинки в соответствии с текущим уровнем подсветки
    t_base = lerp(base2, t_base, m_zoom_deviation.z);

    // Коррекция пропорций в зависимости от разрешения
    I.tc0.x = (I.tc0.x - 0.5f) * m_hud_params.x + 0.5f + m_zoom_deviation.x;
    I.tc0.y = I.tc0.y + m_zoom_deviation.y;
    float4 t_vp2 = tex2D(s_vp2, I.tc0);


    float4 final = lerp(t_vp2, t_base, t_base.a);
    // out
    return float4(final.r, final.g, final.b, min(m_hud_params.y, m_hud_params.a));
}
