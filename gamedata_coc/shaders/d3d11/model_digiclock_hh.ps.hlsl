#include "common.hlsli"
#include "sload.hlsli"

uniform float4 m_digiclock;
uniform float4 m_affects;

float4 main(p_bumped_new I) : SV_Target
{
    // === Выбор цифры из атласа по m_digiclock.a ===
    // m_digiclock.a — индекс цифры (0..9), сдвигаем UV.x
    float digit = m_digiclock.x;
    
    float2 coords = I.tcdh.xy;
    coords.x = digit + (coords.x * 0.1);  // 0.1 — если в атласе 10 цифр
    
    float3 Color = s_base.Sample(smp_base, coords).xyz;
    
    // === Шум выброса ===
    float noise = get_noise(I.tcdh.xy * timers.z) * m_affects.x * m_affects.x * 30;
    Color.r += noise + 0.1;
    Color.g += noise + 0.1;
    Color.b += noise + 0.1;
    
    return float4(GammaToLinear(Color), 0.0f);
}
