#include "common\common.hlsli"
#include "common\sload.hlsli"
#include "common\scene_model.hlsli"

float4 ps_main(p_bumped_new I) : SV_Target
{
    SCENE_MODEL_MATERIAL(I);
    float digit = m_digiclock.z;
    
    float2 coords = I.tcdh.xy;
    coords.x = digit + (coords.x * 0.1);  // 0.1 — если в атласе 10 цифр
    
    float3 Color = SCENE_MODEL_BASE(coords).xyz;
    
    float noise = get_noise(I.tcdh.xy * timers.z) * m_affects.x * m_affects.x * 30;
    Color.r += noise + 0.1;
    Color.g += noise + 0.1;
    Color.b += noise + 0.1;
    
    return float4(SceneFromGamma(Color), 0.0f);
}
