#include "common\common.hlsli"
#include "common\scene_model.hlsli"

float4 ps_main(p_bumped_new I) : SV_Target
{
	SCENE_MODEL_MATERIAL(I);
	float3 Color = SCENE_MODEL_BASE(I.tcdh.xy).xyz;

    float noise = get_noise(I.tcdh.xy * timers.z) * m_affects.x * m_affects.x * 30;
    Color.r += noise + 0.1;
    Color.g += noise + 0.1;
    Color.b += noise + 0.1;

    return float4(SceneFromGamma(Color), 1.0f);
}
