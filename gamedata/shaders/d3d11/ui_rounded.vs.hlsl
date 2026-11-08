#include "common.hlsli"
#include "common_iostructs.hlsli"
cbuffer UIRoundedVertexConstants : register(b6)
{
    float4 ui_round_target;
};
float4 main(v_TL0uv_positiont I) : SV_POSITION
{
    return float4((I.P.x + 0.5) * ui_round_target.z * 2.0 - 1.0,
                  1.0 - (I.P.y + 0.5) * ui_round_target.w * 2.0, 0.0, 1.0);
}
