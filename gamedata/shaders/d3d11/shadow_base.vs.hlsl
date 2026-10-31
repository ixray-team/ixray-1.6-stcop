#ifdef EDITOR_MODE
// Editable meshes of the editors use the model vertex format (float position, normal, basis, uv)
#include "shadow_model.vs.hlsl"
#else
#include "common.hlsli"

void main(PULL_INPUT, out p_shadow O)
{
    PulledVertex V = PullVertex(VertexId);
#ifdef USE_AREF
    O.tc0 = V.UV;
#else
    O.tc0 = 0.0f;
#endif
    O.hpos = mul(m_WVP, float4(V.P, 1.0f));
}
#endif
