// The vertex of a skinned model whose alpha follows how it faces the eye: the arrows and bars of the devices, and
// the glass, ghost and anomaly surfaces that push the picture behind them aside. A permutation is a few defines of
// faded.json:
//   FADED_AWAY   - fades where it faces the eye, not where it is seen edge-on
//   FADED_SCALE  - multiplies the fade
//   FADED_JITTER - takes the frame's sub-pixel jitter
// Drawn from the scene, where it stands comes from the model's record (common\scene_model.hlsli)
#include "common\common.hlsli"
#include "common\scene_model.hlsli"

struct faded_vertex {
    float2 uv : TEXCOORD0;
    float4 fade : COLOR0;
    float4 position_proj : SV_POSITION;
    float fog : FOG;
};

faded_vertex vs_main(PULL_INPUT)
{
    faded_vertex output;
    scene_model model = pull_scene_model(VertexId, DrawId);
    float4 position = model.vertex.P;
    output.uv = model.vertex.tc;
    float facing = abs(dot(normalize(scene_model_view(model, position)), normalize(scene_model_view_turn(model, model.vertex.N))));
#ifdef FADED_AWAY
    facing = 1.0f - facing;
#endif
#ifdef FADED_SCALE
    facing *= FADED_SCALE;
#endif
    output.fade = facing;
    output.position_proj = scene_model_project(model, position);
    output.fog = 1.0f - calc_fogging(scene_model_place(model, position));
#ifdef FADED_JITTER
    output.position_proj.xy += m_taa_jitter.xy * output.position_proj.w;
#endif
    return output;
}
