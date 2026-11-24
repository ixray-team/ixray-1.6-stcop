// The depth pass of the shadow maps. What the vertex is, by a define of shadow.json:
//   SHADOW_TREE  - placed by m_xform, sways with the wind under USE_TREEWAVE
//   SHADOW_MODEL - a skinned model; the editors draw their meshes this way too
//   neither      - level geometry
#include "common\common.hlsli"
#include "common\skin.hlsli"
#include "editor\editor_instancing.hlsli"

#if defined(EDITOR_MODE) && !defined(SHADOW_TREE)
#define SHADOW_MODEL
#endif

p_shadow vs_main(PULL_INPUT)
{
    p_shadow output;
#if defined(SHADOW_MODEL)
    v_model vertex = PullModel(VertexId);
    output.hpos = mul(object_get_xforms(DrawId).WVP, vertex.P);
    output.tc0 = vertex.tc;
#elif defined(SHADOW_TREE)
    PulledVertex vertex = PullVertex(VertexId);
    float4 position = float4(mul(m_xform, float4(vertex.P, 1.0f)).xyz, 1.0f);
#ifdef USE_TREEWAVE
    float base_height = m_xform._24;
    float cycle = calc_cyclic(wave.w + dot(position.xyz, wave.xyz));
    float sway = (position.y - base_height) * cycle;
    float stiffness = DecodeS16x2(vertex.X).x * consts.x;
    position.xz += calc_xz_wave(wind.xz * sway, stiffness);
#endif
    output.hpos = mul(m_VP, position);
    output.tc0 = vertex.UV;
#else
    PulledVertex vertex = PullVertex(VertexId);
#ifdef USE_AREF
    output.tc0 = vertex.UV;
#else
    output.tc0 = 0.0f;
#endif
    output.hpos = mul(m_WVP, float4(vertex.P, 1.0f));
#endif
    return output;
}

// returns nothing a depth pass keeps; under USE_TRANSPARENT, what the surface lets through of the light
float4 ps_main(p_shadow input) : SV_Target
{
#ifdef USE_AREF
    clip(s_base.Sample(smp_linear, input.tc0).w - def_aref);
#elif USE_TRANSPARENT
    float4 color = s_base.Sample(smp_linear, input.tc0);
    color.xyz = 0.2f + color.xyz * 0.8f;
    color *= 1.0f - color.w;
    return color;
#endif
    return 0;
}
