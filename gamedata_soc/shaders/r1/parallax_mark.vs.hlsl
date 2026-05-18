#include "common.hlsli"
#include "skin.hlsli"

// Vertex to Pixel struct
struct vf
{
    float2 tc0 : TEXCOORD0;
    float3 v_pos : TEXCOORD1;
    float3 v_nrm : TEXCOORD2;
    float4 hpos : POSITION;
};

vf     _main (v_model v)
{
    vf o;

    o.hpos = mul(m_WVP, v.pos); // Homogenous position
    o.tc0 = v.tc.xy; //Texture coordinates

    o.v_pos = mul(m_WV, v.pos).xyz; // Position in view space
    o.v_nrm = mul(m_WV, v.norm).xyz; // Normal in view space

    return o;
}

//Skinning
#ifdef SKIN_NONE
vf main(v_model v) { return _main(v); }
#endif

#ifdef SKIN_0
vf main(v_model_skinned_0 v) { return _main(skinning_0(v)); }
#endif

#ifdef SKIN_1
vf main(v_model_skinned_1 v) { return _main(skinning_1(v)); }
#endif

#ifdef SKIN_2
vf main(v_model_skinned_2 v) { return _main(skinning_2(v)); }
#endif

#ifdef SKIN_3
vf main(v_model_skinned_3 v) { return _main(skinning_3(v)); }
#endif

#ifdef SKIN_4
vf main(v_model_skinned_4 v) { return _main(skinning_4(v)); }
#endif
