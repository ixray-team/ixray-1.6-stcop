#include "common.hlsli"
#include "skin.hlsli"

struct vf
{
	float4 hpos : SV_Position;
    float2 tc0 : TEXCOORD0;
	float3 T : TEXCOORD1;
	float3 B : TEXCOORD2;
	float3 N : TEXCOORD3;
	float3 P : TEXCOORD4;
};

vf     _main (v_model v)
{
    vf o;

	o.hpos = mul(m_WVP, v.P);
    o.tc0 = v.tc.xy;
	o.T = mul(m_W, v.T).xyz;
	o.B = mul(m_W, v.B).xyz;
	o.N = mul(m_W, v.N).xyz;
	o.P = mul(m_W, v.P).xyz;

    return o;
}

vf main(PULL_INPUT) { return _main(PullModel(VertexId)); }
