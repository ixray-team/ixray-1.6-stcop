#include "common.hlsli"
#include "skin.hlsli"

struct vf
{
	float4 hpos : POSITION;
	float2 tc0 : TEXCOORD0; // base
	float4 tc1 : TEXCOORD1;
};

vf _main(v_model v)
{
	vf o;
	o.hpos = mul(m_WVP, v.pos);
	o.tc0 = v.tc.xy;

	o.tc1.x = (o.hpos.x + o.hpos.w) * 0.5f;
	o.tc1.y = (o.hpos.w - o.hpos.y) * 0.5f;
	o.tc1.z = 0.0f;
	o.tc1.w = o.hpos.w;
	return o;
}

#define SKIN_LQ
#define SKIN_VF vf
#include "skin_main.hlsli"
