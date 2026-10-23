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
	o.hpos = mul(m_WVP, v.P);
	o.tc0 = v.tc.xy;

	o.tc1.x = (o.hpos.x + o.hpos.w) * 0.5f;
	o.tc1.y = (o.hpos.w - o.hpos.y) * 0.5f;
	o.tc1.z = 0.0f;
	o.tc1.w = o.hpos.w;
	return o;
}

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
