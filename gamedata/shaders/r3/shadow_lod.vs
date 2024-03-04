#include "common.h"

uniform float3x4		m_xform;
uniform float3x4		m_xform_v;
uniform float4 			consts; 	// {1/quant,1/quant,???,???}
uniform float4 			c_scale,c_bias,wind,wave;

//////////////////////////////////////////////////////////////////////////////////////////
// Vertex
void main (in v_shadow_direct_aref I, out p_shadow O) {
	float4 pos = float4(mul(m_xform, I.P).xyz, 1.0f);

#ifdef	USE_TREEWAVE
	float base = m_xform._24;
	float dp = calc_cyclic(wave.w + dot(pos, wave.xyz));
	float H = pos.y - base;
	float inten = H * dp;
	float frac = I.tc.z * consts.x;
	pos.xz += calc_xz_wave(wind.xz * inten, frac);
#endif

	O.hpos = mul(m_VP,	pos);
	O.tc0 = I.tc.xy * consts.xy;
	
#ifndef USE_HWSMAP
	O.depth = O.hpos.z;
#endif
}