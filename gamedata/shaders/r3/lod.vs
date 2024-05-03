#include "common.h"
#define L_SCALE 3.1f

struct v_bolbord {
	float3 pos0 : POSITION0;
	float3 pos1 : POSITION1;
	
	float3 n0 : NORMAL0;
	float3 n1 : NORMAL1;
	
	float2 tc0 : TEXCOORD0;
	float2 tc1 : TEXCOORD1;
	
	float4 rgbh0 : TEXCOORD2;
	float4 rgbh1 : TEXCOORD3;
	
	float4 sun_af : COLOR0;
};

void main (in v_bolbord I, out p_bilbord o) {
	I.sun_af.xyz = I.sun_af.zyx;
	I.rgbh0.xyz = I.rgbh0.zyx;
	I.rgbh1.xyz = I.rgbh1.zyx;
	
	float4 pos = float4(lerp(I.pos0, I.pos1, I.sun_af.w), 1.0f);
	float h = lerp(I.rgbh0.w, I.rgbh1.w, I.sun_af.w) * L_SCALE;
	o.af = float4(h, h, I.sun_af.z, I.sun_af.w);
	
	o.hpos = mul(m_VP, pos);
	o.position = mul(m_V, pos);

	o.tc0 = I.tc0;
	o.tc1 = I.tc1;
	
	o.hpos_curr = mul(m_VP, pos);
	o.hpos_old = mul(m_VP_old, pos);
}

