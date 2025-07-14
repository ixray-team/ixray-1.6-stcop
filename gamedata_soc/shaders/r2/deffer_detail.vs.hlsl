#include "common.hlsli"

uniform float4 consts;

uniform float4 wave;
uniform float4 dir2D;

uniform float2x4 array[50];

float3x3 setMatrix (float3 hpb)
{
        
    float _ch, _cp, _cb, _sh, _sp, _sb, _cc, _cs, _sc, _ss;

    sincos(hpb.x, _sh, _ch);
    sincos(hpb.y, _sp, _cp);
    sincos(hpb.z, _sb, _cb);
    
    _cc = _ch*_cb; _cs = _ch*_sb; _sc = _sh*_cb; _ss = _sh*_sb;
	
    return float3x3(_cc-_sp*_ss, _sp*_sc+_cs, -_cp*_sh,
					-_cp*_sb, 	 _cp*_cb,	  _sp,
					_sp*_cs+_sc, _ss-_sp*_cc, _cp*_ch);
};

void main(in v_detail I, out p_bumped_new O)
{
    int i = I.misc.w;
	float2x4 mm = array[i];
	
	float3x3 mmhpb = setMatrix(mm[0].xyz);
	float3 posi = float3(mm[1].xyz);
	
	float scale = mm[0].w;

	float hemi = abs(mm[1].w);
	float sun = sign(mm[1].w)*0.25f+0.25f;

    float4 m0 = float4(mmhpb[0]*scale, posi.x);
    float4 m1 = float4(mmhpb[1]*scale, posi.y);
    float4 m2 = float4(mmhpb[2]*scale, posi.z);

    float4 pos;
    pos.x = dot(m0, I.pos);
    pos.y = dot(m1, I.pos);
    pos.z = dot(m2, I.pos);
    pos.w = 1.0f;

#ifdef USE_TREEWAVE
    float base = m1.w;
    float H = I.pos.y * length(m1.xyz);
    float fractional = I.misc.z * consts.x;

    float dp = calc_cyclic(dot(pos, wave));
    float inten = H * dp;

    pos.xz += calc_xz_wave(dir2D.xz * inten, fractional);
#endif

    float3 Pe = mul(m_WV, pos);
    float2 tc = I.misc.xy * consts.xy;

    float3 N;
    N.x = pos.x - m0.w;
    N.y = pos.y - m1.w + 0.75f;
    N.z = pos.z - m2.w;

    O.tcdh = float4(tc.xy, hemi, sun);
    O.position = float4(Pe, 1.0f);

    float3x3 xform = mul((float3x3)m_WV, float3x3(
        0.0f, 0.0f, N.x,
        0.0f, 0.0f, N.y,
        0.0f, 0.0f, N.z));

    O.M1 = xform[0];
    O.M2 = xform[1];
    O.M3 = xform[2];

    O.hpos = mul(m_WVP, pos);
}
