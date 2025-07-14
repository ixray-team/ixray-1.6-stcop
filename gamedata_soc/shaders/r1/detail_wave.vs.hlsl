#include "common.hlsli"

struct vf
{
    float4 hpos : POSITION;
    float4 C : COLOR0;
    float2 tc : TEXCOORD0;
    float fog : FOG;
};

uniform float4 consts; // {1/quant,1/quant,diffusescale,ambient}
uniform float4 wave; // cx,cy,cz,tm
uniform float4 dir2D;

uniform float2x4 array[50] : register(c10);

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

vf main(v_detail v)
{
    vf o;

    // index
    int i = v.misc.w;
	float2x4 mm = array[i];
	
	float3x3 mmhpb = setMatrix(mm[0].xyz);
	float3 posi = float3(mm[1].xyz);
	
	float scale = mm[0].w;
	
	float hemi = abs(mm[1].w);
	float sun = sign(mm[1].w)*0.25f+0.25f;
	
    float4 m0 = float4(mmhpb[0]*scale, posi.x);
    float4 m1 = float4(mmhpb[1]*scale, posi.y);
    float4 m2 = float4(mmhpb[2]*scale, posi.z);
	float4 c0 = float4(L_ambient.rgb+L_hemi_color.rgb*hemi+L_sun_color.rgb*sun, 1.0f);
	
    // Transform to world coords
    float4 pos;
    pos.x = dot(m0, v.pos);
    pos.y = dot(m1, v.pos);
    pos.z = dot(m2, v.pos);
    pos.w = 1;

    //
    float base = m1.w;
    float dp = calc_cyclic(dot(pos, wave));
    float H = v.pos.y * length(m1.xyz);
    float fractional = v.misc.z * consts.x; // fractional
    float inten = H * dp;
    float2 result = calc_xz_wave(dir2D.xz * inten, fractional);
    pos = float4(pos.x + result.x, pos.y, pos.z + result.y, 1);
    o.hpos = mul(m_WVP, pos);

    // Calc fog
    o.fog = calc_fogging(pos);

    // Fake lighting
    float dpc = max(0.f, dp);
    o.C = c0 * (consts.w + consts.z * dpc * fractional);

    // final xform, color, tc
    o.tc.xy = (v.misc * consts).xy;

    return o;
}
