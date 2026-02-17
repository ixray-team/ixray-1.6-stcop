#include "common.hlsli"

cbuffer DetailConstants
{
    float4 consts;

    float4 wave;
    float4 wave_old;

    float4 dir2D;
    float4 dir2D_old;
};

//LVutner: Has to match the CPU struct
struct InstanceData
{
	float3 hpb;
	float scale;
	float3 pos;
	float hemi;
};

//LVutner: Always bound to slot0 (see CPP code)
StructuredBuffer<InstanceData> detail_buffer : register(t0);

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

void main(in v_detail I, out p_bumped_new O, uint instance_id : SV_InstanceID)
{
	InstanceData det = detail_buffer[instance_id];
	float3x3 m_rotate = setMatrix(det.hpb);

	float hemi = abs(det.hemi);
	float sun = sign(det.hemi)*0.25f+0.25f;

    float4 m0 = float4(m_rotate[0]*det.scale, det.pos.x);
    float4 m1 = float4(m_rotate[1]*det.scale, det.pos.y);
    float4 m2 = float4(m_rotate[2]*det.scale, det.pos.z);

    float4 pos;
    pos.x = dot(m0, float4(I.pos.xyz, 1.0));
    pos.y = dot(m1, float4(I.pos.xyz, 1.0));
    pos.z = dot(m2, float4(I.pos.xyz, 1.0));
    pos.w = 1.0f;

    float3 Pe = mul(m_WV, pos);
    float2 tc = I.tc.xy;

	float3 N = unpack_normal(I.N.zyx);
	N = mul(m_rotate, N).xyz * 2.0f;

    O.tcdh = float4(tc.xy, hemi, sun);
    O.position = float4(Pe, 1.0f);

    N.xyz = mul((float3x3)m_WV, N.xyz);

    O.M1 = N.xyz;
    O.M2 = N.xyz;
    O.M3 = N.xyz;

    O.hpos = mul(m_WVP, pos);
};