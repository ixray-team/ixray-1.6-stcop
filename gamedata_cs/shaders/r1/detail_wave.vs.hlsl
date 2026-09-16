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

float3x3 QuaternionToMatrix(float4 q)
{
    float xx = q.x * q.x;
    float yy = q.y * q.y;
    float zz = q.z * q.z;
    float xy = q.x * q.y;
    float xz = q.x * q.z;
    float yz = q.y * q.z;
    float wx = q.w * q.x;
    float wy = q.w * q.y;
    float wz = q.w * q.z;

    float3x3 m;
    m[0] = float3(1.0 - 2.0 * (yy + zz), 2.0 * (xy + wz), 2.0 * (xz - wy));
    m[1] = float3(2.0 * (xy - wz), 1.0 - 2.0 * (xx + zz), 2.0 * (yz + wx));
    m[2] = float3(2.0 * (xz + wy), 2.0 * (yz - wx), 1.0 - 2.0 * (xx + yy));
    return m;
}

vf main(v_detail v)
{
    vf o;

    // index
    int i = v.misc.w;
	float2x4 mm = array[i];
	
    float3 qv = mm[0].xyz;
    float w = sqrt(max(0.0, 1.0 - dot(qv, qv)));
    float3x3 m_rotate = QuaternionToMatrix(float4(qv, w));
    
	float3 posi = float3(mm[1].xyz);
	
	float scale = mm[0].w;
	
	float hemi = abs(mm[1].w);
	float sun = sign(mm[1].w)*0.25f+0.25f;
	
    float4 m0 = float4(m_rotate[0]*scale, posi.x);
    float4 m1 = float4(m_rotate[1]*scale, posi.y);
    float4 m2 = float4(m_rotate[2]*scale, posi.z);
    
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
