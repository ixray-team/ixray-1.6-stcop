#include "common.hlsli"

uniform float4 consts;

uniform float4 wave;
uniform float4 dir2D;

uniform float2x4 array[50];

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

void main(in v_detail I, out p_bumped_new O)
{
    int i = I.misc.w;
	float2x4 mm = array[i];
	
    float3 qv = mm[0].xyz;
    float w = sqrt(max(0.0, 1.0 - dot(qv, qv)));
    float3x3 m_rotate = QuaternionToMatrix(float4(qv, w));
	float3 posi = float3(mm[1].xyz);
	
	float scale = mm[0].w;
    float4 m0 = float4(m_rotate[0]*scale, posi.x);
    float4 m1 = float4(m_rotate[1]*scale, posi.y);
    float4 m2 = float4(m_rotate[2]*scale, posi.z);

    float4 pos;
    pos.x = dot(m0, I.pos);
    pos.y = dot(m1, I.pos);
    pos.z = dot(m2, I.pos);
    pos.w = 1.0f;

    float3 Pe = mul(m_WV, pos);
    float2 tc = I.misc.xy * consts.xy;

    float3 N;
    N.x = pos.x - m0.w;
    N.y = pos.y - m1.w + 0.75f;
    N.z = pos.z - m2.w;

    O.tcdh = float4(tc.xy, 0.7, 0.5);
    O.position = float4(Pe, 1.0f);

    N = normalize(mul(m_WV, N));

    O.M1 = N;
    O.M2 = N;
    O.M3 = N;

    O.hpos = mul(m_WVP, pos);
}
