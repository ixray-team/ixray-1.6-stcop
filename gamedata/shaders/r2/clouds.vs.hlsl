#include "common.hlsli"
#include "shared\cloudconfig.hlsli"

struct vi
{
    float4 p : POSITION;
    float4 dir : COLOR0; // dir0,dir1(w<->z)
    float4 color : COLOR1; // rgb. intensity
};

struct vf
{
    float4 hpos : POSITION;
    float4 color : COLOR0;
    float2 tc0 : TEXCOORD0;
    float2 tc1 : TEXCOORD1;
};

void main(in vi v, out vf o)
{
    o.hpos = mul(m_WVP, v.p);

    float2 d0 = v.dir.xy * 2.0f - 1.0f;
    float2 d1 = v.dir.wz * 2.0f - 1.0f;

    o.tc0 = v.p.xz * CLOUD_TILE0 + d0 * timers.z * CLOUD_SPEED0;
    o.tc1 = v.p.xz * CLOUD_TILE1 + d1 * timers.z * CLOUD_SPEED1;

    o.color = v.color;
    o.color.w *= pow(v.p.y, 25);
}
