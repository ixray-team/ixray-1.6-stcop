#include "r1_common.hlsli"
#include "r1_static.hlsli"
struct InstanceData
{
    float3 quat;
    float scale;
    float3 pos;
    float hemi;
    float trample_strength;
    float trample_visual;
    float2 trample_dir;
};
StructuredBuffer<InstanceData> detail_buffer : register(t0);
struct vf
{
    float2 tc0 : TEXCOORD0;
    float3 c0 : COLOR0;
    float fog : TEXCOORD1;
    float4 hpos : SV_POSITION;
};
float3x3 QuaternionToMatrix(float4 q)
{
    return float3x3(
        1.0f - 2.0f * (q.y*q.y + q.z*q.z), 2.0f * (q.x*q.y + q.z*q.w), 2.0f * (q.x*q.z - q.y*q.w),
        2.0f * (q.x*q.y - q.z*q.w), 1.0f - 2.0f * (q.x*q.x + q.z*q.z), 2.0f * (q.y*q.z + q.x*q.w),
        2.0f * (q.x*q.z + q.y*q.w), 2.0f * (q.y*q.z - q.x*q.w), 1.0f - 2.0f * (q.x*q.x + q.y*q.y));
}
void main(in v_detail I, in uint instance_id : SV_InstanceID, out vf O)
{
    InstanceData det = detail_buffer[instance_id];
    float3x3 rotate = QuaternionToMatrix(float4(det.quat, sqrt(max(0.0f, 1.0f - dot(det.quat, det.quat)))));
    float4 pos = float4(mul(rotate, I.pos.xyz * det.scale) + det.pos, 1.0f);
    float3 color = L_ambient.rgb + L_hemi_color.rgb * abs(det.hemi) + L_sun_color.rgb * (sign(det.hemi) * .25f + .25f);
#ifdef USE_TREEWAVE
    float dp = calc_cyclic(dot(pos, wave));
    float rigidity = I.pos.w;
    pos.xz += calc_xz_wave(wind.xz * (I.pos.y * det.scale * dp), rigidity);
    color *= consts.w + consts.z * max(0.0f, dp) * rigidity;
#endif
    O.hpos = mul(m_VP, pos);
    O.tc0 = I.tc;
    O.c0 = saturate(color);
    O.fog = r1_fog(pos.xyz);
}
