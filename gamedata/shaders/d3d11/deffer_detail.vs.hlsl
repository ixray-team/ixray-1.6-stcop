#include "common.hlsli"

cbuffer DetailConstants
{
    float4 wave;
    float4 wave_old;
    float4 dir2D;
    float4 dir2D_old;
};

struct InstanceData
{
    float3 quat;
    float  scale;
    float3 pos;
    float  hemi;
};

StructuredBuffer<InstanceData> detail_buffer : register(t0);

#ifndef DETAIL_SHADOW_PASS
	#define OutStructure p_bumped_new
#else
	#define OutStructure p_shadow
#endif

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

void main(in v_detail I, in uint instance_id : SV_InstanceID, out OutStructure O)
{
    InstanceData det = detail_buffer[instance_id];

    float w = sqrt(max(0.0, 1.0 - dot(det.quat, det.quat)));
    float3x3 m_rotate = QuaternionToMatrix(float4(det.quat, w));

    float3 pos_world = mul(m_rotate, I.pos.xyz * det.scale) + det.pos;
    float3 N = mul(m_rotate, unpack_normal(I.N.zyx));
    
    float hemi = abs(det.hemi);
    float sun = sign(det.hemi) * 0.25f + 0.25f;
    
    float4 pos = float4(pos_world, 1.0f);
	
#ifndef DISABLE_MOTION_VECTORS
    float4 pos_old = pos;
#endif
    
#ifdef USE_TREEWAVE
    float dp = calc_cyclic(dot(pos_world, wave));
    float H = I.pos.y * det.scale;
    float inten = H * dp;
    
    pos.xz += calc_xz_wave(dir2D.xz * inten, I.pos.w);
    
	#ifndef DISABLE_MOTION_VECTORS
		float dp_old = calc_cyclic(dot(pos_world, wave_old));
		float inten_old = H * dp_old;
		pos_old.xz += calc_xz_wave(dir2D_old.xz * inten_old, I.pos.w);
	#endif
#endif
    
    O.hpos = mul(m_VP, pos);
	
#ifndef DETAIL_SHADOW_PASS
    float3 Pe = mul(m_WV, pos);
    
    O.tcdh = float4(I.tc.xy, hemi, sun);
    O.position = float4(Pe, 1.0f);
    
    float3 N_world = mul((float3x3)m_WV, N);
    O.M1 = N_world.xxx;
    O.M2 = N_world.yyy;
    O.M3 = N_world.zzz;

	#ifndef DISABLE_MOTION_VECTORS
		O.hpos_curr = O.hpos;
		O.hpos_old = mul(m_VP_old, pos_old);
	#endif

	O.hpos.xy += m_taa_jitter.xy * O.hpos.w;
#else
    O.tc0 = I.tc.xy;
#endif
}

