#include "common.hlsli"

#define dir2D wind
#define dir2D_old wind_old

cbuffer TrampleConstants
{
    float4 trample_params;
    float4 detail_light;
};

cbuffer WindConstants
{
    float4 wind_global;
    float4 wind_xz1;
    float4 wind_xz2;
    float4 wind_xz3;
};

struct InstanceData
{
    float3 quat;
    float  scale;
    float3 pos;
    float  hemi;
#ifndef DETAIL_FAR_NO_TRAILS
    float  trample_strength;
    float  trample_visual;
    float2 trample_dir;
#endif
#ifndef DETAIL_FAR_STATIC
    float4 wind_bend;
    float4 wind_previous;
    float4 wind_velocity;
    float4 wind_previous_velocity;
#endif
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

struct WindDetailInput
{
    float4 pos : POSITION;
    float2 tc : TEXCOORD0;
    float4 N : NORMAL;
    float2 shape : TEXCOORD1;
};

#include "detail_wind_deform.hlsli"
#include "detail_trample_presentation.hlsli"

void main(in WindDetailInput I, in uint instance_id : SV_InstanceID, out OutStructure O)
{
    InstanceData det = detail_buffer[instance_id];
#ifdef DETAIL_SHADOW_PASS
    float3 LightDelta = det.pos - detail_light.xyz;
    if (detail_light.w >= 0 && dot(LightDelta, LightDelta) >= detail_light.w)
    {
        O = (OutStructure)0;
        O.hpos = float4(0, 0, 2, 1);
        return;
    }
#endif
#ifndef DETAIL_FAR_NO_TRAILS
    bool HasTrail = det.trample_visual > .001 || det.trample_strength > .001;
    float2 TrailState = PresentedTrampleState(det.trample_visual, det.trample_strength,
        float4(det.pos, det.scale), (uint)trample_params.z);
    det.trample_visual = TrailState.x;
#endif

    float w = sqrt(max(0.0, 1.0 - dot(det.quat, det.quat)));
    float3x3 m_rotate = QuaternionToMatrix(float4(det.quat, w));

    float3 pos_world = mul(m_rotate, I.pos.xyz * det.scale) + det.pos;
    float3 N = mul(m_rotate, unpack_normal(I.N.xyz));
    
    float hemi = abs(det.hemi);
    float sun = sign(det.hemi) * 0.25f + 0.25f;
    
    float4 pos = float4(pos_world, 1.0f);
	
#ifndef DISABLE_MOTION_VECTORS
    float4 pos_old = pos;
#endif
    
#ifndef DETAIL_FAR_STATIC
    float3 TrailEntryPosition = pos_world;
    float3 TrailEntryNormal = N;
    bool HasEntryPose = HasTrail && TrampleHasEntryPose(det.wind_velocity.w, det.wind_bend.w);
    if (HasEntryPose && TrailState.y < 1)
    {
        float2 EntryTilt;
        float3 EntryLocal = BendGrass(I.pos.xyz, I.shape, det.wind_bend, m_rotate,
            det.wind_previous_velocity.z, det.wind_previous_velocity.xy, EntryTilt);
        TrailEntryPosition = mul(m_rotate, EntryLocal * det.scale) + det.pos;
        TrailEntryNormal = mul(m_rotate, BendGrassNormal(unpack_normal(I.N.xyz), EntryTilt));
    }

    float WindDistance = length(det.pos.xz - eye_position.xz);
    float WindFade = 1 - smoothstep(wind_xz3.y, wind_xz3.z, WindDistance);
    if (!HasTrail && wind_global.x > .5 && det.wind_bend.w >= .65 && WindFade > 0)
    {
        float Limit = wind_xz2.z * lerp(1, wind_xz2.w, smoothstep(0, 1, saturate(det.trample_visual)));
        float4 CurrentState = BlendGrassState(det.wind_previous, det.wind_bend, det.wind_previous_velocity.xyz,
            det.wind_velocity.xyz, wind_xz2.x, wind_xz3.x, Limit);
        CurrentState.xyz *= WindFade;
        float2 Tilt;
        float3 Local = BendGrass(I.pos.xyz, I.shape, CurrentState, m_rotate, wind_global.y, wind_global.zw, Tilt);
        pos.xyz = mul(m_rotate, Local * det.scale) + det.pos;
        N = mul(m_rotate, BendGrassNormal(unpack_normal(I.N.xyz), Tilt));
        #ifndef DISABLE_MOTION_VECTORS
            float4 OldState = BlendGrassState(det.wind_previous, det.wind_bend, det.wind_previous_velocity.xyz,
                det.wind_velocity.xyz, wind_xz2.y, wind_xz3.x, Limit);
            OldState.xyz *= WindFade;
            float2 OldTilt;
            float3 OldLocal = BendGrass(I.pos.xyz, I.shape, OldState, m_rotate, wind_xz1.x, wind_xz1.yz, OldTilt);
            pos_old.xyz = mul(m_rotate, OldLocal * det.scale) + det.pos;
        #endif
    }
    else if (!HasTrail && wind_global.x <= .5)
    {
        #ifdef USE_TREEWAVE
            float H = I.pos.y * det.scale;
            float WindScale = 1 - .95 * smoothstep(.02, .15, det.trample_visual);
            float Dp = calc_cyclic(dot(pos_world, wave.xyz) + wave.w);
            pos.xz += calc_xz_wave(dir2D.xz * (H * Dp * WindScale), I.pos.w);
            #ifndef DISABLE_MOTION_VECTORS
                float OldDp = calc_cyclic(dot(pos_world, wave_old.xyz) + wave_old.w);
                pos_old.xz += calc_xz_wave(dir2D_old.xz * (H * OldDp * WindScale), I.pos.w);
            #endif
        #endif
    }
#endif

#ifndef DETAIL_FAR_NO_TRAILS
    if (det.trample_visual > 0.001f)
    {
        float strength = det.trample_visual;
        float2 moveDir = det.trample_dir;

        float baseY = det.pos.y;
        float hAbove = max(pos.y - baseY, 0.0f);

        float hw = saturate(I.pos.y);
        float bendT = saturate((hw - 0.15f) / 0.85f);

        float noise = frac(sin(dot(det.pos.xz, float2(12.9898, 78.233))) * 43758.5453);
        float arcY = saturate(1.0f - bendT * strength * trample_params.y);
        arcY = max(arcY, 0.15f + noise * 0.1f * strength);

        pos.y = baseY + hAbove * arcY;

        float spreadScale = 1.0f + hAbove * bendT * strength * trample_params.x * 0.15f;
        float2 bladeOffset = pos.xz - det.pos.xz;
        pos.xz = det.pos.xz + bladeOffset * spreadScale;

        float lean = hAbove * bendT * strength * trample_params.x;
        lean = min(lean, hAbove * 0.5f);
        pos.xz += moveDir * lean;

        #ifndef DISABLE_MOTION_VECTORS
            float hOld = max(pos_old.y - baseY, 0.0f);
            pos_old.y = baseY + hOld * arcY;
            float2 bladeOffsetOld = pos_old.xz - det.pos.xz;
            pos_old.xz = det.pos.xz + bladeOffsetOld * spreadScale;
            pos_old.xz += moveDir * lean;
        #endif
    }
#endif
    
#ifndef DETAIL_FAR_STATIC
    if (HasEntryPose)
    {
        pos.xyz = BlendTrailEntryPose(TrailEntryPosition, pos.xyz, TrailState.y);
        N = normalize(lerp(TrailEntryNormal, N, TrailState.y));
        #ifndef DISABLE_MOTION_VECTORS
            pos_old.xyz = BlendTrailEntryPose(TrailEntryPosition, pos_old.xyz, TrailState.y);
        #endif
    }
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
