#include "common.hlsli"

#define dir2D wind
#define dir2D_old wind_old

cbuffer TrampleConstants : register(b6)
{
    float4 trample_params;
};

cbuffer WindConstants : register(b7)
{
    float4 wind_global;
    float4 wind_xz1;
    float4 wind_xz1_dir;
    float4 wind_xz2;
    float4 wind_xz2_dir;
    float4 wind_xz3;
    float4 wind_xz3_dir;
    float4 wind_swirl;
    float4 wind_swirl_dir;
};

struct InstanceData
{
    float3 quat;
    float  scale;
    float3 pos;
    float  hemi;
    float  trample_strength;
    float  trample_visual;
    float2 trample_dir;
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

float windHash(float2 p)
{
    float px = p.x * 123.34 + p.y * 456.21;
    px = frac(px);
    float d = px * (px + 45.32);
    float r = frac(d * d + p.x * p.y);
    return r;
}

float2 windGrad(float2 p)
{
    float n = windHash(p) * 6.2831853;
    return float2(cos(n), sin(n));
}

float windNoise(float2 p)
{
    float2 i = floor(p);
    float2 f = frac(p);
    float2 u = f * f * f * (f * (f * 6.0 - 15.0) + 10.0);

    float v00 = dot(windGrad(i), f);
    float v10 = dot(windGrad(i + float2(1, 0)), f - float2(1, 0));
    float v01 = dot(windGrad(i + float2(0, 1)), f - float2(0, 1));
    float v11 = dot(windGrad(i + float2(1, 1)), f - float2(1, 1));

    float a = v00 + u.x * (v10 - v00);
    float b = v01 + u.x * (v11 - v01);
    return (a + u.y * (b - a)) * 0.5 + 0.5;
}

float windFBM(float2 p, float seed)
{
    float val = windNoise(p + seed * 7.31);
    val += windNoise(p * 2.173 + seed * 3.7) * 0.5;
    return val * 0.6667;
}

void main(in v_detail I, in uint instance_id : SV_InstanceID, out OutStructure O)
{
    InstanceData det = detail_buffer[instance_id];

    float w = sqrt(max(0.0, 1.0 - dot(det.quat, det.quat)));
    float3x3 m_rotate = QuaternionToMatrix(float4(det.quat, w));

    float3 pos_world = mul(m_rotate, I.pos.xyz * det.scale) + det.pos;
    float3 N = mul(m_rotate, unpack_normal(I.N.xyz));
    
    float hemi = saturate(abs(det.hemi) * 2.0f);
    float sun = det.hemi > 0.0f ? 1.0f : 0.0f;
    
    float4 pos = float4(pos_world, 1.0f);
	
#ifndef DISABLE_MOTION_VECTORS
    float4 pos_old = pos;
#endif
    
#ifdef USE_TREEWAVE
    float H = I.pos.y * det.scale;
    float windScale = 1.0f - 0.95f * smoothstep(0.02f, 0.15f, det.trample_visual);
    float globalInt = wind_global.x;
    
    if (globalInt > 0.001f)
    {
        float t = wave.w;
        float2 wp = pos_world.xz;
        float hw = saturate(I.pos.y);
        float h = max(pos.y - det.pos.y, 0.0f);
        
        float xzDensity = 0.0f;
        float2 xzDisp = float2(0, 0);
        
        if (wind_global.y > 0.5f)
        {
            float c1 = windNoise(wp * wind_xz1.x + wind_xz1_dir.xy * t * wind_xz1.w);
            c1 = saturate((c1 - 0.5f) * (1.0f + wind_xz1.z * 2.0f) + 0.5f);
            float2 perp1 = float2(-wind_xz1_dir.y, wind_xz1_dir.x);
            float2 dir1 = normalize(wind_xz1_dir.xy + perp1 * (c1 - 0.5f) * 2.5f);
            float2 d1 = dir1 * c1 * wind_xz1.y * wind_xz1_dir.z;
            
            float c2 = windNoise(wp * wind_xz2.x + wind_xz2_dir.xy * t * wind_xz2.w + c1 * 0.3f);
            c2 = saturate((c2 - 0.5f) * (1.0f + wind_xz2.z * 2.0f) + 0.5f);
            float2 perp2 = float2(-wind_xz2_dir.y, wind_xz2_dir.x);
            float2 dir2 = normalize(wind_xz2_dir.xy + perp2 * (c2 - 0.5f) * 2.5f);
            float2 d2 = dir2 * c2 * wind_xz2.y * wind_xz2_dir.z;
            
            float c3 = windNoise(wp * wind_xz3.x + wind_xz3_dir.xy * t * wind_xz3.w + c2 * 0.3f);
            c3 = saturate((c3 - 0.5f) * (1.0f + wind_xz3.z * 2.0f) + 0.5f);
            float2 perp3 = float2(-wind_xz3_dir.y, wind_xz3_dir.x);
            float2 dir3 = normalize(wind_xz3_dir.xy + perp3 * (c3 - 0.5f) * 2.5f);
            float2 d3 = dir3 * c3 * wind_xz3.y * wind_xz3_dir.z;
            
            xzDisp = d1 + d2 + d3;
            xzDensity = saturate((c1 * wind_xz1.y * wind_xz1_dir.z + c2 * wind_xz2.y * wind_xz2_dir.z + c3 * wind_xz3.y * wind_xz3_dir.z)
                / max(wind_xz1.y * wind_xz1_dir.z + wind_xz2.y * wind_xz2_dir.z + wind_xz3.y * wind_xz3_dir.z, 0.01f));
        }
        
        float swirlY = 0.0f;
        if (wind_global.z > 0.5f)
        {
            float cs = windNoise(wp * wind_swirl.x + wind_swirl_dir.xy * t * wind_swirl.w);
            cs = saturate((cs - 0.5f) * (1.0f + wind_swirl.z * 2.0f) + 0.5f);
            swirlY = (cs - 0.5f) * wind_swirl.y;
        }
        
        float2 leanDir = normalize(xzDisp + 0.0001f);
        float gust = xzDensity * xzDensity;
        float bend = hw * hw * windScale;
        float leanAmt = bend * gust * globalInt * 1.8f;
        pos.xz += leanDir * leanAmt * h;
        
        pos.y += swirlY * globalInt * hw * h * 0.5f;
        pos.xz += leanDir * swirlY * globalInt * hw * h * 0.3f;
        
        #ifndef DISABLE_MOTION_VECTORS
            float t_old = wave_old.w;
            
            float xzDensity_o = 0.0f;
            float2 xzDisp_o = float2(0, 0);
            
            if (wind_global.y > 0.5f)
            {
                float c1o = windNoise(wp * wind_xz1.x + wind_xz1_dir.xy * t_old * wind_xz1.w);
                c1o = saturate((c1o - 0.5f) * (1.0f + wind_xz1.z * 2.0f) + 0.5f);
                float2 perp1o = float2(-wind_xz1_dir.y, wind_xz1_dir.x);
                float2 dir1o = normalize(wind_xz1_dir.xy + perp1o * (c1o - 0.5f) * 2.5f);
                float2 d1o = dir1o * c1o * wind_xz1.y * wind_xz1_dir.z;
                
                float c2o = windNoise(wp * wind_xz2.x + wind_xz2_dir.xy * t_old * wind_xz2.w + c1o * 0.3f);
                c2o = saturate((c2o - 0.5f) * (1.0f + wind_xz2.z * 2.0f) + 0.5f);
                float2 perp2o = float2(-wind_xz2_dir.y, wind_xz2_dir.x);
                float2 dir2o = normalize(wind_xz2_dir.xy + perp2o * (c2o - 0.5f) * 2.5f);
                float2 d2o = dir2o * c2o * wind_xz2.y * wind_xz2_dir.z;
                
                float c3o = windNoise(wp * wind_xz3.x + wind_xz3_dir.xy * t_old * wind_xz3.w + c2o * 0.3f);
                c3o = saturate((c3o - 0.5f) * (1.0f + wind_xz3.z * 2.0f) + 0.5f);
                float2 perp3o = float2(-wind_xz3_dir.y, wind_xz3_dir.x);
                float2 dir3o = normalize(wind_xz3_dir.xy + perp3o * (c3o - 0.5f) * 2.5f);
                float2 d3o = dir3o * c3o * wind_xz3.y * wind_xz3_dir.z;
                
                xzDisp_o = d1o + d2o + d3o;
                xzDensity_o = saturate((c1o * wind_xz1.y * wind_xz1_dir.z + c2o * wind_xz2.y * wind_xz2_dir.z + c3o * wind_xz3.y * wind_xz3_dir.z)
                    / max(wind_xz1.y * wind_xz1_dir.z + wind_xz2.y * wind_xz2_dir.z + wind_xz3.y * wind_xz3_dir.z, 0.01f));
            }
            
            float swirlYo = 0.0f;
            if (wind_global.z > 0.5f)
            {
                float cso = windNoise(wp * wind_swirl.x + wind_swirl_dir.xy * t_old * wind_swirl.w);
                cso = saturate((cso - 0.5f) * (1.0f + wind_swirl.z * 2.0f) + 0.5f);
                swirlYo = (cso - 0.5f) * wind_swirl.y;
            }
            
            float2 leanDir_o = normalize(xzDisp_o + 0.0001f);
            float gust_o = xzDensity_o * xzDensity_o;
            float bend_o = hw * hw * windScale;
            float leanAmt_o = bend_o * gust_o * globalInt * 1.8f;
            pos_old.xz += leanDir_o * leanAmt_o * h;
            
            pos_old.y += swirlYo * globalInt * hw * h * 0.5f;
            pos_old.xz += leanDir_o * swirlYo * globalInt * hw * h * 0.3f;
        #endif
    }
    else
    {
        float dp = calc_cyclic(dot(pos_world, wave.xyz) + wave.w);
        float inten = H * dp * windScale;
        pos.xz += calc_xz_wave(dir2D.xz * inten, I.pos.w);
        
        #ifndef DISABLE_MOTION_VECTORS
            float dp_old = calc_cyclic(dot(pos_world, wave_old.xyz) + wave_old.w);
            float inten_old = H * dp_old * windScale;
            pos_old.xz += calc_xz_wave(dir2D_old.xz * inten_old, I.pos.w);
        #endif
    }
#endif

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
