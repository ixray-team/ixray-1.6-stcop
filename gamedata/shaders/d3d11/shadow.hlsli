#ifndef SHADOW_H
#define SHADOW_H

#include "common.hlsli"
uniform float4x4 m_shadow;

Texture2D s_smap : register(ps, t0);
SamplerComparisonState smp_smap;
sampler smp_jitter;

Texture2D jitter0;
Texture2D jitter1;

#ifndef USE_ULTRA_SHADOWS
    #define KERNEL 0.6f
#else
    #define KERNEL 1.0f
#endif

float4 sm_gather(float2 tc, int2 offset)
{
#ifdef SM_4_1
    return s_smap.Gather(smp_nofilter, tc, offset);
#else
    static const float scale = float(SMAP_size);
    float2 fc = frac(tc * scale);

    tc -= fc / scale;

    float s0 = s_smap.SampleLevel(smp_nofilter, tc, 0, offset + int2(0, 1));
    float s1 = s_smap.SampleLevel(smp_nofilter, tc, 0, offset + int2(1, 1));
    float s2 = s_smap.SampleLevel(smp_nofilter, tc, 0, offset + int2(1, 0));
    float s3 = s_smap.SampleLevel(smp_nofilter, tc, 0, offset + int2(0, 0));

    return float4(s0, s1, s2, s3);
#endif
}

//////////////////////////////////////////////////////////////////////////////////////////
// hardware + PCF
//////////////////////////////////////////////////////////////////////////////////////////
float sample_hw_pcf(float4 tc, float4 shift)
{
    static const float ts = KERNEL / float(SMAP_size);

    tc.xyz /= tc.w;
    tc.xy += shift.xy * ts;

    return s_smap.SampleCmpLevelZero(smp_smap, tc.xy, tc.z).x;
}

#define GS2 3

float shadow_hw(float4 tc)
{
    float s0 = sample_hw_pcf(tc, float4(-1, -1, 0, 0));
    float s1 = sample_hw_pcf(tc, float4(+1, -1, 0, 0));
    float s2 = sample_hw_pcf(tc, float4(-1, +1, 0, 0));
    float s3 = sample_hw_pcf(tc, float4(+1, +1, 0, 0));

    return (s0 + s1 + s2 + s3) / 4.h;
}

#if SUN_QUALITY >= 4
    #define FILTER_SIZE 11
    #define FS FILTER_SIZE
    #define FS2 (FILTER_SIZE / 2)

static const float W2[11][11] =
{
    { 0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f },
    { 0.0f, 0.2f, 0.2f, 0.2f, 0.2f, 0.2f, 0.2f, 0.2f, 0.2f, 0.2f, 0.0f },
    { 0.0f, 0.2f, 1.0f, 1.0f, 1.0f, 1.0f, 1.0f, 1.0f, 1.0f, 0.2f, 0.0f },
    { 0.0f, 0.2f, 1.0f, 1.0f, 1.0f, 1.0f, 1.0f, 1.0f, 1.0f, 0.2f, 0.0f },
    { 0.0f, 0.2f, 1.0f, 1.0f, 1.0f, 1.0f, 1.0f, 1.0f, 1.0f, 0.2f, 0.0f },
    { 0.0f, 0.2f, 1.0f, 1.0f, 1.0f, 1.0f, 1.0f, 1.0f, 1.0f, 0.2f, 0.0f },
    { 0.0f, 0.2f, 1.0f, 1.0f, 1.0f, 1.0f, 1.0f, 1.0f, 1.0f, 0.2f, 0.0f },
    { 0.0f, 0.2f, 1.0f, 1.0f, 1.0f, 1.0f, 1.0f, 1.0f, 1.0f, 0.2f, 0.0f },
    { 0.0f, 0.2f, 1.0f, 1.0f, 1.0f, 1.0f, 1.0f, 1.0f, 1.0f, 0.2f, 0.0f },
    { 0.0f, 0.2f, 0.2f, 0.2f, 0.2f, 0.2f, 0.2f, 0.2f, 0.2f, 0.2f, 0.0f },
    { 0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f },
};

static const float W1[11][11] =
{
    { 0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f },
    { 0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f },
    { 0.0f, 0.0f, 0.2f, 0.2f, 0.2f, 0.2f, 0.2f, 0.2f, 0.2f, 0.0f, 0.0f },
    { 0.0f, 0.0f, 0.2f, 1.0f, 1.0f, 1.0f, 1.0f, 1.0f, 0.2f, 0.0f, 0.0f },
    { 0.0f, 0.0f, 0.2f, 1.0f, 1.0f, 1.0f, 1.0f, 1.0f, 0.2f, 0.0f, 0.0f },
    { 0.0f, 0.0f, 0.2f, 1.0f, 1.0f, 1.0f, 1.0f, 1.0f, 0.2f, 0.0f, 0.0f },
    { 0.0f, 0.0f, 0.2f, 1.0f, 1.0f, 1.0f, 1.0f, 1.0f, 0.2f, 0.0f, 0.0f },
    { 0.0f, 0.0f, 0.2f, 1.0f, 1.0f, 1.0f, 1.0f, 1.0f, 0.2f, 0.0f, 0.0f },
    { 0.0f, 0.0f, 0.2f, 0.2f, 0.2f, 0.2f, 0.2f, 0.2f, 0.2f, 0.0f, 0.0f },
    { 0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f },
    { 0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f },
};

static const float W0[11][11] =
{
    { 0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f },
    { 0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f },
    { 0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f },
    { 0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f },
    { 0.0f, 0.0f, 0.0f, 0.0f, 0.1f, 0.1f, 0.1f, 0.0f, 0.0f, 0.0f, 0.0f },
    { 0.0f, 0.0f, 0.0f, 0.0f, 0.1f, 1.0f, 0.1f, 0.0f, 0.0f, 0.0f, 0.0f },
    { 0.0f, 0.0f, 0.0f, 0.0f, 0.1f, 0.1f, 0.1f, 0.0f, 0.0f, 0.0f, 0.0f },
    { 0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f },
    { 0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f },
    { 0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f },
    { 0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f },
};

float Fw(int r, int c, float fL)
{
    return (1.0 - fL) * (1.0 - fL) * (1.0 - fL) * W0[r][c] +
        3.0f * (1.0 - fL) * (1.0 - fL) * fL * W1[r][c] +
        3.0f * fL * fL * (1.0 - fL) * W2[r][c] +
        fL * fL * fL * 1.0f;
}

    #define BLOCKER_FILTER_SIZE 11
    #define BFS BLOCKER_FILTER_SIZE
    #define BFS2 (BLOCKER_FILTER_SIZE / 2)

    #define SUN_WIDTH 300.0f

// uses gather for DX11/10.1 and visibilty encoding for DX10.0
float shadow_extreme_quality(float3 tc)
{
    float s = 0.0f;
    float2 stc = (SMAP_size * tc.xy) + float2(0.5, 0.5);
    float2 tcs = floor(stc);
    float2 fc;
    int row;
    int col;
    float w = 0.0;
    float avgBlockerDepth = 0;
    float blockerCount = 0;
    float fRatio;
    float4 v1[FS2 + 1];
    float2 v0[FS2 + 1];
    float2 off;

    fc = stc - tcs;
    tc.xy = tc.xy - ((1.0f / SMAP_size) * fc);
    tc.z -= 0.0001f;

    #if defined(SM_4_1) || defined(SM_5)
    // find number of blockers and sum up blocker depth
    for (row = -BFS2; row <= BFS2; row += 2)
    {
        for (col = -BFS2; col <= BFS2; col += 2)
        {
            float4 d4 = s_smap.Gather(smp_nofilter, tc.xy, int2(col, row));
            float4 b4 = (tc.zzzz <= d4) ? (0.0f).xxxx : (1.0f).xxxx;

            blockerCount += dot(b4, (1.0f).xxxx);
            avgBlockerDepth += dot(d4, b4);
        }
    }
    #else // SM_4_0
    uint vmask[FS + 1];

    [unroll]
    for (col = 0; col <= FS; ++col)
    {
        vmask[col] = uint(0);
    }

    [unroll(11)]
    for (row = -FS2; row <= FS2; row += 2)
    {
        [unroll]
        for (int col = -FS2; col <= FS2; col += 2)
        {
            float4 d4;
            float b;

            d4.w = s_smap.SampleLevel(smp_nofilter, tc.xy, 0, int2(col, row)).x;
            b = (tc.z <= d4.w) ? (0.0f) : (1.0f);
            vmask[col + FS2 + 0] += ((tc.z <= d4.w) ? (uint(1) << uint(row + FS2 + 0)) : uint(0));
            blockerCount += b;
            avgBlockerDepth += d4.w * b;

            d4.z = s_smap.SampleLevel(smp_nofilter, tc.xy, 0, int2(col + 1, row)).x;
            b = (tc.z <= d4.z) ? (0.0f) : (1.0f);
            vmask[col + FS2 + 1] += ((tc.z <= d4.z) ? (uint(1) << uint(row + FS2 + 0)) : uint(0));
            blockerCount += b;
            avgBlockerDepth += d4.z * b;

            d4.x = s_smap.SampleLevel(smp_nofilter, tc.xy, 0, int2(col, row + 1)).x;
            vmask[col + FS2 + 0] += ((tc.z <= d4.x) ? (uint(1) << uint(row + FS2 + 1)) : uint(0));
            b = (tc.z <= d4.x) ? (0.0f) : (1.0f);
            blockerCount += b;
            avgBlockerDepth += d4.x * b;

            d4.y = s_smap.SampleLevel(smp_nofilter, tc.xy, 0, int2(col + 1, row + 1)).x;
            vmask[col + FS2 + 1] += ((tc.z <= d4.y) ? (uint(1) << uint(row + FS2 + 1)) : uint(0));
            b = (tc.z <= d4.y) ? (0.0f) : (1.0f);
            blockerCount += b;
            avgBlockerDepth += d4.y * b;
        }
    }
    #endif

    // compute ratio average blocker depth vs. pixel depth
    if (blockerCount > 0.0)
    {
        avgBlockerDepth /= blockerCount;
        fRatio = saturate(((tc.z - avgBlockerDepth) * SUN_WIDTH) / avgBlockerDepth);
        fRatio *= fRatio;
    }
    else
    {
        fRatio = 0.0;
    }

    for (row = 0; row < FS; ++row)
    {
        for (col = 0; col < FS; ++col)
        {
            w += Fw(row, col, fRatio);
        }
    }

    // filter shadow map samples using the dynamic weights
    [unroll(11)]
    for (row = -FS2; row <= FS2; row += 2)
    {
        [unroll]
        for (int col = -FS2; col <= FS2; col += 2)
        {
    #if (defined(SM_5)) || (defined(SM_4_1))
        #ifdef SM_5
            v1[(col + FS2) / 2] = s_smap.GatherCmpRed(smp_smap, tc.xy, tc.z,
                                                      int2(col, row));
        #else // SM_4_1
            float4 d4 = s_smap.Gather(smp_linear, tc.xy, int2(col, row));
            v1[(col + FS2) / 2] = (tc.zzzz <= d4) ? (1.0f).xxxx : (0.0f).xxxx;
        #endif
    #else
            v1[(col + FS2) / 2].w = ((vmask[col + FS2 + 0] & (uint(1) << uint(row + FS2 + 0))) ? 1.0f : 0.0f);
            v1[(col + FS2) / 2].z = ((vmask[col + FS2 + 1] & (uint(1) << uint(row + FS2 + 0))) ? 1.0f : 0.0f);
            v1[(col + FS2) / 2].x = ((vmask[col + FS2 + 0] & (uint(1) << uint(row + FS2 + 1))) ? 1.0f : 0.0f);
            v1[(col + FS2) / 2].y = ((vmask[col + FS2 + 1] & (uint(1) << uint(row + FS2 + 1))) ? 1.0f : 0.0f);
    #endif
            if (col == -FS2)
            {
                s += (1 - fc.y) * (v1[0].w * (Fw(row + FS2, 0, fRatio) - Fw(row + FS2, 0, fRatio) * fc.x) + v1[0].z * (fc.x * (Fw(row + FS2, 0, fRatio) - Fw(row + FS2, 1, fRatio)) + Fw(row + FS2, 1, fRatio)));
                s += (fc.y) * (v1[0].x * (Fw(row + FS2, 0, fRatio) - Fw(row + FS2, 0, fRatio) * fc.x) + v1[0].y * (fc.x * (Fw(row + FS2, 0, fRatio) - Fw(row + FS2, 1, fRatio)) + Fw(row + FS2, 1, fRatio)));
                if (row > -FS2)
                {
                    s += (1 - fc.y) * (v0[0].x * (Fw(row + FS2 - 1, 0, fRatio) - Fw(row + FS2 - 1, 0, fRatio) * fc.x) + v0[0].y * (fc.x * (Fw(row + FS2 - 1, 0, fRatio) - Fw(row + FS2 - 1, 1, fRatio)) + Fw(row + FS2 - 1, 1, fRatio)));
                    s += (fc.y) * (v1[0].w * (Fw(row + FS2 - 1, 0, fRatio) - Fw(row + FS2 - 1, 0, fRatio) * fc.x) + v1[0].z * (fc.x * (Fw(row + FS2 - 1, 0, fRatio) - Fw(row + FS2 - 1, 1, fRatio)) + Fw(row + FS2 - 1, 1, fRatio)));
                }
            }
            else if (col == FS2)
            {
                s += (1 - fc.y) * (v1[FS2].w * (fc.x * (Fw(row + FS2, FS - 2, fRatio) - Fw(row + FS2, FS - 1, fRatio)) + Fw(row + FS2, FS - 1, fRatio)) + v1[FS2].z * fc.x * Fw(row + FS2, FS - 1, fRatio));
                s += (fc.y) * (v1[FS2].x * (fc.x * (Fw(row + FS2, FS - 2, fRatio) - Fw(row + FS2, FS - 1, fRatio)) + Fw(row + FS2, FS - 1, fRatio)) + v1[FS2].y * fc.x * Fw(row + FS2, FS - 1, fRatio));
                if (row > -FS2)
                {
                    s += (1 - fc.y) * (v0[FS2].x * (fc.x * (Fw(row + FS2 - 1, FS - 2, fRatio) - Fw(row + FS2 - 1, FS - 1, fRatio)) + Fw(row + FS2 - 1, FS - 1, fRatio)) + v0[FS2].y * fc.x * Fw(row + FS2 - 1, FS - 1, fRatio));
                    s += (fc.y) * (v1[FS2].w * (fc.x * (Fw(row + FS2 - 1, FS - 2, fRatio) - Fw(row + FS2 - 1, FS - 1, fRatio)) + Fw(row + FS2 - 1, FS - 1, fRatio)) + v1[FS2].z * fc.x * Fw(row + FS2 - 1, FS - 1, fRatio));
                }
            }
            else
            {
                s += (1 - fc.y) * (v1[(col + FS2) / 2].w * (fc.x * (Fw(row + FS2, col + FS2 - 1, fRatio) - Fw(row + FS2, col + FS2 + 0, fRatio)) + Fw(row + FS2, col + FS2 + 0, fRatio)) +
                                   v1[(col + FS2) / 2].z * (fc.x * (Fw(row + FS2, col + FS2 - 0, fRatio) - Fw(row + FS2, col + FS2 + 1, fRatio)) + Fw(row + FS2, col + FS2 + 1, fRatio)));
                s += (fc.y) * (v1[(col + FS2) / 2].x * (fc.x * (Fw(row + FS2, col + FS2 - 1, fRatio) - Fw(row + FS2, col + FS2 + 0, fRatio)) + Fw(row + FS2, col + FS2 + 0, fRatio)) +
                               v1[(col + FS2) / 2].y * (fc.x * (Fw(row + FS2, col + FS2 - 0, fRatio) - Fw(row + FS2, col + FS2 + 1, fRatio)) + Fw(row + FS2, col + FS2 + 1, fRatio)));
                if (row > -FS2)
                {
                    s += (1 - fc.y) * (v0[(col + FS2) / 2].x * (fc.x * (Fw(row + FS2 - 1, col + FS2 - 1, fRatio) - Fw(row + FS2 - 1, col + FS2 + 0, fRatio)) + Fw(row + FS2 - 1, col + FS2 + 0, fRatio)) +
                                       v0[(col + FS2) / 2].y * (fc.x * (Fw(row + FS2 - 1, col + FS2 - 0, fRatio) - Fw(row + FS2 - 1, col + FS2 + 1, fRatio)) + Fw(row + FS2 - 1, col + FS2 + 1, fRatio)));
                    s += (fc.y) * (v1[(col + FS2) / 2].w * (fc.x * (Fw(row + FS2 - 1, col + FS2 - 1, fRatio) - Fw(row + FS2 - 1, col + FS2 + 0, fRatio)) + Fw(row + FS2 - 1, col + FS2 + 0, fRatio)) +
                                   v1[(col + FS2) / 2].z * (fc.x * (Fw(row + FS2 - 1, col + FS2 - 0, fRatio) - Fw(row + FS2 - 1, col + FS2 + 1, fRatio)) + Fw(row + FS2 - 1, col + FS2 + 1, fRatio)));
                }
            }
            if (row != FS2)
            {
                v0[(col + FS2) / 2] = v1[(col + FS2) / 2].xy;
            }
        }
    }

    return s / w;
}

float4 Fw(int r, int c)
{
    return float4(W0[r][c], W1[r][c], W2[r][c], 1.0f);
}

//======================================================================================
// This shader computes the contact hardening shadow filter
//======================================================================================
float shadow_extreme_quality_fused(float3 tc)
{
    float4 s = (0.0f).xxxx;
    float2 stc = (SMAP_size * tc.xy) + float2(0.5, 0.5);
    float2 tcs = floor(stc);
    float2 fc;
    int row;
    int col;
    float w = 0.0;
    float avgBlockerDepth = 0;
    float blockerCount = 0;
    float fRatio;
    float4 v1[FS2 + 1];
    float2 v0[FS2 + 1];
    float2 off;

    fc = stc - tcs;
    tc.xy = tc.xy - (fc * (1.0f / SMAP_size));

    // filter shadow map samples using the dynamic weights
    [unroll(FS)]
    for (row = -FS2; row <= FS2; row += 2)
    {
        for (col = -FS2; col <= FS2; col += 2)
        {
            float4 d4;

    #ifndef PS_4
            d4 = s_smap.Gather(smp_nofilter, tc.xy + (1.0f / SMAP_size) * float2(col, row));
    #else
            d4.w = s_smap.SampleLevel(smp_nofilter, tc.xy + (1.0f / SMAP_size) * float2(col, row), 0).x;
            d4.z = s_smap.SampleLevel(smp_nofilter, tc.xy + (1.0f / SMAP_size) * float2(col + 1, row), 0).x;
            d4.y = s_smap.SampleLevel(smp_nofilter, tc.xy + (1.0f / SMAP_size) * float2(col + 1, row + 1), 0).x;
            d4.x = s_smap.SampleLevel(smp_nofilter, tc.xy + (1.0f / SMAP_size) * float2(col, row + 1), 0).x;
    #endif
            float4 b4 = (tc.zzzz <= d4) ? (0.0f).xxxx : (1.0f).xxxx;

            v1[(col + FS2) / 2] = (tc.zzzz <= d4) ? (1.0f).xxxx : (0.0f).xxxx;
            blockerCount += dot(b4, (1.0).xxxx);
            avgBlockerDepth += dot(d4, b4);

            if (col == -FS2)
            {
                s += (1 - fc.y) * (v1[0].w * (Fw(row + FS2, 0) -
                                              Fw(row + FS2, 0) * fc.x) +
                                   v1[0].z *
                                       (fc.x * (Fw(row + FS2, 0) -
                                                Fw(row + FS2, 1)) +
                                        Fw(row + FS2, 1)));
                s += (fc.y) * (v1[0].x * (Fw(row + FS2, 0) -
                                          Fw(row + FS2, 0) * fc.x) +
                               v1[0].y * (fc.x * (Fw(row + FS2, 0) -
                                                  Fw(row + FS2, 1)) +
                                          Fw(row + FS2, 1)));
                if (row > -FS2)
                {
                    s += (1 - fc.y) * (v0[0].x * (Fw(row + FS2 - 1, 0) -
                                                  Fw(row + FS2 - 1, 0) * fc.x) +
                                       v0[0].y *
                                           (fc.x * (Fw(row + FS2 - 1, 0) -
                                                    Fw(row + FS2 - 1, 1)) +
                                            Fw(row + FS2 - 1, 1)));
                    s += (fc.y) * (v1[0].w * (Fw(row + FS2 - 1, 0) -
                                              Fw(row + FS2 - 1, 0) * fc.x) +
                                   v1[0].z *
                                       (fc.x * (Fw(row + FS2 - 1, 0) -
                                                Fw(row + FS2 - 1, 1)) +
                                        Fw(row + FS2 - 1, 1)));
                }
            }
            else if (col == FS2)
            {
                s += (1 - fc.y) * (v1[FS2].w * (fc.x * (Fw(row + FS2, FS - 2) -
                                                        Fw(row + FS2, FS - 1)) +
                                                Fw(row + FS2, FS - 1)) +
                                   v1[FS2].z * fc.x *
                                       Fw(row + FS2, FS - 1));
                s += (fc.y) * (v1[FS2].x * (fc.x * (Fw(row + FS2, FS - 2) -
                                                    Fw(row + FS2, FS - 1)) +
                                            Fw(row + FS2, FS - 1)) +
                               v1[FS2].y * fc.x *
                                   Fw(row + FS2, FS - 1));
                if (row > -FS2)
                {
                    s += (1 - fc.y) * (v0[FS2].x * (fc.x *
                                                        (Fw(row + FS2 - 1, FS - 2) -
                                                         Fw(row + FS2 - 1, FS - 1)) +
                                                    Fw(row + FS2 - 1, FS - 1)) +
                                       v0[FS2].y * fc.x * Fw(row + FS2 - 1, FS - 1));
                    s += (fc.y) * (v1[FS2].w * (fc.x *
                                                    (Fw(row + FS2 - 1, FS - 2) -
                                                     Fw(row + FS2 - 1, FS - 1)) +
                                                Fw(row + FS2 - 1, FS - 1)) +
                                   v1[FS2].z * fc.x * Fw(row + FS2 - 1, FS - 1));
                }
            }
            else
            {
                s += (1 - fc.y) * (v1[(col + FS2) / 2].w * (fc.x *
                                                                (Fw(row + FS2, col + FS2 - 1) -
                                                                 Fw(row + FS2, col + FS2 + 0)) +
                                                            Fw(row + FS2, col + FS2 + 0)) +
                                   v1[(col + FS2) / 2].z * (fc.x *
                                                                (Fw(row + FS2, col + FS2 - 0) -
                                                                 Fw(row + FS2, col + FS2 + 1)) +
                                                            Fw(row + FS2, col + FS2 + 1)));
                s += (fc.y) * (v1[(col + FS2) / 2].x * (fc.x *
                                                            (Fw(row + FS2, col + FS2 - 1) -
                                                             Fw(row + FS2, col + FS2 + 0)) +
                                                        Fw(row + FS2, col + FS2 + 0)) +
                               v1[(col + FS2) / 2].y * (fc.x *
                                                            (Fw(row + FS2, col + FS2 - 0) -
                                                             Fw(row + FS2, col + FS2 + 1)) +
                                                        Fw(row + FS2, col + FS2 + 1)));
                if (row > -FS2)
                {
                    s += (1 - fc.y) * (v0[(col + FS2) / 2].x * (fc.x *
                                                                    (Fw(row + FS2 - 1, col + FS2 - 1) -
                                                                     Fw(row + FS2 - 1, col + FS2 + 0)) +
                                                                Fw(row + FS2 - 1, col + FS2 + 0)) +
                                       v0[(col + FS2) / 2].y * (fc.x *
                                                                    (Fw(row + FS2 - 1, col + FS2 - 0) -
                                                                     Fw(row + FS2 - 1, col + FS2 + 1)) +
                                                                Fw(row + FS2 - 1, col + FS2 + 1)));
                    s += (fc.y) * (v1[(col + FS2) / 2].w * (fc.x *
                                                                (Fw(row + FS2 - 1, col + FS2 - 1) -
                                                                 Fw(row + FS2 - 1, col + FS2 + 0)) +
                                                            Fw(row + FS2 - 1, col + FS2 + 0)) +
                                   v1[(col + FS2) / 2].z * (fc.x *
                                                                (Fw(row + FS2 - 1, col + FS2 - 0) -
                                                                 Fw(row + FS2 - 1, col + FS2 + 1)) +
                                                            Fw(row + FS2 - 1, col + FS2 + 1)));
                }
            }

            if (row != FS2)
            {
                v0[(col + FS2) / 2] = v1[(col + FS2) / 2].xy;
            }
        }
    }

    // compute ratio using formulas from PCSS
    if (blockerCount > 0.0)
    {
        avgBlockerDepth /= blockerCount;
        fRatio = saturate(((tc.z - avgBlockerDepth) * SUN_WIDTH) / avgBlockerDepth);
        fRatio *= fRatio;
    }
    else
    {
        fRatio = 0.0;
    }

    // sum up weights of dynamic filter matrix
    for (row = 0; row < FS; ++row)
    {
        for (col = 0; col < FS; ++col)
        {
            w += Fw(row, col, fRatio);
        }
    }

    return dot(s, float4((1.0f - fRatio) * (1.0f - fRatio) * (1.0f - fRatio),
                         3.0f * (1.0 - fRatio) * (1.0 - fRatio) * fRatio,
                         3.0f * fRatio * fRatio * (1.0 - fRatio),
                         fRatio * fRatio * fRatio)) /
           w;
}
#endif

#ifdef SM_4_1

float dx10_1_hw_hq_7x7(float3 tc)
{
    float s = 0.0f;
    float2 stc = (SMAP_size * tc.xy) + float2(0.5, 0.5);
    float2 tcs = floor(stc);
    float2 fc;
    int row;
    int col;

    fc.xy = stc - tcs;
    tc.xy = tcs * (1.0 / SMAP_size);

    // loop over the rows
    for (row = -GS2; row <= GS2; row += 2)
    {
        [unroll] for (col = -GS2; col <= GS2; col += 2)
        {
            float4 v = (tc.zzzz <= s_smap.Gather(smp_nofilter, tc.xy, int2(col, row))) ? (1.0).xxxx : (0.0).xxxx;

            if (row == -GS2) // top row
            {
                if (col == -GS2) // left
                {
                    s += dot(float4(1.0 - fc.x, 1.0, 1.0 - fc.y, (1.0 - fc.x) * (1.0 - fc.y)), v);
                }
                else if (col == GS2) // right
                {
                    s += dot(float4(1.0f, fc.x, fc.x * (1.0 - fc.y), 1.0 - fc.y), v);
                }
                else // center
                {
                    s += dot(float4(1.0, 1.0, 1.0 - fc.y, 1.0 - fc.y), v);
                }
            }
            else if (row == GS2) // bottom row
            {
                if (col == -GS2) // left
                {
                    s += dot(float4((1.0 - fc.x) * fc.y, fc.y, 1.0, (1.0 - fc.x)), v);
                }
                else if (col == GS2) // right
                {
                    s += dot(float4(fc.y, fc.x * fc.y, fc.x, 1.0), v);
                }
                else // center
                {
                    s += dot(float4(fc.yy, 1.0, 1.0), v);
                }
            }
            else // center rows
            {
                if (col == -GS2) // left
                {
                    s += dot(float4((1.0 - fc.x), 1.0, 1.0, (1.0 - fc.x)), v);
                }
                else if (col == GS2) // right
                {
                    s += dot(float4(1.0, fc.x, fc.x, 1.0), v);
                }
                else // center
                {
                    s += dot((1.0).xxxx, v);
                }
            }
        }
    }

    return s * (1.0 / 49.0);
}

#endif

float dx10_0_hw_hq_7x7(float4 tc)
{
    tc.xyz /= tc.w;

    float s = 0.0;
    float2 stc = (SMAP_size * tc.xy) + float2(0.5, 0.5);
    float2 tcs = floor(stc);
    float2 fc;

    fc = stc - tcs;
    tc.xy = tc.xy - (fc * (1.0 / SMAP_size));

    float2 pwAB = ((2.0).xx - fc);
    float2 tcAB = (1.0 / SMAP_size).xx / pwAB;
    float2 tcM = (0.5 / SMAP_size).xx;
    float2 pwGH = ((1.0).xx + fc);
    float2 tcGH = (1.0 / SMAP_size) * (fc / pwGH);

    for (int row = -GS2; row <= GS2; row += 2)
    {
        for (int col = -GS2; col <= GS2; col += 2)
        {
            if (row == -GS2) // top row
            {
                if (col == -GS2) // left
                {
                    s += (pwAB.x * pwAB.y) * s_smap.SampleCmpLevelZero(smp_smap, tc.xy + tcAB, tc.z, int2(col, row)).x;
                }
                else if (col == GS2) // right
                {
                    s += (pwGH.x * pwAB.y) * s_smap.SampleCmpLevelZero(smp_smap, tc.xy + float2(tcGH.x, tcAB.y), tc.z, int2(col, row)).x;
                }
                else // center
                {
                    s += (2.0 * pwAB.y) * s_smap.SampleCmpLevelZero(smp_smap, tc.xy + float2(tcM.x, tcAB.y), tc.z, int2(col, row)).x;
                }
            }
            else if (row == GS2) // bottom row
            {
                if (col == -GS2) // left
                {
                    s += (pwAB.x * pwGH.y) * s_smap.SampleCmpLevelZero(smp_smap, tc.xy + float2(tcAB.x, tcGH.y), tc.z, int2(col, row)).x;
                }
                else if (col == GS2) // right
                {
                    s += (pwGH.x * pwGH.y) * s_smap.SampleCmpLevelZero(smp_smap, tc.xy + tcGH, tc.z, int2(col, row)).x;
                }
                else // center
                {
                    s += (2.0 * pwGH.y) * s_smap.SampleCmpLevelZero(smp_smap, tc.xy + float2(tcM.x, tcGH.y), tc.z, int2(col, row)).x;
                }
            }
            else // center rows
            {
                if (col == -GS2) // left
                {
                    s += (pwAB.x * 2.0) * s_smap.SampleCmpLevelZero(smp_smap, tc.xy + float2(tcAB.x, tcM.y), tc.z, int2(col, row)).x;
                }
                else if (col == GS2) // right
                {
                    s += (pwGH.x * 2.0) * s_smap.SampleCmpLevelZero(smp_smap, tc.xy + float2(tcGH.x, tcM.y), tc.z, int2(col, row)).x;
                }
                else // center
                {
                    s += (2.0 * 2.0) * s_smap.SampleCmpLevelZero(smp_smap, tc.xy + tcM, tc.z, int2(col, row)).x;
                }
            }
        }
    }

    return s / 49.0;
}

float shadow_hw_hq(float4 tc)
{
#if SUN_QUALITY >= 4 // extreme quality
    return shadow_extreme_quality(tc.xyz / tc.w);
#else // SUN_QUALITY<4
    #ifdef SM_4_1
    return dx10_1_hw_hq_7x7(tc.xyz / tc.w);
    #else // SM_4_1
    return dx10_0_hw_hq_7x7(tc);
    #endif // SM_4_1
#endif // SUN_QUALITY==4
}

//////////////////////////////////////////////////////////////////////////////////////////
//	D24X8+PCF
//////////////////////////////////////////////////////////////////////////////////////////

float4 test(float4 tc, float2 offset)
{
    tc.xyz /= tc.w;
    tc.xy += offset;
    return s_smap.SampleCmpLevelZero(smp_smap, tc.xy, tc.z).x;
}

float shadowtest_sun(float4 tc, float4 tcJ) // jittered sampling
{
    float4 r;

    //	const 	float 	scale 	= (2.0f/float(SMAP_size));
    const float scale = (0.7f / float(SMAP_size));

    float2 tc_J = frac(tc.xy / tc.w * SMAP_size / 4.0f) * 0.5f;
    float4 J0 = jitter0.Sample(smp_jitter, tc_J) * scale;
    // float4	J1 	= tex2D	(jitter1,tc_J)*scale;

    const float k = 0.5f / float(SMAP_size);
    r.x = test(tc, J0.xy + float2(-k, -k)).x;
    r.y = test(tc, J0.wz + float2(k, -k)).y;
    r.z = test(tc, -J0.xy + float2(-k, k)).z;
    r.w = test(tc, -J0.wz + float2(k, k)).x;

    return dot(r, 1.0f / 4.0f);
}

// jittered sampling
float shadow_high(float4 tc)
{
    const float scale = (0.5f / float(SMAP_size));

    float2 tc_J = frac(tc.xy / tc.w * SMAP_size / 4.0f) * .5f;
    float4 J0 = jitter0.Sample(smp_jitter, tc_J) * scale;

    const float k = 1.0f / float(SMAP_size);
    float4 r;
    r.x = test(tc, J0.xy + float2(-k, -k)).x;
    r.y = test(tc, J0.wz + float2(k, -k)).y;

    r.z = test(tc, J0.xy + float2(-k, k)).z;
    r.w = test(tc, J0.wz + float2(k, k)).x;

    const float k1 = 1.3f / float(SMAP_size);
    float4 r1;
    r1.x = test(tc, -J0.xy + float2(-k1, 0)).x;
    r1.y = test(tc, -J0.wz + float2(0, -k1)).y;

    r1.z = test(tc, -2 * J0.xy + float2(k1, 0)).z;
    r1.w = test(tc, -2 * J0.wz + float2(0, k1)).x;

    return (r.x + r.y + r.z + r.w + r1.x + r1.y + r1.z + r1.w) * 1.0f / 8.0f;
}

float shadow(float4 tc)
{
#ifdef USE_ULTRA_SHADOWS
    return shadow_hw_hq(tc);
#else
    #if SUN_QUALITY >= 2
    return shadow_hw(tc);
    #else
    return shadow_hw(tc);
    #endif
#endif
}

float shadow_volumetric(float4 tc)
{
    return sample_hw_pcf(tc, float4(-1, -1, 0, 0));
}

// testbed

float shadowtest(float4 tc, float4 tcJ) // jittered sampling
{
    float4 r;

    const float scale = (2.7f / float(SMAP_size));

    tcJ.xy /= tcJ.w;
    float4 J0 = jitter0.Sample(smp_jitter, tcJ) * scale;
    float4 J1 = jitter1.Sample(smp_jitter, tcJ) * scale;

    r.x = test(tc, J0.xy).x;
    r.y = test(tc, J0.wz).y;
    r.z = test(tc, J1.xy).z;
    r.w = test(tc, J1.wz).x;

    return dot(r, 1.h / 4.h);
}

float shadow_rain(float4 tc, float2 tcJ) // jittered sampling
{
    float4 r;

    const float scale = (4.0f / float(SMAP_size));
    //	float4	J0 	= jitter0.Sample( smp_jitter, tcJ )*scale;
    //	float4	J1 	= jitter1.Sample( smp_jitter, tcJ )*scale;
    float4 J0 = jitter0.Sample(smp_linear, tcJ) * scale;
    float4 J1 = jitter1.Sample(smp_linear, tcJ) * scale;

    r.x = test(tc, J0.xy).x;
    r.y = test(tc, J0.wz).y;
    r.z = test(tc, J1.xy).z;
    r.w = test(tc, J1.wz).x;

    return dot(r, 1.h / 4.h);
}

#ifdef USE_SUNMASK
float3x4 m_sunmask;

float sunmask(float4 P)
{
    float2 tc = mul(m_sunmask, P);
    return lerp(0.25f, 1.0f, s_lmap.SampleLevel(smp_linear, tc, 0).w);
}
#else
float sunmask(float4 P)
{
    return 1.f;
}
#endif
#endif

