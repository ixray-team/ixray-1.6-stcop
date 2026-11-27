/**************************************************************************************
* Copyright (C) 2026 IXRay-Team
* IX-Ray Shaders
***************************************************************************************
* Anyone may use, copy, modify and distribute this shader file, changed or
* unchanged, as part of the IX-Ray engine, its games and their modifications.
* It may not be sold on its own.
*
* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND. THE COPYRIGHT
* HOLDERS ARE NOT LIABLE FOR ANY CLAIM OR DAMAGE ARISING FROM IT.
*
* The above copyright notice and this license text must be included in all
* copies or substantial portions of this file.
**************************************************************************************/
#include "common\common.hlsli"
#include "common\post_cb.hlsli"

Texture2D<float> t_gtao_current : register(t25);
Texture2D<float2> t_gtao_history : register(t26);
RWTexture2D<unorm float> u_occ : register(u0);
RWTexture2D<float2> u_history : register(u1);

// The velocity target is built after this pass: a point moves by the camera alone, the HUD stays where it is
[numthreads(8, 8, 1)]
void cs_main(uint2 pixel : SV_DispatchThreadID)
{
    int2 last = int2(pos_decompression_params2.xy) - 1;

    if (any(int2(pixel) > last)) {
        return;
    }

    float depth = s_position.Load(int3(pixel, 0)).x;
    float current = t_gtao_current[pixel];
    float view_z = depth < 1.0f ? GbufferGetViewZ(depth) : 1000.0f;
    float result = current;

    [branch]
    if (gtao_history.x > 0.0f && depth < 1.0f) {
        float2 uv = (float2(pixel) + 0.5f) * pos_decompression_params2.zw;
        float2 uv_prev = uv;
        float z_prev = view_z;

        if (depth >= 0.02f) {
            float4 prev = mul(m_VP_old, float4(mul(m_invV, float4(GbufferGetPointRealUnjitter(uv, depth), 1.0f)).xyz, 1.0f));
            uv_prev = prev.xy / prev.w * float2(0.5f, -0.5f) + 0.5f;
            z_prev = prev.w;
        }

        float low = current;
        float high = current;

        [unroll]
        for (int i = 0; i < 9; ++i) {
            float tap = t_gtao_current[clamp(int2(pixel) + int2(i % 3, i / 3) - 1, 0, last)];
            low = min(low, tap);
            high = max(high, tap);
        }

        float2 target_uv = ViewToTargetBefore(uv_prev);
        float4 taps = abs(t_gtao_history.GatherGreen(smp_rtlinear, target_uv) - z_prev);
        float miss = min(min(taps.x, taps.y), min(taps.z, taps.w)) / (0.01f * z_prev + 0.02f);
        float keep = GetBorderAtten(uv_prev) ? saturate(2.0f - miss) : 0.0f;
        float history = clamp(t_gtao_history.SampleLevel(smp_rtlinear, target_uv, 0.0f).x, low, high);

        result = lerp(current, history, gtao_history.x * keep);
    }

    u_occ[pixel] = result;
    u_history[pixel] = float2(result, view_z);
}
