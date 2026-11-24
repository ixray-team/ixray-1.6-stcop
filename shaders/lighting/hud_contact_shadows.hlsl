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
#include "common\thin_surface.hlsli"
#include "lighting\ScreenSpaceContactShadows.hlsl"

#define HUD_CONTACT_STEPS 24.0f

// returns how much of the sun reaches a point of the HUD past the rest of the HUD: a march through the HUD's depth
// after raymarch.hlsl by Tomasz Stachowiak (Tiny Glade), dual licensed Apache-2.0 / MIT, as SunContactOcclusion of
// ScreenSpaceContactShadows.hlsl is for the world.
// The ray is a line on the screen along which the screen position and 1 / z are both linear; it takes a step a
// pixel at most. A step is in shadow only where the ray is behind both the depth as the texels have it and the
// depth smoothed between them: the texels alone shadow a smooth surface by its own steps, the smoothed depth alone
// stretches a skin between unrelated things. A ray further behind the nearer of the two than the weapon is thick
// has passed behind the thing, not into it. A thickness of a rib of the handguard would shadow the hand by stripes,
// and one of a hand would let the sun past the weapon onto the hand that holds it.
// The start of the ray is shifted by a pattern that repeats every 4x4 pixels, not by noise: hud_contact_filter
// averages a box of 4x4 pixels, which holds every shift once, and this frame has no temporal pass to settle noise
float hud_trace(float2 uv, float3 position, float3 normal, float3 sun_dir)
{
    static const float pattern[16] = { 0.0f, 8.0f, 2.0f, 10.0f, 12.0f, 4.0f, 14.0f, 6.0f, 3.0f, 11.0f, 1.0f, 9.0f, 15.0f, 7.0f, 13.0f, 5.0f };
    uint2 pixel = uint2(uv * pos_decompression_params2.xy);
    float shift = (pattern[(pixel.x & 3u) + (pixel.y & 3u) * 4u] + 0.5f) / 16.0f;

    // At a silhouette the depth around the pixel gives no surface: the shaded normal stands in
    float3 flat = surface_normal(int2(pixel), position, normal);
    flat = any(flat) ? flat : normal;
    float plane = dot(flat, position);

    position *= 0.999f;
    float4 start = mul(m_P_hud, float4(position, 1.0f));
    float4 end = mul(m_P_hud, float4(position - sun_dir * min(position.z, HUD_SHADOWS_TRACE_LEN), 1.0f));
    float3 ray_delta = float3((end.xy / end.w - start.xy / start.w) * float2(0.5f, -0.5f), rcp(end.w) - rcp(start.w));

    float2 exit = max((1.0f - uv) / ray_delta.xy, -uv / ray_delta.xy);
    ray_delta *= min(1.0f, min(exit.x, exit.y));

    float pixels = length(ray_delta.xy * pos_decompression_params2.xy);
    if (pixels < 1.0f) {
        return 1.0f;
    }

    float steps = clamp(floor(pixels), 2.0f, HUD_CONTACT_STEPS);
    float thickness = 0.25f;
    float bias = 0.0005f + position.z * 0.002f;
    float hidden = 0.0f;

    [loop]
    for (float step_idx = 0.0f; step_idx < steps && hidden < 1.0f; ++step_idx) {
        float3 ray = float3(uv, rcp(start.w)) + ray_delta * ((step_idx + shift) / steps);

        // Beside the HUD is the world, far behind it
        float4 depth = s_position.GatherRed(smp_nofilter, ray.xy * view_uv.xy);
        if (any(depth >= 0.02f)) {
            continue;
        }

        float2 part = frac(ray.xy * pos_decompression_params2.xy - 0.5f);
        float4 texel_z = rcp(m_invP_hud._43 * depth * 50.0f + m_invP_hud._44);
        float smooth_z = rcp(m_invP_hud._43 * lerp(lerp(depth.w, depth.z, part.x), lerp(depth.x, depth.y, part.x), part.y) * 50.0f + m_invP_hud._44);
        float nearest_z = part.y < 0.5f ? (part.x < 0.5f ? texel_z.w : texel_z.z) : (part.x < 0.5f ? texel_z.x : texel_z.y);
        float ray_z = rcp(ray.z);

        // What lies in the surface's own plane is the surface, unless the ray is well behind it: near a silhouette the
        // plane the depth gives a wrist leans away from the sun, and the weapon in front of it was taken for the
        // wrist itself
        float2 ndc = float2(ray.x * 2.0f - 1.0f, 1.0f - ray.y * 2.0f) - m_taa_jitter.xy;
        if (min(smooth_z, nearest_z) * dot(flat, float3(ndc.x * m_invP_hud._11, ndc.y * m_invP_hud._22, 1.0f)) - plane < bias && ray_z - min(smooth_z, nearest_z) < 0.02f) {
            continue;
        }

        // A seventh of the ray behind something hides the sun; less than that dims it. One step behind a fold of
        // the surface the ray leaves is not a shadow: a surface near its own terminator would go dark all over
        float behind = saturate((ray_z - max(smooth_z, nearest_z)) / bias - 1.0f);
        hidden += behind * smoothstep(thickness, 0.5f * thickness, ray_z - min(smooth_z, nearest_z)) * 7.0f / steps;
    }

    return 1.0f - saturate(hidden);
}

// returns how much of the sun reaches a pixel of the HUD past the rest of the HUD, traced through its depth; 1 for
// every other pixel. The forward level frame has no other pass that could shadow the weapon by the hands
float ps_main(PSInputFullscreen input) : SV_Target
{
    float depth = s_position.Load(int3(input.hpos.xy, 0)).x;
    if (depth >= 0.02f) {
        return 1.0f;
    }

    // The trace needs the surface's own normal, which the depth pass writes at high shading quality only: a normal
    // built from the depth is flat a triangle, and the trace finds the surface in its own way all over it
#ifndef USE_SURFACE_OCCLUSION_NORMALS
    return 1.0f;
#else
    // The triangle gives the target's UV; the trace works in shares of the view
    float2 uv = input.texcoord / view_uv.xy;
    float3 position = uv_to_view(uv, depth);
    float3 normal = thin_unpack(s_thin.Load(int3(input.hpos.xy, 0))).normal;
    float3 sun_dir = L_sun_dir_e.xyz;

    // What faces away from the sun is not traced: r_lambert_factor lets the sun reach past the terminator, and that
    // light is not shadowed
    if (dot(normal, sun_dir) >= 0.0f) {
        return 1.0f;
    }

    return hud_trace(uv, position, normal, sun_dir);
#endif
}
