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
#include "common\surface_forward.hlsli"
#define SURFACE_NO_DECALS
#define SURFACE_NO_DEBUG_VIEW

// returns the surface under the pixel
surface_data fetch_entry(surface_vertex input)
{
    surface_data surface;
    surface.local = 0.0f;
    surface.sun = 0.0f;
    surface.base = r1_sample_base(input.uv.xy) * input.tint;
#if defined(USE_SURFACE_SOFT_PARTICLES) && !defined(BILLBOARD_GLOW)
    // A particle fades out where it nears what stands behind it, as the frame that shades a pixel once has it: its
    // card would show where it cuts the ground. Both depths are brought to the distance from the eye
    float behind = m_P._34 / (s_position.Load(int3(input.position_proj.xy, 0)).x - m_P._33);
    float own = m_P._34 / (input.position_proj.z - m_P._33);
    float fade = saturate((behind - own) * 1.3f);
    surface.base *= fade > 0.5f ? 1.0f - 2.0f * (1.0f - fade) * (1.0f - fade) : 2.0f * fade * fade;
#endif
#ifdef R1_COLOR_SCALE
    surface.base.rgb *= R1_COLOR_SCALE;
#endif
    surface.detail = 0.5f;
    surface.specular = 0.0f;
    surface.mirror = 0.0f;
    surface.mirrored = 0.0f;
    surface.reflect = 0.0f;
    surface.f0 = 0.0f;
    surface.roughness = 1.0f;
    surface.gloss = 0.0f;
    surface.normal = 0.0f;
#if SURFACE_LIGHT(FEATURE_SET) == SurfaceLight_Environment
    // A card with no side of its own: the sky all around it, the sun as a card turned at random takes it. The frame
    // that shades a pixel once draws a particle in its own colour, which leaves smoke glowing grey at night
    surface.light = L_ambient.xyz + L_hemi_color.xyz + 0.5f * L_sun_color.xyz;
#else
    surface.light = 0.5f;
#endif
    return surface;
}

// returns what one local light adds to the surface's light
float3 light_entry(surface_vertex input, surface_data surface, light_shading_data light)
{
    return default_light(input, surface, light);
}

// returns final output color, the fog thins a particle out instead of tinting it
float4 shade_entry(surface_vertex input, surface_data surface)
{
#ifdef BILLBOARD_GLOW
    // A glow is added as it is, near or far
    return float4(surface_scene(surface.base.rgb), surface.base.a);
#endif
#ifdef USE_SURFACE_CLASSIC
    float4 color = float4(surface_plain(surface.base.rgb, surface), surface.base.a * input.light.w);
#else
    // As the frame that shades a pixel once fogs a particle, by its distance from the eye: one that is added is
    // thinned in its colour and in its share, one that covers goes to the fog's colour. Thinned in its share alone,
    // a glowing gas in a fog stayed several times too bright
    float clear = 1.0f - saturate(length(input.position_world - eye_position) * fog_params.w + fog_params.x);
    // What is behind a far particle has gone into the sky by the fog squared (default_shade): the particle leaves
    // the picture by as much, one that covers in its share, one that is added in its colour
    float far = pow(1.0f - clear, 2.2f);
    float shown = 1.0f - far * far;
#if SURFACE_LIGHT(FEATURE_SET) == SurfaceLight_Environment
    float4 fogged = lerp(fog_color, surface.base, clear);
    fogged.a *= pow(shown, 1.0f / 2.2f);
#else
    float4 fogged = surface.base * clear;
    fogged.rgb *= pow(shown, 1.0f / 2.2f);
#endif
    // That frame decodes a particle's share with its colour, which thins a faint card many times over: measured in
    // a capture, a gas card's share was 0.01 there against 0.04-0.1 here
    float4 color = float4(surface_plain(fogged.rgb, surface), pow(max(fogged.a, 0.0f), 2.2f));
#endif
    clip(color.a - 0.01f / 255.0f);
    return color;
}

#include "common\surface_forward_entry.hlsli"
