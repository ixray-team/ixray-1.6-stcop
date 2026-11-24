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
// The light on the surfaces scene_resolve wrote, a group a tile of the list of every tile with one. What a forward
// surface of high lighting quality sums (default_fetch and default_shade of common\surface_forward.hlsli): the sky
// and the ambient by the surface's share of the sky, the sun through its cascades and the local lights with their
// highlights, what a surface with a normal map mirrors, the fog. Not here yet: decals, wetness
#include "common\fixed_cb.hlsli"
#include "common\common_samplers.hlsli"
#include "common\scene_tables.hlsli"
#include "common\scene_gbuffer.hlsli"
#include "common\thin_surface.hlsli"
#include "common\debug_view.hlsli"

Texture2D<uint2> g_surface : register(t0);
Texture2D<float> g_depth : register(t1);
StructuredBuffer<uint> g_tiles : register(t2);
Texture2D<float> g_occlusion : register(t3);
Texture2D g_sky_behind : register(t4);
Texture2D<uint> g_thin : register(t5); // what scene_resolve told the screen-space passes of the surface
TextureCube g_sky0 : register(t6);
TextureCube g_sky1 : register(t7);
Texture2D g_mirror : register(t8); // what the reflection passes traced for the pixel
Texture2D<uint> g_light_tiles : register(t9); // per tile of SCENE_LIGHT_TILE pixels a bit a local light that reaches it
StructuredBuffer<float4> g_sky_tint : register(t10); // the sky's colour along +x, -x, +y, -y, +z, -z (scene_sky_tint)
RWTexture2D<float4> g_colour : register(u0);

#ifdef USE_SURFACE_SUN_SHADOWS
#define SURFACE_LIGHT_SUN
#endif
#if defined(USE_SURFACE_LIGHTS) && defined(USE_SURFACE_LAMP_SHADOWS)
#define SURFACE_LIGHT_LAMP_SHADOWS
#endif
#if defined(SURFACE_LIGHT_SUN) || defined(SURFACE_LIGHT_LAMP_SHADOWS)
#include "common\pcf_filter.hlsli"

SamplerComparisonState smp_smap : register(s5);
#endif
#include "common\surface_light.hlsli"

// returns the highlight a light of unit colour leaves: GGX by the roughness, or the old power highlight by the gloss
float3 light_highlight(scene_surface surface, float3 f0, float3 to_eye, float3 to_light)
{
    [branch] if (surface.flags & SceneSurface_Gloss) {
        return surface.roughness * saturate(dot(surface.normal, to_light) * 4.0f) * pow(saturate(dot(surface.normal, normalize(to_light + to_eye))), 32.0f);
    }

    return surface_ggx(surface.normal, to_eye, to_light, f0, surface.roughness);
}

[numthreads(SCENE_SHADE_TILE, SCENE_SHADE_TILE, 1)]
void main(uint3 group_id : SV_GroupID, uint3 inside : SV_GroupThreadID)
{
    uint tile = g_tiles[(SCENE_SHADE_LISTS - 1u) * SCENE_SHADE_TILES + group_id.x];
    uint2 pixel = uint2(tile & 0xFFFFu, tile >> 16u) * SCENE_SHADE_TILE + inside.xy;
    // All that is read by the pixel alone, at once: read where each is used, one waited for the other, and two
    // thirds of the pass was those waits
    uint2 packed = g_surface[pixel];
    float seen_depth = g_depth[pixel];
    uint thin_word = g_thin[pixel];
#ifdef USE_SURFACE_OCCLUSION
    float occluded = g_occlusion[pixel];
#endif
#ifdef USE_SURFACE_REFLECTIONS
    float3 traced = g_mirror[pixel].rgb;
#endif

    if (packed.y == 0u) {
        return;
    }

    float3 position = scene_surface_position(pixel, seen_depth);
    float3 to_sun = -L_sun_dir_w.xyz;
    float3 sun_colour = L_sun_color.xyz;
    float sun_reach = 1.0f;
#ifdef USE_SURFACE_SUN_SHADOWS
    // The cascades' filter first, with the place and the normal alone: a warp keeps registers for the most its shader
    // ever holds at once, and the filter holds the most. What lets the sun through is tested off its lit side
    scene_surface shadowed = scene_surface_unpack(packed);
    bool is_turned = (shadowed.material_class == SceneClass_Foliage || (shadowed.material_class == SceneClass_Grass && shadowed.metalness < 0.5f)) && dot(shadowed.normal, to_sun) < 0.0f;
    float2 cascade = surface_sun_shadow(position, is_turned ? -shadowed.normal : shadowed.normal);
    sun_reach = lerp(1.0f, cascade.x, cascade.y);
#ifdef USE_SURFACE_SUNMASK
    sun_colour *= 0.25f + 0.75f * s_sunmask.SampleLevel(smp_linear, mul(m_sunmask, float4(position, 1.0f)).xy, 0.0f).w;
#endif
#endif
    scene_surface surface = scene_surface_unpack(packed);
    float3 to_eye = normalize(eye_position - position);
    float facing = dot(surface.normal, to_sun);
    bool is_leaf = surface.material_class == SceneClass_Foliage || (surface.material_class == SceneClass_Grass && surface.metalness < 0.5f);
    bool is_gloss = (surface.flags & SceneSurface_Gloss) != 0u;
    // A light of one half is the unit of the encoded sum: doubled, then decoded, the cascade's answer with it
    float3 radiance = pow(max(sun_colour * 2.0f, 0.0f), 2.2f) * pow(sun_reach, 2.2f);
    float sun_share = saturate(facing);
    float3 base = pow(surface.albedo * 2.0f, 2.2f);
    float3 f0 = 0.0f;

    if (is_leaf) {
#ifdef USE_LEGACY_LIGHT
        sun_share += surface_through(-facing);
#else
        sun_share = pow(sun_share, 2.2f) + pow(surface_through(-facing), 2.2f);
#endif
    }

    if (!is_gloss) {
        f0 = lerp(0.04f, base, surface.metalness);
        base *= pow(1.0f - surface.metalness, 2.2f);
    }

    // What scatters through the surface: by a soft edge of the lit side through terrain and foliage, through
    // anything else as skin lets it (surface_scatter of common\surface_forward.hlsli)
    float3 scattered = 0.0f;

    [branch] if (surface.scatter > 0.0f) {
        scattered = surface.material_class == SceneClass_Plain
            ? float3(1.0f, 0.35f, 0.2f) * (pow(saturate(dot(to_eye, -to_sun - surface.normal * 0.3f)), 4.0f) * 0.25f + saturate((facing + 0.5f) / 1.5f) - saturate(facing))
            : (float3)surface_through(-facing);
        scattered *= surface.scatter;
    }

#ifdef USE_LEGACY_LIGHT
    // The frame of the old light model summed the sun, the lamps and the sky as gamma values, a light's falloff and
    // the cosine of its angle with them, and that sum is decoded once: a lamp fades with distance as it did there
    float3 direct = sun_colour * 2.0f * sun_reach * (sun_share + scattered);
#else
    float3 direct = radiance * (sun_share + scattered);
#endif
    float3 shine = radiance * light_highlight(surface, f0, to_eye, to_sun);
#ifdef USE_LEGACY_LIGHT
    // A surface with the old gloss map takes its highlights as the old light model gave them: the light's colour by
    // the gloss, added to the lit colour as a gamma value. Added as light, by the gloss squared, a lamp's glint on
    // rusty metal was a tenth of that and the relief of its normal map went flat. The gloss is kept squared
    float gloss = sqrt(surface.roughness);
    float3 glint = is_gloss ? sun_colour * 2.0f * sun_reach * gloss * saturate(facing * 4.0f) * pow(saturate(dot(surface.normal, normalize(to_sun + to_eye))), 32.0f) : 0.0f;
    shine = is_gloss ? 0.0f : shine;
#endif
#ifdef USE_SURFACE_LIGHTS
    // The local lights, as shade_surface of common\surface_forward_entry.hlsli sums them: a light's colour and what
    // it projects are gamma values
    // Only those that reach the pixel's tile of the screen: a group is inside one tile, so its pixels walk the same bits
    uint2 light_tile = pixel / SCENE_LIGHT_TILE;

    uint light_words = ((uint)g_surface_lights[0].position.x + 31u) / 32u;

    for (uint word_idx = 0u; word_idx < light_words; word_idx++) {
        uint bits = g_light_tiles.Load(int3(light_tile.x * SCENE_TILE_WORDS + word_idx, light_tile.y, 0));

        while (bits != 0u) {
        surface_light light = g_surface_lights[word_idx * 32u + firstbitlow(bits) + 1u];
        bits &= bits - 1u;
        float3 to_light = light.position.xyz - position;
        float dist_sq = dot(to_light, to_light);
        float3 light_dir = to_light * rsqrt(max(dist_sq, 1e-10f));
        float reach = dist_sq * light.position.w;
        float window = saturate(1.0f - reach);
        float cone = dot(-light_dir, light.direction.xyz);
        // A spot with an entry is shaped by the texture it projects: its cone only cuts
        bool is_projected = light.color.w != 0.0f && light.direction.w >= -1.0f;
        float falloff = window * window / (1.0f + 2.0f * reach) * (is_projected ? step(light.direction.w, cone) : smoothstep(light.direction.w, light.direction.w + 0.1f, cone));

        [branch] if (falloff > 0.0f) {
            float3 shape = 1.0f;
#ifdef SURFACE_LIGHT_LAMP_SHADOWS
            shape = surface_lamp_shape(light, position, surface.normal, to_light);
#endif
            float3 reaching = pow(max(light.color.rgb * shape, 0.0f), 2.2f) * falloff;
            // r1_lambert of common\r1_static.hlsli
            // A lamp lights the grass as it lights the ground the grass stands on: a blade's normal faces the eye, and
            // by it a torch lit a tuft twice as bright as the ground around it
            float3 lit_normal = surface.material_class == SceneClass_Grass ? float3(0.0f, 1.0f, 0.0f) : surface.normal;
            float lambert = pow(saturate(dot(lit_normal, light_dir) * L_sun_dir_w.w + 1.0f - L_sun_dir_w.w), 1.0f / L_sun_dir_w.w);
#ifdef USE_LEGACY_LIGHT
            direct += max(light.color.rgb * shape, 0.0f) * falloff * lambert;
#else
            direct += reaching * lambert;
#endif
#ifdef USE_LEGACY_LIGHT
            [branch] if (is_gloss) {
                glint += max(light.color.rgb * shape, 0.0f) * falloff * gloss * saturate(dot(surface.normal, light_dir) * 4.0f) * pow(saturate(dot(surface.normal, normalize(light_dir + to_eye))), 32.0f);
            } else {
                shine += reaching * light_highlight(surface, f0, to_eye, light_dir);
            }
#else
            shine += reaching * light_highlight(surface, f0, to_eye, light_dir);
#endif
        }
        }
    }
#endif

    float3 around = L_ambient.xyz + L_hemi_color.xyz * surface.hemi;
#ifdef USE_SURFACE_OCCLUSION
    // The occlusion is a share of linear light, and this light is decoded after it
#ifdef USE_LEGACY_LIGHT
    around *= occluded;
#else
    around *= pow(occluded, 1.0f / 2.2f);
#endif
#endif
    thin_surface thin = thin_unpack(thin_word);
    float3 mirrored = 0.0f;
    float3 mirror = 0.0f;

    // A surface with a normal map mirrors its surroundings: default_fetch of common\surface_forward.hlsli
    [branch] if (thin.reflects) {
        // The sky's light has the colour of the sky the surface looks at, half of it
        // By how much the surface looks along each axis, of the six colours the frame has of its sky
        float3 looks = surface.normal * surface.normal;
        uint3 side = uint3(0u, 2u, 4u) + (surface.normal < 0.0f ? 1u : 0u);
        float3 sky_tint = g_sky_tint[side.x].rgb * looks.x + g_sky_tint[side.y].rgb * looks.y + g_sky_tint[side.z].rgb * looks.z;
        around *= lerp(1.0f, sky_tint / max(dot(sky_tint, 1.0f / 3.0f), 1e-3f), 0.5f);
        float3 mirror_f0 = is_gloss ? (float3)(0.16f * surface.roughness * surface.roughness) : f0;
        float facing_eye = saturate(dot(surface.normal, to_eye));
#ifdef USE_SURFACE_REFLECTIONS
        float3 sky = traced;
        float sky_reach = 1.0f;
#else
        // The sky blurred by the roughness, as far as it reaches the point
        float sky_width, sky_height, sky_levels;
        g_sky0.GetDimensions(0, sky_width, sky_height, sky_levels);
        float3 away = reflect(-to_eye, surface.normal);
        float sky_lod = (sky_levels - 1.0f) * thin.roughness * (1.7f - 0.7f * thin.roughness);
        float3 sky = lerp(g_sky0.SampleLevel(smp_rtlinear, away, sky_lod).rgb, g_sky1.SampleLevel(smp_rtlinear, away, sky_lod).rgb, L_hemi_color.w) * L_sky_color.rgb;
        float sky_reach = saturate(pow(facing_eye + thin.hemi, exp2(-16.0f * thin.roughness - 1.0f)) - 1.0f + thin.hemi);
#ifdef USE_SURFACE_OCCLUSION
        sky_reach *= occluded;
#endif
#endif
        // How much of it a surface of this roughness mirrors at this angle, by a fit to the integrated GGX response
        float4 fit = thin.roughness * float4(-1.0f, -0.0275f, -0.572f, 0.022f) + float4(1.0f, 0.0425f, 1.04f, -0.04f);
        float fit_scale = min(fit.x * fit.x, exp2(-9.28f * facing_eye)) * fit.x + fit.y;
        float2 response = float2(-1.04f, 1.04f) * fit_scale + fit.zw;
        mirrored = saturate(mirror_f0 * response.x + response.y);
#ifdef USE_LEGACY_LIGHT
        // A surface with the old gloss map mirrors as the old light model's ambient highlight did: by its gloss, most
        // of it seen head-on and nothing at a graze. The gloss is kept squared
        if (is_gloss) {
            mirrored = sqrt(surface.roughness) * 0.5f * pow(facing_eye * facing_eye, 16.0f);
        }
#endif
        mirror = sky * pow(mirrored, 1.0f / 2.2f) * sky_reach;
    }

#ifdef USE_LEGACY_LIGHT
    float3 lit = pow(max(around * 2.0f + direct, 0.0f), 2.2f);
#else
    float3 lit = pow(max(around * 2.0f, 0.0f), 2.2f) + direct;
#endif
    // The share it mirrors is taken from what it scatters
    float3 shaded = base * lit + shine;
#ifdef USE_LEGACY_LIGHT
    shaded = is_gloss ? pow(max(surface.albedo * 2.0f * (around * 2.0f + direct) + glint, 0.0f), 2.2f) : shaded;
#endif
    float3 colour = shaded * (1.0f - mirrored) + pow(max(mirror, 0.0f), 2.2f);
    // The fog by how far the pixel is from the eye, and what is far goes into the sky behind it by the fog squared
    float fog = pow(saturate(length(eye_position - position) * fog_params.w + fog_params.x), 2.2f);
    colour = lerp(colour, pow(max(fog_color.xyz, 0.0f), 2.2f), fog);

    [branch] if (fog > 0.0f) {
        colour = lerp(colour, g_sky_behind[pixel].rgb, fog * fog);
    }

    // A debug view in the place of the shading, as debug_surface of common\surface_forward_entry.hlsli shows it
    uint mode = DEBUG_VIEW;

    [branch] if (mode != DebugView_None) {
        float3 shown = 0.5f;

        if (mode == DebugView_Base) {
            shown = surface.albedo * 2.0f;
        } else if (mode == DebugView_Normal) {
            shown = surface.normal * 0.5f + 0.5f;
        } else if (mode == DebugView_Roughness) {
            shown = thin.roughness;
        } else if (mode == DebugView_Hemi) {
            shown = surface.hemi;
        } else if (mode == DebugView_Lit) {
            shown = pow(lit, 1.0f / 2.2f) * 0.5f;
        } else if (mode == DebugView_Highlight) {
            shown = pow(max(shine, 0.0f), 1.0f / 2.2f);
        } else if (mode == DebugView_Mirror) {
            shown = mirror;
        }

        colour = pow(max(shown, 0.0f), 2.2f);
    }

    g_colour[pixel] = float4(max(colour, 0.0f), 1.0f);
}
