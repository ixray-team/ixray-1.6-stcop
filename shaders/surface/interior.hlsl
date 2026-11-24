// A window with a room painted behind it: the pixel looks into a box mapped onto a cube texture, lit by the sky and
// by the sun that falls through the window
#include "common\common.hlsli"

#define DISABLE_STEEPPARALLAX

#include "common\sload.hlsli"
#include "common\shadow.hlsli"

// returns where a ray from a point of the window meets the walls of the room, in the room's own space
float3 room_hit(float3 origin, float3 dir)
{
    float3 box_max = 2.5f;
    box_max.z = 0.0f;
    float3 box_min = -2.5f;
    box_min.z = -3.0f;

    float3 to_max = (box_max - origin) / dir;
    float3 to_min = (box_min - origin) / dir;

    float3 furthest = max(to_max, to_min);
    float dist = min(min(furthest.x, furthest.y), furthest.z);

    return origin + dir * dist;
}

// returns what the sky gives a wall that faces the direction
float3 room_sky_light(float3 dir, float3 hemi)
{
    dir = normalize(dir);

#ifdef IBL_REMAP_IRRADANCE
    RemapVector(dir);
#endif

#ifdef USE_NORMAL_HEMI_DISTRIBUTION
    hemi = min(hemi, dir.yyy * 0.375f + 0.375f);
#endif

    float3 sky_last = env_s0.SampleLevel(smp_linear, dir, 0.0f).xyz;
    float3 sky_next = env_s1.SampleLevel(smp_linear, dir, 0.0f).xyz;

#ifdef USE_CGIM_SKY_TWEAK
    float upward = saturate(dir.y);
    upward *= upward;

    float weight = SMALLSKY_TOP_VECTOR_POWER;
    weight = saturate(weight + (1.0 - weight) * upward) + (1.0 - weight) * 0.5f;

    hemi *= weight * weight;
    float3 irradiance = 1.0f;
    hemi *= lerp(sky_last, sky_next, L_hemi_color.w);
#else
    float3 irradiance = lerp(sky_last, sky_next, L_hemi_color.w);
#endif

#ifdef USE_DIFFUSE_SKY_COLOR
#ifdef USE_BGRA_SKYCOLOR
    irradiance *= L_sky_color.zyx;
#else
    irradiance *= L_sky_color.xyz;
#endif
#else
    irradiance *= L_hemi_color.xyz;
#endif

    irradiance *= irradiance;

    return irradiance * hemi;
}

void ps_main(p_bumped_new input, out IXRayForward output)
{
#if defined(USE_STEEPPARALLAX) && defined(USE_HIGH_QUALITY)
#ifdef USE_PBR
    input.tcdh.xy = UpdateTC(input, input.tcdh.xy, s_bump, 0);
#else
    input.tcdh.xy = UpdateTC(input, input.tcdh.xy, s_bumpX, 3);
#endif
#endif

    float3x3 frame = float3x3(input.M1, input.M2, input.M3);

    float3 view = mul(transpose(frame), input.position.xyz);
    float3 on_window = float3(input.tcdh.xy * 2.0f - 1.0f, 0);
    view = normalize(view);

    float3 on_wall = room_hit(on_window, view);

    float3 sun_dir_view = mul((float3x3)m_V, L_sun_dir_w.xyz);
    float3 to_sun = normalize(mul(transpose(frame), -sun_dir_view).xyz);

    float hemi = input.tcdh.z;

#ifdef USE_LM_HEMI
    float4 baked = s_hemi.Sample(smp_rtlinear, input.tcdh.zw);
    hemi = get_hemi(baked);
#endif

    float4 room = s_env.Sample(smp_base, -on_wall);
    float3 light = room_sky_light(on_wall, hemi) + L_ambient.xyz;

    // The wall behind the glass, as a point of the scene: its place in the last frame gives the velocity
    float3 wall_view = input.position.xyz + length(on_wall - on_window) * normalize(input.position.xyz);
    float4 wall_proj_old = mul(m_VP_old, float4(mul(m_invV, float4(wall_view, 1.0f)).xyz, 1.0f));

    // The sun reaches the wall through the window: from the wall back along the sun to the glass, and from there
    // into the sun's shadow map
    if (to_sun.z > 0.0f) {
        float3 through = room_hit(on_wall, to_sun);
        float3 glass_world = wall_view - length(through - on_wall) * sun_dir_view.xyz + normalize(float3(input.M1.z, input.M2.z, input.M3.z)) * 0.1f;

        glass_world = mul(m_invV, float4(glass_world.xyz, 1.0f)).xyz;

        float shadow = 1.0;

        int cascade_index;
        float3 smap_texcoord;

        bool is_in_bounds = calc_cascades(glass_world.xyz, m_shadow_sun, cascade_index, smap_texcoord);

        if (is_in_bounds) {
            shadow = pcf_3x3(s_smap_sun, smp_smap, smap_texcoord, float2(SMAP_size, 1.0 / SMAP_size), 0.0, cascade_index);
        }

        light += L_sun_color.xyz * min(shadow, 1.0f - s_base.Sample(smp_base, saturate(through.xy * 0.5f + 0.5f)).w);
    }

    output.Color.xyz = GammaToLinear(room.xyz * light);
    output.Color.w = 1.0f - s_base.Sample(smp_base, input.tcdh.xy).w;

    float fog = GammaToLinear(saturate(length(input.position.xyz) * fog_params.w + fog_params.x));
    output.Color = lerp(output.Color, GammaToLinear(fog_color), fog);

#ifdef USE_LENGTH_BUFFER
    output.Color.xyz = saturate(output.Color.xyz * rcp(1.0f + output.Color.xyz));
#else
#ifndef DISABLE_MOTION_VECTORS
    output.Velocity.xy = input.hpos_curr.xy / input.hpos_curr.w - wall_proj_old.xy / wall_proj_old.w;
    output.Velocity.zw = saturate(output.Color.w * 2.0f - 1.0f);
#endif

#ifdef USE_WBOIT_TRANSPARENCY
    WboitBufferPack(output, wall_view);
#endif
#endif
}
