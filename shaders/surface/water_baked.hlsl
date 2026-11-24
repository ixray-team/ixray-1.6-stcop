#define SURFACE_NO_WETNESS
#include "common\surface_forward.hlsli"
#define SURFACE_NO_DECALS
#define SURFACE_NO_DEBUG_VIEW
#include "common\waterconfig.hlsli"
#include "common\watermove.hlsli"
#define USE_VERTEX_ENTRY

// A puddle of the level (WATER_PUDDLE) always traces its mirror and fades in at its shore, with no foam
#ifdef WATER_PUDDLE
#undef USE_SOFT_WATER
#ifndef USE_SSLR_ON_WATER
#define USE_SSLR_ON_WATER
#endif
#endif

// Water of the forward level frame, shaded as the other frame shades it (effects\water.hlsl) and lit by what its
// vertices were baked with. WATER_DISTORT is its second pass, into the target that pushes the picture aside. Drawn
// from the scene's clusters only its base is a map of its material: the ripples, the foam and the caustics (a
// sequence) need not fit the buckets and are bound as they are, at registers below the buckets, as is what it reads
// of the frame. The sky is the bumped surfaces' sky_s0 / sky_s1
#ifdef WATER_DISTORT
Texture2D s_distort0 : register(t25);
Texture2D s_distort1 : register(t26);
#else
Texture2D s_water_nmap : register(t1);
Texture2D s_water_foam : register(t2);
Texture2D s_water_caustic : register(t3);
#endif

// What a ray off the water does not find on the screen is looked for in the cube around the eye (s_water_env,
// s_water_env_dist), where the frame draws one: r4_vslr_enable with the reflections of the world
#if defined(USE_SSLR_ON_WATER) && defined(USE_OFFSCREEN_REFLECTIONS) && defined(USE_SURFACE_REFLECTIONS) && !defined(WATER_DISTORT)
#define WATER_CUBE
#endif

// returns how far from the eye the scene's depth of this value is, along the view
float water_view_depth(float depth)
{
    return m_P._34 / (depth - m_P._33);
}

// returns what the local lights add in the gamma space the water is shaded in: the old light model has it there
float3 water_local(float3 local)
{
#if defined(USE_SURFACE_CLASSIC) || defined(USE_LEGACY_LIGHT)
    return local;
#else
    return pow(max(local, 0.0f), 1.0f / 2.2f);
#endif
}

// returns a colour of the scene in the gamma space the water is shaded in
float3 water_gamma(float3 scene)
{
#ifdef USE_SURFACE_CLASSIC
    return scene;
#else
    return pow(max(scene, 0.0f), 1.0f / 2.2f);
#endif
}

#if defined(USE_SSLR_ON_WATER) && !defined(WATER_DISTORT)
#define WATER_MIRROR_STEPS 32
#define WATER_MIRROR_REFINE 4
#define WATER_CUBE_STEPS 12
#define WATER_CUBE_GROWTH 1.5f

// What the frame before showed, upscaled, before its bloom and tone curve
Texture2D s_frame_before : register(t40);

// returns the pixel's share of a step: the same every frame without TAA, a new one each frame with it
float water_noise(float2 pixel)
{
    pixel += m_taa_jitter.z < 0.0f ? 0.0f : 5.588238f * float(uint(m_taa_jitter.w) % 64u);
    return frac(52.9829189f * frac(dot(pixel, float2(0.06711056f, 0.00583715f))));
}

// returns where a point in view space is on the screen, .z - how far behind the scene's depth there it lies, .w - one
// while it is on the screen
float4 water_mirror_point(float3 point_view)
{
    float4 clip = mul(m_P, float4(point_view, 1.0f));
    float2 uv = clip.xy / max(clip.w, 1e-4f) * float2(0.5f, -0.5f) + 0.5f;
    float scene = water_view_depth(s_position.SampleLevel(smp_nofilter, min(saturate(uv) * view_uv.xy, view_uv.zw), 0.0f).x);
    return float4(uv, point_view.z - scene, clip.w > 1e-4f && all(saturate(uv) == uv) ? 1.0f : 0.0f);
}

// returns what the frame before showed where a ray off the water meets the scene, .a - how much of the mirror it is.
// The ray is marched in view space against the depth in steps that grow, its crossing halved down to the surface it
// meets. The share fades as the ray turns toward the eye, near the end of its reach and at the edges of the screen,
// where the cube around the eye or the sky takes over
float4 water_mirror(float3 position_world, float3 mirrored_world, float noise)
{
    float3 origin = mul(m_V, float4(position_world, 1.0f)).xyz;
    float3 along = mul((float3x3)m_V, mirrored_world);
    float trust = saturate(dot(along, normalize(origin)) * 4.0f);
    [branch]
    if (trust <= 0.0f) {
        return 0.0f;
    }
    // As far as the fog lets anything be seen: a ray of twice the water's distance ended a few metres past the far
    // bank, and the trees on it were mirrored as a strip of their feet
    float reach = max(2.0f * origin.z + 4.0f, fog_params.z);
    float before = 0.0f;
    float at = 0.0f;
    bool is_hit = false;
    [loop]
    for (int step_idx = 0; step_idx < WATER_MIRROR_STEPS; step_idx++) {
        float share = (float(step_idx) + noise) / float(WATER_MIRROR_STEPS);
        at = reach * share * share + 0.05f;
        float4 seen = water_mirror_point(origin + along * at);
        if (seen.w <= 0.0f) {
            break;
        }
        if (seen.z > 0.0f && seen.z < (at - before) * abs(along.z) + 0.5f) {
            is_hit = true;
            break;
        }
        before = at;
    }
    [branch]
    if (!is_hit) {
        return 0.0f;
    }
    [unroll]
    for (int refine_idx = 0; refine_idx < WATER_MIRROR_REFINE; refine_idx++) {
        float middle = 0.5f * (before + at);
        bool is_behind = water_mirror_point(origin + along * middle).z > 0.0f;
        before = is_behind ? before : middle;
        at = is_behind ? middle : at;
    }
    float3 hit_world = position_world + mirrored_world * at;
    float4 now = mul(m_VP, float4(hit_world, 1.0f));
    float4 old = mul(m_VP_old, float4(hit_world, 1.0f));
    float2 uv_now = now.xy / max(now.w, 1e-4f) * float2(0.5f, -0.5f) + 0.5f;
    float2 uv_old = old.xy / max(old.w, 1e-4f) * float2(0.5f, -0.5f) + 0.5f;
    float2 edge = min(min(uv_now, 1.0f - uv_now), min(uv_old, 1.0f - uv_old));
    trust *= saturate(min(edge.x, edge.y) * 8.0f) * saturate((reach - at) / (0.2f * reach));
    return float4(min(s_frame_before.SampleLevel(smp_rtlinear, min(saturate(uv_old) * view_uv_prev.xy, view_uv_prev.zw), 0.0f).rgb, 20.0f), trust);
}

#ifdef WATER_CUBE
TextureCube s_water_env : register(t46);
TextureCube s_water_env_dist : register(t47);

// returns how far the cube around the eye holds a surface in the direction of this point of its space, squared
float water_cube_reach(float3 point_probe)
{
    float reach = s_water_env_dist.SampleLevel(smp_nofilter, point_probe, 0.0f).x;
    return reach * reach;
}

// returns what the cube around the eye shows where a ray off the water meets what it holds, .a - how much of the
// mirror it is, as FastViewReflections of the other frame finds it: the ray is marched against the cube's distances in
// steps that grow and its crossing is halved four times. The cube's colour is never cleared: where the ray meets
// nothing, the share falls to the sky by the fog over the farthest distance it passed
float4 water_cube(float3 position_world, float3 mirrored_world, float noise)
{
    float3 origin = mul(m_probe, float4(mul(m_V, float4(position_world, 1.0f)).xyz, 1.0f));
    float3 along = mul((float3x3)m_probe, mul((float3x3)m_V, mirrored_world));
    float origin_sq = dot(origin, origin);
    float range = fog_params.z - sqrt(origin_sq);
    [branch]
    if (range <= 0.0f) {
        return 0.0f;
    }
    float scale = range / (pow(WATER_CUBE_GROWTH, float(WATER_CUBE_STEPS)) - 1.0f);
    float growth = pow(WATER_CUBE_GROWTH, noise);
    float before = 0.011f;
    float at = before;
    float3 found = origin;
    float found_sq = 0.0f;
    float farthest = 0.0f;
    float was = 0.0f;
    bool is_valid = false;
    bool is_hit = false;
    [loop]
    for (int step_idx = 0; step_idx < WATER_CUBE_STEPS; step_idx++) {
        float next = 0.011f + scale * (growth - 1.0f);
        growth *= WATER_CUBE_GROWTH;
        float3 point_probe = origin + along * next;
        float reach = water_cube_reach(point_probe);
        if (reach < origin_sq) {
            continue;
        }
        before = at;
        at = next;
        found = point_probe;
        found_sq = reach;
        farthest = max(farthest, reach);
        is_valid = true;
        float past = dot(found, found) - reach;
        [branch]
        if (past > 0.0f && was <= 0.0f) {
            float3 half_step = along * (at - before) * 0.5f;
            float3 refined = found - half_step;
            [unroll]
            for (int refine_idx = 0; refine_idx < 4; refine_idx++) {
                float refined_reach = water_cube_reach(refined);
                half_step *= 0.5f;
                refined += dot(refined, refined) > refined_reach ? -half_step : half_step;
            }
            float refined_sq = water_cube_reach(refined);
            float refined_dist = sqrt(refined_sq);
            if (refined_sq >= origin_sq && abs(length(refined) - refined_dist) < 0.25f + 0.02f * refined_dist) {
                found = refined;
                found_sq = refined_sq;
                is_hit = true;
                break;
            }
        }
        was = past;
    }
    [branch]
    if (!is_valid) {
        return 0.0f;
    }
    float share = is_hit ? 1.0f : 1.0f - saturate(2.5f * sqrt(farthest) * fog_params.w + fog_params.x);
    float fog = saturate(sqrt(found_sq) * fog_params.w + fog_params.x);
    return float4(min(s_water_env.SampleLevel(smp_linear, found, 0.0f).rgb, 20.0f), share * (1.0f - fog * fog));
}
#endif
#endif

// returns the vertex riding the wave, laid over the world as the other frame lays water, with the two places it reads
// the ripples at in uv.zw and detail.xy
surface_vertex vtx_entry(surface_vertex output, float4 position, float3 normal)
{
#ifdef WATER_PUDDLE
    // A puddle lies flat
    float3 place = position.xyz;
#else
    float4 moved = watermove(position);
    output.position_proj = mul(m_VP, moved);
    output.position_world = moved.xyz;
    float3 place = moved.xyz;
#endif
    output.uv.xy = place.xz * 0.3f;
    output.uv.zw = watermove_tc(output.uv.xy * W_DISTORT_BASE_TILE_0, place.xz, W_DISTORT_AMP_0);
    output.detail.xy = watermove_tc(output.uv.xy * W_DISTORT_BASE_TILE_1, place.xz, W_DISTORT_AMP_1);
    return output;
}

// returns the surface under the pixel: the sky its ripples mirror, and how much they mirror by how the eye meets them
surface_data fetch_entry(surface_vertex input)
{
    surface_data surface = (surface_data)0;
#ifdef SURFACE_CLUSTER
    surface_material_idx = input.material & DEBUG_MATERIAL_MASK;
#endif
    surface.base = SURFACE_MAP(s_base, smp_base, input.uv.xy);
    surface.detail = 0.5f;
    surface.roughness = 1.0f;
#ifndef WATER_DISTORT
    // The ripples of two maps, in the frame the other frame gives water: its tangent runs along -x
    float3 mapped = s_water_nmap.Sample(smp_base, input.uv.zw).xyz + s_water_nmap.Sample(smp_base, input.detail.xy).xyz - 1.0f;
    float3 flat = normalize(input.normal);
    float3 tangent = float3(-1.0f, 0.0f, 0.0f) + flat.x * flat;
    tangent *= rsqrt(max(dot(tangent, tangent), 1e-8f));
    float3 normal = normalize(tangent * mapped.x + cross(flat, tangent) * mapped.y + flat * max(abs(mapped.z), 1e-3f));
    float3 from_eye = normalize(input.position_world - eye_position);
    float3 mirrored = reflect(from_eye, normal);
    // The sky turned as the other frame turns it and laid on its cube's faces as that frame lays it
    float2 turn;
    sincos(L_sky_color.w, turn.x, turn.y);
    float3 sky_dir = float3(mirrored.x * turn.y - mirrored.z * turn.x, mirrored.y, mirrored.x * turn.x + mirrored.z * turn.y);
#ifndef USE_FULL_SKY_SPHERE
    sky_dir /= max(abs(sky_dir.x), max(abs(sky_dir.y), abs(sky_dir.z)));
    sky_dir.y = sky_dir.y * 2.0f - 1.0f;
#endif
    surface.mirror = lerp(sky_s0.SampleLevel(smp_rtlinear, sky_dir, 0.0f).rgb, sky_s1.SampleLevel(smp_rtlinear, sky_dir, 0.0f).rgb, L_ambient.w);
#ifdef USE_BGRA_SKYCOLOR
    surface.mirror *= L_sky_color.zyx;
#else
    surface.mirror *= L_sky_color.xyz;
#endif
    // How much the water mirrors and covers grows as the eye meets it more at a graze
    surface.sky_reach = pow(saturate(dot(mirrored, from_eye)), 5.0f);
    surface.reflect = mirrored;
    // The sun by the flat surface, not by the ripples
    surface.normal = flat;
    float2 cascade = surface_sun_shadow(surface_world(input.position_world), surface.normal);
    float sun_reach = lerp(input.tint.r, cascade.x, cascade.y);
    surface.sun = input.sun * r1_lambert(dot(surface.normal, -L_sun_dir_w.xyz)) * sun_reach;
    surface.light = input.light.rgb + surface.sun;
#if defined(USE_SOFT_WATER) || defined(WATER_PUDDLE)
    // The sun's glitter on the ripples
    float3 half_dir = from_eye + L_sun_dir_w.xyz;
    surface.specular = surface_scene(L_sun_color.xyz * pow(abs(dot(half_dir, -normal)) * rsqrt(max(dot(half_dir, half_dir), 1e-8f)), 256.0f) * sun_reach);
#endif
#endif
    return surface;
}

// returns what one local light adds to the surface's light
float3 light_entry(surface_vertex input, surface_data surface, light_shading_data light)
{
    return default_light(input, surface, light);
}

// returns final output color. The water is shaded in gamma space as the other frame shades it and decoded once at the
// end: what it mirrors - the trace of the screen over the cube around the eye over the sky - covers a share of what
// lies under it that grows to the grazing eye; the fog tints it and thins it out
float4 shade_entry(surface_vertex input, surface_data surface)
{
#ifdef WATER_DISTORT
    float2 push = (s_distort0.Sample(smp_base, input.uv.zw).xy + s_distort1.Sample(smp_base, input.detail.xy).xy) * 0.5f;
    return float4(lerp(push, 0.5f, surface.base.a), 0.0f, 0.5f);
#else
    float3 to_water = input.position_world - eye_position;
    float3 lit = surface.light + water_local(surface.local);
    float3 mirror = surface.mirror;
#ifdef USE_SSLR_ON_WATER
    float noise = water_noise(input.position_proj.xy);
#ifdef WATER_CUBE
    float4 around = water_cube(input.position_world, surface.reflect, noise);
    mirror = lerp(mirror, water_gamma(around.rgb), around.a);
#endif
    float4 seen = water_mirror(input.position_world, surface.reflect, noise);
    mirror = lerp(mirror, water_gamma(seen.rgb), seen.a);
#endif
    float3 colour = lerp(mirror * (0.2f + 0.2f * surface.sky_reach), surface.base.rgb * lit, surface.base.a);
    float alpha = lerp(0.45f + 0.45f * surface.sky_reach, 1.0f, surface.base.a);
#ifdef WATER_PUDDLE
    // A puddle thins out over the first two centimetres of water at its shore; the sun glitters on it
    float own = water_view_depth(input.position_proj.z);
    float behind = water_view_depth(s_position.Load(int3(input.position_proj.xy, 0)).x);
    alpha *= saturate(max(behind - own, 0.0f) * length(to_water) / own * 50.0f);
    colour += water_gamma(surface.specular);
#endif
#ifdef USE_SOFT_WATER
    // How much water lies between the surface and what is seen through it: the shore fades in, deep water is its
    // own colour, a band of foam follows the shore and the direct light on the bottom ripples
    float own = water_view_depth(input.position_proj.z);
    float behind = water_view_depth(s_position.Load(int3(input.position_proj.xy, 0)).x);
    float deep = max(behind - own, 0.0f) * length(to_water) / own * 0.75f;
    colour = lerp(0.1f * water_intensity.x * lit, colour, alpha);
    alpha = max(1.0f - exp(-4.0f * deep), min(alpha, saturate(deep)));
    float3 bottom = eye_position + to_water * (behind / own);
    float3 caustic = s_water_caustic.Sample(smp_base, bottom.xz * 0.45f).ggg;
    caustic += ddx(caustic) * float3(1.25f, 0.0f, -1.25f);
    caustic += ddy(caustic) * float3(1.25f, 0.0f, -1.25f);
    colour += water_gamma(surface.specular) + max(caustic, 0.0f) * 0.25f * (surface.sun + water_local(surface.local)) * (1.0f - surface.base.a);
    float4 foam = s_water_foam.Sample(smp_base, input.uv.xy);
    float band = saturate(deep * dot(surface.normal, -normalize(to_water)));
    float foamed = foam.a * (1.0f - surface.base.a) * smoothstep(0.025f, 0.05f, band) * smoothstep(0.1f, 0.075f, band);
    colour = lerp(colour, foam.rgb * water_intensity.x * lit, foamed);
    alpha = max(alpha, foamed);
#endif
    float fog = saturate(length(to_water) * fog_params.w + fog_params.x);
    return float4(surface_scene(lerp(colour, fog_color.xyz, fog)), alpha * (1.0f - fog * fog));
#endif
}

#include "common\surface_forward_entry.hlsli"
