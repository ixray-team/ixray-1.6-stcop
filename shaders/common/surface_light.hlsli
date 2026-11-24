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
// What lights a surface, for whatever shades one: the sun through its cascades, the highlight a light leaves and the
// light that comes through a leaf, a local light's record and its shadow. The includer has pcf_filter.hlsli and smp_smap
// where it defines SURFACE_LIGHT_SUN or SURFACE_LIGHT_LAMP_SHADOWS
#ifndef SURFACE_LIGHT_HLSLI
#define SURFACE_LIGHT_HLSLI

// A local light of the frame, in the world. The first record of the buffer holds their count in .x
// and the place of the eye in .yzw
struct surface_light {
    float4 position;                        // .w - 1 / range^2
    float4 color;
    float4 direction;                       // .w - cosine of half the cone, below -1 for a light with no cone
};

StructuredBuffer<surface_light> g_surface_lights : register(t56);

#ifdef SURFACE_LIGHT_LAMP_SHADOWS
Texture2DArray<float> s_smap : register(t51);
// What the frame's spots project, in the slots their shadow entries name
#ifndef FORWARD_PROJECTORS
#define FORWARD_PROJECTORS
Texture2D s_forward_projector0 : register(t36);
Texture2D s_forward_projector1 : register(t38);
Texture2D s_forward_projector2 : register(t39);
Texture2D s_forward_projector3 : register(t41);
Texture2D s_forward_projector4 : register(t42);
Texture2D s_forward_projector5 : register(t43);
Texture2D s_forward_projector6 : register(t44);
Texture2D s_forward_projector7 : register(t45);
#endif

// returns what the projector in a slot lets through at uv, a gamma value
float3 surface_projector(uint slot, float2 uv)
{
    switch (slot) {
    case 1u:
        return s_forward_projector1.SampleLevel(smp_rtlinear, uv, 0.0f).xyz;
    case 2u:
        return s_forward_projector2.SampleLevel(smp_rtlinear, uv, 0.0f).xyz;
    case 3u:
        return s_forward_projector3.SampleLevel(smp_rtlinear, uv, 0.0f).xyz;
    case 4u:
        return s_forward_projector4.SampleLevel(smp_rtlinear, uv, 0.0f).xyz;
    case 5u:
        return s_forward_projector5.SampleLevel(smp_rtlinear, uv, 0.0f).xyz;
    case 6u:
        return s_forward_projector6.SampleLevel(smp_rtlinear, uv, 0.0f).xyz;
    case 7u:
        return s_forward_projector7.SampleLevel(smp_rtlinear, uv, 0.0f).xyz;
    default:
        return s_forward_projector0.SampleLevel(smp_rtlinear, uv, 0.0f).xyz;
    }
}

// returns what of a local light reaches the point: its shadow map and, for a spot, the texture it projects, a gamma
// value. light.color.w is the index of the record the light's shadow entries start at, zero for a light with none;
// an entry is two records, the matrix from the world into the atlas and then color.x - whether the entry has a map,
// .y - its page, .z - the scale from the atlas to the projected texture, .w - that texture's slot plus one,
// direction.xy - the middle of the entry's place in the atlas. A light with no cone has an entry for each side of
// a cube: +X, -X, +Y, -Y, +Z, -Z
float3 surface_lamp_shape(surface_light light, float3 position_world, float3 normal, float3 to_light)
{
    uint entry_idx = (uint)light.color.w;
    [branch]
    if (entry_idx == 0u) {
        return 1.0f;
    }
    if (light.direction.w < -1.0f) {
        float3 size = abs(to_light);
        uint axis = size.x >= size.y && size.x >= size.z ? 0u : (size.y >= size.z ? 1u : 2u);
        entry_idx += (axis * 2u + (to_light[axis] > 0.0f ? 1u : 0u)) * 2u;
    }
    surface_light head = g_surface_lights[entry_idx];
    surface_light tail = g_surface_lights[entry_idx + 1u];
    float4x4 to_atlas = float4x4(head.position, head.color, head.direction, tail.position);
    // The point is lifted off its surface by a texel of the map where it is, more the flatter the light meets it, as
    // the sun's cascades lift theirs: 2.5 cm alone is less than a texel a few metres from a torch, and the ground
    // it grazes shadowed itself in stripes
    float3 across = float3(to_atlas[0].x, to_atlas[1].x, to_atlas[2].x);
    float depth = dot(float4(position_world, 1.0f), float4(to_atlas[0].w, to_atlas[1].w, to_atlas[2].w, to_atlas[3].w));
    float texel = depth / (length(across) * SMAP_size);
    // No more than 5 cm: past that the shadow a plate or a box leaves on the wall it hangs on is lifted away
    float lift = clamp(texel * (1.0f + 3.0f * (1.0f - saturate(dot(normal, normalize(to_light))))), 0.025f, 0.05f);
    float4 cast = mul(float4(position_world + normal * lift, 1.0f), to_atlas);
    float3 coord = cast.xyz / cast.w;
    float3 shape = 1.0f;
    [branch]
    if (tail.color.x != 0.0f) {
#ifdef SURFACE_REFLECTION_FACE
        // A face of the cube around the eye is 256 pixels a side: one tap
        shape = s_smap.SampleCmpLevelZero(smp_smap, float3(coord.xy, tail.color.y), coord.z);
#elif SURFACE_SHADOW_QUALITY == 0
        shape = pcf_3x3(s_smap, smp_smap, coord, float2(SMAP_size, 1.0f / SMAP_size), 1e-5f, int(tail.color.y));
#else
        shape = pcf_5x5(s_smap, smp_smap, coord, float2(SMAP_size, 1.0f / SMAP_size), 1e-5f, int(tail.color.y));
#endif
    }
    [branch]
    if (tail.color.w != 0.0f) {
        shape *= surface_projector((uint)tail.color.w - 1u, (coord.xy - tail.direction.xy) * tail.color.z + 0.5f);
    }
    return shape;
}
#endif

#ifdef SURFACE_LIGHT_SUN
Texture2DArray<float> s_smap_sun : register(t50);
#ifdef USE_SURFACE_SUNMASK
Texture2D s_sunmask : register(t23);
#endif

// returns how much of the sun reaches the texels around the lookup. Level meshes that do not share vertices leave
// cracks a texel wide between their triangles, and a crack opens and closes as the cascade moves: every tap takes
// the nearest of four texels, which closes them. The taps are weighed by where the lookup falls between texels
// r_shadow_quality: 0 - a square of three gathers a side, 1 - of five, 2 - of five, as far apart as the caster is
// from the surface
#ifndef SURFACE_SHADOW_QUALITY
#define SURFACE_SHADOW_QUALITY 0
#endif
#if SURFACE_SHADOW_QUALITY == 0
#define SURFACE_SHADOW_TAPS 3
#else
#define SURFACE_SHADOW_TAPS 5
#endif
// The tangent of the angle the sun's disc is seen at, widened for the look: what a penumbra grows by with a metre
// between the caster and the surface
#define SURFACE_SUN_SPREAD 0.03f
// The gathers across the widest penumbra, two texels each. Four in place of six changed nothing measured where little
// of the screen is penumbra: what is all lit or all shadowed leaves before them
#define SURFACE_SUN_GATHERS 6

// returns how much of the sun reaches coord of one slice. texel: a texel's size in the world; depth_range: what the
// slice's depth of one spans in the world
float surface_cascade_filter(float3 coord, float bias, float slice, float texel, float depth_range)
{
#ifdef SURFACE_REFLECTION_FACE
    // A face of the cube around the eye is 256 pixels a side: one gather, its four texels weighed as a bilinear tap
    // weighs them
    float2 cells = coord.xy * SMAP_size - 0.5f;
    float2 base = floor(cells);
    float2 part = cells - base;
    float4 lit = step(coord.z - bias, s_smap_sun.GatherRed(smp_nofilter, float3((base + 1.0f) / SMAP_size, slice)));
    return lerp(lerp(lit.w, lit.z, part.x), lerp(lit.x, lit.y, part.x), part.y);
#elif SURFACE_SHADOW_QUALITY >= 2
    // What stands between the sun and the surface, as an average over a square around it: the further it is, the
    // wider the filter
    float2 around = floor(coord.xy * SMAP_size - 0.5f);
    float found = 0.0f;
    float nearer = 0.0f;
    [unroll]
    for (int search_y = -1; search_y <= 1; search_y++) {
        [unroll]
        for (int search_x = -1; search_x <= 1; search_x++) {
            float4 depth = s_smap_sun.GatherRed(smp_nofilter, float3((around + float2(search_x, search_y) * 3.0f) / SMAP_size, slice));
            float4 hides = step(depth, coord.z - bias);
            found += dot(hides, 1.0f);
            nearer += dot(hides, depth);
        }
    }
    [branch]
    if (found == 0.0f) {
        return 1.0f;
    }
    [branch]
    if (found == 36.0f) {
        return 0.0f;
    }
    // Every texel of a square of the gathers is tested, and weighed by how far inside the filter's width it lies: the
    // width grows with the gap and the texels stay where they are. Taps that spread apart with the width land on
    // other texels as it changes, and the shadow's edge crawls in steps of the spread
    float gap = (coord.z - nearer / found) * depth_range;
    float reach = clamp(gap * SURFACE_SUN_SPREAD / texel, 1.0f, float(SURFACE_SUN_GATHERS) - 1.5f) + 0.5f;
    bias *= reach * 0.5f + 0.5f;
    float2 centre = coord.xy * SMAP_size - 0.5f;
    float2 first = floor(centre) - float(SURFACE_SUN_GATHERS - 1);
    float2 total = 0.0f;
    [unroll]
    for (int wide_y = 0; wide_y < SURFACE_SUN_GATHERS; wide_y++) {
        [unroll]
        for (int wide_x = 0; wide_x < SURFACE_SUN_GATHERS; wide_x++) {
            float2 corner = first + float2(wide_x, wide_y) * 2.0f;
            float4 depth = s_smap_sun.GatherRed(smp_nofilter, float3((corner + 1.0f) / SMAP_size, slice));
            // A gather's texels: the lower left one, then counter-clockwise
            float4 weight = saturate(reach - abs(corner.x + float4(0.0f, 1.0f, 1.0f, 0.0f) - centre.x)) * saturate(reach - abs(corner.y + float4(1.0f, 1.0f, 0.0f, 0.0f) - centre.y));
            total += float2(dot(weight, step(coord.z - bias, depth)), dot(weight, 1.0f));
        }
    }
    return total.x / total.y;
#else
    // The outer gathers are weighed by where the point is between two of them, as a bilinear tap would be
    float2 cells = coord.xy * SMAP_size - 0.5f;
    float2 base = floor(cells);
    float2 part = cells - base;
    float lit = 0.0f;
    // A tap is the nearest of four texels and the taps stand a texel apart, so a square of them shares its texels:
    // every texel is read once, by a quarter of the gathers and a few more, and a tap is made of what was read. One
    // gather a tap was three fifths of what lighting a pixel cost
    float texels[SURFACE_SHADOW_TAPS + 1][SURFACE_SHADOW_TAPS + 1];
    float2 first = base - float(SURFACE_SHADOW_TAPS - 1) * 0.5f;
    [unroll]
    for (int block_y = 0; block_y < (SURFACE_SHADOW_TAPS + 1) / 2; block_y++) {
        [unroll]
        for (int block_x = 0; block_x < (SURFACE_SHADOW_TAPS + 1) / 2; block_x++) {
            // A gather's texels: the lower left one, then counter-clockwise
            float4 depth = s_smap_sun.GatherRed(smp_nofilter, float3((first + float2(block_x, block_y) * 2.0f + 1.0f) / SMAP_size, slice));
            texels[block_y * 2 + 1][block_x * 2] = depth.x;
            texels[block_y * 2 + 1][block_x * 2 + 1] = depth.y;
            texels[block_y * 2][block_x * 2 + 1] = depth.z;
            texels[block_y * 2][block_x * 2] = depth.w;
        }
    }
    [unroll]
    for (int y = 0; y < SURFACE_SHADOW_TAPS; y++) {
        [unroll]
        for (int x = 0; x < SURFACE_SHADOW_TAPS; x++) {
            float2 weight = float2(x == 0 ? 1.0f - part.x : (x == SURFACE_SHADOW_TAPS - 1 ? part.x : 1.0f), y == 0 ? 1.0f - part.y : (y == SURFACE_SHADOW_TAPS - 1 ? part.y : 1.0f));
            lit += weight.x * weight.y * step(coord.z - bias, min(min(texels[y][x], texels[y][x + 1]), min(texels[y + 1][x], texels[y + 1][x + 1])));
        }
    }
    return lit / float((SURFACE_SHADOW_TAPS - 1) * (SURFACE_SHADOW_TAPS - 1));
#endif
}

// returns .x - how much of the sun reaches the point by one cascade, .y - how far the point is inside that cascade
float2 surface_cascade(float3 position_world, float3 outward, float4x4 to_cascade, float4 capture, float slice)
{
    // The lookup leaves the surface along its normal, further the more the sun grazes it, and the depth test gives
    // a texel and a half away: a wall the sun slides along would shadow itself otherwise
    float texel = 1.0f / (length(to_cascade[0].xyz) * SMAP_size);
#ifndef SURFACE_SINGLE_CASCADE
    position_world += outward * texel * (1.0f + 3.0f * (1.0f - saturate(dot(outward, -capture.xyz))));
    float bias = texel * length(to_cascade[2].xyz) * 1.5f;
#else
    // One wide cascade has texels several times larger: the same count of them would lift a shadow off its caster
    position_world += outward * texel * (0.5f + 1.5f * (1.0f - saturate(dot(outward, -capture.xyz))));
    float bias = texel * length(to_cascade[2].xyz);
#endif
    float4 projected = mul(to_cascade, float4(position_world, 1.0f));
    float3 coord = projected.xyz / projected.w;
    float3 border = 0.5f - abs(coord - 0.5f);
    // A cascade gives way to the next one towards its sides, not along the sun: the eye stands near the far end of
    // its depth, and a fade there handed nearly half of everything around the eye to the coarser cascade
    float inside = saturate(min(border.x, border.y) * 16.0f) * step(0.0f, min(border.z, capture.w - 1e-6f));
    [branch]
    if (inside <= 0.0f) {
        return float2(1.0f, 0.0f);
    }
    return float2(surface_cascade_filter(coord, bias, slice, texel, abs(capture.w)), inside);
}

// High lighting quality has no baked sun past the cascades: the level's heightfield seen from the sun stands in for it
// there (r4_heightfield_shadows, HeightfieldShadowMap.cpp). A texel holds the height a point must reach to see the
// sun, negated
#ifdef USE_SURFACE_DYNAMIC
#define SURFACE_HEIGHTFIELD
Texture2D<float> s_heightfield_shadow : register(t49);

// returns where a point of the world lies on the heightfield, in texels
float2 heightfield_texel(float3 position_world)
{
    return (position_world.xz - HeightfieldWorldToGrid.xy) * HeightfieldWorldToGrid.zw;
}

// returns 0 off the heightfield, 1 from four texels inside its edge
float heightfield_coverage(float2 texel)
{
    float2 edge = min(texel, HeightfieldGrid.xy - 1.0f - texel);
    return smoothstep(0.0f, 4.0f, min(edge.x, edge.y));
}

float heightfield_tap(int2 texel, float height)
{
    if (any(texel < 0) || any(texel >= int2(HeightfieldGrid.xy))) {
        return 1.0f;
    }
    return step(-s_heightfield_shadow.Load(int3(texel, 0)), height);
}

// returns how much of the sun the heightfield lets reach a point at a height, in linear light. heighfield_filtering
// 0: four texels compared, then weighed; a ceiling weighed between a roof and the ground would make a slope that is
// not there. 1 to 3: the 3x3, 5x5 and 7x7 filters
float heightfield_visibility(float2 texel, float height)
{
    uint filtering = uint(HeightfieldParams.w);

    [branch]
    if (filtering > 0u) {
        float4 size = float4(HeightfieldGrid.xy, 1.0f / HeightfieldGrid.xy);
        float3 coord = float3((texel + 0.5f) * size.zw, -height);
        [branch]
        if (filtering == 1u) {
            return pcf_3x3(s_heightfield_shadow, smp_smap, coord, size, 0.0f);
        }
        [branch]
        if (filtering == 2u) {
            return pcf_5x5(s_heightfield_shadow, smp_smap, coord, size, 0.0f);
        }
        return pcf_7x7(s_heightfield_shadow, smp_smap, coord, size, 0.0f);
    }

    int2 base = int2(floor(texel));
    float2 part = frac(texel);
    return lerp(lerp(heightfield_tap(base, height), heightfield_tap(base + int2(1, 0), height), part.x),
        lerp(heightfield_tap(base + int2(0, 1), height), heightfield_tap(base + int2(1, 1), height), part.x), part.y);
}
#endif

// returns .x - how much of the sun reaches the point by the cascades, .y - how far the point is inside them. The
// finest cascade the point is in gives the answer, and gives way to the next one at its edge; past the last one the
// baked sun term is what is left, or the heightfield's. Every cascade is named by a constant index
float2 surface_sun_shadow(float3 position_world, float3 normal)
{
    float3 outward = normal * rsqrt(max(dot(normal, normal), 1e-8f));
    float2 found = surface_cascade(position_world, outward, m_shadow_sun[0], SunCaptureDirectionDepth[0], 0.0f);

    [branch]
    if (found.y < 1.0f) {
        float2 middle = surface_cascade(position_world, outward, m_shadow_sun[1], SunCaptureDirectionDepth[1], 1.0f);
        found = float2(middle.y > 0.0f ? lerp(middle.x, found.x, found.y) : found.x, max(found.y, middle.y));
    }

    [branch]
    if (found.y < 1.0f) {
        float2 far = surface_cascade(position_world, outward, m_shadow_sun[2], SunCaptureDirectionDepth[2], 2.0f);
        found = float2(far.y > 0.0f ? lerp(far.x, found.x, found.y) : found.x, max(found.y, far.y));
    }

#ifdef SURFACE_HEIGHTFIELD
    // The heightfield takes the place of what the cascades leave to the baked sun, as far as it covers the point: the
    // answer and its share are what the caller's lerp from the baked sun then gives. Its light is linear, the
    // cascades' a gamma value
    [branch]
    if (HeightfieldParams.x > 0.0f && found.y < 1.0f) {
        float2 texel = heightfield_texel(position_world);
        float coverage = heightfield_coverage(texel);
        [branch]
        if (coverage > 0.0f) {
            float beyond = pow(heightfield_visibility(texel, position_world.y + HeightfieldParams.z), 1.0f / 2.2f) * coverage * (1.0f - found.y);
            float reach = found.y + coverage * (1.0f - found.y);
            found = float2((beyond + found.x * found.y) / reach, reach);
        }
    }
#endif

    return found;
}
#endif

// returns the GGX highlight of one light on the surface, the cosine of its angle included
float3 surface_ggx(float3 normal, float3 to_eye, float3 to_light, float3 f0, float roughness)
{
    float3 halfway = normalize(to_light + to_eye);
    float n_l = saturate(dot(normal, to_light));
    float n_v = saturate(dot(normal, to_eye));
    float n_h = saturate(dot(normal, halfway));
    float alpha = max(roughness * roughness, 0.002f);
    float alpha_sq = alpha * alpha;
    float lobe = n_h * n_h * (alpha_sq - 1.0f) + 1.0f;
    // No division by pi: the frame that shades a pixel once has none in its distribution, nor in its diffuse light
    float distribution = alpha_sq / (lobe * lobe);
    // GeometrySmithD and FresnelSchlick of common\lighting.hlsli: what reflects next to nothing head-on does not
    // turn into a mirror at a grazing angle
    float k = (roughness + 1.0f) * (roughness + 1.0f) * 0.125f;
    float visibility = 0.25f / (lerp(n_l, 1.0f, k) * lerp(n_v, 1.0f, k));
    float3 fresnel = lerp(f0, min(1.0f, dot(f0, 16.66667f)), pow(1.0f - saturate(dot(halfway, to_eye)), 5.0f));
    return distribution * visibility * fresnel * n_l;
}

// returns the roughness widened by how much the normal turns across the pixel: SpecularAA of common\lighting.hlsli.
// A smooth surface with a busy normal map is a rough one at the size of a pixel, and without this every scratch
// of a weapon throws the sun back as a mirror would
// across, along: how far the normal turns to the next pixel across and down
// turned: a quarter of the squares of how much the normal turns a pixel across and a pixel down, summed
float surface_roughness_spread(float turned, float3 normal, float3 to_eye, float roughness)
{
    float variance = turned;
    float facing = saturate(abs(dot(normal, to_eye)));
    variance /= facing * facing + 0.02f;
    float alpha = roughness * roughness;
    return saturate(sqrt(sqrt(alpha * alpha + min(2.0f * variance, 0.18f))));
}

float surface_roughness_spread(float3 across, float3 along, float3 normal, float3 to_eye, float roughness)
{
    return surface_roughness_spread(0.25f * (dot(across, across) + dot(along, along)), normal, to_eye, roughness);
}

// returns how much of the sun comes through a leaf or a blade: SimpleTranslucency of common\lighting.hlsli. away: the
// cosine between its normal and the way the light travels, so it is the side turned from the sun that glows
float surface_through(float away)
{
    float scale = 0.36f * away;
    float soft = scale + 0.0769f;
    soft *= soft * 1.171f;
    float near_edge = 1.0f - saturate(abs(scale) * 13.0f - 1.0f);
    return saturate(1.5f * lerp(saturate(away), soft, near_edge * near_edge));
}

#endif
