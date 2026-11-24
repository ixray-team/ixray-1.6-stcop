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
#ifndef SURFACE_FORWARD_DEFINED
#define SURFACE_FORWARD_DEFINED

#include "common\r1_common.hlsli"

// A pass drawn into the cube around the eye has no screen of its own to read: no decals, no sky behind it
#ifdef SURFACE_REFLECTION_FACE
#define SURFACE_NO_DECALS
#define SURFACE_NO_DEBUG_VIEW
#endif
#include "common\scene_forward.hlsli"
#include "common\r1_static.hlsli"

// First include of a Surface shader drawn forward. A permutation of it is a FEATURE_SET: what the vertex is, where
// its baked light comes from and which extras it has. The shader's entries follow, then surface_forward_entry.hlsli
#define SurfaceVertex_Static 0x000          // Level geometry, already in the world
#define SurfaceVertex_Terrain 0x001         // Level geometry whose uv is also its lightmap uv
#define SurfaceVertex_Tree 0x002            // Placed by m_xform, sways with the wind
#define SurfaceVertex_Particle 0x003        // A corner of a particle's quad, in the world, with a colour
#define SurfaceVertex_Model 0x004           // A model placed by m_W, skinned before it gets here
#define SurfaceVertex_Detail 0x005          // Grass: placed, turned and lit by its instance
#define SurfaceVertex_Imposter 0x006        // A tree seen from afar: a card blended from two of its views
#define SURFACE_VERTEX(mask) ((mask) & 0x00F)

#define SurfaceLight_Lightmap 0x000         // Lightmap, hemi in a second map
#define SurfaceLight_Vertex 0x010           // Baked into the vertex
#define SurfaceLight_Terrain 0x020          // Lightmap, hemi in the alpha of the base texture
#define SurfaceLight_Tree 0x030             // Vertex value scaled and biased by the instance
#define SurfaceLight_Environment 0x040      // Nothing baked: what the sky and the sun give a point in the open
#define SurfaceLight_None 0x050             // Unlit: it adds to the picture or multiplies it
#define SURFACE_LIGHT(mask) ((mask) & 0x0F0)

#define SurfaceMask_Detail 0x100            // Detail texture
#define SurfaceMask_Layers 0x200            // Four detail textures weighed by a mask
#define SurfaceMask_Still 0x400             // A tree that does not sway
#define SurfaceMask_Bump 0x800              // Normal and material maps: the sun is shaded per pixel by the PBR model
#define SurfaceMask_Environment 0x1000      // The environment shows where the base texture's alpha is low

// The depth pass of a frame that traces reflections or keeps a history writes how its models have moved on the
// screen
#if defined(USE_SURFACE_VELOCITY) && defined(USE_DEPTH_PREPASS)
#define SURFACE_VELOCITY
#endif

// A model with a normal map takes the HUD's raindrops
#if defined(USE_SURFACE_HUD_RAIN) && !defined(SURFACE_REFLECTION_FACE) && ((FEATURE_SET) & SurfaceMask_Bump) && !((FEATURE_SET) & SurfaceMask_Layers)
#define SURFACE_HUD_RAIN
#endif

#include "common\debug_view.hlsli"

// The editors tint what they select or highlight
#if defined(EDITOR_MODE) && !defined(SURFACE_CLUSTER)
#include "common\extra_cb.hlsli"
#define SURFACE_EDITOR_TINT(surface) (surface).base.rgb += editor_selection.xyz
#else
#define SURFACE_EDITOR_TINT(surface)
#endif

struct surface_vertex {
    float4 uv : TEXCOORD0;                  // .xy - base, .zw - lightmap
    float4 detail : TEXCOORD1;              // .xy - detail uv, .z - detail weight, .w - detail bias
    float4 light : TEXCOORD2;               // .rgb - light of the vertex, the hemi colour under a lightmap, .w - fog
    float3 sun : TEXCOORD3;                 // sun at the vertex: what a lightmap's alpha and the sun cascade let through
    float4 tint : TEXCOORD4;                // colour of a particle; .r - how much of the sun the vertex was baked to get
    float3 position_world : TEXCOORD5;
    float3 normal : TEXCOORD6;              // in the world, zero for what has no side (a particle)
#if (FEATURE_SET) & SurfaceMask_Environment
    float3 reflection : TEXCOORD9;          // where the eye's ray goes off the surface
#endif
#if (FEATURE_SET) & SurfaceMask_Bump
    float3 tangent : TEXCOORD7;             // with the normal and the binormal the frame of the normal map
    float3 binormal : TEXCOORD8;
#endif
#ifdef SURFACE_VELOCITY
    float4 position_now : TEXCOORD10;       // on the screen, this frame and the frame before
    float4 position_old : TEXCOORD11;
#endif
#ifdef SURFACE_CLUSTER
    nointerpolation uint material : TEXCOORD12; // of the scene's table; a debug view's tag above DEBUG_TAG_SHIFT
#endif
#ifdef SURFACE_HUD_RAIN
    float4 drops_basis : TEXCOORD13;        // .xyz - the point on the model as it stands unskinned, .w - how much its normal looks up
#endif
    float4 position_proj : SV_POSITION;
};

#if SURFACE_VERTEX(FEATURE_SET) == SurfaceVertex_Model
#include "common\skin.hlsli"
#include "editor\editor_instancing.hlsli"
#elif SURFACE_VERTEX(FEATURE_SET) == SurfaceVertex_Detail
struct detail_instance {
    float3 quat;
    float scale;
    float3 pos;
    float hemi;                             // its sign says whether the sun reaches the place
    float trample_strength;
    float trample_visual;
    float2 trample_dir;
};

StructuredBuffer<detail_instance> detail_buffer : register(t25);

float3x3 detail_turn(float4 q)
{
    return float3x3(
        1.0f - 2.0f * (q.y * q.y + q.z * q.z), 2.0f * (q.x * q.y + q.z * q.w), 2.0f * (q.x * q.z - q.y * q.w),
        2.0f * (q.x * q.y - q.z * q.w), 1.0f - 2.0f * (q.x * q.x + q.z * q.z), 2.0f * (q.y * q.z + q.x * q.w),
        2.0f * (q.x * q.z + q.y * q.w), 2.0f * (q.y * q.z - q.x * q.w), 1.0f - 2.0f * (q.x * q.x + q.y * q.y));
}

#ifdef SURFACE_CLUSTER
#include "common\scene_forward_grass.hlsli"
#endif
#endif

struct surface_data {
    float4 base;                            // .a - alpha
    float3 detail;                          // multiplies the base, 0.5 leaves it as it is
    float3 light;                           // baked light on the surface, the sun and the sky on it
    float3 local;                           // what the local lights add, in linear light
    float3 sun;                             // the sun that reaches the surface, in linear light, where light is summed so
    float3 normal;                          // in the world, zero for what has no side
    float3 specular;                        // highlight of the sun, added on top of the lit colour
    float3 reflect;                         // share of the light on the point it gives back as a mirror of its surroundings
    float3 mirror;                          // what it mirrors of the sky and of the frame before, joins the lit colour as light
    float3 mirrored;                        // the share of light it mirrors: that much less is left to scatter
    float3 f0;                              // reflectance of a PBR surface seen head-on
    float roughness;                        // of a PBR surface
    float gloss;                            // weight of the highlight of a surface with the old gloss map
    float sky_reach;                        // how much of the sky's light the point has
};

#if defined(USE_SURFACE_LAMP_SHADOWS) && !defined(SURFACE_NO_LAMP_SHADOWS) && defined(USE_SURFACE_LIGHTS) && !defined(SURFACE_NO_LIGHTS) && SURFACE_LIGHT(FEATURE_SET) != SurfaceLight_None
#define SURFACE_LAMP_SHADOWS
#define SURFACE_LIGHT_LAMP_SHADOWS
#endif

#if defined(SURFACE_LAMP_SHADOWS) || (defined(USE_SURFACE_SUN_SHADOWS) && !defined(SURFACE_NO_SUN_SHADOWS) && SURFACE_LIGHT(FEATURE_SET) != SurfaceLight_None)
#include "common\pcf_filter.hlsli"

SamplerComparisonState smp_smap : register(s5);
#endif

#if defined(USE_SURFACE_SUN_SHADOWS) && !defined(SURFACE_NO_SUN_SHADOWS) && SURFACE_LIGHT(FEATURE_SET) != SurfaceLight_None
#define SURFACE_LIGHT_SUN
#endif

#include "common\surface_light.hlsli"

struct light_shading_data {
    float3 color;                           // what reaches the point
    float3 dir;                             // from the point to the light
};

// returns the point in the world. The HUD is drawn around the eye, which sits at the origin of its space
float3 surface_world(float3 position)
{
#if defined(USE_SURFACE_LIGHTS) && !defined(SURFACE_NO_LIGHTS)
    return position + g_surface_lights[0].position.yzw * step(dot(eye_position, eye_position), 1e-6f);
#else
    return position;
#endif
}

#ifdef USE_R1_EMISSION
Texture2D s_emission : register(t25);
#endif

#if defined(USE_SURFACE_OCCLUSION) && defined(USE_DEPTH_PREPASSED)
Texture2D s_occ : register(t26);
#endif

#if defined(USE_SURFACE_HUD_SHADOWS) && defined(USE_DEPTH_PREPASSED)
Texture2D s_hud_contact : register(t27);
#endif

// Snow lies on the level geometry: its colour, and the map that holds its roughness. A pixel shader is not told
// what kind of vertex it shades, only how it is lit: what has a lightmap, or is terrain, is the level, and the
// blender of the level's vertex-lit geometry says so (SURFACE_LEVEL). What is cut out by alpha keeps clear of it
#if defined(USE_DYNAMIC_SNOW) && !defined(USE_SURFACE_CLASSIC) && !defined(SURFACE_NO_WETNESS) && !defined(USE_R1_ALPHA_TEST) && (SURFACE_LIGHT(FEATURE_SET) == SurfaceLight_Terrain || SURFACE_LIGHT(FEATURE_SET) == SurfaceLight_Lightmap || defined(SURFACE_LEVEL))
#define SURFACE_SNOW
#ifdef SURFACE_CLUSTER
Texture2D s_snow_base : register(t4);
Texture2D s_snow_bumpX : register(t5);
#else
// A draw of its own has the detail texture's bump maps in t4 and t5
Texture2D s_snow_base : register(t21);
Texture2D s_snow_bumpX : register(t22);
#endif
#endif

#ifdef SURFACE_LIGHT_SUN

// Rain wets what has a normal map, at high shading quality: with fewer taps than the frame that shades a pixel
// once
#if defined(USE_SURFACE_HIGH) && defined(USE_SURFACE_IBL) && ((FEATURE_SET) & SurfaceMask_Bump) && !defined(SURFACE_NO_WETNESS)
#define SURFACE_WETNESS
#define RAIN_RIPPLES
#define RAIN_TAPS 8
#include "common\wetness.hlsli"

// The terrain with a height in its layers holds water where it is low, as the terrain of the advanced frame does
#if defined(USE_RAIN_PUDDLES) && defined(USE_PBR) && ((FEATURE_SET) & SurfaceMask_Layers) && SURFACE_LIGHT(FEATURE_SET) == SurfaceLight_Terrain
#define SURFACE_PUDDLES
#define RAIN_PUDDLE_COMPUTE
#define RAIN_PUDDLE_FORWARD
#ifndef PI
#define PI 3.141592653589793f
#endif
#ifndef EPS
#define EPS 0.0000100f
#endif
#include "common\RainPuddles.hlsli"
#endif
#endif

#else
float2 surface_sun_shadow(float3 position_world, float3 normal)
{
    return float2(1.0f, 0.0f);
}
#endif

// The same by the derivatives of the shaded normal. The pixels of a quad share them, the fine ones by pairs, and a
// highlight shows that as a pattern of 2x2: a surface that can tell its neighbours' normals uses the function above
float surface_roughness_wide(float3 normal, float3 to_eye, float roughness)
{
    return surface_roughness_spread(ddx_fine(normal), ddy_fine(normal), normal, to_eye, roughness);
}

// What the maps of a material are, and what kind of vertex and baked light it has, is read from its record where the
// scene's clusters are drawn (SURFACE_FOLDED): the level's materials and the trees' then share their shaders and a
// draw, and are compiled once, as lightmapped level geometry. Anything else is compiled for what it is, and so is the
// terrain, whose layers are one material. A model's pixel stage has a permutation of its light alone: the cluster
// pass of a model blender tells it what it shades (SURFACE_CLUSTER_MODEL)
#if defined(SURFACE_CLUSTER) && SURFACE_VERTEX(FEATURE_SET) == SurfaceVertex_Static && !defined(SURFACE_CLUSTER_MODEL) && !defined(SURFACE_CARD)
#define SURFACE_FOLDED
#define SURFACE_IS_PBR ((surface_material.flags & SCENE_MATERIAL_PBR) != 0u)
#define SURFACE_IS_LEAF(flags) (((flags) & (SCENE_MATERIAL_PLACED | SCENE_MATERIAL_STILL)) == SCENE_MATERIAL_PLACED)
#elif defined(USE_PBR)
#define SURFACE_IS_PBR true
#define SURFACE_IS_LEAF(flags) true
#else
#define SURFACE_IS_PBR false
#define SURFACE_IS_LEAF(flags) true
#endif

// returns what kind of surface the base texture's material value tells of, 0 to 1: what CRender makes L_material.w
// of it for the frame that shades a pixel once
float surface_kind()
{
#ifdef SURFACE_CLUSTER
    float kind = (surface_material.gloss + 0.5f) * 0.25f;
#else
    float kind = L_material.w;
#endif
    kind = frac(kind);
    kind = max(1.0f - 4.0f * kind, (kind - 0.25f) / 0.75f);
    return kind * kind * (3.0f - 2.0f * kind);
}

// returns the highlight one light of unit colour leaves on the surface, by the model its maps were made for: GGX
// for metalness and roughness maps, the old power highlight for a gloss map. The lit colour is doubled in the end,
// the GGX highlight with it
float3 surface_highlight(surface_data surface, float3 to_eye, float3 to_light)
{
    [branch] if (SURFACE_IS_PBR) {
        return surface_ggx(surface.normal, to_eye, to_light, surface.f0, surface.roughness) * 2.0f;
    }

    float lit = saturate(dot(surface.normal, to_light) * 4.0f);
    return surface.gloss * lit * pow(saturate(dot(surface.normal, normalize(to_light + to_eye))), 32.0f);
}


// A leaf whose maps are the old ones scatters all the sun: the frame that shades a pixel once gives a waving,
// cut-out surface without PBR maps a scatter value of one
#if defined(USE_R1_ALPHA_TEST) && !defined(USE_SURFACE_CLASSIC) && (defined(SURFACE_FOLDED) || (SURFACE_LIGHT(FEATURE_SET) == SurfaceLight_Tree && !((FEATURE_SET) & SurfaceMask_Still) && !defined(USE_PBR)))
#define SURFACE_OLD_LEAF
#endif

// returns the share of the sun that scatters through a surface with a scatter value in its maps, as SunLight of
// common\lighting.hlsli adds it: through foliage and terrain by a soft edge of the lit side, through anything else
// as skin lets it, seen against the light and past the edge of the lit side
float3 surface_scatter(float3 normal, float3 to_sun, float3 to_eye)
{
    float facing = dot(normal, to_sun);
#if SURFACE_LIGHT(FEATURE_SET) == SurfaceLight_Terrain || SURFACE_LIGHT(FEATURE_SET) == SurfaceLight_Tree
    return surface_through(-facing);
#else
#ifdef SURFACE_FOLDED
    [branch] if (surface_material.flags & SCENE_MATERIAL_PLACED) {
        return surface_through(-facing);
    }

#endif
    float through = pow(saturate(dot(to_eye, -to_sun - normal * 0.3f)), 4.0f) * 0.25f;
    float past_edge = saturate((facing + 0.5f) / 1.5f) - saturate(facing);
    return float3(1.0f, 0.35f, 0.2f) * (through + past_edge);
#endif
}

// A GGX highlight is doubled for the gamma sum, which the linear sum has no use for
#ifdef USE_SURFACE_CLASSIC
#define SURFACE_HIGHLIGHT_LINEAR 1.0f
#else
#define SURFACE_HIGHLIGHT_LINEAR (SURFACE_IS_PBR ? 0.5f : 1.0f)
#endif

// Parallax of what the level is built of: where the texture asks for it, at high shading quality, as the frame that
// shades a pixel once has it. What is cut out by its texture is left alone: its depth pass cuts it where the
// surface is, not where the eye's ray meets its height
// The terrain's four layers each have a height map of their own
#if defined(ALLOW_STEEPPARALLAX) && defined(USE_SURFACE_HIGH) && defined(SURFACE_CLUSTER) && !defined(SURFACE_CLUSTER_MODEL) && ((FEATURE_SET) & SurfaceMask_Bump) && !defined(USE_R1_ALPHA_TEST) && !defined(USE_DEPTH_PREPASS)
#if ((FEATURE_SET) & SurfaceMask_Layers) && SURFACE_LIGHT(FEATURE_SET) == SurfaceLight_Terrain && defined(USE_TERRAIN_PARALLAX)
#define SURFACE_TERRAIN_PARALLAX
#elif !((FEATURE_SET) & SurfaceMask_Layers) && (defined(USE_STEEPPARALLAX) || defined(SURFACE_FOLDED))
#define SURFACE_PARALLAX
#endif
#endif

#if defined(SURFACE_PARALLAX) || defined(SURFACE_TERRAIN_PARALLAX)
#ifndef PLANE_CORRECTION_VAL
#define PLANE_CORRECTION_VAL 1e-3f
#endif

// UpdateTC of sload.hlsli: where the eye's ray meets a height map. height_map: the tail, head and info of the map
// with the height in channel; ray: towards the eye in the surface's tangent frame; depth: the pixel's distance along
// the view
float2 surface_shift_uv(uint4 height_map, uint channel, bool is_pbr, float2 uv, float2 uv_dx, float2 uv_dy, float3 ray, float depth)
{
    // Past the march the plain offset fades out too, and past that the height is not read at all: a quarter of what
    // a far pixel of terrain cost went into a shift nobody sees
    const float start_fade = 8.0f;
    const float stop_fade = 12.0f;
    const float stop_offset = 16.0f;

    [branch] if (height_map.x == SCENE_TEXTURE_ABSENT || depth >= stop_offset) {
        return uv;
    }

#ifdef PARALLAX_HEIGHT
    float scale = PARALLAX_HEIGHT;
#else
    float scale = is_pbr ? 0.02f : 0.01f;
#endif
    float height = sample_map(height_map.x, height_map.y, height_map.z, uv, uv_dx, uv_dy, 16.0f)[channel];
    float2 reach = ray.xy * scale;
#ifdef USE_PARALLAX_PLANE_CORRECTION
    reach *= rcp(max(PLANE_CORRECTION_VAL, abs(ray.z)));
#endif
    float2 shifted = is_pbr ? uv - reach * (1.0f - height) : uv + reach * (height - 0.5f);

    [branch] if (depth < stop_fade) {
        float layer_depth = rcp(lerp(20.0f, 8.0f, ray.z * ray.z));
        float2 stride = reach * layer_depth;
        float2 march_uv = uv;
        float surface = 1.0f - height;
        float layer = is_pbr ? 0.0f : 0.5f;
        // The march reads one slice at the pixel's own level, worked out once: the head while it is loaded and
        // the level is its own, the tail past that
        float2 repeat = rcp(float2(1u << (height_map.z & 15u), 1u << ((height_map.z >> 4) & 15u)));
        float2 axes = float2(dot(uv_dx * repeat, uv_dx * repeat), dot(uv_dy * repeat, uv_dy * repeat));
        float grain = sqrt(max(min(axes.x, axes.y), max(axes.x, axes.y) * (1.0f / 256.0f)));
        uint stream = height_map.z >> SCENE_TEXTURE_STREAM_SHIFT;
        uint head = stream != 0u ? resolve_heads[stream - 1u] : height_map.y;
        uint slice = head != SCENE_TEXTURE_ABSENT && grain * SCENE_TEXTURE_TAIL < 1.0f ? head : height_map.x;

        [loop] while (layer < surface) {
            layer += layer_depth;
            march_uv -= stride;
            surface = 1.0f - sample_slice(slice, march_uv * repeat, float2(grain, 0.0f), float2(0.0f, grain))[channel];
        }

        float2 before = march_uv + stride;
        float after = surface - layer;
        surface = 1.0f - sample_map(height_map.x, height_map.y, height_map.z, before, uv_dx, uv_dy, 16.0f)[channel];
        float ahead = surface - layer + layer_depth;
        march_uv = lerp(march_uv, before, after * rcp(after - ahead));
        shifted = lerp(shifted, march_uv, smoothstep(stop_fade, start_fade, depth));
    }

    return lerp(shifted, uv, smoothstep(stop_fade, stop_offset, depth));
}
#endif

#ifdef SURFACE_HUD_RAIN
#include "common\hud_raindrops.hlsli"

// returns the HUD's raindrops on the pixel with their tilt in the world. The scene's draw of a model has no matrix to
// turn the tilt by: it is turned by how the model's points and the world's cross the pixel. Nothing off the HUD,
// whose depth is the nearest fiftieth of the range
HudRainDrops surface_hud_rain(surface_vertex input)
{
    float3 object_x = ddx(input.drops_basis.xyz);
    float3 object_y = ddy(input.drops_basis.xyz);
    float3 world_x = ddx(input.position_world);
    float3 world_y = ddy(input.position_world);
    float3 object_n = cross(object_x, object_y);
    float3 world_n = cross(world_x, world_y);
    float3 tilt;
    HudRainDrops drops = HudRain_EvaluateObject(input.drops_basis.xyz, input.position_proj.z < 0.02f ? input.drops_basis.w : 0.0f, tilt);
    // The tilt by the pixel's two steps and the face's normal, which grows by the square of the model's scale
    float area = max(dot(object_n, object_n), 1e-30f);
    float3 along = float3(dot(cross(object_y, object_n), tilt), dot(cross(object_n, object_x), tilt), dot(object_n, tilt)) / area;
    drops.tilt = along.x * world_x + along.y * world_y + along.z * world_n * sqrt(sqrt(area) / max(length(world_n), 1e-30f));
    return drops;
}
#endif

// The detail texture's own normal and material maps, at high shading quality and in the colour pass alone, as
// MaterialDetail of common\material.hlsli adds them. A cluster reads from its record whether it has them
#if defined(USE_SURFACE_HIGH) && ((FEATURE_SET) & SurfaceMask_Bump) && !((FEATURE_SET) & SurfaceMask_Layers) && SURFACE_LIGHT(FEATURE_SET) != SurfaceLight_Terrain && (!defined(USE_DEPTH_PREPASS) || defined(SURFACE_WRITES_SURFACE)) && !defined(SURFACE_REFLECTION_FACE) && (defined(SURFACE_FOLDED) || (defined(USE_TDETAIL_BUMP) && ((FEATURE_SET) & SurfaceMask_Detail)))
#define SURFACE_DETAIL_BUMP
#endif

// What a surface is before anything lights it: its maps read, and what the weather laid on it. default_matter reads
// it and default_lit lights it; a pass that keeps the surface for a later one to light takes the first alone
struct surface_matter {
    float4 base;                            // .a - alpha; the environment, the snow and the wetness in it
    float3 detail;                          // multiplies the base, 0.5 leaves it as it is
    float3 normal;                          // in the world as the maps give it, the vertex's where there are none
    float3 wet_normal;                      // with the rain's rings and the water that stands on it: what mirrors
    float3 f0;                              // reflectance of a PBR surface seen head-on
    float3 hemi;                            // baked share of the sky's light: a lightmapped surface's, a terrain's
    float roughness;                        // of a PBR surface, widened by how its normal turns across the pixel
    float gloss;                            // weight of the highlight of a surface with the old gloss map
    float metalness;
    float occlusion;                        // of its maps, before water fills it
    float scatter;                          // how much of the sun its maps let through
    float sky_known;                        // its share of the sky where something tells it, below 0 where nothing does
    float snow;
    float wet;
    float puddle;
    bool is_pbr;
};

surface_matter default_matter(inout surface_vertex input)
{
#if SURFACE_VERTEX(FEATURE_SET) == SurfaceVertex_Detail && !defined(USE_SURFACE_CLASSIC)
    // Grass is seen from both sides: its normal faces the eye
    input.normal *= dot(input.normal, eye_position - input.position_world) < 0.0f ? -1.0f : 1.0f;
#endif
    surface_matter matter;
    matter.detail = 0.5f;
    matter.normal = input.normal;
    matter.f0 = 0.0f;
    matter.hemi = 0.0f;
    matter.roughness = 1.0f;
    matter.gloss = 0.0f;
    matter.metalness = 0.0f;
    matter.occlusion = 1.0f;
    matter.scatter = 0.0f;
    matter.snow = 0.0f;
    matter.wet = 0.0f;
    matter.puddle = 0.0f;
    matter.is_pbr = false;
#ifdef SURFACE_CLUSTER
    surface_material_idx = input.material & DEBUG_MATERIAL_MASK;
#endif
#if (FEATURE_SET) & SurfaceMask_Bump
#if ((FEATURE_SET) & SurfaceMask_Layers) && SURFACE_LIGHT(FEATURE_SET) == SurfaceLight_Terrain
    // Terrain has no maps of its own: its four detail layers bring theirs, weighed by the mask
    float4 layers = SURFACE_MAP(s_mask, smp_base, input.uv.xy);
    layers = layers / dot(layers, 1.0f);
#ifdef SURFACE_CLUSTER
    float2 bump_dx = ddx(input.detail.xy);
    float2 bump_dy = ddy(input.detail.xy);
    float4 bump = 0.0f;
    float4 bump_x = 0.0f;
#ifdef SURFACE_TERRAIN_PARALLAX
    {
        // A march a pixel, on the layer the mask gives it the most of: every layer is read where the eye's ray meets
        // that one's height map. Where the next layer comes near it the two are marched and mixed, even where they
        // are even, so the shift never jumps from one layer's height to another's. A march a layer cost a fifth
        // of a near pixel for the same picture. The mask and the lightmap stay where the surface is
        float3 eye = eye_position - input.position_world;
        float3 ray = normalize(float3(dot(input.tangent, eye), dot(input.binormal, eye), dot(input.normal, eye)));
#ifdef USE_PBR
        const uint heights = SCENE_TERRAIN_BUMP;
        const uint channel = 0u;
        const bool is_pbr_height = true;
#else
        const uint heights = SCENE_TERRAIN_BUMP_X;
        const uint channel = 3u;
        const bool is_pbr_height = false;
#endif
        uint lead = layers.g > layers.r ? 1u : 0u;
        lead = layers.b > layers[lead] ? 2u : lead;
        lead = layers.a > layers[lead] ? 3u : lead;
        float4 others = float4(uint4(0u, 1u, 2u, 3u) != lead) * layers;
        uint next = others.g > others.r ? 1u : 0u;
        next = others.b > others[next] ? 2u : next;
        next = others.a > others[next] ? 3u : next;
        float share = 0.5f + 0.5f * saturate((layers[lead] - others[next]) * 8.0f);
        float2 shifted = surface_shift_uv(surface_material.terrain[heights + lead], channel, is_pbr_height, input.detail.xy, bump_dx, bump_dy, ray, input.position_proj.w);

        [branch] if (share < 1.0f) {
            shifted = lerp(surface_shift_uv(surface_material.terrain[heights + next], channel, is_pbr_height, input.detail.xy, bump_dx, bump_dy, ray, input.position_proj.w), shifted, share);
        }

        input.detail.xy = shifted;
    }
#endif

    [unroll] for (uint bump_idx = 0u; bump_idx < 4u; bump_idx++) {
        [branch] if (layers[bump_idx] > 0.0f) {
            bump += surface_layer_map(SCENE_TERRAIN_BUMP + bump_idx, input.detail.xy, bump_dx, bump_dy) * layers[bump_idx];
            bump_x += surface_layer_map(SCENE_TERRAIN_BUMP_X + bump_idx, input.detail.xy, bump_dx, bump_dy) * layers[bump_idx];
        }
    }
#else
    float4 bump = SURFACE_MAP(s_dn_r, smp_base, input.detail.xy) * layers.r + SURFACE_MAP(s_dn_g, smp_base, input.detail.xy) * layers.g + SURFACE_MAP(s_dn_b, smp_base, input.detail.xy) * layers.b + SURFACE_MAP(s_dn_a, smp_base, input.detail.xy) * layers.a;
    float4 bump_x = SURFACE_MAP(s_dn_rX, smp_base, input.detail.xy) * layers.r + SURFACE_MAP(s_dn_gX, smp_base, input.detail.xy) * layers.g + SURFACE_MAP(s_dn_bX, smp_base, input.detail.xy) * layers.b + SURFACE_MAP(s_dn_aX, smp_base, input.detail.xy) * layers.a;
#endif
#else
#ifdef SURFACE_PARALLAX
    float2 flat_dx = ddx(input.uv.xy);
    float2 flat_dy = ddy(input.uv.xy);

#ifdef SURFACE_FOLDED
    [branch] if (surface_material.flags & SCENE_MATERIAL_STEEP)
#endif
    {
        float3 eye = eye_position - input.position_world;
        float3 ray = normalize(float3(dot(input.tangent, eye), dot(input.binormal, eye), dot(input.normal, eye)));
        bool is_pbr_height = SURFACE_IS_PBR;
        uint4 heights = is_pbr_height ? uint4(surface_material.bump, surface_material.bump_head, surface_material.bump_info, 0u) : uint4(surface_material.bump_x, surface_material.bump_x_head, surface_material.bump_x_info, 0u);
        float2 shifted = surface_shift_uv(heights, is_pbr_height ? 0u : 3u, is_pbr_height, input.uv.xy, flat_dx, flat_dy, ray, input.position_proj.w);
        // The detail texture follows the base texture; the lightmap stays where the surface is
        input.detail.xy += (shifted - input.uv.xy) * surface_material.detail_scale;
        input.uv.xy = shifted;
    }
#endif
#ifdef SURFACE_HUD_RAIN
    // Its maps and its colour are read through the HUD's raindrops
    HudRainDrops drops = surface_hud_rain(input);
    input.uv.xy += drops.tangent * kHudRainRefraction;
#endif
    float4 bump = SURFACE_MAP(s_bump, smp_base, input.uv.xy);
    float4 bump_x = SURFACE_MAP(s_bumpX, smp_base, input.uv.xy);
#ifdef SURFACE_DETAIL_BUMP
    // Faded out with the detail texture's colour
    float detail_fade = input.detail.z;
    float4 detail_bump = 0.0f;
    float4 detail_bump_x = 0.0f;
    float detail_gloss = 0.0f;
#ifdef SURFACE_CLUSTER
    float2 detail_bump_dx = ddx(input.detail.xy);
    float2 detail_bump_dy = ddy(input.detail.xy);
    bool has_detail_bump = (surface_material.flags & SCENE_MATERIAL_DETAIL_BUMP) != 0u && surface_material.detail != SCENE_TEXTURE_ABSENT && surface_material.detail_bump != SCENE_TEXTURE_ABSENT && surface_material.detail_bump_x != SCENE_TEXTURE_ABSENT;
    detail_fade = has_detail_bump ? detail_fade : 0.0f;

    [branch] if (detail_fade > 0.0f) {
        detail_bump = SURFACE_MAP_AT(s_detailBump, smp_base, input.detail.xy, detail_bump_dx, detail_bump_dy);
        detail_bump_x = SURFACE_MAP_AT(s_detailBumpX, smp_base, input.detail.xy, detail_bump_dx, detail_bump_dy);
        detail_gloss = SURFACE_MAP_AT(s_detail, smp_base, input.detail.xy, detail_bump_dx, detail_bump_dy).a;
    }
#else
    detail_bump = SURFACE_MAP(s_detailBump, smp_base, input.detail.xy);
    detail_bump_x = SURFACE_MAP(s_detailBumpX, smp_base, input.detail.xy);
    detail_gloss = SURFACE_MAP(s_detail, smp_base, input.detail.xy).a;
#endif
#endif
#endif
    float2 base_dx = ddx(input.uv.xy);
    float2 base_dy = ddy(input.uv.xy);
    bool is_pbr = SURFACE_IS_PBR;
    float3 mapped;
    float metalness = 0.0f;
    float occlusion = 1.0f;
    float2 mapped_across = 0.0f;
    float2 mapped_along = 0.0f;
    // What the detail texture's normal adds, after the green is turned
    float3 detail_shift = 0.0f;

    [branch] if (is_pbr) {
        mapped = float3(bump.wy * 2.0f - 1.0f, 0.0f);
        // The normal map a pixel across and a pixel down, by the texture's own step: read, not derived, so that every
        // pixel has its own (surface_roughness_spread)
        mapped_across = SURFACE_MAP_AT(s_bump, smp_base, input.uv.xy + base_dx, base_dx, base_dy).wy * 2.0f - 1.0f;
        mapped_along = SURFACE_MAP_AT(s_bump, smp_base, input.uv.xy + base_dy, base_dx, base_dy).wy * 2.0f - 1.0f;
        mapped.z = sqrt(1.0f - saturate(dot(mapped.xy, mapped.xy)));
#ifdef SURFACE_DETAIL_BUMP
        float2 detail_xy = detail_bump.wy * 2.0f - 1.0f;
        detail_shift = float3(detail_xy.x, -detail_xy.y, sqrt(saturate(1.0f - dot(detail_xy, detail_xy)))) * detail_fade;
        bump_x = saturate(bump_x * lerp((float4)1.0f, detail_bump_x * float4(2.0f, 2.0f, 1.0f, 1.0f), detail_fade));
#endif
        // A PBR normal map has its green the other way up: MaterialFinish of common\material.hlsli turns it too
        mapped.y = -mapped.y;
        mapped += detail_shift;
        metalness = bump_x.x;
        occlusion = bump_x.w;
        matter.roughness = max(0.02f, bump_x.y);
        // A metal reflects its colour, and that is a linear value: the encoded one of a dark metal is several times it
        matter.f0 = lerp(0.04f, pow(max(SURFACE_MAP_AT(s_base, smp_base, input.uv.xy, base_dx, base_dy).rgb, 0.0f), 2.2f), metalness);
#ifdef USE_SURFACE_HIGH
        matter.scatter = bump_x.z;
#endif
    } else {
        mapped = bump.wzy + bump_x.xyz - 1.0f;
#ifdef SURFACE_DETAIL_BUMP
        mapped += (detail_bump.wzy + detail_bump_x.xyz - 1.0f) * detail_fade;
#endif
        // MaterialFinish of common\material.hlsli: an old normal map has half the height, which doubles its relief
        mapped.z = abs(mapped.z * 0.5f);
        matter.gloss = bump.x * bump.x;
#ifdef SURFACE_DETAIL_BUMP
        matter.gloss *= lerp(1.0f, detail_gloss * 2.0f, detail_fade);
#endif
    }

    float3 normal = normalize(input.tangent * mapped.x + input.binormal * mapped.y + input.normal * mapped.z);
    float3 to_eye = normalize(eye_position - input.position_world);
    // The vertex normal turns evenly over a triangle: its derivative is every pixel's own
    float3 normal_across = normalize(input.tangent * (mapped_across.x + detail_shift.x) + input.binormal * (detail_shift.y - mapped_across.y) + input.normal * (sqrt(1.0f - saturate(dot(mapped_across, mapped_across))) + detail_shift.z));
    float3 normal_along = normalize(input.tangent * (mapped_along.x + detail_shift.x) + input.binormal * (detail_shift.y - mapped_along.y) + input.normal * (sqrt(1.0f - saturate(dot(mapped_along, mapped_along))) + detail_shift.z));
    float widened = surface_roughness_spread(normal_across - normal + ddx(input.normal), normal_along - normal + ddy(input.normal), normal, to_eye, matter.roughness);
    matter.roughness = is_pbr ? widened : matter.roughness;
#ifdef SURFACE_HUD_RAIN
    // The drops' relief, and their water: smoother where they lie, a gloss map the brighter. hud_rain.z is the sky's
    // light that scales the relief
    HudRain_Perturb(normal, drops, max(hud_rain.z * 30.0f, 3.0f), input.position_world - eye_position);
    matter.roughness = is_pbr ? saturate(matter.roughness - drops.coverage * 0.45f) : matter.roughness;
    matter.gloss = is_pbr ? matter.gloss : saturate(matter.gloss + drops.coverage * 0.5f);
#endif
    matter.normal = normal;
    matter.is_pbr = is_pbr;
#endif
    // The base texture, and the share of the sky's light the surface has where something tells it
#if SURFACE_LIGHT(FEATURE_SET) == SurfaceLight_Terrain
    matter.base = SURFACE_MAP(s_base, smp_base, input.uv.xy);
    matter.hemi = matter.base.a;
    matter.sky_known = matter.base.a;
#elif defined(SURFACE_FOLDED)
    matter.base = r1_sample_base(input.uv.xy);

    [branch] if (surface_material.flags & (SCENE_MATERIAL_VERTEX_LIT | SCENE_MATERIAL_PLACED)) {
        matter.sky_known = input.tint.g - 1.0f;
    } else {
        float4 hemi_map = SURFACE_MAP_AT(s_hemi, smp_rtlinear, input.uv.zw, ddx(input.uv.zw), ddy(input.uv.zw));
#ifdef USE_SOC_LIGHTING
        matter.hemi = dot(hemi_map.rgb, 1.0f / 3.0f);
#else
        matter.hemi = hemi_map.a;
#endif
        matter.sky_known = matter.hemi.x;
    }
#elif SURFACE_LIGHT(FEATURE_SET) == SurfaceLight_Lightmap
    matter.base = r1_sample_base(input.uv.xy);
    matter.hemi = r1_p_hemi(input.uv.zw);
    matter.sky_known = matter.hemi.x;
#elif SURFACE_LIGHT(FEATURE_SET) == SurfaceLight_None
    matter.base = r1_sample_base(input.uv.xy);
    matter.sky_known = 1.0f;
#else
    matter.base = r1_sample_base(input.uv.xy);
    // The vertex hands its hemi over where it has one; tint.g is one more than it, so that zero stays "none"
    matter.sky_known = input.tint.g - 1.0f;
#endif
#if SURFACE_VERTEX(FEATURE_SET) == SurfaceVertex_Detail && defined(USE_SURFACE_HIGH)
    // The rain wets the grass with the ground it stands on, as it wets an old map, by how much of the sky the blade
    // has: the rain's map is not asked, a pixel of grass is written too many times over for its taps
    [branch] if (rain_setup.w > 0.0f) {
        float grass_open = smoothstep(0.1f, 0.6f, saturate(dot(input.light.rgb, 1.0f / 3.0f) / max(dot(L_hemi_color.rgb + L_ambient.rgb, 1.0f / 3.0f), 1e-3f)));
        float grass_far = 1.0f - smoothstep(min(rain_setup.y - 15.0f, rain_setup.x), rain_setup.y, input.position_proj.w);
        float grass_soak = grass_open * grass_far * grass_far * saturate(rain_params.x) * dot(matter.base.rgb, 0.33f);
        matter.base.rgb *= max(1.0f - sqrt(grass_soak), 0.5f);
    }
#endif
#ifdef SURFACE_SNOW
    // Where the level has snow, what faces up under the open sky takes its colour before anything lights it, and
    // with PBR maps its roughness and its plain reflectance; the surface keeps its own relief. rain_params.w is
    // whether the level has snow at all
    float2 snow_uv = surface_world(input.position_world).xz * 0.2f;
    float snow = smoothstep(0.2f, 0.3f, matter.sky_known >= 0.0f ? saturate(matter.sky_known) : 1.0f) * smoothstep(0.7f, 0.8f, normalize(matter.normal).y) * saturate(rain_params.w);
#ifdef SURFACE_FOLDED
    snow *= (surface_material.flags & SCENE_MATERIAL_PLACED) ? 0.0f : 1.0f;
#endif
    matter.base.rgb = lerp(matter.base.rgb, s_snow_base.Sample(smp_base, snow_uv).rgb, snow);
    matter.snow = snow;
#if (FEATURE_SET) & SurfaceMask_Bump
#ifdef SNOW_PBR
    float snow_roughness = max(0.02f, s_snow_bumpX.Sample(smp_base, snow_uv).y);
#else
    float snow_roughness = 0.6f;
#endif

    if (is_pbr) {
        matter.roughness = lerp(matter.roughness, snow_roughness, snow);
        matter.f0 = lerp(matter.f0, 0.04f, snow);
        metalness *= 1.0f - snow;
        occlusion = lerp(occlusion, 1.0f, snow);
    }
#endif
#endif
#if (FEATURE_SET) & SurfaceMask_Environment
    matter.base.rgb = lerp(SURFACE_ENV(input.reflection).rgb, matter.base.rgb, matter.base.a);
#endif
    matter.wet_normal = matter.normal;
#if (FEATURE_SET) & SurfaceMask_Bump
    matter.metalness = metalness;
    matter.occlusion = occlusion;
#ifdef SURFACE_WETNESS
    // What the rain reaches is wet before anything lights it or is mirrored in it: darker unless it is a metal,
    // with the rings and streaks of the rain in its normal. The HUD has its own drops
    [branch]
    if (rain_setup.w > 0.0f && input.position_proj.z >= 0.02f) {
        // How much of the sky's light the point has: where nothing tells it, the share of the sky's light in the
        // light of its vertex
        float sky_reach = matter.sky_known >= 0.0f ? saturate(matter.sky_known) : saturate(dot(input.light.rgb, 1.0f / 3.0f) / max(dot(L_hemi_color.rgb + L_ambient.rgb, 1.0f / 3.0f), 1e-3f));
        float3 wet_at = surface_world(input.position_world);
        float rain_reached;
        float wet = rain_exposure(wet_at, normalize(input.normal), sky_reach, input.position_proj.w, input.position_proj.xy, rain_reached);
        float puddle = 0.0f;
        matter.wet_normal = normalize(matter.normal + rain_ripples(wet_at, matter.normal) * wet);
#ifdef SURFACE_PUDDLES
        // Water fills the terrain from its low places up as it stays wet, as far as the rain reaches: smooth, dark,
        // with its own ripples, and no wet ground under it. The layers' weighed heights are in bump.x
        float3 puddle_normal;
        float2 puddle_refraction;
        puddle = GetRainPuddle(wet_at.xz * rain_level.zw - rain_level.xy, mul((float3x3)m_V, input.normal), mul(m_V, float4(wet_at, 1.0f)).xyz, saturate(bump.x), true, puddle_normal, puddle_refraction);
        puddle *= rain_reached * step(1e-4f, wet);
        matter.wet_normal = normalize(lerp(matter.wet_normal, mul(puddle_normal, (float3x3)m_V), puddle));
        matter.base.rgb *= lerp(1.0f, 0.3f * occlusion, puddle);
        wet *= 1.0f - puddle;
#endif

        if (is_pbr) {
            matter.base.rgb *= lerp(1.0f, lerp(0.66f, 1.0f, metalness), wet);
        } else {
            // The old maps get wet as their own frame wet them: darker by how bright the surface is, by a half at
            // most, and glossier by the same
            float soak = wet * dot(matter.base.rgb, 0.33f);
            matter.base.rgb *= max(1.0f - sqrt(soak), 0.5f);
            // The gloss is kept squared: that frame added 0.8 of the soak to the gloss itself
            float gloss = saturate(sqrt(matter.gloss) + 0.8f * soak);
            matter.gloss = gloss * gloss;
        }

        matter.wet = wet;
        matter.puddle = puddle;
    }
#endif
#endif

#if (FEATURE_SET) & SurfaceMask_Layers
    float4 mask = SURFACE_MAP(s_mask, smp_base, input.uv.xy);
    mask = mask / dot(mask, 1.0f);
#ifdef SURFACE_CLUSTER
    float2 detail_dx = ddx(input.detail.xy);
    float2 detail_dy = ddy(input.detail.xy);
    matter.detail = 0.0f;

    [unroll] for (uint detail_idx = 0u; detail_idx < 4u; detail_idx++) {
        [branch] if (mask[detail_idx] > 0.0f) {
            matter.detail += surface_layer_map(SCENE_TERRAIN_COLOUR + detail_idx, input.detail.xy, detail_dx, detail_dy).rgb * mask[detail_idx];
        }
    }
#else
    matter.detail = SURFACE_MAP(s_dt_a, smp_base, input.detail.xy).rgb * mask.a + SURFACE_MAP(s_dt_b, smp_base, input.detail.xy).rgb * mask.b + SURFACE_MAP(s_dt_g, smp_base, input.detail.xy).rgb * mask.g + SURFACE_MAP(s_dt_r, smp_base, input.detail.xy).rgb * mask.r;
#endif
#elif ((FEATURE_SET) & SurfaceMask_Detail) && SURFACE_LIGHT(FEATURE_SET) == SurfaceLight_Terrain
    matter.detail = SURFACE_MAP(s_detail, smp_base, input.detail.xy).rgb;
#elif defined(SURFACE_FOLDED)
    // A material with no detail texture has none in its record
    float2 detail_dx = ddx(input.detail.xy);
    float2 detail_dy = ddy(input.detail.xy);

    [branch] if (surface_material.detail != SCENE_TEXTURE_ABSENT) {
        matter.detail = SURFACE_MAP_AT(s_detail, smp_base, input.detail.xy, detail_dx, detail_dy).rgb * input.detail.z + input.detail.w;
    }
#elif (FEATURE_SET) & SurfaceMask_Detail
    matter.detail = SURFACE_MAP(s_detail, smp_base, input.detail.xy).rgb * input.detail.z + input.detail.w;
#endif
#ifdef SURFACE_SNOW
    // The ground's detail does not show through the snow
    matter.detail = lerp(matter.detail, 0.5f, snow);
#endif
    return matter;
}

surface_data default_lit(surface_vertex input, surface_matter matter)
{
#if defined(USE_SURFACE_SUNMASK) && defined(USE_SURFACE_SUN_SHADOWS) && !defined(SURFACE_NO_SUN_SHADOWS) && !defined(USE_SURFACE_CLASSIC) && SURFACE_LIGHT(FEATURE_SET) != SurfaceLight_None
    // The clouds' shadow dims the sun, as sunmask of common\sun_shadow.hlsli dims it in the other frame: an
    // encoded share, decoded with the light it scales. Without it the sun stood three times stronger against the
    // sky's light than there, and what the sun does not reach looked black
    input.sun *= 0.25f + 0.75f * s_sunmask.SampleLevel(smp_linear, mul(m_sunmask, float4(surface_world(input.position_world), 1.0f)).xy, 0.0f).w;
#endif
    surface_data surface;
    surface.base = matter.base;
    surface.detail = matter.detail;
    surface.specular = 0.0f;
    surface.reflect = 0.0f;
    surface.mirror = 0.0f;
    surface.mirrored = 0.0f;
    surface.f0 = matter.f0;
    surface.roughness = matter.roughness;
    surface.gloss = matter.gloss;
    surface.sky_reach = 1.0f;
    surface.local = 0.0f;
    surface.sun = 0.0f;
    surface.normal = matter.normal;
#if (FEATURE_SET) & SurfaceMask_Bump
    // The vertex hands over the sun's colour alone: how it falls on the mapped normal is worked out here
    bool is_pbr = matter.is_pbr;
    float metalness = matter.metalness;
    float occlusion = matter.occlusion;
    float3 normal = matter.normal;
    float3 to_sun = -L_sun_dir_w.xyz;
    float3 to_eye = normalize(eye_position - input.position_world);
    float3 sun = input.sun * r1_lambert(dot(normal, to_sun));
    // The share of the sun's light the surface takes, where the light is summed in linear
    float3 sun_share = saturate(dot(normal, to_sun));
    float3 highlight = input.sun * surface_highlight(surface, to_eye, to_sun);
#if defined(USE_SURFACE_HIGH) || defined(SURFACE_OLD_LEAF)
    float3 scatter = 0.0f;
#ifdef USE_SURFACE_HIGH
    // The light through the skin, less on the HUD: the hands are held in front of the sun far too often
    [branch] if (is_pbr) {
        scatter = surface_scatter(normal, to_sun, to_eye) * matter.scatter * (input.position_proj.z < 0.02f ? 0.35f : 1.0f);
    }
#endif
#ifdef SURFACE_OLD_LEAF
    [branch] if (!is_pbr && SURFACE_IS_LEAF(surface_material.flags)) {
        scatter = surface_scatter(normal, to_sun, to_eye);
    }
#endif
    sun += input.sun * scatter;
    sun_share += scatter;
#endif
#else
#if SURFACE_VERTEX(FEATURE_SET) == SurfaceVertex_Detail && !defined(USE_SURFACE_CLASSIC)
    // A blade lets the sun through on the side turned from it: SimpleTranslucency of common\lighting.hlsli. The
    // cosine is encoded with the light: the sun is decoded after it is doubled
    float blade_facing = dot(normalize(input.normal), -L_sun_dir_w.xyz);
    // Only what sways lets the sun through, as in the other frame: a bush or a fir that stands still is not a blade
    // The share is what the linear sum takes: without it every blade had all of the sun, whichever way it faced
    float3 sun_share = saturate(blade_facing) + pow(surface_through(-blade_facing), 2.2f) * input.detail.w;
    float3 sun = input.sun * pow(sun_share, 1.0f / 2.2f);
#else
    float3 sun = input.sun;
    float3 sun_share = 1.0f;
#endif
    float3 highlight = 0.0f;
#endif
    // The HUD takes the sun's shadow of one point near the eye, as the dynamic renderer's HUD does: its own points
    // are not places in the world, and its depth is the nearest fiftieth of the range
    bool is_hud = input.position_proj.z < 0.02f;
    float3 shadow_normal = input.normal;
#if SURFACE_VERTEX(FEATURE_SET) == SurfaceVertex_Detail && !defined(USE_SURFACE_CLASSIC)
    bool lets_through = input.detail.w > 0.0f;
#elif defined(SURFACE_OLD_LEAF)
    bool lets_through = SURFACE_IS_LEAF(surface_material.flags);
#else
    bool lets_through = false;
#endif
    // SunShadowPoint of common\sun_shadow.hlsli: what lets the sun through is tested off its lit side, or the side
    // turned from the sun is hidden by the leaf itself and has nothing to let through
    if (lets_through && dot(shadow_normal, L_sun_dir_w.xyz) > 0.0f) {
        shadow_normal = -shadow_normal;
    }

    float2 cascade = surface_sun_shadow(surface_world(is_hud ? eye_position - L_sun_dir_w.xyz * 0.4f : input.position_world), is_hud ? -L_sun_dir_w.xyz : shadow_normal);
    float sky_known = matter.sky_known;

#if SURFACE_LIGHT(FEATURE_SET) == SurfaceLight_Terrain
    float4 baked = SURFACE_MAP(s_lmap, smp_base, input.uv.zw);
#ifdef USE_SURFACE_DYNAMIC
    // High lighting quality takes nothing a lightmap holds: no baked lamp, and the sun wherever no cascade hides it
    baked = float4(0.0f, 0.0f, 0.0f, 1.0f);
#endif
    float sun_reach = lerp(baked.a, cascade.x, cascade.y);
    float3 around = L_ambient.xyz + baked.rgb + input.light.rgb * matter.hemi;
    surface.specular = highlight * sun_reach;
// What is unlit stays unlit when its draw reads its material from the record
#elif defined(SURFACE_FOLDED) && SURFACE_LIGHT(FEATURE_SET) != SurfaceLight_None
    float sun_reach;
    float3 around;

    [branch] if (surface_material.flags & (SCENE_MATERIAL_VERTEX_LIT | SCENE_MATERIAL_PLACED)) {
        // What the vertex was baked to get of the sun gives way to the cascade inside it, as a lightmap's alpha does
#ifdef USE_SURFACE_DYNAMIC
        sun_reach = lerp(1.0f, cascade.x, cascade.y);
#else
        sun_reach = lerp(input.tint.r, cascade.x, cascade.y);
#endif
        around = input.light.rgb;
    } else {
        float4 baked = SURFACE_MAP_AT(s_lmap, smp_rtlinear, input.uv.zw, ddx(input.uv.zw), ddy(input.uv.zw));
#ifdef USE_SURFACE_DYNAMIC
        baked = float4(0.0f, 0.0f, 0.0f, 1.0f);
#endif
        sun_reach = lerp(baked.a, cascade.x, cascade.y);
        around = L_ambient.xyz + baked.rgb + input.light.rgb * matter.hemi;
    }

    surface.specular = highlight * sun_reach;
#elif SURFACE_LIGHT(FEATURE_SET) == SurfaceLight_Lightmap
    float4 baked = SURFACE_MAP(s_lmap, smp_rtlinear, input.uv.zw);
#ifdef USE_SURFACE_DYNAMIC
    baked = float4(0.0f, 0.0f, 0.0f, 1.0f);
#endif
    float sun_reach = lerp(baked.a, cascade.x, cascade.y);
    float3 around = L_ambient.xyz + baked.rgb + input.light.rgb * matter.hemi;
    surface.specular = highlight * sun_reach;
#elif SURFACE_LIGHT(FEATURE_SET) == SurfaceLight_None
#if defined(SURFACE_SELFLIGHT) && defined(USE_LEGACY_LIGHT)
    // What gives off light gives off four times its texture, as the old light model's frame added it
    float3 around = 2.0f;
#else
    float3 around = 0.5f;
#endif
#else
    // What the vertex was baked to get of the sun gives way to the cascade inside it, as a lightmap's alpha does
#ifdef USE_SURFACE_DYNAMIC
    float sun_reach = lerp(1.0f, cascade.x, cascade.y);
#else
    float sun_reach = lerp(input.tint.r, cascade.x, cascade.y);
#endif
#if defined(USE_SURFACE_HUD_SHADOWS) && defined(USE_DEPTH_PREPASSED)
    // The contact shadows of the HUD, settled by hud_contact_filter: 1 everywhere else. No sun is left in them, as
    // in the dynamic renderer's HUD
    // Only the HUD has them: its depth is the nearest fiftieth of the range. The darkest of the pixel and its four neighbours is taken:
    // under r_msaa a pixel on a silhouette holds two surfaces and one value, that of the nearer one, and the other
    // would take it for its own and show a lit line along the edge
    [branch]
    if (input.position_proj.z < 0.02f) {
        int3 at = int3(input.position_proj.xy, 0);
        float contact = min(s_hud_contact.Load(at).x, min(min(s_hud_contact.Load(at, int2(-1, 0)).x, s_hud_contact.Load(at, int2(1, 0)).x),
            min(s_hud_contact.Load(at, int2(0, -1)).x, s_hud_contact.Load(at, int2(0, 1)).x)));
        sun_reach *= contact;
    }
#endif
    float3 around = input.light.rgb;
    surface.specular = highlight * sun_reach;
#endif
#ifdef SURFACE_SNOW
    // The sun's highlight was the ground's: snow scatters, it does not shine
    surface.specular *= 1.0f - matter.snow;
#endif
#if defined(USE_SURFACE_OCCLUSION) && defined(USE_DEPTH_PREPASSED) && SURFACE_LIGHT(FEATURE_SET) != SurfaceLight_None
    // What reaches the pixel from all around is dimmed by its occlusion, computed from the depth pass; the sun and
    // the local lights are not
    // The occlusion is a share of linear light and this light is decoded in the end: its gamma encoding is taken,
    // or half an occlusion would leave a fifth of the light
#ifdef USE_LEGACY_LIGHT
    around *= s_occ.Load(int3(input.position_proj.xy, 0)).x;
#else
    around *= pow(s_occ.Load(int3(input.position_proj.xy, 0)).x, 1.0f / 2.2f);
#endif
#endif
#if (FEATURE_SET) & SurfaceMask_Bump
    // The sky lights what looks up more than what looks down. The vertex weighed it by its own normal: the mapped
    // normal's share replaces it, so a surface the sun does not reach still shows its relief
    float3 flat = input.normal * rsqrt(max(dot(input.normal, input.normal), 1e-8f));
    float relief = (0.5f + 0.5f * surface.normal.y) / max(0.5f + 0.5f * flat.y, 0.25f);
    // A third of it: the ambient cube of the dynamic renderer changes by a tenth with the direction, not by half
    relief = lerp(1.0f, relief, 0.3f);
    if (is_pbr) {
        // What the surroundings reflect is taken as the light that falls on the point, the local lights included: a
        // metal has nothing else to show
        surface.reflect = surface.f0 * 2.0f;
        surface.base.rgb *= 1.0f - metalness;
        // The occlusion map darkens what comes from all around, not what a light sends straight at the point
        relief *= occlusion;
    }
    // How much of the sky's light the point has: its baked hemi, or what its tracker measured for an object.
    // Where neither is known, the share of the sky's light in the light on it, before the mapped normal's tilt
    float sky_reach = saturate(dot(around, 1.0f / 3.0f) / max(dot(L_hemi_color.rgb + L_ambient.rgb, 1.0f / 3.0f), 1e-3f));
    sky_reach = sky_known >= 0.0f ? saturate(sky_known) : sky_reach;
    around *= relief;
#ifdef USE_SURFACE_IBL
    // The sky's light has the colour of the sky the surface looks at, as the ambient cube of the dynamic renderer
    // gives it: the coarsest level of the sky cube that way, its brightness left out. Half of its colour: the sky
    // itself is more saturated than the cube that holds its light
    float3 sky_tint = lerp(sky_s0.SampleLevel(smp_rtlinear, surface.normal, 16.0f).rgb, sky_s1.SampleLevel(smp_rtlinear, surface.normal, 16.0f).rgb, L_hemi_color.w);
    sky_tint = lerp(1.0f, sky_tint / max(dot(sky_tint, 1.0f / 3.0f), 1e-3f), 0.5f);
    around *= sky_tint;
#endif
    // What mirrors, and what the local lights fall on, has the rain's rings and the water that stands on it
    surface.normal = matter.wet_normal;
    // The water fills what the occlusion map holds
    occlusion = lerp(occlusion, 1.0f, matter.puddle);
#ifdef USE_SURFACE_IBL
    // The sky mirrored in the surface, blurred by its roughness, as far as the sky reaches the point: that is
    // taken from how much of the sky's light the point has. Where it does not reach, the surroundings stay what
    // the light on the point makes of them
    // An old surface as the frame that shades a pixel once makes it of its maps (MaterialFinish of
    // common\material.hlsli, GbufferUnpackColor): as smooth as its texture's material value tells, and mirroring by
    // its gloss squared. One minus the gloss for the roughness made a mirror of every glossy barrel
    float old_roughness = surface_roughness_wide(surface.normal, to_eye, surface_kind() * 0.25f + 0.15f);
    float3 mirror_f0 = is_pbr ? surface.f0 : (float3)(0.16f * surface.gloss * surface.gloss);
    float mirror_roughness = is_pbr ? surface.roughness : old_roughness;
#ifdef SURFACE_WETNESS
    mirror_roughness = lerp(lerp(mirror_roughness, min(0.2f, mirror_roughness), matter.wet), 0.03f, matter.puddle);
    mirror_f0 = lerp(mirror_f0, WATER_F0, matter.puddle);
#endif
    surface.sky_reach = sky_reach;
    surface.roughness = mirror_roughness;
    // What occludes a mirrored ray, as SpecularOcclusion of the advanced frame's ambient light has it: the
    // rougher the surface and the more head-on the eye, the more of the occlusion it takes
    float facing_eye = saturate(dot(surface.normal, to_eye));
    float narrowing = exp2(-16.0f * mirror_roughness - 1.0f);
    float mirror_occlusion = occlusion;
#if defined(USE_SURFACE_REFLECTIONS) && defined(USE_DEPTH_PREPASSED)
    // What the reflection passes traced from the depth pass's word of this pixel: the frame before where the ray
    // hit it, the sky as far as it reaches the point where it did not. The occlusion map of the surface is left
    float3 sky = s_refl.Load(int3(input.position_proj.xy, 0)).rgb;
    sky_reach = saturate(pow(facing_eye + mirror_occlusion, narrowing) - 1.0f + mirror_occlusion);
#else
    float sky_width, sky_height, sky_levels;
    sky_s0.GetDimensions(0, sky_width, sky_height, sky_levels);
    float3 mirrored = reflect(-to_eye, surface.normal);
    float sky_lod = (sky_levels - 1.0f) * mirror_roughness * (1.7f - 0.7f * mirror_roughness);
    float3 sky = lerp(sky_s0.SampleLevel(smp_rtlinear, mirrored, sky_lod).rgb, sky_s1.SampleLevel(smp_rtlinear, mirrored, sky_lod).rgb, L_hemi_color.w) * L_sky_color.rgb;
    sky_reach = saturate(pow(facing_eye + sky_reach, narrowing) - 1.0f + sky_reach) * mirror_occlusion;
#if defined(USE_SURFACE_OCCLUSION) && defined(USE_DEPTH_PREPASSED)
    sky_reach *= s_occ.Load(int3(input.position_proj.xy, 0)).x;
#endif
#endif
    // How much of it a surface of this roughness mirrors at this angle, by a fit to the integrated GGX response
    float4 fit = mirror_roughness * float4(-1.0f, -0.0275f, -0.572f, 0.022f) + float4(1.0f, 0.0425f, 1.04f, -0.04f);
    float fit_scale = min(fit.x * fit.x, exp2(-9.28f * saturate(dot(surface.normal, to_eye)))) * fit.x + fit.y;
    float2 response = float2(-1.04f, 1.04f) * fit_scale + fit.zw;
    // The frame is lit in gamma space, and a share of light is a share of linear light
    surface.mirrored = saturate(mirror_f0 * response.x + response.y);
#ifdef USE_LEGACY_LIGHT
    // A surface with the old gloss map mirrors as the old light model's ambient highlight did: by its gloss, most of
    // it seen head-on and nothing at a graze. The gloss is kept squared
    if (!is_pbr) {
        surface.mirrored = sqrt(surface.gloss) * 0.5f * pow(facing_eye * facing_eye, 16.0f);
    }
#endif
    surface.mirror = sky * pow(surface.mirrored, 1.0f / 2.2f) * sky_reach;
    surface.reflect *= 1.0f - surface.sky_reach;
#endif
#endif
    // The sun stays out of what comes from all around: only low shading quality sums the two, and a pass that asks
    // for neither drops the sun's shadow with it
    surface.light = around;
#if defined(USE_SURFACE_CLASSIC) && SURFACE_LIGHT(FEATURE_SET) != SurfaceLight_None
    surface.light += sun * sun_reach;
#endif
#ifdef USE_R1_EMISSION
    surface.light += SURFACE_MAP(s_emission, smp_base, input.uv.xy).rgb;
#endif
#if !defined(USE_SURFACE_CLASSIC) && SURFACE_LIGHT(FEATURE_SET) != SurfaceLight_None
    // From medium shading quality up the light is summed in linear, as the dynamic renderer sums it. The sun leaves
    // the gamma sum, which keeps what comes from all around, and is handed over as linear light, its highlight
    // with it. A light of one half is the unit of the gamma sum: its values are doubled before they are decoded
    float3 radiance = pow(max(input.sun * 2.0f, 0.0f), 2.2f);
    // The cascade's answer is decoded like any other light of the gamma sum, as SunLight of common\lighting.hlsli
    // decodes its shadow: half a shadow leaves a fifth of the sun, and a penumbra is darker for it
    float sun_through = pow(sun_reach, 2.2f);
#ifdef USE_LEGACY_LIGHT
    // The old light model sums the sun with the rest as a gamma value: surface_lit decodes the sum
    surface.sun = input.sun * 2.0f * sun_share * sun_reach;
#else
    surface.sun = radiance * sun_share * sun_through;
#endif
    surface.specular = radiance * (surface.specular / max(input.sun * max(sun_reach, 1e-6f), 1e-6f)) * sun_through * SURFACE_HIGHLIGHT_LINEAR;
#endif
    return surface;
}

surface_data default_fetch(surface_vertex input)
{
    surface_matter matter = default_matter(input);
    return default_lit(input, matter);
}

float3 default_light(surface_vertex input, surface_data surface, light_shading_data light)
{
#if SURFACE_VERTEX(FEATURE_SET) == SurfaceVertex_Detail
    // A lamp lights the grass as it lights the ground the grass stands on
    float facing = r1_lambert(light.dir.y);
#else
    float facing = dot(surface.normal, surface.normal) > 0.0f ? r1_lambert(dot(normalize(surface.normal), light.dir)) : 1.0f;
#endif
    return light.color * facing;
}

// returns what a pass writes to the scene of a gamma colour. Above low shading quality the scene holds linear light,
// as the frame that shades a pixel once has it: what blends over it then adds and covers as light does. Blended in
// gamma, five thin layers of a glowing gas came out eight times as bright
float3 surface_scene(float3 color)
{
#ifdef USE_SURFACE_CLASSIC
    return color;
#else
    return pow(max(color, 0.0f), 2.2f);
#endif
}

// returns the light on a surface, linear. The old light model (USE_LEGACY_LIGHT) sums the sky, the sun and the
// local lights as gamma values, as its own frame did, and decodes the sum once; sun and local are gamma values then
float3 surface_lit(surface_data surface)
{
#ifdef USE_LEGACY_LIGHT
    return pow(max(surface.light * 2.0f + surface.sun + surface.local, 0.0f), 2.2f);
#else
    return pow(max(surface.light * 2.0f, 0.0f), 2.2f) + surface.sun + surface.local;
#endif
}

// returns the colour of a plain surface of this base colour under the light on it, as the scene takes it. Low
// shading quality sums the light in gamma space, as the classic frame did; above it the light around, the sun and
// the local lights are summed in linear
float3 surface_plain(float3 base, surface_data surface)
{
#ifdef USE_SURFACE_CLASSIC
    return surface.light * base * 2.0f;
#else
    return pow(max(base, 0.0f), 2.2f) * surface_lit(surface);
#endif
}

// The sky and the clouds behind the pixel, as the scene holds them
Texture2D s_sky_behind : register(t58);

float4 default_shade(surface_vertex input, surface_data surface)
{
#ifdef USE_SURFACE_CLASSIC
    float3 color = (surface.light * surface.base.rgb * 2.0f) * surface.detail * 2.0f + surface.specular + surface.reflect * surface.light;
    color = lerp(fog_color.xyz, color, input.light.w);
    // What is far goes into the sky behind it, as below: fogged to one colour alone a far tree stood as a pale shape
    // against the clouds
#ifndef SURFACE_REFLECTION_FACE
    float fog = 1.0f - input.light.w;

    [branch] if (fog > 0.0f) {
        color = lerp(color, s_sky_behind.Load(int3(input.position_proj.xy, 0)).rgb, fog * fog);
    }
#endif
#else
    // The surface's colour under a light of one half is its albedo; what it mirrors of the light on it and of its
    // surroundings is added as light, and the share it mirrors is taken from what it scatters
    float3 albedo = pow(max(surface.base.rgb * surface.detail * 2.0f, 0.0f), 2.2f);
    float3 lit = surface_lit(surface);
    float3 color = albedo * lit + surface.specular;
#ifdef USE_SURFACE_IBL
    color = color * (1.0f - surface.mirrored) + pow(max(surface.mirror, 0.0f), 2.2f);
#else
    // With nothing mirrored, a metal shows the light on it instead
    color += surface.reflect * 0.5f * lit;
#endif
    // The fog as the frame that shades a pixel once lays it: over linear colour, its amount and its colour decoded.
    // Laid over the encoded colour it lifted a dark scene several times over
    // By how far the pixel is from the eye, as ApplyFog of common\ambient.hlsli: a plane's distance set at the
    // vertices fogs the sides of the view less and a large triangle wrongly
    float fog = pow(saturate(length(eye_position - input.position_world) * fog_params.w + fog_params.x), 2.2f);
    color = lerp(color, pow(max(fog_color.xyz, 0.0f), 2.2f), fog);
    // What is far goes into the sky behind it by the fog squared, as ApplyFog hands it to that frame's blend over
    // the sky: fogged to one colour alone, a far hill stood as a flat shape against the clouds
#ifndef SURFACE_REFLECTION_FACE
    [branch] if (fog > 0.0f) {
        color = lerp(color, s_sky_behind.Load(int3(input.position_proj.xy, 0)).rgb, fog * fog);
    }
#endif
    color = max(color, 0.0f);
#endif
#if SURFACE_LIGHT(FEATURE_SET) == SurfaceLight_Terrain
    return float4(color, 1.0f);
#else
    return float4(color, surface.base.a);
#endif
}

#endif
