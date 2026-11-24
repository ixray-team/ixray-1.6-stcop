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
// The tiles of the screen that hold ids, listed by the classes of material in them and once more whatever they hold:
// a pass that shades the ids is dispatched a group a tile of its list, by the list's count. TILES_RESET: the counts set to none, before the tiles are listed
#include "common\fixed_cb.hlsli"
#include "common\common_samplers.hlsli"
#include "common\scene_tables.hlsli"
#include "common\scene_id.hlsli"
#include "common\scene_gbuffer.hlsli"
#include "common\thin_surface.hlsli"

Texture2D<uint> g_ids : register(t0);
Texture2D<uint2> g_surface : register(t1); // what the depth pass of a model has kept already
RWBuffer<uint> g_tile_counts : register(u0); // a list: its groups along x, then 1 and 1
RWStructuredBuffer<uint> g_tiles : register(u1); // a list: SCENE_SHADE_TILES places, a tile's x | y << 16

#ifndef TILES_RESET
// What the screen-space passes read of a blade of grass: its depth pass keeps the surface alone
RWTexture2D<uint> g_thin : register(u2);
#endif

#ifdef TILES_RESET
[numthreads(1, 1, 1)]
void main()
{
    for (uint list_idx = 0u; list_idx < SCENE_SHADE_LISTS; list_idx++) {
        g_tile_counts[list_idx * 3u] = 0u;
        g_tile_counts[list_idx * 3u + 1u] = 1u;
        g_tile_counts[list_idx * 3u + 2u] = 1u;
    }
}
#else
// A thread a tile: it gathers the tile's sixty-four pixels itself and shares nothing. A group of the tile's own
// pixels that met in shared memory was a seventh of what shading the ids cost
[numthreads(SCENE_SHADE_TILE, SCENE_SHADE_TILE, 1)]
void main(uint3 tile_id : SV_DispatchThreadID)
{
    uint2 size;
    g_ids.GetDimensions(size.x, size.y);
    uint2 first = tile_id.xy * SCENE_SHADE_TILE;

    if (any(first >= size)) {
        return;
    }

    uint classes = 0u;

    for (uint gather_idx = 0u; gather_idx < SCENE_SHADE_TILE * SCENE_SHADE_TILE / 4u; gather_idx++) {
        uint2 corner = first + uint2(gather_idx % (SCENE_SHADE_TILE / 2u), gather_idx / (SCENE_SHADE_TILE / 2u)) * 2u;
        float2 at = (float2(corner) + 1.0f) / float2(size);
        uint4 ids = g_ids.GatherRed(smp_nofilter, at);
        // A surface with no id is lit with the rest and resolved by no class
        uint4 kept = g_surface.GatherGreen(smp_nofilter, at);
        uint4 bits = (ids != 0u ? 1u << (ids & SCENE_ID_CLASS_MASK) : 0u) | (kept != 0u ? 1u << (SCENE_SHADE_LISTS - 1u) : 0u);
        classes |= bits.x | bits.y | bits.z | bits.w;

        // A gather's texels: the lower left one, then counter-clockwise
        for (uint texel_idx = 0u; texel_idx < 4u; texel_idx++) {
            scene_surface blade = scene_surface_unpack(uint2(0u, kept[texel_idx]));

            [branch] if (kept[texel_idx] != 0u && blade.material_class == SceneClass_Grass) {
                uint2 pixel = corner + uint2(texel_idx == 1u || texel_idx == 2u, texel_idx < 2u);
                g_thin[pixel] = thin_pack(normalize(mul((float3x3)m_V, blade.normal)), 1.0f, 1.0f, 0u, false, false);
            }
        }
    }

    if (classes == 0u) {
        return;
    }

    // The last list is of every tile; a class's of the tiles with its ids
    classes |= 1u << (SCENE_SHADE_LISTS - 1u);

    for (uint list_idx = 0u; list_idx < SCENE_SHADE_LISTS; list_idx++) {
        if (classes & (1u << list_idx)) {
            uint place;
            InterlockedAdd(g_tile_counts[list_idx * 3u], 1u, place);
            g_tiles[list_idx * SCENE_SHADE_TILES + place] = tile_id.x | tile_id.y << 16u;
        }
    }
}
#endif
