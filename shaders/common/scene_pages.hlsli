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
// A cluster of a level mesh as its page (ClusterPages.h): a header at SceneCluster.vertices, then two words of
// attributes a vertex; positions as offsets from the cluster's anchor on a grid of the level, each axis in its own
// count of bits; a baked word a vertex where the mesh has one; a corner a byte at SceneCluster.index_start. The
// includer names the words: #define SCENE_PAGE_WORDS <a Buffer<uint>>
#ifndef SCENE_PAGES_HLSLI
#define SCENE_PAGES_HLSLI

#define SCENE_PAGE_HEADER 9u
#define SCENE_PAGE_GRID (1.0f / 1024.0f)
#define ScenePageBaked_None 0u
#define ScenePageBaked_Lightmap 1u
#define ScenePageBaked_Colour 2u
#define ScenePageBaked_Tree 3u

uint scene_page_corner(uint first, uint corner_idx)
{
    return (SCENE_PAGE_WORDS[first + (corner_idx >> 2u)] >> ((corner_idx & 3u) * 8u)) & 0xFFu;
}

// returns the word a cluster's meshlet starts at: a byte of how many places its vertices stand in, a byte a place
// of the vertex that stands for it, then three bytes a triangle, each a place. Its bytes are read as corners are
uint scene_page_places(uint first, uint triangle_count)
{
    return first + ((triangle_count * 3u + 3u) >> 2u);
}

uint scene_page_field(uint4 words, uint at, uint count)
{
    uint word_idx = at >> 5u;
    uint shift = at & 31u;
    uint low = words[word_idx] >> shift;
    uint high = shift != 0u && word_idx < 3u ? words[word_idx + 1u] << (32u - shift) : 0u;
    return (low | high) & ((1u << count) - 1u);
}

float3 scene_page_position(uint header, uint vertex_idx)
{
    uint3 grid = uint3(SCENE_PAGE_WORDS[header], SCENE_PAGE_WORDS[header + 1u], SCENE_PAGE_WORDS[header + 2u]);
    uint3 widths = (grid >> 24u) & 31u;
    int3 anchor = int3(grid << 8u) >> 8;
    uint bit = vertex_idx * (widths.x + widths.y + widths.z);
    uint word_idx = SCENE_PAGE_WORDS[header + 3u] + (bit >> 5u);
    uint4 words = uint4(SCENE_PAGE_WORDS[word_idx], SCENE_PAGE_WORDS[word_idx + 1u], SCENE_PAGE_WORDS[word_idx + 2u], SCENE_PAGE_WORDS[word_idx + 3u]);
    uint at = bit & 31u;
    uint3 offset = uint3(scene_page_field(words, at, widths.x), scene_page_field(words, at + widths.x, widths.y), scene_page_field(words, at + widths.x + widths.y, widths.z));
    return float3(anchor + int3(offset)) * SCENE_PAGE_GRID;
}

// returns the UV: an offset from the cluster's least, in the steps the heap's vertex holds its UV in
float2 scene_page_uv(uint header, uint vertex_idx)
{
    uint packed = SCENE_PAGE_WORDS[header + SCENE_PAGE_HEADER + vertex_idx * 2u + 1u];
    int2 least = int2(SCENE_PAGE_WORDS[header + 4u], SCENE_PAGE_WORDS[header + 5u]);
    uint2 shift = uint2(SCENE_PAGE_WORDS[header + 6u], SCENE_PAGE_WORDS[header + 7u]);
    return float2(least + int2(uint2(packed & 0xFFFFu, packed >> 16u) << shift)) * (1.0f / 262144.0f);
}

// returns the baked word of a vertex, 0 where the mesh has none
uint scene_page_baked(uint header, uint vertex_idx)
{
    uint first = SCENE_PAGE_WORDS[header + 8u];
    return first != 0xFFFFFFFFu ? SCENE_PAGE_WORDS[first + vertex_idx] : 0u;
}

// returns the normal of a vertex, as its mesh has it
float3 scene_page_normal(uint header, uint vertex_idx)
{
    uint frame = SCENE_PAGE_WORDS[header + SCENE_PAGE_HEADER + vertex_idx * 2u];
    float2 folded = (float2(frame & 511u, (frame >> 9u) & 511u) - 255.0f) * (1.0f / 255.0f);
    float3 raw = float3(folded, 1.0f - abs(folded.x) - abs(folded.y));
    float under = saturate(-raw.z);
    raw.xy += raw.xy >= 0.0f ? -under : under;
    return normalize(raw);
}

// returns a vertex's baked share of the sky's light
float scene_page_hemi(uint header, uint vertex_idx)
{
    uint hemi = SCENE_PAGE_WORDS[header + SCENE_PAGE_HEADER + vertex_idx * 2u] >> 26u;
    return float((hemi << 2u) | (hemi >> 4u)) * (1.0f / 255.0f);
}

// returns the kind of baked word the cluster's vertices have, ScenePageBaked_
uint scene_page_kind(uint header)
{
    return (SCENE_PAGE_WORDS[header] >> 29u) & 3u;
}

// returns where a vertex is in its lightmap, from a baked word of ScenePageBaked_Lightmap
float2 scene_page_lightmap(uint baked)
{
    return float2(int2((baked.xx >> uint2(0u, 13u)) & 0x1FFFu) * 8 - 32764) * (1.0f / 32768.0f);
}

// The frame of a vertex's normal map, and its baked share of the sky's light
struct page_frame {
    float3 normal;
    float3 tangent;
    float3 bitangent;
    float hemi;
};

page_frame scene_page_frame(uint header, uint vertex_idx)
{
    uint frame = SCENE_PAGE_WORDS[header + SCENE_PAGE_HEADER + vertex_idx * 2u];
    float2 folded = (float2(frame & 511u, (frame >> 9u) & 511u) - 255.0f) * (1.0f / 255.0f);
    float3 raw = float3(folded, 1.0f - abs(folded.x) - abs(folded.y));
    float under = saturate(-raw.z);
    raw.xy += raw.xy >= 0.0f ? -under : under;

    page_frame basis;
    basis.normal = normalize(raw);
    float3 axis = normalize(abs(raw.y) > abs(raw.z) ? float3(raw.y, -raw.x, 0.0f) : float3(raw.z, 0.0f, -raw.x));
    float turn = float((frame >> 18u) & 127u) * (1.0f / 127.0f);
    float along = turn <= 0.5f ? 1.0f - 4.0f * turn : 4.0f * turn - 3.0f;
    float2 diamond = normalize(float2(along, (turn <= 0.5f ? 1.0f : -1.0f) * (1.0f - abs(along))));
    basis.tangent = diamond.x * axis + diamond.y * cross(axis, basis.normal);
    basis.bitangent = ((frame & 0x2000000u) ? -1.0f : 1.0f) * cross(basis.normal, basis.tangent);
    uint hemi = frame >> 26u;
    basis.hemi = float((hemi << 2u) | (hemi >> 4u)) * (1.0f / 255.0f);
    return basis;
}

#ifdef VERTEX_HLSLI
// returns the vertex as the heap's 32 bytes decode: what reads one reads a page's as it is
PulledVertex scene_page_vertex(uint header, uint vertex_idx)
{
    uint frame = SCENE_PAGE_WORDS[header + SCENE_PAGE_HEADER + vertex_idx * 2u];
    uint kind = scene_page_kind(header);
    uint baked = scene_page_baked(header, vertex_idx);
    page_frame basis = scene_page_frame(header, vertex_idx);

    PulledVertex vertex;
    vertex.P = scene_page_position(header, vertex_idx);
    vertex.N = basis.normal;
    vertex.T = basis.tangent;
    vertex.B = basis.bitangent;
    vertex.UV = scene_page_uv(header, vertex_idx);

    // The hemi as the first byte of the colour, what was baked of the sun as the second; the last word as its kind
    // had it: a lightmap's place, a vertex's baked light, a tree's sway
    uint hemi = frame >> 26u;
    uint sun = kind == ScenePageBaked_Colour ? baked & 0xFFu : (kind == ScenePageBaked_Tree ? (baked >> 16u) & 0xFFu : 0u);
    int2 lightmap = int2((baked.xx >> uint2(0u, 13u)) & 0x1FFFu) * 8 - 32764;
    vertex.C = ((hemi << 2u) | (hemi >> 4u)) << 16u | sun << 8u | 0xFFu;
    vertex.X = kind == ScenePageBaked_Lightmap ? (uint(lightmap.x) & 0xFFFFu) | (uint(lightmap.y) << 16u) : (kind == ScenePageBaked_Colour ? baked >> 8u : baked & 0xFFFFu);
    return vertex;
}
#endif

#endif
