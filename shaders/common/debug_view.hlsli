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
// What the frame shows in place of its shading: CBFrame's debug_view.x, the console's r_debug_view. Shared with C++,
// which takes the defines only
#ifndef DEBUG_VIEW_HLSLI
#define DEBUG_VIEW_HLSLI

#define DebugView_None 0
#define DebugView_OverdrawRaster 1          // Every pixel of what the cull let through, hidden or not
#define DebugView_OverdrawShaded 2          // The pixels that pass the depth test, in the order of the draws
#define DebugView_Clusters 3
#define DebugView_Triangles 4
#define DebugView_Instances 5
#define DebugView_Materials 6
#define DebugView_Lights 7                  // Local lights that reach the pixel
#define DebugView_Base 8
#define DebugView_Normal 9
#define DebugView_Roughness 10
#define DebugView_Hemi 11
#define DebugView_Lmap 12
#define DebugView_Streaming 13              // The base texture's head: loaded or not, needed or not
#define DebugView_Lit 14                    // The light that falls on the surface, half of it shown: the sun, the sky, the lamps
#define DebugView_Highlight 15              // The sun's highlight
#define DebugView_Mirror 16                 // What the surface mirrors of its surroundings
#define DebugView_Heightfield 17            // The sun the heightfield lets through; blue where it does not answer
#define DebugView_Count 18

#define DEBUG_LIGHT_BUDGET 8.0f             // Lights on a pixel that show as red
#define DEBUG_TAG_SHIFT 16u                 // A cluster's vertices carry a tag above their material while a view is on

#ifndef __cplusplus
#define DEBUG_VIEW ((uint)debug_view.x)
#define DEBUG_MATERIAL_MASK (DEBUG_VIEW != DebugView_None ? (1u << DEBUG_TAG_SHIFT) - 1u : 0xFFFFFFFFu)
// A layer of overdraw: red is full at 7 layers, yellow at 25, white at 100
#define DEBUG_OVERDRAW_STEP float3(0.15f, 0.04f, 0.01f)

float3 debug_hash_color(uint value)
{
    float hue = frac(float(value) * 0.61803398875f);
    float3 color = saturate(abs(frac(hue + float3(0.0f, 2.0f / 3.0f, 1.0f / 3.0f)) * 6.0f - 3.0f) - 1.0f);
    return lerp(1.0f, color, 0.85f) * (0.55f + 0.45f * frac(float(value) * 0.3819660113f));
}

// returns green at none, yellow at half the budget, red at all of it
float3 debug_heat_color(float count, float budget)
{
    float t = saturate(count / budget);
    return t < 0.5f ? lerp(float3(0.0f, 1.0f, 0.15f), float3(1.0f, 0.85f, 0.0f), t * 2.0f) : lerp(float3(1.0f, 0.85f, 0.0f), float3(1.0f, 0.05f, 0.0f), t * 2.0f - 1.0f);
}

// returns what a vertex of a cluster adds to its material for the view that is on: the pixel takes a colour from it
uint debug_tag(uint cluster_idx, uint instance_idx, uint triangle_idx)
{
    uint mode = DEBUG_VIEW;
    uint tag = mode == DebugView_Clusters ? cluster_idx + 1u : mode == DebugView_Triangles ? cluster_idx * 64u + triangle_idx + 1u : mode == DebugView_Instances ? instance_idx + 1u : 0u;
    return max((tag * 0x9E3779B1u) >> DEBUG_TAG_SHIFT, min(tag, 1u)) << DEBUG_TAG_SHIFT;
}
#endif

#endif
