// Which of the frame's lamps the eye can see anything of: a thread a lamp tests its sphere against the depth pyramid
// of the frame before. A lamp that is wholly behind what that frame drew lights nothing on the screen

struct lamp_view {
    float4x4 view;
    float4 hiz; // xy: the projection's scale of x and y, z: the near plane
    float4 hiz_depth; // depth = x + y / view z
    float4 hiz_size; // xy: size of the pyramid's first level, z: its last level
};

// What common\scene_occlusion.hlsli reads as the view: the spheres come in the pyramid's own view space
static lamp_view view;

#define LAMPS_MOST 64u

// 0-2: hiz, hiz_depth, hiz_size; 3: x the lamps, y the frame's bits; then a sphere a lamp, w its reach
StructuredBuffer<float4> g_lamps : register(t0);
Texture2D<float> g_hiz : register(t1);
// A word a lamp, 1 when it is seen; the last word is the frame's bits
RWStructuredBuffer<uint> g_seen : register(u0);

#include "common\scene_occlusion.hlsli"

[numthreads(LAMPS_MOST, 1, 1)]
void main(uint3 thread_id : SV_DispatchThreadID)
{
    view.view = float4x4(1.0f, 0.0f, 0.0f, 0.0f, 0.0f, 1.0f, 0.0f, 0.0f, 0.0f, 0.0f, 1.0f, 0.0f, 0.0f, 0.0f, 0.0f, 1.0f);
    view.hiz = g_lamps[0];
    view.hiz_depth = g_lamps[1];
    view.hiz_size = g_lamps[2];

    if (thread_id.x == 0u) {
        g_seen[LAMPS_MOST] = asuint(g_lamps[3].y);
    }

    if (thread_id.x >= uint(g_lamps[3].x)) {
        return;
    }

    float4 bounds = g_lamps[4u + thread_id.x];
    // What of the sphere is past the screen's edge is left out: the part on the screen is what the eye could see
    g_seen[thread_id.x] = sphere_occluded(g_hiz, bounds) ? 0u : 1u;
}
