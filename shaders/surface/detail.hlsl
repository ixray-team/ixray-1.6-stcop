// The brush of the detail layers editor, drawn over the ground it paints. BRUSH_TRAIL is the layer of the strokes
// already painted; without it, the cursor: the two guide circles and the preview of the noise inside them
#include "common\common.hlsli"
#include "common\extra_cb.hlsli"

struct brush_vertex {
    float4 position_proj : SV_POSITION;
    float4 color : COLOR0;
    float4 position_world : TEXCOORD0;
};

brush_vertex vs_main(PULL_INPUT)
{
    brush_vertex output;
    RingVertex vertex = PullRing(VertexId);
    output.position_world = float4(vertex.P, 1.0f);
    output.position_proj = mul(m_WVP, output.position_world);
    output.color = vertex.C.zyxw;
    return output;
}

// returns value noise on the integer lattice, bit-identical to fastNoise2D() of DetailManager_Decompress.cpp, the
// noise the grass scale generator uses
float lattice_noise(float2 at)
{
    int2 cell = int2(floor(at));
    float2 part = at - floor(at);
    part = part * part * (3.0f - 2.0f * part);

    uint4 hash = uint4(cell.x, cell.x + 1, cell.x, cell.x + 1) * 374761393u + uint4(cell.y, cell.y, cell.y + 1, cell.y + 1) * 668265263u;
    hash = (hash ^ (hash >> 13)) * 1274126177u;
    float4 corner = float4(hash & 0x7fffffffu) / 2147483648.0f;

    float2 row = corner.xz * (1.0f - part.x) + corner.yw * part.x;
    return row.x * (1.0f - part.y) + row.y * part.y;
}

// returns the pattern anchored in the world, bit-compatible with dv_pattern of DetailLayersEditor.cpp
float brush_pattern(float2 at, float frequency, float seed)
{
    float2 scaled = at * frequency;
    float2 warp = float2(lattice_noise(scaled * 1.7f + 500.0f), lattice_noise(scaled * 1.7f + 1500.0f));
    float2 moved = scaled + seed * float2(0.7f, 0.3f) + (warp - 0.5f) * 0.4f;
    float value = lattice_noise(moved);
    value += 0.5f * lattice_noise(moved * 2.13f + float2(137.7f, 273.1f));
    value += 0.25f * lattice_noise(moved * 4.71f + float2(107.3f, 531.7f));
    value = saturate(value * 0.5714286f);
    return value * value * (3.0f - 2.0f * value);
}

// returns 1 on a circle's line and 0 a line's width away from it
float brush_line(float dist, float width)
{
    float part = saturate(dist / width);
    return (1.0f - part) * (1.0f - part);
}

float4 ps_main(brush_vertex input) : SV_Target
{
    float dist = length(input.position_world.xz - brush_worldpos.xy);
    float radius = max(brush_params.x, 0.001f);
    float core = radius * (1.0f - clamp(brush_params.y, 0.01f, 0.99f));

#ifndef BRUSH_TRAIL
    // Mode 3: the cells already painted, shown as a mask
    if (brush_params.w >= 2.5f && brush_params.w < 3.5f) {
        return float4(input.color.rgb, saturate(brush_params.z));
    }
#endif

    // The falloff of the stroke as the editor computes it: full inside the hard core, the edge hardness at the
    // radius
    float falloff = lerp(1.0f, clamp(brush_color.a, 0.0f, 0.99f), smoothstep(core, radius, dist));

    // The noise mask, never below 0.35 so the whole disk shows
    float shown = 1.0f;
    if (brush_noise.w > 0.0001f) {
        float value = brush_pattern(input.position_world.xz, brush_noise.y, brush_noise.x);
        shown = lerp(0.35f, 1.0f, smoothstep(brush_noise.z - brush_noise.w * 0.5f, brush_noise.z + brush_noise.w * 0.5f, value));
    }

#ifdef BRUSH_TRAIL
    // Fades with the stamp's alpha, which the editor settles from its age
    float alpha = falloff * shown * saturate(brush_params.z);
    float3 color = brush_color.rgb;
#else
    // The guide circles, at the radius and at the edge of the hard core. The mesh reaches past the radius, so they
    // always have triangles under them
    float width = max(0.12f, radius * 0.04f);
    float ring = max(brush_line(abs(radius - dist), width), core > width * 3.0f ? brush_line(abs(core - dist), width) : 0.0f);

    // Wiping and eroding are red, the rest takes the overlay's hue; the circles are bright lines on top
    float3 tint = brush_params.w >= 0.5f && brush_params.w < 1.5f ? float3(1.0f, 0.3f, 0.2f) : brush_color.rgb;
    float3 color = lerp(tint, float3(1.0f, 0.98f, 0.9f), brush_extra.x * smoothstep(0.0f, 0.35f, ring));

    // The preview ends at the radius, across a line's width
    float inside = 1.0f - smoothstep(radius - width, radius + width, dist);
    float alpha = max(ring * brush_extra.x, falloff * shown * brush_extra.y * inside);
#endif
    if (alpha <= 0.001f) {
        discard;
    }
    return float4(color, saturate(alpha));
}
