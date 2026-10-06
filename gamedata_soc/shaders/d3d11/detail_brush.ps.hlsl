#include "common.hlsli"

float4 brush_worldpos; // xy = brush center (world XZ)
float4 brush_params;   // x = radius, y = soft (0..1), z = press/alpha, w = mode:
                       //   0 hover, 1 erase (red), 2 paint warning, 3 mask overlay
float4 brush_noise;    // x = seed, y = pattern freq, z = threshold, w = sharp/0=off
float4 brush_color;    // rgb = tint, a = edge hardness (falloff at rim, 0..0.9)
float4 brush_extra;    // x = cursor circle alpha, y = pattern preview alpha

struct v2p
{
	float4 hpos : SV_POSITION;
	float4 color : COLOR0;
	float4 wp : TEXCOORD0;
};

// Integer-lattice value noise, bit-identical to fastNoise2D() in
// DetailManager_Decompress.cpp - the exact noise the grass scale generator uses.
static float fnoise2(float x, float y)
{
	int ix = int(floor(x));
	int iy = int(floor(y));
	float fx = x - floor(x);
	float fy = y - floor(y);
	fx = fx * fx * (3.0 - 2.0 * fx);
	fy = fy * fy * (3.0 - 2.0 * fy);

	uint h00 = uint(ix) * 374761393u + uint(iy) * 668265263u;
	uint h10 = uint(ix + 1) * 374761393u + uint(iy) * 668265263u;
	uint h01 = uint(ix) * 374761393u + uint(iy + 1) * 668265263u;
	uint h11 = uint(ix + 1) * 374761393u + uint(iy + 1) * 668265263u;

	h00 = (h00 ^ (h00 >> 13)) * 1274126177u;
	h10 = (h10 ^ (h10 >> 13)) * 1274126177u;
	h01 = (h01 ^ (h01 >> 13)) * 1274126177u;
	h11 = (h11 ^ (h11 >> 13)) * 1274126177u;

	float n00 = (h00 & 0x7fffffffu) / 2147483648.0;
	float n10 = (h10 & 0x7fffffffu) / 2147483648.0;
	float n01 = (h01 & 0x7fffffffu) / 2147483648.0;
	float n11 = (h11 & 0x7fffffffu) / 2147483648.0;

	float nx0 = n00 * (1.0 - fx) + n10 * fx;
	float nx1 = n01 * (1.0 - fx) + n11 * fx;
	return nx0 * (1.0 - fy) + nx1 * fy;
}

// World-anchored pattern, bit-compatible with dv_pattern in DetailLayersEditor.cpp.
static float pattern_local(float2 p, float freq, float seed)
{
	float x = p.x * freq;
	float z = p.y * freq;
	float sx = x + seed * 0.7;
	float sz = z + seed * 0.3;
	float wa = fnoise2(x * 1.7 + 500.0, z * 1.7 + 500.0);
	float wb = fnoise2(x * 1.7 + 1500.0, z * 1.7 + 1500.0);
	float mx = sx + (wa - 0.5) * 0.4;
	float mz = sz + (wb - 0.5) * 0.4;
	float v = fnoise2(mx, mz);
	v += 0.5 * fnoise2(mx * 2.13 + 137.7, mz * 2.13 + 273.1);
	v += 0.25 * fnoise2(mx * 4.71 + 107.3, mz * 4.71 + 531.7);
	v *= 0.5714286;
	v = saturate(v);
	return v * v * (3.0 - 2.0 * v);
}

// Thin ring-line falloff: 1 right on the circle, 0 at `ringw` away.
static float lineMask(float d, float ringw)
{
	float t = saturate(d / ringw);
	return (1.0 - t) * (1.0 - t);
}

float4 main(v2p I) : SV_TARGET
{
	float2 dv = I.wp.xz - brush_worldpos.xy;
	float dist = length(dv);
	float radius = max(brush_params.x, 0.001);
	float soft = clamp(brush_params.y, 0.01, 0.99);
	float innerR = radius * (1.0 - soft);

	// Mode 3: mask overlay of already-painted cells (early return, unrelated to brush).
	if (brush_params.w >= 2.5 && brush_params.w < 3.5)
		return float4(I.color.rgb, clamp(brush_params.z, 0.0, 1.0));

	// Falloff identical to the CPU stroke: 1.0 inside the hard core (innerR), drops
	// to edge_hardness at the outer radius.
	float h = clamp(brush_color.a, 0.0, 0.99);
	float fall = lerp(1.0, h, smoothstep(innerR, radius, dist));

	// Noise mask clipped by the threshold/sharpness, modulated 0.35..1.0 over the
	// WHOLE disk - continuous field, never gated away or cut by an inner ring.
	float patternVis = 1.0;
	if (brush_noise.w > 0.0001)
	{
		float v = pattern_local(I.wp.xz, brush_noise.y, brush_noise.x);
		float stroke = smoothstep(brush_noise.z - brush_noise.w * 0.5,
			brush_noise.z + brush_noise.w * 0.5, v);
		patternVis = lerp(0.35, 1.0, stroke);
	}

	// Guide circles: outer = brush radius, inner = hard-core edge. Thin, sharp lines
	// (fixed width relative to the radius, nothing adaptive). The mesh overhangs the
	// brush radius (padding), so the rings always have triangles under them and can
	// never be clipped as the brush moves.
	float ringw = max(0.12, radius * 0.04);
	float ringO = lineMask(abs(radius - dist), ringw);
	float ringI = 0.0;
	if (innerR > ringw * 3.0)
		ringI = lineMask(abs(innerR - dist), ringw);
	float ring = max(ringO, ringI);
	float ringA = ring * brush_extra.x; // driven by the show_cursor toggle

	// Tint: wipe/erode red, everything else the overlay hue.
	float3 tint = brush_color.rgb;
	if (brush_params.w >= 0.5 && brush_params.w < 1.5)
		tint = float3(1.0, 0.3, 0.2);

	// Content: the live brush cursor = noise preview inside the radius (fades over the
	// ring line width at the edge, so the padding mesh beyond the circles stays clean).
	// Painted strokes are a SEPARATE shader layer (detail_brush_trail) and never come
	// through this shader.
	float inside = 1.0 - smoothstep(radius - ringw, radius + ringw, dist);
	float contentA = fall * patternVis * brush_extra.y;
	contentA *= inside;

	// Rings sit on top as bright lines.
	float3 ringTint = float3(1.0, 0.98, 0.9);
	float3 col = lerp(tint, ringTint, brush_extra.x * smoothstep(0.0, 0.35, ring));

	float a = max(ringA, contentA);
	if (a <= 0.001)
		discard;
	return float4(col, saturate(a));
}