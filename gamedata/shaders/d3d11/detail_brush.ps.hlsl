#include "common.hlsli"

float4 brush_worldpos; // xy = world-space brush center
float4 brush_params;   // x = radius, y = soft (0..1), z = alpha, w = mode (0 paint, 1 erase, 2 white press, 3 mask overlay)
float4 brush_noise;    // x = seed, y = frequency (pattern scale inside the brush, 1/m), z = threshold, w = sharp/0=off
float4 brush_color;    // rgba

struct v2p
{
	float4 hpos : SV_POSITION;
	float4 color : COLOR0;
	float4 wp : TEXCOORD0;
};

// Integer-lattice value noise, bit-identical to fastNoise2D() in
// DetailManager_Decompress.cpp - the exact noise the grass scale generator uses.
// Same hash constants, same smoothed bilinear patch, so the brush shows the same
// noise the terrain itself is built from.
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

// World-anchored pattern (p = world-space meters, freq 1/m): the noise is glued to the
// terrain, so it scrolls through the brush as the brush rolls over the ground and the
// painted imprint stays continuous. Same domain warp as the cluster generator: two
// offset warp lattices bend a master lattice into organic patches - no flat tones, no
// repetitive round dots, and it scales down to sub-meter detail for fine work.
static float pattern_local(float2 p, float freq, float seed)
{
	float x = p.x * freq;
	float z = p.y * freq;
	float sx = x + seed * 0.7;
	float sz = z + seed * 0.3;
	float wa = fnoise2(x * 1.7 + 500.0, z * 1.7 + 500.0);
	float wb = fnoise2(x * 1.7 + 1500.0, z * 1.7 + 1500.0);
	return saturate(fnoise2(sx + (wa - 0.5) * 0.4, sz + (wb - 0.5) * 0.4));
}

float4 main(v2p I) : SV_TARGET
{
	float2 dv = I.wp.xz - brush_worldpos.xy;
	float dist = length(dv);
	float radius = max(brush_params.x, 0.001);
	float soft = clamp(brush_params.y, 0.01, 0.99);

	float fall = saturate((radius - dist) / max(radius * (1.0 - soft), 0.001));
	if (fall <= 0.0)
		discard;

	// Mask overlay (mode 3): persistent highlight of already-painted cells. The tint is
	// per-vertex from the editor (green = painted, red = cleared, amber = asset mix).
	if (brush_params.w >= 2.5)
		return float4(I.color.rgb, clamp(brush_params.z, 0.0, 1.0));

	// World-anchored modulation, not gating: the pattern only scales the strength
	// (0.35..1) so every cell under the brush is affected - strokes always land where
	// the player points, and the imprint scrolls with the terrain while rolling.
	float stroke = 1.0;
	float sharp = brush_noise.w;
	if (sharp > 0.0001)
	{
		stroke = smoothstep(brush_noise.z - sharp * 0.5, brush_noise.z + sharp * 0.5,
			pattern_local(I.wp.xz, brush_noise.y, brush_noise.x));
	}

	// Concentric rings for the white press imprint (mode 2): a crisp outer edge plus
	// two inner guide rings, drawn even where the pattern modulates the cells.
	float ring = 0.0;
	if (brush_params.w >= 1.5)
	{
		float ww = max(radius * 0.035, 0.02);
		ring = max(1.0 - saturate(abs(radius - dist) / ww),
				max(1.0 - saturate(abs(radius * 0.72 - dist) / ww),
					1.0 - saturate(abs(radius * 0.45 - dist) / ww)));
	}
	else if (brush_params.w < 0.5f)
	{
		// Passive hover: crisp outer boundary ring so the brush circle is always obvious
		// even where the pattern silhouette leaves cells transparent.
		float ww = max(radius * 0.03, 0.02);
		ring = 1.0 - saturate(abs(radius - dist) / ww);
	}

	float3 tint;
	if (brush_params.w >= 1.5)
		tint = float3(1.0, 1.0, 1.0);
	else
		tint = (brush_params.w < 0.5f) ? brush_color.rgb : float3(1.0, 0.35, 0.25);
	// Hover shows the exact stamp: cells below the threshold stay transparent, above it
	// light up - a silhouette of what the stroke will plant BEFORE the mouse is pressed.
	// Painting modes keep the 0.35..1 modulation floor so the imprint is always visible.
	float patternVis = (sharp > 0.0001)
		? (brush_params.w < 0.5f ? stroke : lerp(0.35, 1.0, stroke))
		: 1.0;
	float press = saturate(brush_params.z);
	float coreA = fall * patternVis * press;
	float ringA = ring * press;
	float a = max(coreA, ringA);
	return float4(tint, a * brush_color.a);
}