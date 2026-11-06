// Sky rainbow overlay.
// Algorithm originally by Meltac.
// Implementation: ForserX

#define RB_INTENSITY float(0.38)    // Absolute maximal intensity of the rainbows
#define RB_SECONDARY float(0.1)     // Relative intensity of the secondary rainbow
#define RB_SATURATION float(0.75)   // Saturation of the rainbows
#define RB_DECAY float(5.0)         // Intensity decay / falloff
#define RB_COLRATIO float(0.60)     // Color ratio
#define RB_COLSHIFT float(0.0)      // Color shift
#define RB_COLPOWER float(0.52)     // Color power
#define RB_ENDXMIN float(-6.0)      // Amount of fade out towards the ground
#define RB_ENDXMAX float(5.0)       // Amount of fade out towards the ground
#define RB_COFX float(1.5)          // Amount of color towards the ground
#define RB_COFY float(1.0)          // Amount of color towards the sky
#define RB_SUNFACTOR float(3.0)

struct RainbowBand
{
	float Radius;
	float Thickness;
	float Strength;
	float ColorDirection;
};

struct RainbowBandResult
{
	float3 Color;
	float Blend;
};

float3 rainbow_spectrum(float t)
{
	float hue = frac(1.0 - (t + 0.3333333));
	float3 color = saturate(abs(frac(hue.xxx + float3(0.0, 0.6666667, 0.3333333)) * 6.0 - 3.0) - 1.0);
	return color;
}


float3 rainbow_color(float t, bool white)
{
	float3 color = rainbow_spectrum(t);
	return lerp(color, 1.0.xxx, float(white));
}


float rainbow_band_mask(float distance, RainbowBand band)
{
	return step(band.Radius, distance) * step(distance, band.Radius + band.Thickness);
}


RainbowBandResult evaluate_rainbow_band(float2 pos, float2 center, float distance, RainbowBand band, float intensity, bool white)
{
	RainbowBandResult result;

	float k = saturate((distance - band.Radius) / band.Thickness);
	float color_position = pow(max(k * RB_COLRATIO - RB_COLSHIFT, 0.0), RB_COLPOWER);

	color_position = lerp(color_position, 1.0 - color_position, step(0.0, -band.ColorDirection));
	result.Color = rainbow_color(color_position, white);

	float blend = band.Strength * pow(1.0 - abs(k - 0.5), RB_DECAY);

	float fade_exp = RB_ENDXMIN + intensity * (RB_ENDXMAX - RB_ENDXMIN);
	float horizontal = max((pos.x - center.x) * rcp(band.Radius), 0.0);
	float vertical = max((pos.y - center.y) / band.Radius, 0.0);

	blend *= saturate(RB_COFX - pow(horizontal, fade_exp));
	blend *= saturate(RB_COFY - (vertical * vertical));

	result.Blend = blend;

	return result;
}


float4 draw_rainbow(float2 pos, float2 center, float intensity, bool enabled, bool white)
{
	if (!enabled)
	{
		return 0.0.xxxx;
	}

	float white_f = white;

	float primary_radius = lerp(1.3, 0.9, white_f);
	float primary_thickness = lerp(0.16, 0.18, white_f);
	float secondary_radius = lerp(1.6, 1.2, white_f);
	float secondary_thickness = lerp(0.1, 0.75, white_f);

	center.y += 0.2 * white_f;
	pos.y *= 10.0 / 16.0;

	float2 delta = pos - center;
	float distance = length(delta);

	float side_mask = step(pos.x, center.x);

	RainbowBand primary_band =
	{
		primary_radius,
		primary_thickness,
		1.0,
		1.0
	};

	RainbowBand secondary_band =
	{
		secondary_radius,
		secondary_thickness,
		RB_SECONDARY,
		-1.0
	};

	float primary_mask = rainbow_band_mask(distance, primary_band);
	float secondary_mask = rainbow_band_mask(distance, secondary_band);

	RainbowBandResult primary = evaluate_rainbow_band(pos, center, distance, primary_band, intensity, white);
	RainbowBandResult secondary = evaluate_rainbow_band(pos, center, distance, secondary_band, intensity, white);

	float3 color = primary.Color * primary.Blend * primary_mask + secondary.Color * secondary.Blend * secondary_mask;
	float blend = primary.Blend * primary_mask + secondary.Blend * secondary_mask;

	float luminance = dot(color, float3(0.3, 0.59, 0.11));
	color = lerp(luminance.xxx, color, RB_SATURATION);

	float mask = side_mask;

	return float4(RB_INTENSITY * blend * color * mask, RB_INTENSITY * blend * mask);
}