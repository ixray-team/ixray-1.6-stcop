#ifndef cgim_h_included
#define cgim_h_included

// creates more light by a vector from the sky 
#define SMALLSKY_TOP_VECTOR_POWER 0.75f

// Break default bloom to soften the overall picture 
#define BROKE_BLOOM_POWER 1.5f

#define CGIM_LUM float3(0.2126f, 0.7152f, 0.0722f)

float Luminance(float3 Color)
{
	return dot(Color, CGIM_LUM);
}

float3 TonemapRobo(float3 c)
{
	float l = Luminance(c);
	return c / sqrt(1.0 + l * l);
}

float TonemapRobo(float c)
{
	return c / sqrt(1.0 + c * c);
}

float4 BrokeBloom(float4 c)
{
	c *= BROKE_BLOOM_POWER;
	c = float4(TonemapRobo(c.rgb), TonemapRobo(c.a));
	c /= BROKE_BLOOM_POWER;
	
	return c;
}

float3 Uncharted2ACES(float3 x)
{
	static const float A = 0.15f; // Shoulder strength
	static const float B = 0.50f; // Linear strength
	static const float C = 0.10f; // Linear angle
	static const float D = 0.20f; // Toe strength
	static const float E = 0.02f; // Toe numerator
	static const float F = 0.30f; // Toe denominator
	return ((x * (A * x + C * B) + D * E) / (x * (A * x + B) + D * F)) - E / F;
}
/*
// Uncharted 2 tonemapping
#define UNCHARTED2TONEMAP_WHITEPT 1.35
#define UNCHARTED2TONEMAP_EXPOSURE 1.0

float3 Uncharted2Tonemap(float3 c)
{
	c *= UNCHARTED2TONEMAP_EXPOSURE;
	
	float3 tc = Uncharted2ACES(c);
	float l = Luminance(c);
	
	c = lerp(c * Uncharted2ACES(l) / l, tc, tc);
	c /= Uncharted2ACES(UNCHARTED2TONEMAP_WHITEPT);

	return c;
}
*/
#endif