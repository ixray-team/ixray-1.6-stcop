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
#include "common\scene_tables.hlsli"

#ifdef USE_GRASS
cbuffer SceneGrassBlock : register(b1)
{
	SceneGrassView grass;
}

#define ALPHA_LIMIT grass.lod.y
#else
cbuffer SceneViewBlock : register(b0)
{
	SceneView view;
}

#define ALPHA_LIMIT view.material.x
#endif

// The alpha of every alpha-tested texture, one per slice
Texture2DArray<float> g_Alpha : register(t0);
SamplerState smp_scene : register(s0);

#ifdef USE_GRASS
void main(float4 Position : SV_Position, float2 UV : TEXCOORD0, nointerpolation uint Slice : TEXCOORD1, nointerpolation float2 Fade : TEXCOORD2)
{
	// A blade's shadow dissolves as the blade does
	if (!GrassPixelKept(Fade.x, Fade.y, Position.xy, 0.0f))
	{
		discard;
	}

	// The cut grows with the mip, as the forward frame's does (surface_alpha)
	clip(g_Alpha.Sample(smp_scene, float3(UV, float(Slice))) * (1.0f + max(g_Alpha.CalculateLevelOfDetail(smp_scene, UV), 0.0f) * 0.25f) - ALPHA_LIMIT);
}
#else
void main(float4 Position : SV_Position, float2 UV : TEXCOORD0, nointerpolation uint Slice : TEXCOORD1)
{
	clip(g_Alpha.Sample(smp_scene, float3(UV, float(Slice))) - ALPHA_LIMIT);
}
#endif
