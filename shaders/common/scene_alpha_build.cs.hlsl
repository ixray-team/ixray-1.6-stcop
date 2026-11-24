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

// Fills one mip of the alpha array: a slice is the alpha of one texture, stretched over the whole slice,
// taken from the texture's head or tail at the mip that matches. USE_WHOLE: of one texture the buckets have no
// place for, from its whole copy, into a view that starts at its slice

StructuredBuffer<SceneMaterial> g_Sources : register(t0);
RWTexture2DArray<float> u_Alpha : register(u0);

#ifdef USE_WHOLE
Texture2D g_Whole : register(t1);

[numthreads(8, 8, 1)]
void main(uint3 Id : SV_DispatchThreadID)
{
	uint3 Size;
	u_Alpha.GetDimensions(Size.x, Size.y, Size.z);

	if (any(Id.xy >= Size.xy))
	{
		return;
	}

	uint Width, Height, Levels;
	g_Whole.GetDimensions(0u, Width, Height, Levels);
	uint Level = min(uint(max(log2(float(max(Width, Height)) / float(Size.x)), 0.0f) + 0.5f), Levels - 1u);
	uint2 Mip = max(uint2(Width, Height) >> Level, uint2(1u, 1u));
	int2 Texel = int2((float2(Id.xy) + 0.5f) / float2(Size.xy) * float2(Mip));
	u_Alpha[uint3(Id.xy, 0u)] = g_Whole.Load(int3(Texel, int(Level))).a;
}
#else
Texture2DArray g_Bucket0 : register(t1);
Texture2DArray g_Bucket1 : register(t2);
Texture2DArray g_Bucket2 : register(t3);
Texture2DArray g_Bucket3 : register(t4);
Texture2DArray g_Bucket4 : register(t5);
Texture2DArray g_Bucket5 : register(t6);
Texture2DArray g_Bucket6 : register(t7);
Texture2DArray g_Bucket7 : register(t8);
Texture2DArray g_Bucket8 : register(t9);
Texture2DArray g_Bucket9 : register(t10);
Texture2DArray g_Bucket10 : register(t11);
Texture2DArray g_Bucket11 : register(t12);
Texture2DArray g_Bucket12 : register(t13);
Texture2DArray g_Bucket13 : register(t14);
Texture2DArray g_Bucket14 : register(t15);
Texture2DArray g_Bucket15 : register(t16);

// A texel of a slice's mip
float LoadAlpha(uint Slot, int2 Texel, uint Level)
{
	int4 Location = int4(Texel, Slot & 0xFFFFu, Level);

	switch (Slot >> 16)
	{
	case 1u:
		return g_Bucket1.Load(Location).a;
	case 2u:
		return g_Bucket2.Load(Location).a;
	case 3u:
		return g_Bucket3.Load(Location).a;
	case 4u:
		return g_Bucket4.Load(Location).a;
	case 5u:
		return g_Bucket5.Load(Location).a;
	case 6u:
		return g_Bucket6.Load(Location).a;
	case 7u:
		return g_Bucket7.Load(Location).a;
	case 8u:
		return g_Bucket8.Load(Location).a;
	case 9u:
		return g_Bucket9.Load(Location).a;
	case 10u:
		return g_Bucket10.Load(Location).a;
	case 11u:
		return g_Bucket11.Load(Location).a;
	case 12u:
		return g_Bucket12.Load(Location).a;
	case 13u:
		return g_Bucket13.Load(Location).a;
	case 14u:
		return g_Bucket14.Load(Location).a;
	case 15u:
		return g_Bucket15.Load(Location).a;
	default:
		return g_Bucket0.Load(Location).a;
	}
}

// The alpha of a slice's mip at UV, weighed between its four texels: a texture smaller than the array's slice, as a
// streamed one without its head is, stood in the slice as squares, and so did everything cut out by it.
// Share: the part of the slice the texture holds
float BlendAlpha(uint Slot, float2 UV, float2 Share, uint Level, float Size)
{
	float Side = max(floor(Size / exp2(float(Level))), 1.0f);
	float2 Last = max(floor(Side * Share), 1.0f) - 1.0f;
	float2 At = UV * Side - 0.5f;
	float2 Low = floor(At);
	float2 Part = At - Low;
	int2 First = int2(clamp(Low, 0.0f, Last));
	int2 Second = int2(clamp(Low + 1.0f, 0.0f, Last));
	return lerp(lerp(LoadAlpha(Slot, First, Level), LoadAlpha(Slot, int2(Second.x, First.y), Level), Part.x),
		lerp(LoadAlpha(Slot, int2(First.x, Second.y), Level), LoadAlpha(Slot, Second, Level), Part.x), Part.y);
}

[numthreads(8, 8, 1)]
void main(uint3 Id : SV_DispatchThreadID)
{
	uint3 Size;
	u_Alpha.GetDimensions(Size.x, Size.y, Size.z);

	if (any(Id >= Size))
	{
		return;
	}

	SceneMaterial Source = g_Sources[Id.z];

	if (Source.base == SCENE_TEXTURE_ABSENT)
	{
		u_Alpha[Id] = 1.0f;
		return;
	}

	uint HeadMips = Source.base_head == SCENE_TEXTURE_ABSENT ? 0u : (Source.base_info >> 8) & 15u;
	float Tail = exp2(float((Source.base_info >> 12) & 15u));
	uint TailMips = (Source.base_info >> 16) & 15u;
	float Texels = Tail * exp2(float(HeadMips));
	uint Level = uint(max(log2(Texels / float(Size.x)), 0.0f) + 0.5f);
	float2 Share = 1.0f / float2(1u << (Source.base_info & 15u), 1u << ((Source.base_info >> 4) & 15u));
	float2 UV = (float2(Id.xy) + 0.5f) / float2(Size.xy) * Share;

	[branch]
	if (Level < HeadMips)
	{
		u_Alpha[Id] = BlendAlpha(Source.base_head, UV, Share, Level, Texels);
	}
	else
	{
		u_Alpha[Id] = BlendAlpha(Source.base, UV, Share, min(Level - HeadMips, TailMips - 1u), Tail);
	}
}
#endif
