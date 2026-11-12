#include "shared\scene_tables.hlsli"

cbuffer SceneViewBlock : register(b0)
{
	SceneView view;
}

// One array per texture bucket; a material's texture is a slice of one of them
Texture2DArray g_Bucket0 : register(t0);
Texture2DArray g_Bucket1 : register(t1);
Texture2DArray g_Bucket2 : register(t2);
Texture2DArray g_Bucket3 : register(t3);
Texture2DArray g_Bucket4 : register(t4);
Texture2DArray g_Bucket5 : register(t5);
Texture2DArray g_Bucket6 : register(t6);
Texture2DArray g_Bucket7 : register(t7);
Texture2DArray g_Bucket8 : register(t8);
Texture2DArray g_Bucket9 : register(t9);
Texture2DArray g_Bucket10 : register(t10);
Texture2DArray g_Bucket11 : register(t11);
Texture2DArray g_Bucket12 : register(t12);
Texture2DArray g_Bucket13 : register(t13);
Texture2DArray g_Bucket14 : register(t14);
Texture2DArray g_Bucket15 : register(t15);
SamplerState smp_scene : register(s0);

float SampleSlice(uint Slot, float2 UV, float2 Dx, float2 Dy)
{
	float3 Location = float3(UV, float(Slot & 0xFFFFu));

	switch (Slot >> 16)
	{
	case 1u:
		return g_Bucket1.SampleGrad(smp_scene, Location, Dx, Dy).a;
	case 2u:
		return g_Bucket2.SampleGrad(smp_scene, Location, Dx, Dy).a;
	case 3u:
		return g_Bucket3.SampleGrad(smp_scene, Location, Dx, Dy).a;
	case 4u:
		return g_Bucket4.SampleGrad(smp_scene, Location, Dx, Dy).a;
	case 5u:
		return g_Bucket5.SampleGrad(smp_scene, Location, Dx, Dy).a;
	case 6u:
		return g_Bucket6.SampleGrad(smp_scene, Location, Dx, Dy).a;
	case 7u:
		return g_Bucket7.SampleGrad(smp_scene, Location, Dx, Dy).a;
	case 8u:
		return g_Bucket8.SampleGrad(smp_scene, Location, Dx, Dy).a;
	case 9u:
		return g_Bucket9.SampleGrad(smp_scene, Location, Dx, Dy).a;
	case 10u:
		return g_Bucket10.SampleGrad(smp_scene, Location, Dx, Dy).a;
	case 11u:
		return g_Bucket11.SampleGrad(smp_scene, Location, Dx, Dy).a;
	case 12u:
		return g_Bucket12.SampleGrad(smp_scene, Location, Dx, Dy).a;
	case 13u:
		return g_Bucket13.SampleGrad(smp_scene, Location, Dx, Dy).a;
	case 14u:
		return g_Bucket14.SampleGrad(smp_scene, Location, Dx, Dy).a;
	case 15u:
		return g_Bucket15.SampleGrad(smp_scene, Location, Dx, Dy).a;
	default:
		return g_Bucket0.SampleGrad(smp_scene, Location, Dx, Dy).a;
	}
}

// Ref is SceneMaterial's tail, head and info
float SampleAlpha(uint3 Ref, float2 UV)
{
	UV /= float2(1u << (Ref.z & 15u), 1u << ((Ref.z >> 4) & 15u));
	float2 Dx = ddx(UV);
	float2 Dy = ddy(UV);

	[branch]
	if (Ref.y == SCENE_TEXTURE_ABSENT)
	{
		return SampleSlice(Ref.x, UV, Dx, Dy);
	}

	// The level in the head's own mips; its last one blends into the tail's first
	float HeadMips = float((Ref.z >> 8) & 15u);
	float Texels = SCENE_TEXTURE_TAIL * exp2(HeadMips);
	float Level = 0.5f * log2(max(max(dot(Dx, Dx), dot(Dy, Dy)) * Texels * Texels, 1e-8f));
	float Blend = saturate(Level - (HeadMips - 1.0f));
	float Alpha = 0.0f;

	[branch]
	if (Blend < 1.0f)
	{
		Alpha = SampleSlice(Ref.y, UV, Dx, Dy) * (1.0f - Blend);
	}

	[branch]
	if (Blend > 0.0f)
	{
		Alpha += SampleSlice(Ref.x, UV, Dx, Dy) * Blend;
	}

	return Alpha;
}

void main(float4 Position : SV_Position, float2 UV : TEXCOORD0, nointerpolation uint3 Ref : TEXCOORD1)
{
	clip(SampleAlpha(Ref, UV) - view.material.x);
}
