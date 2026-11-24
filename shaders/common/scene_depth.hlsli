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
// What the stages of the depth passes share: the view, the tables and the heap pages they pull vertices from
#ifndef SCENE_DEPTH_HLSLI
#define SCENE_DEPTH_HLSLI

// The 32 byte vertex of the geometry heap (PackedVertex): depth needs its position only
struct SceneVertex
{
	float3 P;
	uint N;
	uint TF;
	uint UV;
	uint C;
	uint X;
};

cbuffer SceneViewBlock : register(b0)
{
	SceneView view;
}

#ifdef USE_GRASS
// Grass: the frame's instances in place of the moving objects, its own view block
cbuffer SceneGrassBlock : register(b1)
{
	SceneGrassView grass;
}
#endif

StructuredBuffer<SceneCluster> g_Clusters : register(t0);
StructuredBuffer<SceneTransform> g_Transforms : register(t1);
StructuredBuffer<SceneInstance> g_Instances : register(t2);
StructuredBuffer<SceneMaterial> g_Materials : register(t3);
StructuredBuffer<SceneVertex> g_Vertices0 : register(t4);
StructuredBuffer<SceneVertex> g_Vertices1 : register(t5);
StructuredBuffer<SceneVertex> g_Vertices2 : register(t6);
StructuredBuffer<SceneVertex> g_Vertices3 : register(t7);
StructuredBuffer<SceneVertex> g_Vertices4 : register(t8);
#ifdef USE_GRASS
StructuredBuffer<SceneGrassItem> g_Items : register(t9);
#else
StructuredBuffer<SceneVertex> g_SkinnedVertices : register(t9);
#endif
Buffer<uint> g_ClusterIndices : register(t10);
Buffer<uint> g_Visible : register(t11);
Buffer<uint> g_Indices0 : register(t12);
Buffer<uint> g_Indices1 : register(t13);
Buffer<uint> g_Indices2 : register(t14);
Buffer<uint> g_Indices3 : register(t15);
Buffer<uint> g_Indices4 : register(t16);
StructuredBuffer<SceneModel> g_Models : register(t17); // the frame's table of moving objects, from view.second.z

#define SCENE_PAGE_WORDS g_ClusterIndices
#include "common\scene_pages.hlsli"

// The pixel stage reads what comes before the clip distances
struct Output
{
	float4 hpos : SV_Position;
#ifdef USE_AREF
	float2 uv : TEXCOORD0;
	nointerpolation uint alpha_slice : TEXCOORD1;
#endif
#ifdef USE_TILES
	float4 clip : SV_ClipDistance0;
#endif
};

SceneVertex LoadVertex(uint Page, uint Index)
{
	[branch]
	if (Page == 1u)
	{
		return g_Vertices1[Index];
	}

	[branch]
	if (Page == 2u)
	{
		return g_Vertices2[Index];
	}

	[branch]
	if (Page == 3u)
	{
		return g_Vertices3[Index];
	}

	[branch]
	if (Page == 4u)
	{
		return g_Vertices4[Index];
	}

	return g_Vertices0[Index];
}

uint LoadIndex(uint Page, uint Index)
{
	[branch]
	if (Page == 1u)
	{
		return g_Indices1[Index];
	}

	[branch]
	if (Page == 2u)
	{
		return g_Indices2[Index];
	}

	[branch]
	if (Page == 3u)
	{
		return g_Indices3[Index];
	}

	[branch]
	if (Page == 4u)
	{
		return g_Indices4[Index];
	}

	return g_Indices0[Index];
}

float calc_cyclic(float x)
{
	float f = 1.4142f * sin(x * 3.14159f);
	return f * f - 1.0f;
}

#endif
