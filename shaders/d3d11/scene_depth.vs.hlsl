#include "shared\scene_tables.hlsli"

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

cbuffer SceneDynamicBlock : register(b1)
{
	SceneDynamic dynamic_meshes[SCENE_DYNAMIC_MESHES];
}

StructuredBuffer<SceneCluster> g_Clusters : register(t0);
StructuredBuffer<SceneTransform> g_Transforms : register(t1);
StructuredBuffer<SceneInstance> g_Instances : register(t2);
StructuredBuffer<SceneMaterial> g_Materials : register(t3);
StructuredBuffer<SceneVertex> g_Vertices0 : register(t4);
StructuredBuffer<SceneVertex> g_Vertices1 : register(t5);
StructuredBuffer<SceneVertex> g_Vertices2 : register(t6);
StructuredBuffer<SceneVertex> g_Vertices3 : register(t7);
StructuredBuffer<SceneVertex> g_Vertices4 : register(t8);
StructuredBuffer<SceneVertex> g_SkinnedVertices : register(t9);
Buffer<uint> g_ClusterIndices : register(t10);
Buffer<uint> g_Visible : register(t11);
Buffer<uint> g_Indices0 : register(t12);
Buffer<uint> g_Indices1 : register(t13);
Buffer<uint> g_Indices2 : register(t14);
Buffer<uint> g_Indices3 : register(t15);
Buffer<uint> g_Indices4 : register(t16);

struct Output
{
	float4 hpos : SV_Position;
#ifdef USE_AREF
	float2 uv : TEXCOORD0;
	nointerpolation uint3 texture_ref : TEXCOORD1;
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

Output main(uint Corner : SV_VertexID, uint Slot : SV_InstanceID)
{
	uint Entry = g_Visible[Slot];
	Output O = (Output)0;
	// The unused tail of a cluster: a triangle outside the view
	O.hpos = float4(2.0f, 2.0f, 2.0f, 1.0f);

	[branch]
	if (Entry & SCENE_VISIBLE_DYNAMIC)
	{
		SceneDynamic D = dynamic_meshes[Entry & (SCENE_DYNAMIC_MESHES - 1u)];
		uint Offset = ((Entry >> SCENE_VISIBLE_RUN_SHIFT) & SCENE_DYNAMIC_RUNS) * SCENE_CLUSTER_TRIANGLES * 3u + Corner;

		[branch]
		if (Offset < D.triangle_count * 3u)
		{
			uint Page = D.source & SCENE_DYNAMIC_PAGE_MASK;
			uint Index = D.vertices + LoadIndex(Page, D.index_start + Offset);
			float3 Local;

			[branch]
			if (D.source & SCENE_DYNAMIC_SKINNED)
			{
				Local = g_SkinnedVertices[Index].P;
			}
			else
			{
				Local = LoadVertex(Page, Index).P;
			}

			O.hpos = mul(view.view_projection, float4(mul(D.world, float4(Local, 1.0f)), 1.0f));
		}

		return O;
	}

	SceneCluster C = g_Clusters[Entry];

	[branch]
	if (Corner >= (C.triangles & SCENE_CLUSTER_COUNT_MASK) * 3u)
	{
		return O;
	}

	SceneVertex V = LoadVertex(C.vertices >> 24, (C.vertices & 0xFFFFFFu) + g_ClusterIndices[C.index_start + Corner]);
	float3 P = V.P;
	uint Transform = C.triangles >> SCENE_CLUSTER_TRANSFORM_SHIFT;

	[branch]
	if (Transform)
	{
		float3x4 World = g_Transforms[Transform].world;
		P = mul(World, float4(P, 1.0f));

		[branch]
		if (C.triangles & SCENE_CLUSTER_WAVE)
		{
			float Sway = (P.y - World._24) * calc_cyclic(view.wave.w + dot(P, view.wave.xyz));
			float Weight = float(asint(V.X << 16) >> 16) * view.wind.w;
			P.xz += view.wind.xz * (Sway * Weight);
		}
	}

	O.hpos = mul(view.view_projection, float4(P, 1.0f));
#ifdef USE_AREF
	// DecodeUV of shared\vertex.hlsli
	int2 Q = (asint(uint2(V.UV << 16, V.UV)) >> 16 << 8) | int2((V.TF.xx >> uint2(16, 24)) & 0xFFu);
	O.uv = float2(Q) * (1.0f / 262144.0f);
	SceneMaterial Material = g_Materials[g_Instances[C.instance].material];
	O.texture_ref = uint3(Material.base, Material.base_head, Material.base_info);
#endif
	return O;
}
