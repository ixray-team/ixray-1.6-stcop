#include "shared\scene_tables.hlsli"

// CDetail::SlotItem
struct GrassItem
{
	float3 quat;
	float scale;
	float3 pos;
	float hemi;
	float trample_strength;
	float trample_visual;
	float2 trample_dir;
};

cbuffer SceneGrassBlock : register(b0)
{
	SceneGrassView grass;
}

StructuredBuffer<GrassItem> g_Items : register(t0);
RWBuffer<uint> g_Args : register(u0); // per bin, the arguments of an indexed instanced indirect draw
RWBuffer<uint> g_Visible : register(u1); // per bin from its first instance on, the instances in view

[numthreads(64, 1, 1)]
void main(uint3 Id : SV_DispatchThreadID)
{
	if (Id.x == 0u)
	{
		for (uint Each = 0; Each < grass.counts.y; ++Each)
		{
			g_Args[Each * SCENE_GRASS_ARGS] = grass.bins[Each].index_count;
			g_Args[Each * SCENE_GRASS_ARGS + 2u] = grass.bins[Each].index_start;
		}
	}

	if (Id.x >= grass.counts.x)
	{
		return;
	}

	// The bins are in the order of their instances: the last one that starts at or before this one
	uint Low = 0u;
	uint High = grass.counts.y;

	[loop]
	while (High - Low > 1u)
	{
		uint Middle = (Low + High) >> 1;
		if (grass.bins[Middle].first <= Id.x)
		{
			Low = Middle;
		}
		else
		{
			High = Middle;
		}
	}

	GrassItem Item = g_Items[Id.x];
	float Radius = grass.bins[Low].radius * Item.scale;

	for (uint Plane = 0; Plane < grass.counts.z; ++Plane)
	{
		if (dot(grass.planes[Plane].xyz, Item.pos) + grass.planes[Plane].w > Radius)
		{
			return;
		}
	}

	uint Slot;
	InterlockedAdd(g_Args[Low * SCENE_GRASS_ARGS + 1u], 1u, Slot);
	g_Visible[grass.bins[Low].first + Slot] = Id.x;
}
