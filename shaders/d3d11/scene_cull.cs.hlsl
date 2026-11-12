#include "shared\scene_tables.hlsli"

cbuffer SceneViewBlock : register(b0)
{
	SceneView view;
}

cbuffer SceneDynamicBlock : register(b1)
{
	SceneDynamic dynamic_meshes[SCENE_DYNAMIC_MESHES];
}

StructuredBuffer<SceneInstance> g_Instances : register(t0);
StructuredBuffer<SceneCluster> g_Clusters : register(t1);
RWBuffer<uint> g_Counters : register(u0);
RWBuffer<uint> g_Visible : register(u1); // draw instance -> cluster
RWBuffer<uint> g_VisibleAlpha : register(u2);

bool IsVisible(SceneCluster C)
{
	for (uint Plane = 0; Plane < view.counts.z; ++Plane)
	{
		if (dot(view.planes[Plane].xyz, C.bounds.xyz) + view.planes[Plane].w > C.bounds.w)
		{
			return false;
		}
	}

	if (view.counts.y & SCENE_VIEW_FACING)
	{
		float3 Look = view.thresholds.yzw;
		float Limit = C.cone.w;

		if (view.counts.y & SCENE_VIEW_PERSPECTIVE)
		{
			// Every point of the cluster has to see the back: the sphere stands in for the points
			Look = (C.bounds.xyz - Look) * view.material.y;
			Limit = Limit * length(Look) + C.bounds.w;
		}

		if (dot(Look, C.cone.xyz) >= Limit)
		{
			return false;
		}
	}

	SceneInstance I = g_Instances[C.instance];
	if (I.sector < SCENE_VIEW_SECTORS && !(view.sectors[I.sector >> 7][(I.sector >> 5) & 3u] & (1u << (I.sector & 31u))))
	{
		return false;
	}

	if (view.counts.y & SCENE_VIEW_KEPT)
	{
		return !(I.flags & SCENE_INSTANCE_TREE);
	}

	float R = I.bounds.w;
	float3 Offset = I.bounds.xyz - view.camera.xyz;
	float DistanceSq = dot(Offset, Offset) + 0.00001f;
	float Fog = view.camera.w + R;

	if (R / DistanceSq <= view.thresholds.x || DistanceSq > Fog * Fog)
	{
		return false;
	}

	if (!(I.flags & SCENE_INSTANCE_KEEP))
	{
		float Volume = 4.1887902f * R * R * R;
		float Distance = sqrt(DistanceSq);

		for (uint Step = 0; Step < view.counts.w; ++Step)
		{
			if (Volume < view.steps[Step].x && Distance > view.steps[Step].y)
			{
				return false;
			}
		}
	}

	return true;
}

[numthreads(64, 1, 1)]
void main(uint3 Id : SV_DispatchThreadID)
{
	if (Id.x == 0u)
	{
		g_Counters[SCENE_COUNTER_VERTICES] = SCENE_CLUSTER_TRIANGLES * 3u;
		g_Counters[SCENE_COUNTER_ALPHA + SCENE_COUNTER_VERTICES] = SCENE_CLUSTER_TRIANGLES * 3u;
	}

	if (Id.x >= view.counts.x)
	{
		uint Mesh = Id.x - view.counts.x;

		if (Mesh < (view.counts.y >> SCENE_VIEW_DYNAMIC_SHIFT))
		{
			uint Runs = (dynamic_meshes[Mesh].triangle_count + SCENE_CLUSTER_TRIANGLES - 1u) / SCENE_CLUSTER_TRIANGLES;
			uint First;
			InterlockedAdd(g_Counters[SCENE_COUNTER_CLUSTERS], Runs, First);

			if (First + Runs <= view.counts.x + SCENE_DYNAMIC_CLUSTERS)
			{
				for (uint Run = 0; Run < Runs; ++Run)
				{
					g_Visible[First + Run] = SCENE_VISIBLE_DYNAMIC | (Run << SCENE_VISIBLE_RUN_SHIFT) | Mesh;
				}
			}
			else
			{
				InterlockedAdd(g_Counters[SCENE_COUNTER_CLUSTERS], 0u - Runs);
			}
		}

		return;
	}

	SceneCluster C = g_Clusters[Id.x];

	if (IsVisible(C))
	{
		uint Slot;

		[branch]
		if (C.triangles & SCENE_CLUSTER_ALPHA)
		{
			InterlockedAdd(g_Counters[SCENE_COUNTER_ALPHA + SCENE_COUNTER_CLUSTERS], 1u, Slot);
			g_VisibleAlpha[Slot] = Id.x;
		}
		else
		{
			InterlockedAdd(g_Counters[SCENE_COUNTER_CLUSTERS], 1u, Slot);
			g_Visible[Slot] = Id.x;
		}
	}
}
