#include "stdafx.h"
#include "xr_area.h"

namespace
{
struct TriangleKey
{
	xr_array<u32, 9> Vertices;
	u32 Material;
	u16 Sector;
	bool operator==(const TriangleKey&) const = default;
};

struct TriangleHash
{
	size_t operator()(const TriangleKey& Key) const
	{
		size_t Hash = Key.Material;
		for (u32 Value : Key.Vertices)
		{
			Hash ^= size_t(Value) + 0x9e3779b9u + (Hash << 6) + (Hash >> 2);
		}
		return Hash ^ Key.Sector;
	}
};

TriangleKey MakeTriangleKey(const CDB::RESULT& Result)
{
	const auto& Tri = Result.model->tris[Result.tris_id];
	TriangleKey Key{{}, Tri.dummy, Result.Sector};
	for (u32 Index = 0; Index < 3; ++Index)
	{
		Fvector Vertex;
		Result.ModelWorldTransform.transform_tiny(Vertex, Result.model->verts[Tri.verts[Index]]);
		Key.Vertices[Index * 3] = std::bit_cast<u32>(Vertex.x == 0.f ? 0.f : Vertex.x);
		Key.Vertices[Index * 3 + 1] = std::bit_cast<u32>(Vertex.y == 0.f ? 0.f : Vertex.y);
		Key.Vertices[Index * 3 + 2] = std::bit_cast<u32>(Vertex.z == 0.f ? 0.f : Vertex.z);
	}
	return Key;
}
} // namespace

template <class Query>
void CDB::COLLIDER::QueryModels(CObjectSpace& Space, Query&& QueryModel, u32 Options)
{
	r_clear();
	if (!Space.IsStreamingEnabled())
	{
		StreamingModels.clear();
		QueryModel(*this, *Space.GetStaticModel());
		return;
	}
	StreamingModels = Space.GetStaticModels();
	xr_hash_set<TriangleKey, TriangleHash> Seen;
	COLLIDER Local;
	Local.ray_mode = ray_mode;
	Local.box_mode = box_mode;
	Local.obb_mode = obb_mode;
	Local.frustum_mode = frustum_mode;
	Local.custom_mode = custom_mode;
	Local.sphere_mode = sphere_mode;
	Local.cylinder_mode = cylinder_mode;
	Local.cone_mode = cone_mode;
	for (const auto& Model : StreamingModels)
	{
		QueryModel(Local, *Model);
		for (const auto& Result : Local.rd)
		{
			// Boundary geometry may be present in several resident tiles. Report it
			// only once, otherwise bullets and physical contacts would be duplicated.
			if (Seen.insert(MakeTriangleKey(Result)).second)
			{
				rd.push_back(Result);
			}
		}
		if (!rd.empty() && (Options & OPT_ONLYFIRST))
		{
			break;
		}
	}
	if (!rd.empty() && (Options & OPT_ONLYNEAREST))
	{
		const auto Nearest = *std::min_element(rd.begin(), rd.end(), [](const RESULT& A, const RESULT& B)
											   { return A.range < B.range; });
		rd.assign(1, Nearest);
	}
}

void CDB::COLLIDER::ray_query(CObjectSpace& Space, const Fvector& Start, const Fvector& Direction, float Range)
{
	QueryModels(Space, [&](COLLIDER& Local, const MODEL& Model)
				{ Local.ray_query(Model, Start, Direction, Range); },
				ray_mode);
}

void CDB::COLLIDER::box_query(CObjectSpace& Space, const Fbox& Box, ColliderCallback OnCheckNode)
{
	QueryModels(Space, [&](COLLIDER& Local, const MODEL& Model)
				{ Local.box_query(Model, Box, OnCheckNode); },
				box_mode);
}

void CDB::COLLIDER::box_query(CObjectSpace& Space, const Fvector& Center, const Fvector& Size, ColliderCallback OnCheckNode)
{
	box_query(Space, Fbox().set(Center - Size, Center + Size), OnCheckNode);
}

void CDB::COLLIDER::obb_query(CObjectSpace& Space, const Fobb& Box, ColliderCallback OnCheckNode)
{
	QueryModels(Space, [&](COLLIDER& Local, const MODEL& Model)
				{ Local.obb_query(Model, Box, OnCheckNode); },
				obb_mode);
}

void CDB::COLLIDER::frustum_query(CObjectSpace& Space, const CFrustum& Frustum, ColliderCallback OnCheckNode)
{
	QueryModels(Space, [&](COLLIDER& Local, const MODEL& Model)
				{ Local.frustum_query(Model, Frustum, OnCheckNode); },
				frustum_mode);
}

void CDB::COLLIDER::custom_query(CObjectSpace& Space, CheckFunc AABBCheck, void* AABBParams, TrisFunc GetTris, void* TriParams)
{
	QueryModels(Space, [&](COLLIDER& Local, const MODEL& Model)
				{ Local.custom_query(Model, AABBCheck, AABBParams, GetTris, TriParams); });
}

void CDB::COLLIDER::sphere_query(CObjectSpace& Space, const Fsphere& Sphere, ColliderCallback OnCheckNode)
{
	QueryModels(Space, [&](COLLIDER& Local, const MODEL& Model)
		{ Local.sphere_query(Model, Sphere, OnCheckNode); }, sphere_mode);
}

void CDB::COLLIDER::cylinder_query(CObjectSpace& Space, const Fcylinder& Cylinder, ColliderCallback OnCheckNode)
{
	QueryModels(Space, [&](COLLIDER& Local, const MODEL& Model)
		{ Local.cylinder_query(Model, Cylinder, OnCheckNode); }, cylinder_mode);
}

void CDB::COLLIDER::cone_query(CObjectSpace& Space, const Fcone& Cone, ColliderCallback OnCheckNode)
{
	QueryModels(Space, [&](COLLIDER& Local, const MODEL& Model)
		{ Local.cone_query(Model, Cone, OnCheckNode); }, cone_mode);
}
