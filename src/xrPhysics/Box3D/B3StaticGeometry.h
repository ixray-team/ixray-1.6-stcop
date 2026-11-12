#pragma once

#include "../../xrCore/Collision/xrCDB.h"
#include "PhysicsCommon.h"

// Geometry is cooked in world space so non-uniform scale and nested instances do
// not depend on Box3D's rigid body transform. Parts preserve the original CDB identity.
class CB3StaticGeometry
{
	struct SPart
	{
		const CDB::MODEL* Model;
		Fmatrix Transform;
		u32 VertexOffset;
		u16 Sector;
	};
	struct STriangle
	{
		u32 Part;
		u32 Index;
	};

public:
	struct STriangleKey
	{
		xr_array<u32, 9> Vertices;
		u32 Surface;
		u16 Sector;
		bool operator==(const STriangleKey&) const = default;
	};
	struct STriangleHash
	{
		size_t operator()(const STriangleKey& Key) const
		{
			size_t Hash = Key.Surface;
			for (u32 Value : Key.Vertices)
			{
				Hash ^= size_t(Value) + 0x9e3779b9u + (Hash << 6) + (Hash >> 2);
			}
			return Hash ^ Key.Sector;
		}
	};
	using TriangleSet = xr_hash_set<STriangleKey, STriangleHash>;

	explicit CB3StaticGeometry(const CDB::MODEL& Model)
	{
		AddModel(Model, Fmatrix{Fmatrix::EIdentity::Identity}, u16(-1), 0);
	}

	const xr_vector<Fvector>& GetVertices() const { return Vertices; }

	TriabgleCDBData GetTriangle(u32 Index) const
	{
		const STriangle& Triangle = Triangles[Index];
		const SPart& Part = Parts[Triangle.Part];
		return {Part.Transform, Part.Model, Triangle.Index};
	}

	xr_array<u32, 3> GetIndices(u32 Index) const
	{
		const STriangle& Triangle = Triangles[Index];
		const SPart& Part = Parts[Triangle.Part];
		const CDB::TRI& Tri = Part.Model->tris[Triangle.Index];
		return {Part.VertexOffset + Tri.verts[0], Part.VertexOffset + Tri.verts[1], Part.VertexOffset + Tri.verts[2]};
	}

	xr_vector<u32> SelectUniqueTriangles(TriangleSet& Seen) const
	{
		xr_vector<u32> Selected;
		Selected.reserve(Triangles.size());
		for (u32 Index = 0; Index < Triangles.size(); ++Index)
		{
			const STriangle& Triangle = Triangles[Index];
			const SPart& Part = Parts[Triangle.Part];
			const CDB::TRI& Tri = Part.Model->tris[Triangle.Index];
			STriangleKey Key{{}, Tri.dummy, Part.Sector == u16(-1) ? u16(Tri.sector) : Part.Sector};
			const auto Indices = GetIndices(Index);
			for (u32 Vertex = 0; Vertex < 3; ++Vertex)
			{
				const Fvector& Position = Vertices[Indices[Vertex]];
				// Canonicalize signed zero, as in CDB's streamed query deduplication.
				Key.Vertices[Vertex * 3] = std::bit_cast<u32>(Position.x == 0.f ? 0.f : Position.x);
				Key.Vertices[Vertex * 3 + 1] = std::bit_cast<u32>(Position.y == 0.f ? 0.f : Position.y);
				Key.Vertices[Vertex * 3 + 2] = std::bit_cast<u32>(Position.z == 0.f ? 0.f : Position.z);
			}
			if (Seen.insert(Key).second)
			{
				Selected.push_back(Index);
			}
		}
		return Selected;
	}

private:
	void AddModel(const CDB::MODEL& Model, const Fmatrix& Transform, u16 Sector, u32 Depth)
	{
		// CDB also bounds the depth of its instance stack. Reject malformed cycles.
		if (Depth >= 4)
		{
			Msg("! [Box3D] collision instance nesting exceeds the CDB limit");
			return;
		}
		Model.load_task.wait();
		const u32 Part = u32(Parts.size());
		Parts.push_back({&Model, Transform, u32(Vertices.size()), Sector});
		for (const Fvector& Vertex : Model.verts)
		{
			Fvector WorldVertex;
			Transform.transform_tiny(WorldVertex, Vertex);
			Vertices.push_back(WorldVertex);
		}
		for (u32 Index = 0; Index < Model.tris.size(); ++Index)
		{
			Triangles.push_back({Part, Index});
		}
		for (const CDB::InstanceData& Instance : Model.instances)
		{
			if (Instance.ModelIndex >= Model.models.size() || !Model.models[Instance.ModelIndex])
			{
				Msg("! [Box3D] invalid collision instance prototype");
				continue;
			}
			Fmatrix ToWorld;
			ToWorld.mul(Transform, Instance.Transform);
			AddModel(*Model.models[Instance.ModelIndex], ToWorld, Instance.Sector, Depth + 1);
		}
	}

	xr_vector<SPart> Parts;
	xr_vector<STriangle> Triangles;
	xr_vector<Fvector> Vertices;
};
