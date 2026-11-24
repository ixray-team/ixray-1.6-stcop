/**************************************************************************************
* Copyright (C) 2026 IXRay-Team
* IX-Ray Renderer
***************************************************************************************
* Source code is licensed under the following terms:
*
* "Team" means the maintainers of the official IX-Ray repository.
*
* 1. IX-Ray Team License
*    The Team may copy, modify, merge, publish and distribute this Software
*    and its documentation, in source and binary form, without restriction.
*
* 2. Public Access License
*    Non-exclusive rights granted to everyone else.
*
*    Permitted rights:
*      a. Read and clone the source code.
*      b. Fork the repository and modify the source code for the sole purpose
*         of proposing the change to the official repository. By proposing it,
*         the author grants the Team a worldwide, perpetual, irrevocable,
*         royalty-free right to use, modify, relicense and distribute it.
*      c. Redistribute binaries built and published by the Team, unmodified
*         and together with this notice.
*
* Usage of this Software beyond the rights granted above is strictly prohibited,
* including distribution of the source code or of any modified version of it and
* distribution of binaries not built by the Team.
*
* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND. THE COPYRIGHT
* HOLDERS ARE NOT LIABLE FOR ANY CLAIM OR DAMAGE ARISING FROM IT.
*
* The above copyright notice and this license text must be included in all
* copies or substantial portions of the Software.
**************************************************************************************/
#include "stdafx.h"
#include "GpuSceneInternal.h"
#include "ClusterPages.h"
#include <meshoptimizer.h>
#define CLUSTERLOD_IMPLEMENTATION
#include <clusterlod.h>

namespace LevelPacking
{
PackedVertex EncodeLevelVertex(const CRender::LevelVertexChunk& Chunk, const u8* Source);
}

namespace GpuSceneInternal
{
namespace
{
constexpr size_t ClusterVertices = 192;
// How far the wind may carry a vertex out of its cluster's bounds
constexpr float SwayReach = 1.0f;

struct ClusterCache
{
	// Changes with the layout below and with how a mesh is split
	static constexpr u32 Version = 0x434C530A;

	struct Mesh
	{
		u64 Id;
		u32 First;
		u32 Count;
	};

	struct Cluster
	{
		float Bounds[4];
		u32 IndexStart;
		u32 TriangleCount;
		// Centre, radius and error of its own simplification (0: as built), then of the group made from it
		float Self[5];
		float Parent[5];
		float Extent[3]; // half the size of its box, around the centre of Bounds
	};

	xr_vector<Mesh> Meshes;
	xr_vector<Cluster> Clusters;
	xr_vector<u16> Indices;

	void Load(const char* Name)
	{
		if (!FS.exist("$app_data_root$", Name))
		{
			return;
		}

		IReader* File = FS.r_open("$app_data_root$", Name);
		u32 Header[4] = {};

		if (u64(File->length()) >= sizeof(Header))
		{
			File->r(Header, sizeof(Header));
		}

		const u64 Size = sizeof(Header) + u64(Header[1]) * sizeof(Mesh) + u64(Header[2]) * sizeof(Cluster) + u64(Header[3]) * sizeof(u16);
		bool IsValid = Header[0] == Version && Size == u64(File->length());
		if (IsValid)
		{
			Meshes.resize(Header[1]);
			Clusters.resize(Header[2]);
			Indices.resize(Header[3]);
			File->r(Meshes.data(), u32(Meshes.size() * sizeof(Mesh)));
			File->r(Clusters.data(), u32(Clusters.size() * sizeof(Cluster)));
			File->r(Indices.data(), u32(Indices.size() * sizeof(u16)));
		}

		FS.r_close(File);

		for (const Mesh& Item : Meshes)
		{
			IsValid &= u64(Item.First) + Item.Count <= Clusters.size();
		}

		for (const Cluster& Item : Clusters)
		{
			IsValid &= Item.TriangleCount <= SCENE_CLUSTER_TRIANGLES && u64(Item.IndexStart) + Item.TriangleCount * 3 <= Indices.size();
		}

		if (!IsValid)
		{
			*this = {};
		}
	}

	void Save(const char* Name) const
	{
		IWriter* File = FS.w_open("$app_data_root$", Name);
		if (!File)
		{
			return;
		}

		const u32 Header[4] = { Version, u32(Meshes.size()), u32(Clusters.size()), u32(Indices.size()) };
		File->w(Header, sizeof(Header));
		File->w(Meshes.data(), u32(Meshes.size() * sizeof(Mesh)));
		File->w(Clusters.data(), u32(Clusters.size() * sizeof(Cluster)));
		File->w(Indices.data(), u32(Indices.size() * sizeof(u16)));
		FS.w_close(File);
	}
};

// Level meshes are small and cut by material, and a simplification keeps the border of what it is given: one mesh
// alone has little to give up. Meshes of one material, sector and cell of this size are made one mesh first
constexpr float WeldCell = 24.0f;

struct WeldKey
{
	u32 Material;
	u32 Sector;
	u32 Views;
	u32 Page;
	s32 Cell[3];

	auto operator<=>(const WeldKey&) const = default;
};

// A surface whose vertex stage is its own (the r1_ shaders of the XML blends: water, wallmarks, light planes, self-lit
// surfaces) keeps the mesh it was built with: the hierarchy is simplified by positions alone, and a flat sheet of water
// made of a few triangles has no waves and none of the light baked into its vertices
bool IsOwnVertex(const Caster& Item)
{
	const ShaderElement* Element = Item.Visual->shader._get() ? Item.Visual->shader->E[ShaderVariant::Cluster]._get() : nullptr;
	const SPass* Pass = Element && !Element->passes.empty() ? Element->passes.back()._get() : nullptr;
	return Pass && Pass->vs && IsNamed(Pass->vs->cName, "r1_");
}
} // namespace

u32 BuildClusters(const xr_vector<Caster>& Casters, xr_vector<Hlsl::SceneInstance>& Instances, xr_vector<Hlsl::SceneCluster>& Records, xr_vector<Hlsl::SceneClusterLod>& Lods, xr_vector<u32>& Words)
{
	CRender& Render = RImplementation;
	string_path Name;
	xr_strconcat(Name, "level_cache\\", FS.get_path("$level$")->m_Add, "clusters.cache");
	ClusterCache Stored, Used;
	Stored.Load(Name);
	// A mesh's clusters as pages, a page a cluster of Used: its vertices in cluster order, positions on the level's grid
	ClusterPages::Store Pages;

	xr_hash_map<u64, u32> StoredIds;

	for (u32 Index = 0; Index < Stored.Meshes.size(); ++Index)
	{
		StoredIds.emplace(Stored.Meshes[Index].Id, Index);
	}

	xr_hash_map<u32, u32> UsedIds;
	xr_vector<u32> Wide;
	xr_vector<u32> Remap;
	xr_vector<u64> Edges;
	xr_vector<u8> Locks;
	u32 Split = 0;

	// The clusters of a mesh, from the cache or made now: the mesh as clusters, and coarser clusters made of groups of
	// them, level over level. Only indices are made, every level draws the mesh's own vertices. IsWhole: what is cut
	// out by its texture keeps the mesh it was built with, a simplified leaf card is a missing leaf
	const auto Make = [&](ClusterCache::Mesh& Mesh, u32 Count, const float* Positions, u32 VertexCount, u32 Stride, bool IsWhole)
	{
		Mesh.First = u32(Used.Clusters.size());
		Mesh.Count = 0;
		const auto Found = StoredIds.find(Mesh.Id);
		if (Found != StoredIds.end())
		{
			const ClusterCache::Mesh& Old = Stored.Meshes[Found->second];
			for (u32 Index = 0; Index < Old.Count; ++Index)
			{
				ClusterCache::Cluster Cluster = Stored.Clusters[Old.First + Index];
				const auto First = Stored.Indices.begin() + Cluster.IndexStart;
				Cluster.IndexStart = u32(Used.Indices.size());
				Used.Indices.insert(Used.Indices.end(), First, First + Cluster.TriangleCount * 3);
				Used.Clusters.push_back(Cluster);
			}

			Mesh.Count = Old.Count;
			return;
		}

		clodConfig Config = clodDefaultConfig(SCENE_CLUSTER_TRIANGLES);
		Config.max_vertices = ClusterVertices;
		// The permissive mode collapses across seams and borders, and is meant to be held back by vertex attributes:
		// with positions alone it left a flight of steps as a few loose triangles, drawn from three metres
		Config.simplify_permissive = false;
		// The sloppy fallback, where a group will not simplify, clusters the vertices of every corner apart: a level
		// made of it is a hull whose triangles take their UVs from any island. Such a group stays as it is
		Config.simplify_fallback_sloppy = false;
		// What is cut out by its texture is cards that share no edge: gathered by their edges they make clusters as
		// wide as a crown, which nothing can hide. Such a mesh is gathered by place
		Config.cluster_spatial = IsWhole;
		clodMesh Source = {};
		Source.indices = Wide.data();
		Source.index_count = Count;
		Source.vertex_count = VertexCount;
		Source.vertex_positions = Positions;
		Source.vertex_positions_stride = Stride;

		// An open edge is shared with another mesh, simplified apart: its vertices stay, or the two leave a crack
		Remap.resize(VertexCount);
		meshopt_generatePositionRemap(Remap.data(), Positions, VertexCount, Stride);
		Edges.clear();

		for (u32 Corner = 0; Corner < Count; ++Corner)
		{
			const u32 A = Remap[Wide[Corner]];
			const u32 B = Remap[Wide[Corner % 3 == 2 ? Corner - 2 : Corner + 1]];
			Edges.push_back(u64(std::min(A, B)) << 32 | std::max(A, B));
		}

		std::sort(Edges.begin(), Edges.end());
		Locks.assign(VertexCount, 0);

		for (size_t Edge = 0; Edge < Edges.size(); ++Edge)
		{
			if ((Edge == 0 || Edges[Edge - 1] != Edges[Edge]) && (Edge + 1 == Edges.size() || Edges[Edge + 1] != Edges[Edge]))
			{
				Locks[u32(Edges[Edge] >> 32)] = Locks[u32(Edges[Edge])] = meshopt_SimplifyVertex_Lock;
			}
		}

		for (u32 Vertex = 0; Vertex < VertexCount; ++Vertex)
		{
			Locks[Vertex] = Locks[Remap[Vertex]];
		}

		Source.vertex_lock = Locks.data();
		xr_vector<clodBounds> Groups;

		clodBuild(Config, Source, [&](clodGroup Group, const clodCluster* Made, size_t MadeCount)
		{
			const int Id = int(Groups.size());
			Groups.push_back(Group.simplified);

			for (size_t Index = 0; Index < MadeCount; ++Index)
			{
				const clodCluster& From = Made[Index];
				if (IsWhole && From.refined >= 0)
				{
					continue;
				}

				const meshopt_Bounds Bounds = meshopt_computeClusterBounds(From.indices, From.index_count, Positions, VertexCount, Stride);
				ClusterCache::Cluster Cluster = { { Bounds.center[0], Bounds.center[1], Bounds.center[2], Bounds.radius }, u32(Used.Indices.size()), u32(From.index_count / 3) };
				std::fill_n(Cluster.Extent, 3, 0.0f);

				for (size_t Corner = 0; Corner < From.index_count; ++Corner)
				{
					const float* At = reinterpret_cast<const float*>(reinterpret_cast<const u8*>(Positions) + size_t(From.indices[Corner]) * Stride);

					for (u32 Axis = 0; Axis < 3; ++Axis)
					{
						Cluster.Extent[Axis] = std::max(Cluster.Extent[Axis], std::abs(At[Axis] - Bounds.center[Axis]));
					}
				}

				const clodBounds Own = From.refined >= 0 ? Groups[From.refined] : clodBounds{};
				clodBounds Coarser = Group.simplified;
				Coarser.error = IsWhole ? FLT_MAX : Coarser.error;
				std::copy_n(Own.center, 3, Cluster.Self);
				Cluster.Self[3] = Own.radius;
				Cluster.Self[4] = Own.error;
				std::copy_n(Coarser.center, 3, Cluster.Parent);
				Cluster.Parent[3] = Coarser.radius;
				Cluster.Parent[4] = Coarser.error;
				Used.Clusters.push_back(Cluster);

				for (size_t Vertex = 0; Vertex < From.index_count; ++Vertex)
				{
					Used.Indices.push_back(u16(From.indices[Vertex]));
				}

				++Mesh.Count;
			}

			return Id;
		});

		++Split;
	};

	// The scene's records of a mesh's clusters, for the instance that stands at World (null: where the mesh was built)
	const auto Emit = [&](const ClusterCache::Mesh& Mesh, u32 Instance, const Fmatrix* World, bool IsWaving, bool IsAlpha, u32 Transform, u32 Views)
	{
		for (u32 Index = 0; Index < Mesh.Count; ++Index)
		{
			const ClusterCache::Cluster& Cluster = Used.Clusters[Mesh.First + Index];
			Hlsl::SceneCluster Record = {};
			Record.bounds.set(Cluster.Bounds[0], Cluster.Bounds[1], Cluster.Bounds[2], Cluster.Bounds[3]);
			Record.extent.set(Cluster.Extent[0], Cluster.Extent[1], Cluster.Extent[2], 0.0f);
			float Scale = 1.0f;

			if (World)
			{
				const float Scales[3] = { World->i.magnitude(), World->j.magnitude(), World->k.magnitude() };
				Scale = std::max({ Scales[0], Scales[1], Scales[2] });
				Fvector Centre;
				World->transform_tiny(Centre, Fvector().set(Cluster.Bounds[0], Cluster.Bounds[1], Cluster.Bounds[2]));
				Record.bounds.set(Centre.x, Centre.y, Centre.z, Cluster.Bounds[3] * Scale + (IsWaving ? SwayReach : 0.0f));

				// The box of the turned box, and as far around it as the wind takes a tree
				const Fvector Half = { Cluster.Extent[0], Cluster.Extent[1], Cluster.Extent[2] };
				const float Reach = IsWaving ? SwayReach : 0.0f;
				Record.extent.set(std::abs(World->i.x) * Half.x + std::abs(World->j.x) * Half.y + std::abs(World->k.x) * Half.z + Reach, std::abs(World->i.y) * Half.x + std::abs(World->j.y) * Half.y + std::abs(World->k.y) * Half.z + Reach,
					std::abs(World->i.z) * Half.x + std::abs(World->j.z) * Half.y + std::abs(World->k.z) * Half.z + Reach, 0.0f);
			}

			Hlsl::SceneClusterLod Lod = {};
			Fvector Own = { Cluster.Self[0], Cluster.Self[1], Cluster.Self[2] };
			Fvector Coarser = { Cluster.Parent[0], Cluster.Parent[1], Cluster.Parent[2] };

			if (World)
			{
				World->transform_tiny(Own);
				World->transform_tiny(Coarser);
			}

			Lod.self.set(Own.x, Own.y, Own.z, Cluster.Self[3] * Scale);
			Lod.parent.set(Coarser.x, Coarser.y, Coarser.z, Cluster.Parent[3] * Scale);
			Lod.errors.set(Cluster.Self[4] * Scale, Cluster.Parent[4] >= FLT_MAX ? FLT_MAX : Cluster.Parent[4] * Scale, 0.0f, 0.0f);
			Lods.push_back(Lod);

			Record.extent.w = 1.0f;
			Record.instance = Instance;
			Record.index_start = Pages.Clusters[Mesh.First + Index].FirstCorner;
			Record.triangles = Cluster.TriangleCount | (IsWaving ? SCENE_CLUSTER_WAVE : 0u) | (IsAlpha ? SCENE_CLUSTER_ALPHA : 0u) | (Transform << SCENE_CLUSTER_TRANSFORM_SHIFT) | Views;
			Record.vertices = Pages.Clusters[Mesh.First + Index].Header;
			Records.push_back(Record);
		}
	};

	// What a mesh is: its vertices and the indices a caster draws of it
	struct Piece
	{
		const CRender::LevelVertexChunk* Chunk;
		const u8* Vertices;
		const float* Positions;
		const u16* Narrow;
		u32 Count;
		u32 VertexCount;
		u32 Stride;
		u64 Id;
	};

	const auto Find = [&](const Caster& Item, Piece& Out)
	{
		if (Item.Mesh->LevelMesh == u32(-1))
		{
			return false;
		}

		const xr_array<u32, 6>& Key = Render.LevelMeshes[Item.Mesh->LevelMesh].Key;
		const CRender::LevelVertexChunk& Chunk = Render.LevelVertices[Key[0]];
		const u32 Offset = Item.Window ? std::min(Item.Window->offset, Key[5]) : 0;
		Out.Chunk = &Chunk;
		Out.Vertices = Chunk.Data.data() + size_t(Key[1]) * Chunk.Stride;
		Out.Positions = reinterpret_cast<const float*>(Out.Vertices + Chunk.Position);
		Out.Narrow = Render.LevelIndices[Key[3]].data() + Key[4] + Offset;
		Out.Count = std::min(Key[5] - Offset, Item.Window ? u32(Item.Window->num_tris) * 3 : Item.Mesh->dwPrimitives * 3);
		Out.VertexCount = Key[2];
		Out.Stride = Chunk.Stride;
		Out.Id = u64(crc32(Out.Narrow, Out.Count * sizeof(u16))) << 32 | crc32(Out.Vertices, Out.VertexCount * Chunk.Stride);
		return true;
	};

	xr_vector<PackedVertex> Packed;
	u64 SharedVertices = 0;

	const auto Fill = [&](const Piece& Part, u32 Shift)
	{
		for (u32 Vertex = 0; Vertex < Part.VertexCount; ++Vertex)
		{
			Packed[Shift + Vertex] = LevelPacking::EncodeLevelVertex(*Part.Chunk, Part.Vertices + size_t(Vertex) * Part.Stride);
		}
	};

	const auto Pack = [&](const ClusterCache::Mesh& Mesh, const CRender::LevelVertexChunk& Chunk, bool KeepsUv)
	{
		using ClusterPages::Baked;
		const Baked Kind = Chunk.Tex0Short4 ? Baked::Tree : Chunk.Color != CRender::LevelVertexChunk::Absent ? Baked::Colour : Chunk.Tex1 != CRender::LevelVertexChunk::Absent ? Baked::Lightmap : Baked::None;
		SharedVertices += Packed.size();

		for (u32 Index = 0; Index < Mesh.Count; ++Index)
		{
			const ClusterCache::Cluster& Cluster = Used.Clusters[Mesh.First + Index];
			ClusterPages::Append(Pages, Packed.data(), Used.Indices.data() + Cluster.IndexStart, Cluster.TriangleCount, Kind, KeepsUv);
		}
	};

	// Which casters are welded: opaque meshes that stand where they were built outside a group, each drawn by one caster only
	xr_hash_map<u32, u32> Uses;

	for (const Caster& Item : Casters)
	{
		++Uses[Item.Mesh->LevelMesh];
	}

	xr_map<WeldKey, xr_vector<u32>> Cells;

	for (u32 Index = 0; Index < Casters.size(); ++Index)
	{
		const Caster& Item = Casters[Index];
		const Hlsl::SceneInstance& Owner = Instances[Item.Instance];
		if (Item.World || Item.Transform || Item.IsAlpha || Item.IsSorted || Item.Window || Uses[Item.Mesh->LevelMesh] != 1 || Item.Mesh->LevelMesh == u32(-1) || IsOwnVertex(Item))
		{
			continue;
		}

		const WeldKey Key = { Owner.material, Owner.sector, Item.Views, Render.LevelMeshes[Item.Mesh->LevelMesh].Key[0], { s32(std::floor(Owner.bounds.x / WeldCell)), s32(std::floor(Owner.bounds.y / WeldCell)), s32(std::floor(Owner.bounds.z / WeldCell)) } };
		Cells[Key].push_back(Index);
	}

	xr_vector<u8> IsWelded(Casters.size(), 0);
	xr_vector<float> Merged;
	u32 Welds = 0;
	u32 WeldedMeshes = 0;
	u32 Candidates = 0;
	u32 Neighbours = 0;

	for (auto& [Key, Members] : Cells)
	{
		Candidates += u32(Members.size());
		Neighbours += Members.size() > 1 ? u32(Members.size()) : 0;
		// A member's vertices: where they start in the cell's vertex buffer, and where they end
		const auto Range = [&](size_t Member) -> const xr_array<u32, 6>& { return Render.LevelMeshes[Casters[Members[Member]].Mesh->LevelMesh].Key; };
		std::sort(Members.begin(), Members.end(), [&](u32 Left, u32 Right) { return Render.LevelMeshes[Casters[Left].Mesh->LevelMesh].Key[1] < Render.LevelMeshes[Casters[Right].Mesh->LevelMesh].Key[1]; });

		for (size_t First = 0; First < Members.size();)
		{
			// A cluster names its vertices by 16 bits from the first vertex of its mesh
			const u32 Base = Range(First)[1];
			size_t Last = First + 1;

			while (Last < Members.size() && Range(Last)[1] + Range(Last)[2] - Base <= 0x10000)
			{
				++Last;
			}

			if (Last - First < 2)
			{
				First = Last;
				continue;
			}

			const u32 VertexCount = Range(Last - 1)[1] + Range(Last - 1)[2] - Base;
			Merged.assign(size_t(VertexCount) * 3, 0.0f);
			Packed.assign(VertexCount, PackedVertex{});
			Wide.clear();
			ClusterCache::Mesh Mesh = { 0x57454C44, 0, 0 };
			Hlsl::SceneInstance Whole = Instances[Casters[Members[First]].Instance];
			Fvector Centre = { 0.0f, 0.0f, 0.0f };

			for (size_t Member = First; Member < Last; ++Member)
			{
				const Caster& Item = Casters[Members[Member]];
				const Hlsl::SceneInstance& Owner = Instances[Item.Instance];
				const u32 Shift = Range(Member)[1] - Base;
				Piece Part;
				Find(Item, Part);
				Fill(Part, Shift);

				for (u32 Vertex = 0; Vertex < Part.VertexCount; ++Vertex)
				{
					std::memcpy(&Merged[size_t(Shift + Vertex) * 3], reinterpret_cast<const u8*>(Part.Positions) + size_t(Vertex) * Part.Stride, sizeof(float) * 3);
				}

				for (u32 Corner = 0; Corner < Part.Count; ++Corner)
				{
					Wide.push_back(Shift + Part.Narrow[Corner]);
				}

				Mesh.Id = (Mesh.Id * 1099511628211ull) ^ Part.Id ^ (u64(Shift) << 17);
				Whole.flags |= Owner.flags;
				Centre.add(Fvector().set(Owner.bounds.x, Owner.bounds.y, Owner.bounds.z));
				IsWelded[Members[Member]] = 1;
			}

			Centre.div(float(Last - First));
			float Radius = 0.0f;

			for (size_t Member = First; Member < Last; ++Member)
			{
				const Hlsl::SceneInstance& Owner = Instances[Casters[Members[Member]].Instance];
				Radius = std::max(Radius, Centre.distance_to(Fvector().set(Owner.bounds.x, Owner.bounds.y, Owner.bounds.z)) + Owner.bounds.w);
			}

			Whole.bounds.set(Centre.x, Centre.y, Centre.z, Radius);
			Whole.flags |= (Key.Views & SCENE_CLUSTER_DEPTH) ? SCENE_INSTANCE_DEPTH_BATCH : 0u;
			const u32 Instance = u32(Instances.size());
			Instances.push_back(Whole);

			Make(Mesh, u32(Wide.size()), Merged.data(), VertexCount, sizeof(float) * 3, false);
			Piece Lead;
			Find(Casters[Members[First]], Lead);
			Pack(Mesh, *Lead.Chunk, false);
			Emit(Mesh, Instance, nullptr, false, false, 0, Key.Views);
			Used.Meshes.push_back(Mesh);
			++Welds;
			WeldedMeshes += u32(Last - First);
			First = Last;
		}
	}

	for (u32 Index = 0; Index < Casters.size(); ++Index)
	{
		const Caster& Item = Casters[Index];
		Piece Part;

		if (!IsWelded[Index])
		{
			if (!Find(Item, Part))
			{
				continue;
			}

			// A sorted visual draws its mesh once, in no order of detail: its clusters are the mesh as built. A mesh is
			// made whole and with detail apart
			const bool IsWhole = Item.IsAlpha || Item.IsSorted || IsOwnVertex(Item);
			const auto [Slot, IsNew] = UsedIds.try_emplace(Item.Mesh->LevelMesh ^ (IsWhole ? 0x80000000u : 0u), u32(Used.Meshes.size()));
			if (IsNew)
			{
				ClusterCache::Mesh Mesh = { Part.Id ^ u64(IsWhole), 0, 0 };
				Wide.assign(Part.Narrow, Part.Narrow + Part.Count);
				Make(Mesh, Part.Count, Part.Positions, Part.VertexCount, Part.Stride, IsWhole);
				Packed.resize(Part.VertexCount);
				Fill(Part, 0);
				Pack(Mesh, *Part.Chunk, IsWhole);
				Used.Meshes.push_back(Mesh);
			}

			if (Item.IsSorted)
			{
				SortedStatics[Item.Visual].First = u32(Records.size());
				SortedStatics[Item.Visual].Count = Used.Meshes[Slot->second].Count;
			}

			Emit(Used.Meshes[Slot->second], Item.Instance, Item.World, Item.IsWaving, Item.IsAlpha, Item.Transform, Item.Views);
		}

		if (Item.Views & SCENE_CLUSTER_DEPTH)
		{
			Instances[Item.Instance].flags |= SCENE_INSTANCE_DEPTH_BATCH;
			Item.Visual->IsDepthBatched = true;
		}

		Item.Visual->IsForwardBatched |= (Item.Views & SCENE_CLUSTER_MAIN) != 0;
	}

	Msg("* [scene] welded %u meshes into %u: %u of %u casters could be, %u share a material in a cell of %.0f m (%u cells)", WeldedMeshes, Welds, Candidates, u32(Casters.size()), Neighbours, WeldCell, u32(Cells.size()));

	const u64 PageVertices = Pages.Vertices;
	const u64 PageBytes = (Pages.Positions.size() + Pages.Attributes.size() + Pages.BakedWords.size()) * sizeof(u32) + Pages.Corners.size();
	Msg("* [scene] pages: %u clusters, %llu vertices in cluster order of %llu shared, %llu places, %.1f position bits a vertex; positions %.1f MB, attributes %.1f MB, baked %.1f MB (%llu vertices), corners %.1f MB: %.1f MB against %.1f MB of shared vertices and 16-bit corners",
		u32(Pages.Clusters.size()), PageVertices, SharedVertices, Pages.Places, PageVertices ? double(Pages.Positions.size()) * 32.0 / double(PageVertices) : 0.0, double(Pages.Positions.size()) * 4.0 / 1048576.0, double(Pages.Attributes.size()) * 4.0 / 1048576.0,
		double(Pages.BakedWords.size()) * 4.0 / 1048576.0, u64(Pages.BakedWords.size()), double(Pages.Corners.size()) / 1048576.0, double(PageBytes) / 1048576.0, double(SharedVertices * sizeof(PackedVertex) + Used.Indices.size() * sizeof(u16)) / 1048576.0);

	if (Split || Used.Meshes.size() != Stored.Meshes.size())
	{
		Used.Save(Name);
	}

	const u32 CornerBase = ClusterPages::Join(Pages, Words);

	for (Hlsl::SceneCluster& Record : Records)
	{
		Record.index_start += CornerBase;
	}

	return Split;
}
} // namespace GpuSceneInternal
