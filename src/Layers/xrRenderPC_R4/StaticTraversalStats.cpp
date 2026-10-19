#include "stdafx.h"

#include "StaticTraversalStats.h"
#include "r__sector.h"
#include "FHierrarhyVisual.h"
#include "../../xrEngine/Fmesh.h"

CStaticTraversalStats g_StaticTraversalStats;

namespace StaticTraversalTree
{
struct TreeInfo
{
	u32 Nodes = 0;
	u32 Leaves = 0;
	u32 SmallLeaves = 0;
	u32 Depth = 0;
	u32 MaxChildren = 0;
};

constexpr float SmallLeafRadius = 0.5f;

void Walk(dxRender_Visual* Visual, u32 Depth, bool MeshLodChild, TreeInfo& Info)
{
	if (!Visual)
	{
		return;
	}

	++Info.Nodes;
	Info.Depth = std::max(Info.Depth, Depth);

	const bool Hierarchy = MeshLodChild || Visual->Type == MT_HIERRARHY || Visual->Type == MT_LOD || Visual->Type == MT_MESH_LODS;
	if (!Hierarchy)
	{
		++Info.Leaves;
		if (Visual->vis.sphere.R < SmallLeafRadius)
		{
			++Info.SmallLeaves;
		}
		return;
	}

	FHierrarhyVisual* Parent = static_cast<FHierrarhyVisual*>(Visual);
	Info.MaxChildren = std::max(Info.MaxChildren, u32(Parent->children.size()));
	for (dxRender_Visual* Child : Parent->children)
	{
		Walk(Child, Depth + 1, Visual->Type == MT_MESH_LODS, Info);
	}
}

u32 SectorIndex(CSector* Sector)
{
	const auto& All = RImplementation.Sectors;
	auto It = std::find(All.begin(), All.end(), Sector);
	return It == All.end() ? u32(-1) : u32(It - All.begin());
}
}  

void CStaticTraversalStats::Request()
{
	Requested = true;
}

bool CStaticTraversalStats::BeginFrame()
{
	if (!Requested.exchange(false))
	{
		return false;
	}

	Active = true;
	Sectors.clear();
	Sectors.reserve(PortalTraverser.r_sectors.size());
	FrameTimer.Start();
	return true;
}

void CStaticTraversalStats::BeginSector(CSector* Sector)
{
	if (!Active)
	{
		return;
	}

	SectorCounters& Counters = Sectors.emplace_back();
	Counters.Sector = Sector;
	Counters.Frustums = u32(Sector->r_frustums.size());
	Current = &Counters;
	SectorTimer.Start();
}

void CStaticTraversalStats::EndSector()
{
	if (!Current)
	{
		return;
	}

	Current->TimeMs = SectorTimer.GetElapsed_ms_f();
	Current = nullptr;
}

void CStaticTraversalStats::EndFrame()
{
	if (!Active)
	{
		return;
	}

	Active = false;
	Current = nullptr;
	const float FrameMs = FrameTimer.GetElapsed_ms_f();

	SectorCounters Total;
	for (const SectorCounters& Counters : Sectors)
	{
		Total.Frustums += Counters.Frustums;
		Total.Visited += Counters.Visited;
		Total.RejectValuable += Counters.RejectValuable;
		Total.RejectFrustum += Counters.RejectFrustum;
		Total.RejectFullDetail += Counters.RejectFullDetail;
		Total.RejectHOM += Counters.RejectHOM;
		Total.RejectFog += Counters.RejectFog;
		Total.PartialHierarchies += Counters.PartialHierarchies;
		Total.FullHierarchies += Counters.FullHierarchies;
		Total.InsertCalls += Counters.InsertCalls;
		Total.InsertDuplicates += Counters.InsertDuplicates;
		Total.InsertDiscarded += Counters.InsertDiscarded;
		Total.Inserted += Counters.Inserted;
	}

	const Fvector& Camera = Device.vCameraPosition;
	const Fvector& Direction = Device.vCameraDirection;
	Msg("* [static traversal] frame=%u camera=(%.2f, %.2f, %.2f) dir=(%.3f, %.3f, %.3f) camera_sector=%u sectors=%u/%u frustums=%u time=%.3f ms",
		Device.dwFrame, Camera.x, Camera.y, Camera.z, Direction.x, Direction.y, Direction.z,
		StaticTraversalTree::SectorIndex(RImplementation.pLastSector), u32(Sectors.size()), u32(RImplementation.Sectors.size()), Total.Frustums, FrameMs);
	Msg("* [static traversal] total: visited=%u reject(valuable=%u frustum=%u full_detail=%u hom=%u fog=%u) hierarchies(partial=%u full=%u) insert(calls=%u duplicate=%u discard=%u added=%u)",
		Total.Visited, Total.RejectValuable, Total.RejectFrustum, Total.RejectFullDetail, Total.RejectHOM, Total.RejectFog,
		Total.PartialHierarchies, Total.FullHierarchies, Total.InsertCalls, Total.InsertDuplicates, Total.InsertDiscarded, Total.Inserted);

	xr_vector<const SectorCounters*> Sorted;
	Sorted.reserve(Sectors.size());
	for (const SectorCounters& Counters : Sectors)
	{
		Sorted.push_back(&Counters);
	}
	std::sort(Sorted.begin(), Sorted.end(), [](const SectorCounters* A, const SectorCounters* B) { return A->TimeMs > B->TimeMs; });

	Msg("* [static traversal] sector | frustums portals | time_ms visited | valuable frustum full_detail hom fog | partial full | calls duplicate discard added | tree: nodes leaves small depth max_children | size");
	for (const SectorCounters* Counters : Sorted)
	{
		StaticTraversalTree::TreeInfo Tree;
		dxRender_Visual* Root = Counters->Sector->root();
		StaticTraversalTree::Walk(Root, 0, false, Tree);

		Fvector Size{};
		if (Root)
		{
			Root->vis.box.getsize(Size);
		}

		Msg("* [static traversal] %u | %u %u | %.3f %u | %u %u %u %u %u | %u %u | %u %u %u %u | tree: %u %u %u %u %u | %.0fx%.0fx%.0f",
			StaticTraversalTree::SectorIndex(Counters->Sector), Counters->Frustums, u32(Counters->Sector->portals().size()),
			Counters->TimeMs, Counters->Visited,
			Counters->RejectValuable, Counters->RejectFrustum, Counters->RejectFullDetail, Counters->RejectHOM, Counters->RejectFog,
			Counters->PartialHierarchies, Counters->FullHierarchies,
			Counters->InsertCalls, Counters->InsertDuplicates, Counters->InsertDiscarded, Counters->Inserted,
			Tree.Nodes, Tree.Leaves, Tree.SmallLeaves, Tree.Depth, Tree.MaxChildren,
			Size.x, Size.y, Size.z);
	}
}
