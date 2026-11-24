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
#include "CHudInitializer.h"
#include "IconPictures.h"

namespace GpuSceneInternal
{
xr_hash_map<const dxRender_Visual*, SortedStatic> SortedStatics;
xr_vector<u32> SortedStreams;
u32 SortedStart = 0;
xr_vector<SortedSegment> SortedSegments;

namespace
{
const char* const SortedLeftNames[u32(SortedLeft::Count)] = { "taken", "moving object", "not a level mesh", "no sorted cluster pass", "maps neither in the buckets nor bound",
	"an environment cube outside the array", "no clusters of its mesh", "weighed apart", "frame caps" };

// The lists the walks draw in their order: the sorted one, the weighed one, the HUD's; distortion of the world and of
// the HUD; the scope's mask
enum : u32
{
	ListSorted,
	ListWeighed,
	ListHud,
	ListDistortion,
	ListHudDistortion,
	ListScope,
	ListCount
};

const char* const ListNames[ListCount] = { "sorted list", "weighed surfaces (WBOIT)", "HUD's sorted list", "distortion", "HUD's distortion", "scope's mask" };

struct SortedMade
{
	nri::Pipeline* Pipeline = nullptr;
	u32 Samples = 0;
};

xr_hash_map<const SPass*, SortedMade> SortedPipelines;
nri::Buffer* SortedUploads[FrameCount] = {};
xr_vector<u32> Entries; // the frame's clusters and model runs of the lists, in the order of their segments
u32 EntryFrame = u32(-1);
u32 Uploaded = 0;
u32 ModelsCopied = u32(-1); // the frame whose sorted model records are in the scene's table

// Neighbours of one pass: Count clusters from First, or with Model the first Count runs of that model record
struct SortedRun
{
	const SPass* Pass;
	u32 First;
	u32 Count;
	u32 Model = u32(-1);
};

xr_vector<SortedRun> Runs;
u32 Queued = 0; // the entries of the runs

// The 3D icons of the frame IconFrame: IconCount records from FrameModels written, IconsCopied of them in the table
u32 IconFrame = u32(-1);
u32 IconCount = 0;
u32 IconsCopied = 0;
// What a pass reads of the scene: the model records and the visible list in its vertex stage (a cluster pass); the material
// table in either stage
constexpr u64 ModelReads = 1ull << 6 | 1ull << 11;
constexpr u64 VertexMaterialRead = 1ull << 3;
constexpr u64 PixelMaterialRead = 1ull << 22;

// A mesh part of the icon being drawn: its record among IconRecords, its runs, its distance from the eye
struct IconPart
{
	const SPass* Pass;
	u32 Record;
	u32 Runs;
	float Distance;
	bool IsBlended;
	CSkeletonX* Skin;
};

struct IconLeaf
{
	dxRender_Visual* Visual;
	const Fmatrix* Xform;
};

xr_vector<IconLeaf> IconLeaves;
xr_vector<IconPart> IconParts;
xr_vector<Hlsl::SceneModel> IconRecords;
IconPictures Pictures;
// The streamed heads as the icons of the frame HeadsFrame read them
u32 HeadsFrame = u32(-1);
u32 HeadsKey = 0;

// The mesh parts of a visual, as the graph took them from a skeleton or a hierarchy
void AddLeaves(dxRender_Visual* Visual, const Fmatrix& Xform)
{
	if (Visual->Type == MT_HIERRARHY || Visual->Type == MT_SKELETON_ANIM || Visual->Type == MT_SKELETON_RIGID)
	{
		for (dxRender_Visual* Child : static_cast<FHierrarhyVisual*>(Visual)->children)
		{
			AddLeaves(Child, Xform);
		}

		return;
	}

	IconLeaves.push_back({ Visual, &Xform });
}

void CollectLeaves(xr_span<const UIIconVisual> Visuals)
{
	IconLeaves.clear();

	for (const UIIconVisual& Item : Visuals)
	{
		if (Item.Visual)
		{
			AddLeaves((dxRender_Visual*)Item.Visual, Item.Xform);
		}
	}
}

// A particle effect of the list a walk draws: its place among the packets left (the sort key in a sorted list, the
// number of packets before it in one drawn in insertion order), and the stream's batches it was recorded as
struct SortedParticle
{
	u64 Order;
	ShaderElement* Element;
	dxRender_Visual* Visual;
	DynamicBatches Batches;
};

xr_vector<SortedParticle> ParticleDraws;

// Per list what the scene drew, Taken, and why the rest stayed, its draws and its particle effects; then what the graph
// drew of the second priority: its static and its moving packets
struct SortedCounts
{
	u32 Left[ListCount][u32(SortedLeft::Count)] = {};
	u32 Draws[ListCount] = {};
	u32 Particles[ListCount] = {};
	u32 Graph[2] = {};
	u32 Icons = 0;
	u32 IconsKept = 0;
	u32 IconsLost = 0;
	u32 IconDraws = 0;
	u32 IconParts[u32(SortedLeft::Count)] = {};
};

SortedCounts CountsNow, CountsLast, CountsCopied;
u32 CountsFrame = u32(-1);

SortedCounts& Counts()
{
	if (CountsFrame != RFrame.dwFrame)
	{
		CountsFrame = RFrame.dwFrame;
		CountsLast = CountsNow;
		CountsNow = {};
	}

	return CountsNow;
}

// Taken when the scene draws the packet of the list: its run in Run
SortedLeft Classify(const R_dsgraph::DynamicPacket& Node, u32 Kind, SortedRun& Run)
{
	const bool IsWeighed = Kind == ListWeighed;
	const bool IsHud = Kind == ListHud || Kind == ListHudDistortion || Kind == ListScope;
	// Distortion and the scope's mask are an XML blend's Special element: its pass compiled as a cluster pass
	// (ClusterModel) with the vertices of a model or of the level's clusters. The element is read every frame: a
	// material compiled again keeps its address, not its passes
	const bool IsSpecial = Kind >= ListDistortion;
	const auto SpecialPass = [&](bool IsModel) -> const SPass*
	{
		const ShaderElement* Element = Node.Visual->shader->E[ShaderVariant::ClusterModel]._get();
		const bool IsKind = Element && Element->passes.size() == 1 && (Kind == ListScope ? Element->flags.bScopeMask : Element->flags.bDistort);
		const SPass* Pass = IsKind ? Element->passes[0]._get() : nullptr;
		return Pass && (Kind == ListScope || IsModelVertex(Pass) == IsModel) ? Pass : nullptr;
	};

	// A moving surface has a model record of this frame (TakeModels), or why not in the mesh parts' stats line
	if (Node.Object || IsHud)
	{
		const auto Found = std::lower_bound(SortedModels.begin(), SortedModels.end(), Node.Visual, [](const SortedModel& Item, const dxRender_Visual* Key) { return Item.Visual < Key; });

		// The scope's mask is the HUD's: the graph drew any other by the HUD's projection too
		if (ModelFrame != RFrame.dwFrame || Found == SortedModels.end() || Found->Visual != Node.Visual || (Kind == ListScope && !(ModelRecords[Found->Model].source & SCENE_MODEL_HUD)))
		{
			return SortedLeft::Moving;
		}

		const SPass* Own = IsSpecial ? SpecialPass(true) : Found->Element->passes[0]._get();
		const SPass* Pass = Found->IsBound ? BoundPass(Own)._get() : Own;
		Run = { Pass, 0, (ModelRecords[Found->Model].triangle_count + SCENE_CLUSTER_TRIANGLES - 1) / SCENE_CLUSTER_TRIANGLES, Found->Model };
		return !Pass ? SortedLeft::Pass : !IsSpecial && (Found->Element->flags.OrderIndependent != 0) != IsWeighed ? SortedLeft::Weighed : Entries.size() + Queued + Run.Count > SortedEntries ? SortedLeft::Caps : SortedLeft::Taken;
	}

	const auto Found = SortedStatics.find(Node.Visual);

	if (Found == SortedStatics.end() || Found->second.Why != SortedLeft::Taken)
	{
		return Found != SortedStatics.end() ? Found->second.Why : SortedLeft::Level;
	}

	const ShaderElement* Element = Node.Visual->shader->E[ShaderVariant::ClusterSorted]._get();
	const SPass* Own = IsSpecial ? SpecialPass(false) : Element && Element->passes.size() == 1 ? Element->passes[0]._get() : nullptr;
	const SPass* Pass = Found->second.IsBound ? BoundPass(Own)._get() : Own;
	Run = { Pass, Found->second.First, Found->second.Count };

	return !Pass ? SortedLeft::Pass : !IsSpecial && (Element->flags.OrderIndependent != 0) != IsWeighed ? SortedLeft::Weighed : Entries.size() + Queued + Run.Count > SortedEntries ? SortedLeft::Caps : SortedLeft::Taken;
}

// The runs so far as segments of neighbours of one pass, drawn by the scene into the targets. A register holds one
// texture in a draw: a segment that binds another one where an earlier one binds its own starts the next draw
void FlushRuns(GpuScene& Scene, GfxCmdList& Cmd, IRHIRenderTargetView* const* Targets, u32 TargetCount, IRHIDepthStencilView* Depth, u32& Draws, u32 Parts = 32)
{
	PROF_EVENT("SortedFlush");
	const CTexture* Bound[64] = {};
	SortedSegments.clear();

	const auto Draw = [&]()
	{
		if (SortedSegments.empty())
		{
			return;
		}

		Draws += u32(SortedSegments.size());
		Scene.DrawForward(Cmd, Targets, TargetCount, Depth, true, Parts);
		SortedSegments.clear();
		std::fill(std::begin(Bound), std::end(Bound), nullptr);
	};

	for (const SortedRun& Run : Runs)
	{
		if (SortedSegments.empty() || SortedSegments.back().Pass != Run.Pass)
		{
			if (!AddForwardReads(*Run.Pass, Bound))
			{
				Draw();
				AddForwardReads(*Run.Pass, Bound);
			}

			SortedSegments.push_back({ Run.Pass, u32(Entries.size()), 0 });
		}

		for (u32 Item = 0; Item < Run.Count; ++Item)
		{
			Entries.push_back(Run.Model != u32(-1) ? SCENE_VISIBLE_DYNAMIC | (Item << SCENE_VISIBLE_MODEL_SHIFT) | Run.Model : Run.First + Item);
		}

		SortedSegments.back().Count += Run.Count;
	}

	Draw();
	Runs.clear();
	Queued = 0;
}

// The particle effects of the first Count packets of a list leave it, each recorded with every pass of its element as the
// stream's batches with what the walk binds, by the HUD's projection for the HUD's lists. Returns the packets left
size_t TakeParticles(GfxCmdList& Cmd, R_dsgraph::DynamicList& List, size_t Count, u32 Kind)
{
	const bool IsInserted = Kind >= ListDistortion;
	ParticleDraws.clear();
	size_t Kept = 0;

	for (size_t Index = 0; Index < Count; ++Index)
	{
		R_dsgraph::DynamicPacket& Node = List[Index];

		if (Node.Visual->dcast_ParticleCustom())
		{
			ParticleDraws.push_back({ IsInserted ? u64(Kept) : Node.Key, Node.Element, Node.Visual });
			continue;
		}

		if (Kept != Index)
		{
			List[Kept] = Node;
		}

		++Kept;
	}

	List.erase(List.begin() + Kept, List.begin() + Count);

	if (ParticleDraws.empty())
	{
		return Kept;
	}

	std::optional<CHudInitializer> Hud;

	if (Kind == ListHud || Kind == ListHudDistortion || Kind == ListScope)
	{
		Hud.emplace(Cmd, true, Kind != ListScope);
	}

	for (SortedParticle& Item : ParticleDraws)
	{
		RImplementation.Dynamic.BeginRecord();

		for (u32 Pass = 0; Pass < u32(Item.Element->passes.size()); ++Pass)
		{
			{
				PROF_EVENT("ParticlePass");
				Cmd.SetPass(Item.Element, Pass);
			}

			PROF_EVENT("ParticleRender");
			Item.Visual->Render(Cmd, 0.0f);
		}

		Item.Batches = RImplementation.Dynamic.EndRecord();
	}

	return Kept;
}

// Draws the first Count packets of a list in their order and takes them out of it: the scene's runs, neighbours of one
// pass in one draw, the graph's packets and the particle effects between them. What is weighed (ListWeighed) is drawn
// in any order: the graph's first, the scene's by pass. The HUD's graph draws by the HUD's projection, the scope's mask
// over the whole depth; the scene's HUD records carry their own
void BeginEntries()
{
	if (EntryFrame != RFrame.dwFrame)
	{
		EntryFrame = RFrame.dwFrame;
		Entries.clear();
		Uploaded = 0;
	}
}

void WalkList(GpuScene& Scene, GfxCmdList& Cmd, R_dsgraph_structure& Graph, R_dsgraph::DynamicList& List, size_t Count, u32 Kind, IRHIRenderTargetView* const* Targets, u32 TargetCount, IRHIDepthStencilView* Depth)
{
	BeginEntries();
	SortedCounts& Frame = Counts();
	const bool IsWeighed = Kind == ListWeighed;
	const bool IsInserted = Kind >= ListDistortion;
	const bool IsHud = Kind == ListHud || Kind == ListHudDistortion || Kind == ListScope;
	const bool IsHeld = Scene.IsForwardBinned() && &Graph == &RImplementation.GraphMain && !DevicePtr->IsEditorMode();
	Runs.clear();
	Queued = 0;
	{
		PROF_EVENT("SortedParticles");
		Count = TakeParticles(Cmd, List, Count, Kind);
	}

	Frame.Particles[Kind] += u32(ParticleDraws.size());
	size_t Submitted = 0;

	const auto SubmitTo = [&](u64 Place)
	{
		if (Submitted == ParticleDraws.size() || ParticleDraws[Submitted].Order > Place)
		{
			return;
		}

		if (!IsWeighed)
		{
			FlushRuns(Scene, Cmd, Targets, TargetCount, Depth, Frame.Draws[Kind]);
		}

		PROF_EVENT("SortedSubmit");

		// The effects were recorded in this order: their batches stand in a row, and a row is submitted at once
		DynamicBatches Row = {};

		for (; Submitted < ParticleDraws.size() && ParticleDraws[Submitted].Order <= Place; ++Submitted)
		{
			const DynamicBatches& Next = ParticleDraws[Submitted].Batches;

			if (Row.Count && Next.First != Row.First + Row.Count)
			{
				RImplementation.Dynamic.Submit(Cmd, Row);
				Row = {};
			}

			Row.First = Row.Count ? Row.First : Next.First;
			Row.Count += Next.Count;
		}

		RImplementation.Dynamic.Submit(Cmd, Row);
	};

	for (size_t Index = 0; Index < Count; ++Index)
	{
		R_dsgraph::DynamicPacket& Node = List[Index];
		SubmitTo(IsInserted ? u64(Index) : Node.Key);
		SortedRun Run = {};
		// Counted where the scene could hold them: the editors and a level without clusters are the graph's alone
		const SortedLeft Why = IsHeld ? Classify(Node, Kind, Run) : SortedLeft::Level;
		Frame.Left[Kind][u32(Why)] += IsHeld ? 1u : 0u;

		if (Why == SortedLeft::Taken)
		{
			Runs.push_back(Run);
			Queued += Run.Count;
			continue;
		}

		if (!IsWeighed)
		{
			FlushRuns(Scene, Cmd, Targets, TargetCount, Depth, Frame.Draws[Kind]);
		}

		std::optional<CHudInitializer> Hud;

		if (IsHud)
		{
			Hud.emplace(Cmd, true, Kind != ListScope);
		}

		PROF_EVENT("SortedNode");
		Graph.r_dsgraph_render_node(Node);
	}

	SubmitTo(u64(-1));

	if (IsWeighed)
	{
		std::sort(Runs.begin(), Runs.end(), [](const SortedRun& Left, const SortedRun& Right) { return Left.Pass < Right.Pass; });
	}

	FlushRuns(Scene, Cmd, Targets, TargetCount, Depth, Frame.Draws[Kind]);
	List.erase(List.begin(), List.begin() + Count);
}
} // namespace

SortedLeft KeepSorted(const dxRender_Visual& Visual, const Hlsl::SceneMaterial& Record)
{
	Shader* Material = Visual.shader._get();
	const ShaderElement* Own = Material ? Material->E[ShaderVariant::NormalHQ]._get() : nullptr;

	if (!Own || !Own->flags.bStrictB2F)
	{
		return SortedLeft::Count;
	}

	// A model shader's cluster pass reads model records, not the level's clusters
	const ShaderElement* Element = Material->E[ShaderVariant::ClusterSorted]._get();
	const SPass* Pass = Element && Element->passes.size() == 1 ? Element->passes[0]._get() : nullptr;
	const SortedLeft Why = !Pass || !Pass->vs || IsModelVertex(Pass) ? SortedLeft::Pass
		: EnvOf(*Material) == NoEnv ? SortedLeft::Env
		: !(Record.flags & SCENE_MATERIAL_COMPLETE) ? SortedLeft::Incomplete : SortedLeft::Taken;

	SortedStatic& Item = SortedStatics[&Visual];
	Item.Why = Why;
	Item.IsBound = Why == SortedLeft::Taken && HasBoundMap(Record);

	// Its pass of the textures bound as they are is made at the load, not by the walk that draws it first
	if (Item.IsBound)
	{
		BoundPass(Pass);
	}

	return Why;
}

void ReleaseSortedPipelines()
{
	for (const auto& [Pass, Made] : SortedPipelines)
	{
		if (Made.Pipeline)
		{
			Api.DestroyPipeline(Made.Pipeline);
		}
	}

	SortedPipelines.clear();
}

void ReleaseSorted()
{
	ReleaseSortedPipelines();

	for (nri::Buffer*& Upload : SortedUploads)
	{
		if (Upload)
		{
			Api.DestroyBuffer(Upload);
			Upload = nullptr;
		}
	}

	SortedStatics.clear();
	SortedStreams.clear();
	Entries.clear();
	EntryFrame = u32(-1);
	ModelsCopied = u32(-1);
}

nri::Pipeline*& SortedPipeline(const SPass* Pass, u32 Samples)
{
	SortedMade& Made = SortedPipelines[Pass];

	// r_msaa changed: frames in flight may still draw with the old one
	if (Made.Pipeline && Made.Samples != Samples)
	{
		GRHI->DevicePtr->Flush();
		Api.DestroyPipeline(Made.Pipeline);
		Made.Pipeline = nullptr;
	}

	Made.Samples = Samples;
	return Made.Pipeline;
}

void UploadSorted(nri::CommandBuffer& Commands)
{
	// The sorted lists' model records follow those the cull copied, from the same upload buffer
	if (ModelFrame == RFrame.dwFrame && ModelsCopied != ModelFrame && ModelOpaque < ModelRecords.size())
	{
		ModelsCopied = ModelFrame;
		const u64 First = u64(ModelOpaque) * sizeof(Hlsl::SceneModel);
		Move(Commands, Models, nri::AccessBits::COPY_DESTINATION, nri::StageBits::COPY);
		Api.CmdCopyBuffer(Commands, *Models.Buffer, First, ModelUpload(ModelFrame), First, (ModelRecords.size() - ModelOpaque) * sizeof(Hlsl::SceneModel));
		Move(Commands, Models, nri::AccessBits::SHADER_RESOURCE, nri::StageBits::VERTEX_SHADER);
	}

	// The icons' records at the table's top, written after the frame's
	if (IconFrame == RFrame.dwFrame && IconsCopied < IconCount)
	{
		const u64 First = u64(FrameModels + IconsCopied) * sizeof(Hlsl::SceneModel);
		Move(Commands, Models, nri::AccessBits::COPY_DESTINATION, nri::StageBits::COPY);
		Api.CmdCopyBuffer(Commands, *Models.Buffer, First, ModelUpload(RFrame.dwFrame), First, u64(IconCount - IconsCopied) * sizeof(Hlsl::SceneModel));
		Move(Commands, Models, nri::AccessBits::SHADER_RESOURCE, nri::StageBits::VERTEX_SHADER);
		IconsCopied = IconCount;
	}

	const u32 Count = u32(Entries.size()) - Uploaded;

	if (!Count)
	{
		return;
	}

	nri::Buffer*& Upload = SortedUploads[RFrame.dwFrame % FrameCount];

	if (!Upload)
	{
		nri::BufferDesc Desc = {};
		Desc.size = SortedEntries * sizeof(u32);
		Require(Api.CreateCommittedBuffer(*GRHI->Nri->Device, nri::MemoryLocation::HOST_UPLOAD, 0.0f, Desc, Upload), "The graphics device could not create a scene buffer.");
	}

	const u64 Offset = u64(Uploaded) * sizeof(u32);
	const u64 Bytes = u64(Count) * sizeof(u32);
	std::memcpy(Api.MapBuffer(*Upload, Offset, Bytes), Entries.data() + Uploaded, Bytes);
	Api.UnmapBuffer(*Upload);

	Move(Commands, Visible, nri::AccessBits::COPY_DESTINATION, nri::StageBits::COPY);
	Api.CmdCopyBuffer(Commands, *Visible.Buffer, u64(SortedStart) * sizeof(u32) + Offset, *Upload, Offset, Bytes);
	Move(Commands, Visible, nri::AccessBits::SHADER_RESOURCE, nri::StageBits::VERTEX_SHADER);
	Uploaded += Count;
}

void CopySortedCounts()
{
	CountsCopied = CountsFrame == RFrame.dwFrame ? CountsLast : CountsFrame + 1 == RFrame.dwFrame ? CountsNow : SortedCounts();
}

void LogSortedCounts()
{
	for (u32 List = 0; List < ListCount; ++List)
	{
		string512 Left = {};

		for (u32 Reason = 1; Reason < u32(SortedLeft::Count); ++Reason)
		{
			string64 Part;
			xr_sprintf(Part, "%s%s %u", Reason > 1 ? ", " : "", SortedLeftNames[Reason], CountsCopied.Left[List][Reason]);
			xr_strcat(Left, Part);
		}

		Msg("* [scene] %s in the frame before: the scene drew %u surfaces in %u draws and %u particle effects; left in the graph by reason: %s", ListNames[List], CountsCopied.Left[List][u32(SortedLeft::Taken)], CountsCopied.Draws[List],
			CountsCopied.Particles[List], Left);
	}

	Msg("* [scene] graph passes in the frame before: second priority static %u, moving %u", CountsCopied.Graph[0], CountsCopied.Graph[1]);
	const u32* Icon = CountsCopied.IconParts;
	Msg("* [scene] 3D icons in the frame before: %u drawn, %u kept from their pictures, %u pictures lost to icons drawn over them or to a full list; pictures held now %u of %u; the scene drew %u mesh parts in %u draws; not drawn: no interface cluster pass %u, material joining %u, mesh not in the heap's pages %u, frame caps %u",
		CountsCopied.Icons, CountsCopied.IconsKept, CountsCopied.IconsLost, Pictures.Size(), IconPictures::Capacity, Icon[u32(SortedLeft::Taken)], CountsCopied.IconDraws, Icon[u32(SortedLeft::Pass)], Icon[u32(SortedLeft::Incomplete)],
		Icon[u32(SortedLeft::Mesh)], Icon[u32(SortedLeft::Caps)]);
}
} // namespace GpuSceneInternal

void GpuScene::DrawSorted(GfxCmdList& Cmd, R_dsgraph_structure& Graph, IRHIRenderTargetView* const* Targets, u32 TargetCount, IRHIDepthStencilView* Depth, bool IsWeighed, bool IsHud)
{
	R_dsgraph::DynamicList& List = IsHud ? Graph.mapHUDSorted : Graph.mapSorted;
	const auto End = IsWeighed ? std::partition(List.begin(), List.end(), [](const R_dsgraph::DynamicPacket& Node) { return Node.Element->flags.OrderIndependent != 0; }) : List.end();

	if (!IsWeighed)
	{
		PROF_EVENT("SortedOrder");
		std::sort(List.begin(), List.end(), [](const R_dsgraph::DynamicPacket& Left, const R_dsgraph::DynamicPacket& Right) { return Left.Key < Right.Key; });
	}

	WalkList(*this, Cmd, Graph, List, size_t(End - List.begin()), IsHud ? ListHud : IsWeighed ? ListWeighed : ListSorted, Targets, TargetCount, Depth);
}

void GpuScene::DrawDistortion(GfxCmdList& Cmd, R_dsgraph_structure& Graph, IRHIRenderTargetView* Target, IRHIDepthStencilView* Depth, bool IsScope)
{
	if (IsScope)
	{
		WalkList(*this, Cmd, Graph, Graph.mapHUDScopeMask, Graph.mapHUDScopeMask.size(), ListScope, &Target, 1, Depth);
		return;
	}

	WalkList(*this, Cmd, Graph, Graph.mapDistort, Graph.mapDistort.size(), ListDistortion, &Target, 1, Depth);
	WalkList(*this, Cmd, Graph, Graph.mapHUDDistort, Graph.mapHUDDistort.size(), ListHudDistortion, &Target, 1, Depth);
}

void GpuScene::CountGraphPasses(const R_dsgraph_structure& Graph)
{
	Counts().Graph[0] = u32(Graph.StaticPasses[1].size());
	Counts().Graph[1] = u32(Graph.DynamicPasses[1].size());
}

u32 GpuScene::IconKey(xr_span<const UIIconVisual> Visuals)
{
	PROF_EVENT("SceneIconKey");

	if (HeadsFrame != RFrame.dwFrame)
	{
		HeadsFrame = RFrame.dwFrame;
		const xr_vector<u32>& Heads = Textures.GetHeads();
		HeadsKey = crc32(Heads.data(), Heads.size() * sizeof(u32));
	}

	const UIIconLighting* Light = DevicePtr->m_pRender->GetUIIconLighting();
	const UIIconLighting Lit = Light ? *Light : UIIconLighting{};
	const float Words[] = { Lit.Color.x, Lit.Color.y, Lit.Color.z, Lit.Intensity, Lit.Direction.x, Lit.Direction.y, Lit.Direction.z, Lit.Ambient, ps_r2_def_aref_quality };
	u32 Key = crc32(Words, sizeof(Words), HeadsKey);
	Key = crc32(&RFrame.mView, sizeof(Fmatrix), Key);
	Key = crc32(&RFrame.mProject, sizeof(Fmatrix), Key);

	for (const UIIconVisual& Item : Visuals)
	{
		Key = crc32(&Item.Xform, sizeof(Fmatrix), Key);
		IKinematics* Bones = Item.Visual ? Item.Visual->dcast_PKinematics() : nullptr;

		if (!Bones)
		{
			continue;
		}

		const VisMask Visible = Bones->LL_GetBonesVisible();
		Key = crc32(&Visible, sizeof(Visible), Key);

		for (u16 Bone = 0, Count = Bones->LL_BoneCount(); Bone < Count; ++Bone)
		{
			Key = crc32(&Bones->LL_GetBoneInstance(Bone).mRenderTransform, sizeof(Fmatrix), Key);
		}
	}

	CollectLeaves(Visuals);

	for (const IconLeaf& Item : IconLeaves)
	{
		dxRender_Visual* Leaf = Item.Visual;
		const bool IsPart = Leaf->Type == MT_SKELETON_GEOMDEF_ST || Leaf->Type == MT_SKELETON_GEOMDEF_PM;
		const IRender_Mesh* Mesh = dynamic_cast<const IRender_Mesh*>(Leaf);
		const u64 Part[] = { u64(uintptr_t(Leaf->shader._get())), Leaf->Type | u64(IsPart && static_cast<CSkeletonX*>(Leaf)->visible) << 32, Mesh ? Mesh->vBase | u64(Mesh->iBase) << 32 : 0,
			Mesh ? Mesh->dwPrimitives | u64(Mesh->Alloc.Page) << 32 : 0 };
		Key = crc32(Part, sizeof(Part), Key);
	}

	return Key;
}

bool GpuScene::IsIconKept(const Irect& Place, u32 Key)
{
	const bool IsKept = Pictures.IsKept(Place, Key, RFrame.dwFrame);
	Counts().IconsKept += IsKept;
	return IsKept;
}

void GpuScene::ForgetIcons()
{
	Pictures.Clear();
}

void GpuScene::DrawIcon(GfxCmdList& Cmd, xr_span<const UIIconVisual> Visuals, const Irect& Place, u32 Key, IRHIRenderTargetView* Target, IRHIDepthStencilView* Depth)
{
	SortedCounts& Frame = Counts();
	const auto Keep = [&](bool IsWhole) { Frame.IconsLost += Pictures.Drawn(Place, Key, RFrame.dwFrame, IsWhole); };

	if (Visuals.empty() || !ModelCapacity || !IsForwardBinned())
	{
		Keep(false);
		return;
	}

	PROF_EVENT("SceneDrawIcon");
	BeginEntries();

	if (IconFrame != RFrame.dwFrame)
	{
		IconFrame = RFrame.dwFrame;
		IconCount = 0;
		IconsCopied = 0;
	}

	++Frame.Icons;
	IconParts.clear();
	IconRecords.clear();
	CollectLeaves(Visuals);
	bool IsSkinned = false;
	bool IsWhole = true;

	for (const IconLeaf& Item : IconLeaves)
	{
		dxRender_Visual* Leaf = Item.Visual;
		const Fmatrix& Xform = *Item.Xform;
		Shader* Material = Leaf->shader._get();
		const u32 Type = Leaf->Type;
		const bool IsPart = Type == MT_SKELETON_GEOMDEF_ST || Type == MT_SKELETON_GEOMDEF_PM;
		CSkeletonX* Owner = IsPart ? static_cast<CSkeletonX*>(Leaf) : nullptr;

		if (!Material || (Owner && (!Owner->Parent || !Owner->visible)))
		{
			continue;
		}

		// The interface element compiled as a cluster pass pulls the model's vertices from its record
		const ShaderElement* Element = Material->E[ShaderVariant::UI]._get();
		const SPass* Own = Element && Element->passes.size() == 1 ? Element->passes[0]._get() : nullptr;

		if (Own && Own->vs && Own->ps)
		{
			Own->vs->CompileState.WaitCompiled();
			Own->ps->CompileState.WaitCompiled();
		}

		const bool IsCluster = Own && Own->vs && Own->ps && (Own->vs->ReadSlots & ModelReads) == ModelReads;
		const bool IsMaterialRead = IsCluster && ((Own->vs->ReadSlots & VertexMaterialRead) || (Own->ps->ReadSlots & PixelMaterialRead));
		const auto Joined = ModelBinIds.find(Material);
		const IRender_Mesh* Mesh = dynamic_cast<const IRender_Mesh*>(Leaf);
		const u32 Page = Mesh ? Mesh->Alloc.Page : PageSlots;
		FProgressive* Progressive = Owner && Owner->progressive_mesh ? Owner : nullptr;
		Progressive = Type == MT_PROGRESSIVE ? dynamic_cast<FProgressive*>(Leaf) : Progressive;
		const FSlideWindow* Window = Progressive ? Progressive->PickWindow(1.0f) : nullptr;
		const u32 Triangles = Window ? Window->num_tris : Mesh ? Mesh->dwPrimitives : 0;
		const u32 RunCount = (Triangles + SCENE_CLUSTER_TRIANGLES - 1) / SCENE_CLUSTER_TRIANGLES;
		const bool IsMesh = Mesh && (IsPart || Type == MT_NORMAL || Type == MT_PROGRESSIVE) && Page < PageSlots && IndexPageBuffers[Page] && Triangles && RunCount <= SCENE_DYNAMIC_RUNS;

		const SortedLeft Why = !IsCluster ? SortedLeft::Pass
			: IsMaterialRead && Joined == ModelBinIds.end() ? SortedLeft::Incomplete
			: !IsMesh ? SortedLeft::Mesh
			: IconCount + IconRecords.size() >= IconModels || Entries.size() + Queued + RunCount > SortedEntries ? SortedLeft::Caps : SortedLeft::Taken;
		++Frame.IconParts[u32(Why)];
		const bool IsJoining = Why == SortedLeft::Incomplete && !ModelRejects.contains(Material);
		// A part a later frame may draw leaves the picture unkept: its material joining, its mesh's page, the caps
		IsWhole &= Why == SortedLeft::Taken || Why == SortedLeft::Pass || (Why == SortedLeft::Incomplete && !IsJoining);

		// Its material joins the scene with the next frame's models, as the world would join it
		if (IsJoining && std::none_of(IconJoins.begin(), IconJoins.end(), [Material](const ref_shader& Item) { return Item._get() == Material; }))
		{
			IconJoins.emplace_back()._set(Material);
		}

		if (Why != SortedLeft::Taken)
		{
			continue;
		}

		Fmatrix World = Xform;
		Fmatrix Old = Xform;

		if (Owner && Owner->IsSkinned())
		{
			RImplementation.Skinning.Request(*Owner, Xform);
			IsSkinned = true;
		}
		else if (Owner)
		{
			RImplementation.Skinning.Rigid(*Owner->Parent, Xform, u16(Owner->RMS_boneid), World, Old);
		}

		const Fsphere& Sphere = PartSphere(Leaf);
		Fvector Centre;
		Xform.transform_tiny(Centre, Sphere.P);
		const float Scale = std::max({ Xform.i.magnitude(), Xform.j.magnitude(), Xform.k.magnitude() });

		Hlsl::SceneModel& Record = IconRecords.emplace_back();
		Record.world = World;
		Record.world_old = Old;
		Record.bounds.set(Centre.x, Centre.y, Centre.z, Sphere.R * Scale);
		Record.index_start = Mesh->iBase + (Window ? Window->offset : 0);
		Record.triangle_count = Triangles;
		Record.vertices = Mesh->vBase;
		Record.source = Page;
		Record.material = Joined != ModelBinIds.end() ? Joined->second.Material : 0;
		Record.bin = SCENE_MATERIAL_NO_PIPELINE;
		Record.alpha = SCENE_TEXTURE_ABSENT;
		Record.sector = SCENE_VIEW_SECTORS;
		Record.object = SCENE_OBJECT_NONE;

		// The viewer's sky light for the shaders that light by the hemi cube, read as the main view left it
		if (CROS_impl* Track = RImplementation.ViewerTrack)
		{
			const float* Cube = Track->drawn_hemi_cube();
			Record.hemi_cube_pos.set(Cube[CROS_impl::CUBE_FACE_POS_X], Cube[CROS_impl::CUBE_FACE_POS_Y], Cube[CROS_impl::CUBE_FACE_POS_Z], 0.5f * Track->drawn_sun());
			Record.hemi_cube_neg.set(Cube[CROS_impl::CUBE_FACE_NEG_X], Cube[CROS_impl::CUBE_FACE_NEG_Y], Cube[CROS_impl::CUBE_FACE_NEG_Z], 0.5f * Track->drawn_hemi());
		}

		const bool IsBound = IsMaterialRead && Joined->second.IsBound;
		IconParts.push_back({ IsBound ? BoundPass(Own)._get() : Own, u32(IconRecords.size() - 1), RunCount, Centre.distance_to_sqr(RFrame.vCameraPosition), Element->flags.bStrictB2F || Element->flags.iPriority > 1,
			Owner && Owner->IsSkinned() ? Owner : nullptr });
		Queued += RunCount;
	}

	if (IconParts.empty())
	{
		Queued = 0;
		Keep(IsWhole);
		return;
	}

	// What the skinning pass makes of the parts, where they stand once posed
	if (IsSkinned)
	{
		RImplementation.Skinning.Flush(Cmd);
		Cmd.RestoreInput();
	}

	for (const IconPart& Part : IconParts)
	{
		if (Part.Skin)
		{
			Hlsl::SceneModel& Record = IconRecords[Part.Record];
			Record.vertices = Part.Skin->SkinOutBase;
			Record.source |= SCENE_DYNAMIC_SKINNED;
			Record.world_old = RImplementation.Skinning.OldWorld(*Part.Skin->Parent);
		}
	}

	const u32 First = FrameModels + IconCount;
	const u64 Offset = u64(First) * sizeof(Hlsl::SceneModel);
	const u64 Bytes = IconRecords.size() * sizeof(Hlsl::SceneModel);
	nri::Buffer& Upload = ModelUpload(RFrame.dwFrame);
	std::memcpy(Api.MapBuffer(Upload, Offset, Bytes), IconRecords.data(), Bytes);
	Api.UnmapBuffer(Upload);
	IconCount += u32(IconRecords.size());

	// The opaque parts in their order, then the blended ones back to front
	const auto Blended = std::stable_partition(IconParts.begin(), IconParts.end(), [](const IconPart& Part) { return !Part.IsBlended; });
	std::stable_sort(Blended, IconParts.end(), [](const IconPart& Left, const IconPart& Right) { return Left.Distance > Right.Distance; });

	for (const IconPart& Part : IconParts)
	{
		Runs.push_back({ Part.Pass, 0, Part.Runs, First + Part.Record });
	}

	FlushRuns(*this, Cmd, &Target, 1, Depth, Frame.IconDraws, 32 | 128);
	Keep(IsWhole);
}
