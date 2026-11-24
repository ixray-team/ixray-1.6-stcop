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

namespace GpuSceneInternal
{
u32 ModelOpaque = 0;
xr_vector<SortedModel> SortedModels;
xr_vector<CSkeletonX*> ModelSkins;
u32 ShadowFirst = 0;
xr_vector<std::pair<Fmatrix, const Fsphere*>> ShadowSpheres;
xr_vector<MainPart> MainParts;
xr_vector<ref_shader> IconJoins;

namespace
{
// Shaders that join a frame: each reads the tails of the textures it brings from their files on the main thread
constexpr u32 ModelJoins = 8;
// The bin of a material the sorted lists draw: it has none
constexpr u32 SortedBin = SCENE_MATERIAL_NO_PIPELINE - 1;
// A record of the sorted lists while the frame collects: its index among SortedRecords with this bit
constexpr u32 SortedRef = 1u << 31;
u32 UploadFrame = u32(-1); // the frame whose table is written
bool IsTaking = false; // the frame's main view takes its moving meshes as records
bool IsFullDetail = false;

// Where a main view's leaf goes: a world mesh, the HUD's mesh, a surface of the world's or the HUD's sorted list,
// distortion of the world, the HUD's distortion or a scope's mask, or a world mesh only the graph draws (Second: its
// element's first pass is of the second priority)
enum class LeafKind : u32
{
	World,
	Hud,
	Sorted,
	HudSorted,
	Distortion,
	HudSpecial,
	Second
};

bool IsSortedKind(LeafKind Kind)
{
	return Kind != LeafKind::World && Kind != LeafKind::Hud && Kind != LeafKind::Second;
}

bool IsHudKind(LeafKind Kind)
{
	return Kind == LeafKind::Hud || Kind == LeafKind::HudSorted || Kind == LeafKind::HudSpecial;
}

// The frame's leftover list: a leaf the scene does not draw, the element the graph selected for it, and why
struct MovingLeft
{
	MovingLeaf Leaf;
	ShaderElement* Element;
	LeafKind Kind;
	ModelLeft Why;
};

// What a record waits for until the calls are done: a rigid part where its skeleton is posed, the light of its object
struct ModelFix
{
	u32 Record;
	CSkeletonX* Rigid;
	IRenderable* Object;
	Fmatrix Matrix;
};

xr_vector<MovingLeft> MovingLefts;
xr_vector<ModelFix> ModelFixes;
xr_vector<Hlsl::SceneModel> SortedRecords;
xr_vector<CSkeletonX*> SortedSkins;

// The element a moving object of the material is drawn with by the scene, ShaderVariant::Count for none. Selected: the
// element its packet of a sorted list was given, null for the opaque bins. A model blender's own cluster passes, the
// HUD's element of it as ClusterModel; a level blender's passes made with model vertices (ClusterModel)
// A model's material, a blender's or an XML blend's: its own element draws model vertices
bool IsModelBlender(const Shader& Material)
{
	const ShaderElement* Own = Material.E[ShaderVariant::NormalHQ]._get();
	return Own && !Own->passes.empty() && IsModelVertex(Own->passes[0]._get());
}

u32 MovingVariant(const Shader& Material, const ShaderElement* Selected)
{
	const ShaderElement* Own = Material.E[ShaderVariant::NormalHQ]._get();
	const bool IsModel = IsModelBlender(Material);
	const u32 Variant = !Selected ? (IsModel ? ShaderVariant::Cluster : ShaderVariant::ClusterModel) : IsModel && Selected != Material.E[ShaderVariant::HUD]._get() ? ShaderVariant::ClusterSorted : ShaderVariant::ClusterModel;
	const ShaderElement* Element = Material.E[Variant]._get();
	const SPass* Pass = Element && !Element->passes.empty() ? Element->passes.back()._get() : nullptr;
	const bool IsShaped = Selected ? Element && Element->passes.size() == 1 : !(Own && Own->flags.bStrictB2F);
	return IsModelVertex(Pass) && IsShaped ? Variant : u32(ShaderVariant::Count);
}

struct Joining
{
	Shader* Material;
	u32 Variant;
	bool IsSorted;
};

// The shaders of models the frame met that the scene does not know yet, by the list that met them first: the world's
// opaque one, the HUD's, then their sorted ones
xr_vector<std::pair<LeafKind, Joining>> JoinWanted;
} // namespace

// The upload buffer of a frame's slot for the records of the frame's models: the main view's, then the shadow views' casters
nri::Buffer& ModelUpload(u32 Frame)
{
	if (!ModelUploads[0])
	{
		for (nri::Buffer*& Upload : ModelUploads)
		{
			nri::BufferDesc Desc = {};
			Desc.size = u64(ModelCapacity) * sizeof(Hlsl::SceneModel);
			Desc.structureStride = sizeof(Hlsl::SceneModel);
			Desc.usage = nri::BufferUsageBits::SHADER_RESOURCE;
			Require(Api.CreateCommittedBuffer(*GRHI->Nri->Device, nri::MemoryLocation::HOST_UPLOAD, 0.0f, Desc, Upload), "The graphics device could not create a scene buffer.");
		}
	}

	return *ModelUploads[Frame % FrameCount];
}

namespace
{
// Gives a model's shader its material record and a draw, or a sorted list's material its record; Taken when it has
// them, else why it stays in the graph. Record and Id: the record to copy into the table; a level shader known from the
// load keeps the level's record (IsWritten false)
ModelLeft JoinModel(const Joining& Item, nri::CommandBuffer& Commands, Hlsl::SceneMaterial& Record, u32& Id, bool& IsWritten)
{
	Shader* Material = Item.Material;
	const SPass* Pass = MaterialPass(Material);

	if (!Pass || EnvOf(*Material) == NoEnv)
	{
		return Pass ? ModelLeft::Env : ModelLeft::Shader;
	}

	CTexture* Maps[SCENE_MATERIAL_MAPS];
	Record = EmptyMaterial();
	ReadMaterial(*Pass, Record, Maps);

	// The registers of an XML blend's list are its own: its base alone is a map, the rest is bound as it is (DrawForward)
	if (Material->E[ShaderVariant::NormalHQ]->flags.OwnMaps)
	{
		std::fill(std::begin(Maps) + 1, std::end(Maps), nullptr);
	}

	bool IsComplete = Maps[0] != nullptr;
	bool HasNew = false;

	for (u32 Map = 0; Map < SCENE_MATERIAL_MAPS; ++Map)
	{
		if (!Maps[Map])
		{
			continue;
		}

		const auto [Found, IsNew] = TextureRefs.try_emplace(Maps[Map]);
		const char* Why = "";
		Found->second = IsNew ? Textures.Add(*Maps[Map], Commands, *Streamer) : Found->second;
		Found->second = Found->second.Tail == GpuTextures::Absent ? BindTexture(Maps[Map], Why) : Found->second;
		HasNew |= IsNew;
		u32* Words = &Record.base + Map * 4;
		Words[0] = Found->second.Tail;
		Words[1] = Found->second.Head;
		Words[2] = Found->second.Info;
		IsComplete &= Found->second.Tail != GpuTextures::Absent;
	}

	const u32 Base = IsComplete ? Textures.Format(Record.base) : 0;
	Record.flags |= (IsComplete ? SCENE_MATERIAL_COMPLETE : 0u) | (Base == 71 || Base == 72 ? SCENE_MATERIAL_BASE_BC1 : 0u);
	Record.pipeline = SCENE_MATERIAL_NO_PIPELINE;
	const auto Known = MaterialIds.find(Material);
	Id = Known != MaterialIds.end() ? Known->second : u32(MaterialIds.size());
	IsWritten = Known == MaterialIds.end() || IsModelBlender(*Material);
	// The level's record of the shader has the maps the load gave it
	IsComplete &= IsWritten || !HasNew;
	const bool IsHeld = IsComplete && Id < MaterialCapacity;
	const u32 Bin = !IsHeld ? SCENE_MATERIAL_NO_PIPELINE : Item.IsSorted ? SortedBin : FindBin(Material, Record, LevelBinCount, Item.Variant);

	if (Bin == SCENE_MATERIAL_NO_PIPELINE)
	{
		return IsComplete ? ModelLeft::NoRoom : ModelLeft::Incomplete;
	}

	MaterialIds.try_emplace(Material, Id);
	const bool IsBound = HasBoundMap(Record);
	ModelBinIds[Material] = { Bin, Id, IsBound };

	// A sorted list's pass of such a material is made here, not by the walk that draws it first
	if (Item.IsSorted && IsBound)
	{
		BoundPass(Material->E[Item.Variant]->passes[0]._get());
	}

	// No cull asks for the heads of a sorted list's textures: they are asked for always
	for (u32 Map = 0; Map < SCENE_MATERIAL_PLAIN_MAPS && Item.IsSorted; ++Map)
	{
		if (const u32 Stream = (&Record.base)[Map * 4 + 2] >> SCENE_TEXTURE_STREAM_SHIFT)
		{
			SortedStreams.push_back(Stream - 1);
		}
	}

	return ModelLeft::Taken;
}

// The shaders of models the scene does not know yet join, a few a frame, before the cull reads the materials: those of
// the opaque lists of the world and the HUD, then those of their sorted lists
void JoinModels()
{
	Joining Items[ModelJoins];
	u32 Count = 0;
	std::stable_sort(JoinWanted.begin(), JoinWanted.end(), [](const auto& Left, const auto& Right) { return Left.first < Right.first; });

	for (size_t Index = 0; Index < JoinWanted.size() && Count < ModelJoins; ++Index)
	{
		Items[Count++] = JoinWanted[Index].second;
	}

	// The icons' materials the frame before asked for, as the world would join them: held until they are joined
	size_t IconsSeen = 0;

	for (; IconsSeen < IconJoins.size() && Count < ModelJoins; ++IconsSeen)
	{
		Shader* Material = IconJoins[IconsSeen]._get();
		const ShaderElement* Own = Material->E[ShaderVariant::NormalHQ]._get();
		const bool IsSorted = Own && Own->flags.bStrictB2F;
		const u32 Variant = ModelBinIds.contains(Material) || ModelRejects.contains(Material) ? u32(ShaderVariant::Count) : MovingVariant(*Material, IsSorted ? Own : nullptr);

		if (Variant != ShaderVariant::Count && std::find_if(Items, Items + Count, [Material](const Joining& Item) { return Item.Material == Material; }) == Items + Count)
		{
			Items[Count++] = { Material, Variant, IsSorted };
		}
	}

	if (!Count)
	{
		IconJoins.erase(IconJoins.begin(), IconJoins.begin() + IconsSeen);
		return;
	}

	PROF_EVENT("SceneJoinModels");
	nri::CommandBuffer& Commands = *BeginNriCommands(nullptr, 0, nullptr, 0, nullptr);
	Hlsl::SceneMaterial Records[ModelJoins];
	u32 Ids[ModelJoins];
	u32 Joined = 0;
	u32 Written = 0;

	for (u32 Index = 0; Index < Count; ++Index)
	{
		ModelShaders.emplace_back()._set(Items[Index].Material);
		bool IsWritten = false;
		const ModelLeft Why = JoinModel(Items[Index], Commands, Records[Written], Ids[Written], IsWritten);
		Joined += Why == ModelLeft::Taken;
		Written += Why == ModelLeft::Taken && IsWritten;

		if (Why != ModelLeft::Taken)
		{
			ModelRejects[Items[Index].Material] = Why;
		}
	}

	IconJoins.erase(IconJoins.begin(), IconJoins.begin() + IconsSeen);
	std::sort(SortedStreams.begin(), SortedStreams.end());
	SortedStreams.erase(std::unique(SortedStreams.begin(), SortedStreams.end()), SortedStreams.end());

	if (Written)
	{
		Tracked Table = { Materials, { nri::AccessBits::SHADER_RESOURCE, nri::StageBits::ALL } };
		Move(Commands, Table, nri::AccessBits::COPY_DESTINATION, nri::StageBits::COPY);
		const nri::StreamerCopyBatch Batch = StreamApi.BeginStreamerCopyBatch(*Streamer);

		for (u32 Index = 0; Index < Written; ++Index)
		{
			const nri::DataSize Chunk = { &Records[Index], sizeof(Hlsl::SceneMaterial) };
			nri::StreamBufferDataDesc Data = {};
			Data.dataChunks = &Chunk;
			Data.dataChunkNum = 1;
			Data.placementAlignment = 16;
			Data.copyBatch = Batch;
			Data.dstBuffer = Materials;
			Data.dstOffset = u64(Ids[Index]) * sizeof(Hlsl::SceneMaterial);
			StreamApi.StreamBufferData(*Streamer, Data);
		}

		StreamApi.CmdCopyStreamedData(Commands, *Streamer, Batch);
		Move(Commands, Table, nri::AccessBits::SHADER_RESOURCE, nri::StageBits::ALL);
	}

	EndNriCommands(&Commands);
	Msg("* [scene] model shaders joined: %u of %u; %u materials, %u forward pipelines, %u of %u registers of textures bound as they are", Joined, Count, u32(MaterialIds.size()), u32(Bins.size()), u32(BoundTextures.size()), SCENE_TEXTURE_BOUND);
}
void PushOrdered(R_dsgraph::DynamicList& List, const MovingLeaf& Leaf, ShaderElement* Element)
{
	List.push_back({ u64(~std::bit_cast<u32>(Leaf.Distance)) << 32 | u32(List.size()), nullptr, Element, Leaf.Visual, Leaf.Object, Leaf.Object ? Leaf.Object->renderable_ROS() : nullptr, Leaf.Ssa, Leaf.World });
}

u32 RecordIndex(u32 Record)
{
	return Record != NoRecord && (Record & SortedRef) ? ModelOpaque + (Record & ~SortedRef) : Record;
}

void ResetModels()
{
	ModelRecords.clear();
	ModelSkins.clear();
	SortedRecords.clear();
	SortedSkins.clear();
	SortedModels.clear();
	MainParts.clear();
	ShadowSpheres.clear();
	MovingLefts.clear();
	ModelFixes.clear();
	JoinWanted.clear();
	ShadowFirst = 0;
	ModelRuns = 0;
	ModelOpaque = 0;
	std::memset(ModelsLeft, 0, sizeof(ModelsLeft));
	ShadersLeft.clear();

	for (u32 Bin = LevelBinCount; Bin < Bins.size(); ++Bin)
	{
		Bins[Bin].Count = 0;
	}
}

// A leaf's record, its reference (SortedRef for one of the sorted lists), else NoRecord and why the graph keeps it.
// Element: the one the graph selected for it. A mesh part decides for all the passes of its element: it is drawn by the
// scene or by the graph, never by both
u32 TakeModel(const MovingLeaf& Leaf, const ShaderElement* Element, LeafKind Kind, ModelLeft& Why)
{
	if (!IsTaking)
	{
		Why = ModelLeft::Off;
		return NoRecord;
	}

	dxRender_Visual* Visual = Leaf.Visual;
	Shader* Material = Visual->shader._get();
	const auto Model = ModelBinIds.find(Material);
	const auto Rejected = ModelRejects.find(Material);
	const ShaderElement* Own = Material->E[ShaderVariant::NormalHQ]._get();
	const ShaderElement* Special = Material->E[ShaderVariant::Special]._get();
	const IRender_Mesh* Mesh = dynamic_cast<const IRender_Mesh*>(Visual);
	const u32 Type = Visual->Type;
	const bool IsPart = Type == MT_SKELETON_GEOMDEF_ST || Type == MT_SKELETON_GEOMDEF_PM;
	const bool IsSorted = IsSortedKind(Kind);
	const bool IsHud = IsHudKind(Kind);
	const ShaderElement* Selected = IsSorted ? Element : nullptr;
	const u32 Variant = MovingVariant(*Material, Selected);
	CSkeletonX* Owner = IsPart ? static_cast<CSkeletonX*>(Visual) : nullptr;
	const u32 Page = Mesh ? Mesh->Alloc.Page : PageSlots;

	// The graph draws the emissive pass of an element over its colour
	Why = Model == ModelBinIds.end() ? (Rejected != ModelRejects.end() ? Rejected->second : Variant != ShaderVariant::Count ? ModelLeft::Joining : ModelLeft::Shader)
		: Variant == ShaderVariant::Count || (!IsSorted && Model->second.Bin == SortedBin) ? ModelLeft::Element
		: !IsSorted && (!Own || Own->passes.empty() || Element != Own) ? ModelLeft::Element
		: (IsSorted ? Selected : Own)->flags.bEmissive ? ModelLeft::Emissive
		: !Mesh || (!IsPart && Type != MT_NORMAL && Type != MT_PROGRESSIVE) ? ModelLeft::Kind
		: Page >= PageSlots || !IndexPageBuffers[Page] ? ModelLeft::Page
		: Owner && (!Owner->Parent || !Owner->visible) ? ModelLeft::Hidden
		: ModelRecords.size() + SortedRecords.size() >= FrameModels ? ModelLeft::Caps : ModelLeft::Taken;

	// What has no element the scene draws a moving object with, by the vertex shader of its pass, for the stats line
	if (Why == ModelLeft::Shader)
	{
		const SPass* Pass = Element && !Element->passes.empty() ? Element->passes[0]._get() : nullptr;
		const shared_str Vertex = Pass && Pass->vs ? Pass->vs->cName : shared_str();
		const auto Named = std::find_if(ShadersLeft.begin(), ShadersLeft.end(), [&Vertex](const std::pair<shared_str, u32>& Item) { return Item.first == Vertex; });
		++(Named != ShadersLeft.end() ? Named->second : ShadersLeft.emplace_back(Vertex, 0u).second);
	}

	// Distortion and the scope's mask ask for no shader to join
	if (Why == ModelLeft::Joining && Kind < LeafKind::Distortion)
	{
		const auto Wanted = std::find_if(JoinWanted.begin(), JoinWanted.end(), [Material](const std::pair<LeafKind, Joining>& Item) { return Item.second.Material == Material; });

		if (Wanted == JoinWanted.end())
		{
			JoinWanted.push_back({ Kind, { Material, Variant, IsSorted } });
		}
		else if (Kind < Wanted->first)
		{
			*Wanted = { Kind, { Material, Variant, IsSorted } };
		}
	}

	if (Why != ModelLeft::Taken)
	{
		return NoRecord;
	}

	// A progressive mesh draws the window Render picks for the leaf's detail
	FProgressive* Progressive = Owner && Owner->progressive_mesh ? Owner : nullptr;
	Progressive = Type == MT_PROGRESSIVE ? dynamic_cast<FProgressive*>(Visual) : Progressive;
	const float Detail = IsFullDetail ? 1.0f : _sqrt(clampr((Leaf.Ssa - r_ssaGLOD_end) / (r_ssaGLOD_start - r_ssaGLOD_end), 0.0f, 1.0f));
	const FSlideWindow* Window = Progressive ? Progressive->PickWindow(Detail) : nullptr;
	const u32 Triangles = Window ? Window->num_tris : Mesh->dwPrimitives;
	const u32 Runs = (Triangles + SCENE_CLUSTER_TRIANGLES - 1) / SCENE_CLUSTER_TRIANGLES;

	// The opaque bins' runs have their room in the visible list; the sorted walk counts its own
	if (!Triangles || Runs > SCENE_DYNAMIC_RUNS || (!IsSorted && ModelRuns + Runs > ModelClusters))
	{
		Why = !Triangles || Runs > SCENE_DYNAMIC_RUNS ? ModelLeft::Triangles : ModelLeft::Caps;
		return NoRecord;
	}

	Fmatrix Old = Leaf.World;

	if (Owner && Owner->IsSkinned())
	{
		RImplementation.Skinning.Request(*Owner, Leaf.World);
	}
	else if (!Owner)
	{
		const auto Before = std::lower_bound(ModelWorlds[1].begin(), ModelWorlds[1].end(), Visual, [](const ModelPlace& Item, const dxRender_Visual* Key) { return Item.first < Key; });
		Old = Before != ModelWorlds[1].end() && Before->first == Visual ? Before->second : Leaf.World;
		ModelWorlds[0].emplace_back(Visual, Leaf.World);
	}

	const Fsphere& Sphere = PartSphere(Visual);
	Fvector Centre;
	Leaf.World.transform_tiny(Centre, Sphere.P);
	const float Scale = std::max({ Leaf.World.i.magnitude(), Leaf.World.j.magnitude(), Leaf.World.k.magnitude() });

	xr_vector<Hlsl::SceneModel>& Records = IsSorted ? SortedRecords : ModelRecords;
	const u32 Ref = u32(Records.size()) | (IsSorted ? SortedRef : 0u);
	Hlsl::SceneModel& Record = Records.emplace_back();
	Record.world = Leaf.World;
	Record.world_old = Old;
	Record.bounds.set(Centre.x, Centre.y, Centre.z, Sphere.R * Scale);
	Record.index_start = Mesh->iBase + (Window ? Window->offset : 0);
	Record.triangle_count = Triangles;
	Record.vertices = Mesh->vBase;
	Record.source = Page | (IsHud ? SCENE_MODEL_HUD : 0u);
	Record.material = Model->second.Material;
	Record.bin = IsSorted ? SCENE_MATERIAL_NO_PIPELINE : Model->second.Bin;
	Record.alpha = SCENE_TEXTURE_ABSENT;
	Record.flags = SCENE_OBJECT_MAIN | (IsHud ? SCENE_OBJECT_HUD : 0u) | (IsSorted ? SCENE_OBJECT_SORTED : 0u) | (Special && (Special->flags.bDistort || Special->flags.bScopeMask) ? SCENE_OBJECT_DISTORT : 0u);
	Record.sector = SCENE_VIEW_SECTORS;
	Record.object = Kind == LeafKind::World || Kind == LeafKind::Sorted ? Leaf.Id : SCENE_OBJECT_NONE;
	(IsSorted ? SortedSkins : ModelSkins).push_back(Owner && Owner->IsSkinned() ? Owner : nullptr);
	ModelFixes.push_back({ Ref, Owner && !Owner->IsSkinned() ? Owner : nullptr, Leaf.Object, Leaf.World });
	++ModelsLeft[IsHud][u32(ModelLeft::Taken)];

	if (IsSorted)
	{
		SortedModels.push_back({ Visual, Ref, Material->E[Variant]._get(), Model->second.IsBound });
		return Ref;
	}

	Bins[Record.bin].Count += Runs;
	ModelRuns += Runs;
	return Ref;
}

// What an object of the table added of the world, taken or not: the shadow views cast it
void AddPart(const MovingLeaf& Leaf, LeafKind Kind, u32 Record)
{
	if (Leaf.Id != SCENE_OBJECT_NONE && (Kind == LeafKind::World || Kind == LeafKind::Second || Kind == LeafKind::Sorted) && !Leaf.Visual->dcast_ParticleCustom())
	{
		MainParts.push_back({ Leaf.Visual, Leaf.World, Leaf.Ssa, Leaf.Id, Record });
	}
}

// A taken world mesh leaves the graph only its passes of the second priority
void Place(R_dsgraph_structure& Graph, const MovingLeaf& Leaf, ShaderElement* Element, LeafKind Kind, u32 Record)
{
	if (Kind == LeafKind::World)
	{
		InsertPasses(Graph, Element, Leaf.Visual, Leaf.Object, Leaf.World, Leaf.Ssa, 2);
	}

	AddPart(Leaf, Kind, Record);
}

u32 Keep(R_dsgraph_structure& Graph, const MovingLeaf& Leaf, ShaderElement* Element, LeafKind Kind)
{
	ModelLeft Why = ModelLeft::Taken;
	const u32 Record = TakeModel(Leaf, Element, Kind, Why);

	if (Record == NoRecord)
	{
		MovingLefts.push_back({ Leaf, Element, Kind, Why });
		return NoRecord;
	}

	Place(Graph, Leaf, Element, Kind, Record);
	return Record;
}
} // namespace

void AddMainLeaf(R_dsgraph_structure& Graph, const MovingLeaf& Leaf, bool IsInvisible, bool IsDistortion, const bool (&Mask)[3])
{
	dxRender_Visual* Visual = Leaf.Visual;
	Shader& Material = *Visual->shader._get();
	ShaderElement* Special = Material.E[ShaderVariant::Special]._get();
	const bool IsParticle = Visual->dcast_ParticleCustom() != nullptr;
	LeafKind Specials[2];
	u32 SpecialCount = 0;

	if (IsDistortion && Special && Special->flags.bDistort && Mask[Special->flags.iPriority / 2] && !psDeviceFlags.test(rsClearBB))
	{
		PushOrdered(Leaf.IsHud ? Graph.mapHUDDistort : Graph.mapDistort, Leaf, Special);
		RequestMovingSkin(Visual, Leaf.World);
		Specials[SpecialCount++] = Leaf.IsHud ? LeafKind::HudSpecial : LeafKind::Distortion;
	}

	if (Special && Special->flags.bScopeMask && Mask[0])
	{
		PushOrdered(Graph.mapHUDScopeMask, Leaf, Special);
		RequestMovingSkin(Visual, Leaf.World);
		Specials[SpecialCount++] = LeafKind::HudSpecial;
	}

	// The HUD keeps a blended surface sorted where the world weighs it order-independently
	ShaderElement* World = Material.E[ShaderVariant::NormalHQ]._get();
	ShaderElement* Hud = Material.E[ShaderVariant::HUD]._get();
	ShaderElement* Element = Leaf.IsHud && World && World->flags.OrderIndependent && Hud && !Hud->passes.empty() ? Hud : World;
	u32 Sorted = NoRecord;

	if (Element && !IsInvisible)
	{
		RequestMovingSkin(Visual, Leaf.World);
		const bool IsListed = Mask[Element->flags.iPriority / 2];
		const bool IsStrict = Element->flags.bStrictB2F;
		const SPass* First = Element->passes.empty() ? nullptr : Element->passes[0]._get();
		const u32 FirstList = First ? (First->iPriority != u8(-1) ? First->iPriority : Element->flags.iPriority) / 2 : 2;

		if (IsListed && Leaf.IsHud && (IsStrict || Element->flags.iPriority > 1))
		{
			PushOrdered(Graph.mapHUDSorted, Leaf, Element);
			Sorted = IsParticle ? NoRecord : Keep(Graph, Leaf, Element, LeafKind::HudSorted);
		}
		else if (IsListed && Leaf.IsHud)
		{
			Keep(Graph, Leaf, Element, LeafKind::Hud);
		}
		else if (IsListed && (IsStrict || IsParticle))
		{
			PushOrdered(Graph.mapSorted, Leaf, Element);
			Sorted = Leaf.Object && !IsParticle ? Keep(Graph, Leaf, Element, LeafKind::Sorted) : NoRecord;
		}
		else if (FirstList == 0 && Mask[0])
		{
			Keep(Graph, Leaf, Element, LeafKind::World);
		}
		else
		{
			MovingLefts.push_back({ Leaf, Element, LeafKind::Second, FirstList == 1 ? ModelLeft::Priority : ModelLeft::Element });
		}
	}

	// Distortion and the scope's mask draw the record of the surface's sorted list: one with none is taken for them
	for (u32 Index = 0; Index < SpecialCount && !IsParticle; ++Index)
	{
		if (Sorted == NoRecord && (Specials[Index] == LeafKind::HudSpecial || Leaf.Object))
		{
			Sorted = Keep(Graph, Leaf, Special, Specials[Index]);
		}
	}
}
} // namespace GpuSceneInternal

void GpuScene::BeginObjects(R_dsgraph_structure& Main)
{
	LogFrameObjects();
	ResetModels();
	IsTaking = ps_r_scene_models && IsForwardBinned();
	IsFullDetail = Main.ViewParams.FullDetail;
	ModelFrame = TakesCasters() ? RFrame.dwFrame : ModelFrame;

	if (IsTaking)
	{
		std::swap(ModelWorlds[0], ModelWorlds[1]);
		ModelWorlds[0].clear();
	}

	Collector.Begin(Main.ViewParams, Main.pmask, &Main);
}

IDSGraphManager& GpuScene::Objects()
{
	return Collector;
}

void GpuScene::BuildObjects(R_dsgraph_structure& Graph)
{
	if (DevicePtr->IsEditorMode())
	{
		ResetModels();
		return;
	}

	PROF_EVENT("SceneBuildObjects");

	if (IsTaking)
	{
		JoinModels();
	}

	// The leftovers: those whose shaders joined now are taken, the graph draws the rest, a mesh part as its packets; a
	// surface of a sorted list waits in its list already
	for (const MovingLeft& Left : MovingLefts)
	{
		const MovingLeaf& Leaf = Left.Leaf;
		const bool IsSpecial = Left.Kind == LeafKind::Distortion || Left.Kind == LeafKind::HudSpecial;

		if (IsSpecial && std::any_of(SortedModels.begin(), SortedModels.end(), [&Leaf](const SortedModel& Item) { return Item.Visual == Leaf.Visual; }))
		{
			continue;
		}

		ModelLeft Why = Left.Why;
		const u32 Record = Why == ModelLeft::Joining ? TakeModel(Leaf, Left.Element, Left.Kind, Why) : NoRecord;

		if (Record != NoRecord)
		{
			Place(Graph, Leaf, Left.Element, Left.Kind, Record);
			continue;
		}

		++ModelsLeft[IsHudKind(Left.Kind)][u32(Why)];

		if (Left.Kind != LeafKind::Second || Why == ModelLeft::Priority)
		{
			AddPart(Leaf, Left.Kind, NoRecord);
		}

		if (Left.Kind == LeafKind::World || Left.Kind == LeafKind::Second)
		{
			InsertPasses(Graph, Left.Element, Leaf.Visual, Leaf.Object, Leaf.World, Leaf.Ssa);
		}
		else if (Left.Kind == LeafKind::Hud)
		{
			PushOrdered(Graph.mapHUD, Leaf, Left.Element);
		}
	}

	// The sorted lists' records after the opaque ones, which the cull takes alone
	ModelOpaque = u32(ModelRecords.size());
	ModelRecords.insert(ModelRecords.end(), SortedRecords.begin(), SortedRecords.end());
	ModelSkins.insert(ModelSkins.end(), SortedSkins.begin(), SortedSkins.end());

	for (SortedModel& Item : SortedModels)
	{
		Item.Model = RecordIndex(Item.Model);
	}

	for (MainPart& Part : MainParts)
	{
		Part.Record = RecordIndex(Part.Record);
	}

	// A rigid part stands where its skeleton is posed once the calls are done. The light apply_object hands the graph's
	// draw under baked light: a level shader on a moving object takes it too, not its own lightmap
	for (const ModelFix& Fix : ModelFixes)
	{
		Hlsl::SceneModel& Record = ModelRecords[RecordIndex(Fix.Record)];

		if (CSkeletonX* Rigid = Fix.Rigid)
		{
			RImplementation.Skinning.Snapshot(*Rigid->Parent, Fix.Matrix);
			const CBoneInstance& Bone = Rigid->Parent->LL_GetBoneInstance(u16(Rigid->RMS_boneid));
			Fmatrix World, Old;
			World.mul_43(Fix.Matrix, Bone.mRenderTransform_tmp);
			Old.mul_43(Rigid->Parent->mOldWorldMartrix, Bone.mRenderTransform_old);
			Record.world = World;
			Record.world_old = Old;
		}

		IRenderable* Lit = Fix.Object && Fix.Object->renderable_ROS() ? Fix.Object : nullptr;

		if (CROS_impl* Track = Lit ? static_cast<CROS_impl*>(Lit->renderable_ROS()) : RImplementation.ViewerLightTrack())
		{
			Track->update_smooth(Lit);
			const float* Cube = Track->get_hemi_cube();
			Record.hemi_cube_pos.set(Cube[CROS_impl::CUBE_FACE_POS_X], Cube[CROS_impl::CUBE_FACE_POS_Y], Cube[CROS_impl::CUBE_FACE_POS_Z], 0.5f * Track->get_sun());
			Record.hemi_cube_neg.set(Cube[CROS_impl::CUBE_FACE_NEG_X], Cube[CROS_impl::CUBE_FACE_NEG_Y], Cube[CROS_impl::CUBE_FACE_NEG_Z], 0.5f * Track->get_hemi());
		}
	}

	// A visual once, by its record where it has one
	std::sort(MainParts.begin(), MainParts.end(), [](const MainPart& Left, const MainPart& Right) { return Left.Visual < Right.Visual || (Left.Visual == Right.Visual && Left.Record < Right.Record); });
	MainParts.erase(std::unique(MainParts.begin(), MainParts.end(), [](const MainPart& Left, const MainPart& Right) { return Left.Visual == Right.Visual; }), MainParts.end());
	ShadowFirst = u32(ModelRecords.size());
	std::sort(ModelWorlds[0].begin(), ModelWorlds[0].end(), [](const ModelPlace& Left, const ModelPlace& Right) { return Left.first < Right.first; });
	std::sort(SortedModels.begin(), SortedModels.end(), [](const SortedModel& Left, const SortedModel& Right) { return Left.Visual < Right.Visual; });
}

void GpuScene::UploadModels(GfxCmdList& Cmd)
{
	if (ModelFrame != RFrame.dwFrame || UploadFrame == ModelFrame)
	{
		return;
	}

	UploadFrame = ModelFrame;

	if (ModelRecords.empty())
	{
		return;
	}

	PROF_EVENT("SceneUploadModels");
	RImplementation.Skinning.Flush(Cmd);
	Cmd.RestoreInput();

	// A model bin's entries follow the level's in the main view's first part
	u32 Start = ClusterCount;

	for (u32 Bin = LevelBinCount; Bin < Bins.size(); ++Bin)
	{
		Bins[Bin].Start = Start;
		Start += Bins[Bin].Count;
	}

	for (u32 Index = 0; Index < ModelRecords.size(); ++Index)
	{
		Hlsl::SceneModel& Record = ModelRecords[Index];
		Record.visible_start = Index < ModelOpaque ? Bins[Record.bin].Start : 0;

		// What only the shadow views draw is tested where its skeleton stands once posed
		if (Index >= ShadowFirst)
		{
			const auto& [Matrix, Sphere] = ShadowSpheres[Index - ShadowFirst];
			Fvector Centre;
			Matrix.transform_tiny(Centre, Sphere->P);
			const float Scale = std::max({ Matrix.i.magnitude(), Matrix.j.magnitude(), Matrix.k.magnitude() });
			Record.bounds.set(Centre.x, Centre.y, Centre.z, Sphere->R * Scale);
		}

		// What the skinning pass made of the part, where it stood after its skeleton was posed
		if (const CSkeletonX* Part = ModelSkins[Index])
		{
			Record.vertices = Part->SkinOutBase;
			Record.source |= SCENE_DYNAMIC_SKINNED;
			Record.world_old = RImplementation.Skinning.OldWorld(*Part->Parent);
		}
	}

	nri::Buffer& Upload = ModelUpload(ModelFrame);
	const u64 Bytes = ModelRecords.size() * sizeof(Hlsl::SceneModel);
	std::memcpy(Api.MapBuffer(Upload, 0, Bytes), ModelRecords.data(), Bytes);
	Api.UnmapBuffer(Upload);
}
