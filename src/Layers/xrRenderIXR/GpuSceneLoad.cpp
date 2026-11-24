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
#include <numeric>

namespace GpuSceneInternal
{
namespace
{
constexpr u32 AlphaLayers = 2048; // the most a texture array has on D3D11
constexpr u32 ModelMaterials = 1024;
// Why a level visual stays outside the depth batch (Load)
enum class StaticLeft : u32
{
	Batched,
	Kind,
	NoElement,
	Passes,
	Sorted,
	Shader,
	State,
	Alpha,
	Mesh,
	MeshLods,
	Count
};
const char* const StaticLeftNames[u32(StaticLeft::Count)] = { "batched", "no mesh, material or known kind", "no shadow element", "more than one shadow pass", "sorted back to front at the first priority",
	"other shadow shader", "other raster or depth state", "cut out by a texture without an alpha slice", "mesh outside the level's mesh data", "a mesh LOD group" };
// Why a level visual stays outside the main view's bins (Load), whatever its shadow
enum class MainLeft : u32
{
	Batched,
	Kind,
	MeshLods,
	Model,
	Pass,
	Env,
	Incomplete,
	Mesh,
	Count
};
const char* const MainLeftNames[u32(MainLeft::Count)] = { "batched", "no mesh, material or known kind", "a mesh LOD group", "a model shader", "no cluster pass", "an environment cube outside the array", "a map neither in the buckets nor bound", "mesh outside the level's mesh data" };

enum class ECaster : u8
{
	None,
	Opaque,
	Alpha
};

// The shadow pass of a level visual the batch draws: Opaque, or Alpha with the texture it is cut out by in Base; Why
// when it is None. The batch's own shaders and state draw at the main view too. Pending: a pass the batch draws for the
// shadows alone once its state is found to draw as the batch's (IsStaticState): the model shader, whose world is the
// identity for a static, or a state of its own. IsLate: a pass of a later priority, which only a lamp's translucent pass
// draws; it writes depth alone, so the batch draws it with the rest wherever that pass is drawn
ECaster ClassifyCaster(const dxRender_Visual& Visual, bool IsTree, CTexture*& Base, const SPass*& Pending, StaticLeft& Why, bool& IsLate)
{
	const u32 BaseSlot = ShaderBindings::Texture("s_base");
	const ShaderElement* Element = Visual.shader->E[ShaderVariant::Shadow]._get();
	const SPass* Pass = Element && Element->passes.size() == 1 ? Element->passes[0]._get() : nullptr;
	// As the graph files it: a sorted element by its own priority, a pass by its own where it has one
	const u32 Priority = Pass && !Element->flags.bStrictB2F && Pass->iPriority != u8(-1) ? Pass->iPriority : Element ? u32(Element->flags.iPriority) : 0u;
	IsLate = Priority / 2 != 0;
	Why = !Element ? StaticLeft::NoElement
		: !Pass ? StaticLeft::Passes
		: Element->flags.bStrictB2F && !IsLate ? StaticLeft::Sorted : StaticLeft::Shader;

	if (Why != StaticLeft::Shader || !Pass->vs || !Pass->ps || !Pass->state || (Pass->ps->Texture2DSlots & ~(1ull << BaseSlot)))
	{
		return ECaster::None;
	}

	Base = nullptr;
	Pending = nullptr;

	if (Pass->ps->Texture2DSlots && Pass->T)
	{
		for (const auto& [Stage, Texture] : *Pass->T)
		{
			if (Stage == CTexture::rstPixel + BaseSlot)
			{
				Base = Texture._get();
			}
		}
	}

	// A tree is placed by its own matrix, which only the tree's shader takes
	const auto IsNamed = [](const shared_str& Name, const char* Prefix) { return Name.size() && !strncmp(*Name, Prefix, xr_strlen(Prefix)); };
	const bool IsOwn = IsNamed(Pass->vs->cName, IsTree ? "shadow_lod" : "shadow_base");
	if ((Pass->ps->Texture2DSlots && !Base) || !IsNamed(Pass->ps->cName, "shadow_base") || (!IsOwn && (IsTree || !IsNamed(Pass->vs->cName, "shadow_model"))) || (Pass->gs && Pass->gs->gs) ||
		(Pass->hs && Pass->hs->sh) || (Pass->ds && Pass->ds->sh))
	{
		return ECaster::None;
	}

	if (!DepthState && IsOwn)
	{
		DepthState = Pass->state._get();
	}

	Why = StaticLeft::Batched;
	Pending = IsOwn && DepthState == Pass->state._get() ? nullptr : Pass;
	return Base ? ECaster::Alpha : ECaster::Opaque;
}

MainLeft LevelBinWhy(Shader* Material, const Hlsl::SceneMaterial& Record)
{
	const ShaderElement* Element = Material ? Material->E[ShaderVariant::Cluster]._get() : nullptr;
	return !Material ? MainLeft::Kind
		: ModelBinIds.contains(Material) || IsModelShader(*Material) ? MainLeft::Model
		: !Element || Element->passes.empty() || !Element->passes.back() ? MainLeft::Pass
		: EnvOf(*Material) == NoEnv ? MainLeft::Env
		: !(Record.flags & SCENE_MATERIAL_COMPLETE) ? MainLeft::Incomplete : MainLeft::Batched;
}

void BuildBins(const xr_vector<Hlsl::SceneInstance>& Instances, const xr_vector<Hlsl::SceneCluster>& Clusters, xr_vector<Hlsl::SceneMaterial>& Materials)
{
	Bins.clear();
	BinLimit = SIZE_MAX;

	// The record of a model's material names no bin: a level visual of the same shader stays in the graph
	for (const auto& [Material, Id] : MaterialIds)
	{
		Materials[Id].pipeline = LevelBinWhy(Material, Materials[Id]) == MainLeft::Batched ? FindBin(Material, Materials[Id], 0) : SCENE_MATERIAL_NO_PIPELINE;
	}

	LevelBinCount = u32(Bins.size());

	for (auto& [Material, Model] : ModelBinIds)
	{
		Model.Bin = FindBin(Material, Materials[Model.Material], LevelBinCount);
	}

	std::erase_if(ModelBinIds, [](const auto& Item) { return Item.second.Bin == SCENE_MATERIAL_NO_PIPELINE; });

	u32 Total = 0;

	for (const Hlsl::SceneCluster& Cluster : Clusters)
	{
		const u32 Bin = Materials[Instances[Cluster.instance].material].pipeline;
		if ((Cluster.triangles & SCENE_CLUSTER_MAIN) && Bin != SCENE_MATERIAL_NO_PIPELINE)
		{
			++Bins[Bin].Count;
			++Total;
		}
	}

	u32 Start = 0;
	u32 Used = 0;
	u32 Blended = 0;
	u32 BoundBins = 0;

	for (SceneBin& Bin : Bins)
	{
		Bin.Start = Start;
		Start += Bin.Count;
		Used += Bin.Count != 0;
		Blended += Bin.Count && Bin.Order == 4;
		BoundBins += Bin.IsBound;
	}

	for (Hlsl::SceneMaterial& Material : Materials)
	{
		Material.pipeline_start = Material.pipeline != SCENE_MATERIAL_NO_PIPELINE ? Bins[Material.pipeline].Start : 0;
	}

	Msg("* [scene] forward pipelines: %u used of %u for %u materials, %u clusters; %u of them for %u model materials; %u used ones blend, drawn after the sky; %u read the textures bound as they are",
		Used, u32(Bins.size()), u32(Materials.size()), Total, u32(Bins.size()) - LevelBinCount, u32(ModelBinIds.size()), Blended, BoundBins);
}

// A shadow pass of a level visual whose state the batch's pipelines draw as it would: the batch state of moving casters,
// its fill and bias clamp and depth test too, no blend, the depth written
bool IsStaticState(const SPass& Pass)
{
	const GfxPipelineState& State = *Pass.state->state;
	const GfxPipelineState& Batch = *DepthState->state;

	return IsBatchState(Pass) && State.Raster.FillMode == Batch.Raster.FillMode && State.Raster.DepthBiasClamp == Batch.Raster.DepthBiasClamp && State.Depth.DepthEnable == Batch.Depth.DepthEnable &&
		!State.Blend.RenderTarget[0].BlendEnable && State.Depth.DepthWriteMask == RHI_DEPTH_WRITE_MASK_ALL;
}

bool HasSortedTaken(const dxRender_Visual* Visual)
{
	if (const FHierrarhyVisual* Node = dynamic_cast<const FHierrarhyVisual*>(Visual))
	{
		return std::any_of(Node->children.begin(), Node->children.end(), HasSortedTaken);
	}

	const auto Found = SortedStatics.find(Visual);
	return Found != SortedStatics.end() && Found->second.Why == SortedLeft::Taken;
}

// GpuScene::GetMainStatics of a sector, depth first as the main view's walk meets them
void AddMainStatics(dxRender_Visual* Visual, xr_vector<dxRender_Visual*>& List)
{
	if (!Visual)
	{
		return;
	}

	const FHierrarhyVisual* Node = dynamic_cast<const FHierrarhyVisual*>(Visual);

	if (Node && Visual->Type != MT_LOD)
	{
		for (dxRender_Visual* Child : Node->children)
		{
			AddMainStatics(Child, List);
		}

		return;
	}

	// And every leaf the scene's draw leaves to the graph: a few dozen of a level's thousands, which the sectors
	// were walked whole for, a millisecond of a worker a frame with the frame waiting on it
	if (!Visual->IsForwardBatched || HasSortedTaken(Visual))
	{
		List.push_back(Visual);
	}
}
} // namespace
} // namespace GpuSceneInternal

void GpuScene::Load(const xr_vector<dxRender_Visual*>& Visuals, const xr_vector<IRender_Sector*>& Sectors, xr_vector<GrassModel>& Details)
{
	ForgetIcons();
	nri::nriGetInterface(*GRHI->Nri->Device, NRI_INTERFACE(nri::CoreInterface), &Api);
	nri::nriGetInterface(*GRHI->Nri->Device, NRI_INTERFACE(nri::HelperInterface), &Help);
	nri::nriGetInterface(*GRHI->Nri->Device, NRI_INTERFACE(nri::StreamerInterface), &StreamApi);

	xr_vector<Hlsl::SceneMesh> MeshRecords;
	xr_vector<Hlsl::SceneInstance> InstanceRecords;
	xr_vector<Hlsl::SceneTransform> TransformRecords;
	xr_vector<Hlsl::SceneTreeLight> TreeLightRecords(1);
	xr_hash_map<const dxRender_Visual*, const FLOD*> Groups;
	xr_hash_map<MeshKey, u32> MeshIds;
	xr_vector<Caster> Casters, Candidates;
	MaterialIds.clear();
	BoundTextures.clear();
	SortedStatics.clear();
	SortedStreams.clear();

	ShaderExternalMap WaveDefines;
	WaveDefines["USE_TREEWAVE"] = "1";
	WaveShader = DEV->_CreateVS("shadow_lod", WaveDefines);
	WaveDefines["USE_AREF"] = "1";
	WaveAlphaShader = DEV->_CreateVS("shadow_lod", WaveDefines);

	Hlsl::SceneTransform Identity = {};
	Identity.world = Fidentity;
	Identity.world_old = Fidentity;
	TransformRecords.push_back(Identity);

	const auto FindMesh = [&](const IRender_Mesh& Mesh)
	{
		const MeshKey Key = { Mesh.Alloc.Page, Mesh.LevelMesh != u32(-1) ? Mesh.LevelMesh : Mesh.vBase, Mesh.iBase, Mesh.dwPrimitives * 3 };
		const auto [Found, Added] = MeshIds.try_emplace(Key, u32(MeshRecords.size()));
		if (Added)
		{
			Hlsl::SceneMesh Record = {};
			Record.index_start = Key.IndexStart;
			Record.index_count = Key.IndexCount;
			Record.vertex_base = Key.VertexBase;
			Record.source = DRAW_SOURCE_STATIC | (Key.Page << DRAW_PAGE_SHIFT);
			Record.next_lod = u32(-1);
			Record.page = Key.Page;
			MeshRecords.push_back(Record);
		}

		return Found->second;
	};

	xr_vector<dxRender_Visual*> Stack;
	xr_vector<std::pair<dxRender_Visual*, u32>> Leaves;
	xr_hash_map<const dxRender_Visual*, StaticLeft> LeftWhy;
	xr_hash_map<const dxRender_Visual*, MainLeft> MainWhy;
	// What the main view may draw from the clusters and the batch does not cast: decided by its material's record
	xr_vector<Caster> Others;
	// The LOD cards and their sectors, and their materials; the editors draw theirs themselves
	xr_vector<std::pair<FLOD*, u32>> Cards;
	xr_hash_set<const Shader*> CardShaders;

	// A caster goes to the batch's lists, anything else waits for its material's record
	const auto Add = [&](Caster Item, ECaster Kind, const Shader* Material)
	{
		if (Kind == ECaster::None)
		{
			const ShaderElement* Element = Material ? Material->E[ShaderVariant::Cluster]._get() : nullptr;
			const SPass* Colour = Element && !Element->passes.empty() ? Element->passes.back()._get() : nullptr;
			Item.Views = 0;
			Item.IsAlpha = Colour && Colour->state._get() && Colour->state->state->AlphaRef;
		}

		(Kind == ECaster::Alpha ? Candidates : Kind == ECaster::Opaque ? Casters : Others).push_back(Item);
	};

	// The main view drops a mesh of a group where the group's card stands in, as the graph does
	const auto SetGroup = [](Hlsl::SceneTreeLight& Light, const FLOD& Group) { Light.lod.set(Group.vis.sphere.P.x, Group.vis.sphere.P.y, Group.vis.sphere.P.z, Group.vis.sphere.R * Group.lod_factor); };

	for (IRender_Sector* Item : Sectors)
	{
		CSector* Sector = static_cast<CSector*>(Item);
		Stack.push_back(Sector->root());

		while (!Stack.empty())
		{
			dxRender_Visual* Visual = Stack.back();
			Stack.pop_back();

			if (!Visual)
			{
				continue;
			}

			// A group that shows one of its levels by distance is walked by the graph in every view, as a leftover
			if (Visual->Type == MT_MESH_LODS)
			{
				Leaves.emplace_back(Visual, Sector->index);
				LeftWhy[Visual] = StaticLeft::MeshLods;
				MainWhy[Visual] = MainLeft::MeshLods;
				continue;
			}

			if (FHierrarhyVisual* Node = dynamic_cast<FHierrarhyVisual*>(Visual))
			{
				if (Visual->Type == MT_LOD && Visual->shader._get() && !DevicePtr->IsEditorMode())
				{
					Cards.emplace_back(static_cast<FLOD*>(Visual), Sector->index);
					CardShaders.insert(Visual->shader._get());
					MaterialIds.try_emplace(Visual->shader._get(), u32(MaterialIds.size()));
				}

				// What a group shows by distance, in place of its card
				const auto Outer = Groups.find(Visual);
				const FLOD* Group = Visual->Type == MT_LOD ? static_cast<const FLOD*>(Visual) : Outer != Groups.end() ? Outer->second : nullptr;

				for (const dxRender_Visual* Child : Node->children)
				{
					if (Group && Child)
					{
						Groups[Child] = Group;
					}
				}

				Stack.insert(Stack.end(), Node->children.begin(), Node->children.end());
				continue;
			}

			Leaves.emplace_back(Visual, Sector->index);
			LeftWhy[Visual] = StaticLeft::Kind;
			MainWhy[Visual] = MainLeft::Kind;
			const IRender_Mesh* Mesh = dynamic_cast<const IRender_Mesh*>(Visual);
			if (!Mesh || !Mesh->dwPrimitives)
			{
				continue;
			}

			Shader* Material = Visual->shader._get();
			Hlsl::SceneInstance Record = {};
			Record.bounds.set(Visual->vis.sphere.P.x, Visual->vis.sphere.P.y, Visual->vis.sphere.P.z, Visual->vis.sphere.R);
			Record.mesh = FindMesh(*Mesh);
			Record.mesh_depth = Record.mesh;
			Record.material = MaterialIds.try_emplace(Material, u32(MaterialIds.size())).first->second;
			Record.light = u32(-1);
			Record.sector = Sector->index;

			if (Visual->IsIgnoreOptimize || (Material && Material->E[0] && Material->E[0]->flags.bLandscape))
			{
				Record.flags |= SCENE_INSTANCE_KEEP;
			}

			if (const FTreeVisual* Tree = dynamic_cast<const FTreeVisual*>(Visual))
			{
				Hlsl::SceneTransform Placement = {};
				Placement.world = Tree->GetTransform();
				Placement.world_old = Tree->GetTransform();
				Record.transform = u32(TransformRecords.size());
				Record.flags |= SCENE_INSTANCE_TREE;
				// What deffer_lod makes the hemi of: the bits of two floats
				Record.light = std::bit_cast<u32>(ps_r__Tree_SBC * Tree->GetHemiScale());
				Record.extra = std::bit_cast<u32>(ps_r__Tree_SBC * Tree->GetHemiBias());
				TransformRecords.push_back(Placement);

				Hlsl::SceneTreeLight Light = {};
				Fvector4 Scale, Bias;
				Fvector2 Sun;
				Tree->GetLight(Scale, Bias, Sun);
				Light.scale.set(Scale.x, Scale.y, Scale.z, Scale.w);
				Light.scale.mul(ps_r__Tree_SBC);
				Light.bias.set(Bias.x, Bias.y, Bias.z, Bias.w);
				Light.bias.mul(ps_r__Tree_SBC);
				Light.sun.set(ps_r__Tree_SBC * Sun.x, ps_r__Tree_SBC * Sun.y, 0.0f, 0.0f);

				if (const auto Group = Groups.find(Visual); Group != Groups.end())
				{
					SetGroup(Light, *Group->second);
				}

				TreeLightRecords.push_back(Light);

				CTexture* Base = nullptr;
				const SPass* Pending = nullptr;
				bool IsLate = false;
				const ECaster Kind = Material ? ClassifyCaster(*Visual, true, Base, Pending, LeftWhy[Visual], IsLate) : ECaster::None;
				Record.flags |= IsLate ? SCENE_INSTANCE_TRANSLUCENT : 0u;
				const FTreeVisual_PM* Detailed = dynamic_cast<const FTreeVisual_PM*>(Visual);
				// The shadow sways as the main view sways the tree: the baked light's shadow shader never does, the
				// material of the tree says whether it stands still
				const SPass* Main = MaterialPass(Material);
				const bool IsSwaying = Main && (Main->Features & (MaterialFeature::Placed | MaterialFeature::Still)) == MaterialFeature::Placed;
				const bool IsWaving = IsSwaying || (Kind != ECaster::None && Material->E[ShaderVariant::Shadow]->passes[0]->vs._get() == (Kind == ECaster::Alpha ? WaveAlphaShader : WaveShader)._get());
				Add({ Visual, Mesh, u32(InstanceRecords.size()), Record.transform, &Tree->GetTransform(), IsWaving, Kind == ECaster::Alpha, Base, SCENE_CLUSTER_DEPTH, Detailed ? Detailed->GetWindow() : nullptr, Pending }, Kind, Material);
			}
			else if (dynamic_cast<Fvisual*>(Visual))
			{
				if (Visual->Type == MT_PROGRESSIVE)
				{
					Record.flags |= SCENE_INSTANCE_PROGRESSIVE;
				}

				// A mesh of a group has a slot of the tables for the group alone, its world the identity
				if (const auto Group = Groups.find(Visual); Group != Groups.end())
				{
					Hlsl::SceneTreeLight Light = {};
					SetGroup(Light, *Group->second);
					Record.transform = u32(TransformRecords.size());
					TransformRecords.push_back(Identity);
					TreeLightRecords.push_back(Light);
				}

				// A progressive mesh casts the clusters the main view's detail picks, as every other mesh does
				CTexture* Base = nullptr;
				const SPass* Pending = nullptr;
				bool IsLate = false;
				const ECaster Kind = Material ? ClassifyCaster(*Visual, false, Base, Pending, LeftWhy[Visual], IsLate) : ECaster::None;
				Record.flags |= IsLate ? SCENE_INSTANCE_TRANSLUCENT : 0u;
				const FProgressive* Progressive = dynamic_cast<const FProgressive*>(Visual);
				Add({ Visual, Mesh, u32(InstanceRecords.size()), Record.transform, nullptr, false, Kind == ECaster::Alpha, Base, SCENE_CLUSTER_DEPTH, Progressive ? Progressive->GetWindow() : nullptr, Pending }, Kind, Material);
			}

			InstanceRecords.push_back(Record);
		}
	}

	// A shadow pass that is not the batch's own is drawn by it where its state draws as the batch's: known now
	for (xr_vector<Caster>* List : { &Casters, &Candidates })
	{
		std::erase_if(*List, [&](const Caster& Item)
		{
			if (!Item.Pending || (DepthState && IsStaticState(*Item.Pending)))
			{
				return false;
			}

			LeftWhy[Item.Visual] = StaticLeft::State;
			Others.push_back(Item);
			Others.back().Views = 0;
			return true;
		});
	}

	// A detail model is an instance of its own: what its clusters name for the material
	xr_vector<u32> DetailInstances(Details.size(), u32(-1));

	for (u32 Index = 0; Index < Details.size(); ++Index)
	{
		if (!Details[Index].Material)
		{
			continue;
		}

		Hlsl::SceneInstance Record = {};
		Record.material = MaterialIds.try_emplace(Details[Index].Material, u32(MaterialIds.size())).first->second;
		Record.light = u32(-1);
		Record.sector = u32(-1);
		DetailInstances[Index] = u32(InstanceRecords.size());
		InstanceRecords.push_back(Record);
	}

	// The shaders of models there are now: a material each, with its textures in the buckets and a draw of its own
	// (BuildBins). The forward frame takes their meshes from the graph; the shader of a model made later joins when
	// the frame first meets it (JoinModels)
	ModelBinIds.clear();
	ModelShaders.clear();

	DEV->GetShaders(ModelShaders);
	std::erase_if(ModelShaders, [](const ref_shader& Item) { return !IsModelShader(*Item); });

	// What the level's and the models' materials mirror is one cube array; a model made later that mirrors a cube it
	// does not hold stays in the graph
	xr_vector<CTexture*> Cubes;

	for (const auto& [Material, Id] : MaterialIds)
	{
		Cubes.push_back(Material ? MirroredCube(*Material) : nullptr);
	}

	for (const ref_shader& Item : ModelShaders)
	{
		Cubes.push_back(MirroredCube(*Item));
	}

	std::sort(Cubes.begin(), Cubes.end());
	Cubes.erase(std::unique(Cubes.begin(), Cubes.end()), Cubes.end());
	std::erase(Cubes, nullptr);
	BuildEnv(Cubes);
	std::erase_if(ModelShaders, [](const ref_shader& Item) { return EnvOf(*Item) == NoEnv; });

	for (const ref_shader& Item : ModelShaders)
	{
		ModelBinIds[Item._get()] = { SCENE_MATERIAL_NO_PIPELINE, MaterialIds.try_emplace(Item._get(), u32(MaterialIds.size())).first->second };
	}

	// The tails of everything the main view's shaders read; the heads are only measured. The
	// alpha-tested bases keep their heads, for the alpha array.
	const u32 BaseStage = CTexture::rstPixel + ShaderBindings::Texture("s_base");
	xr_vector<CTexture*> Sources;
	xr_vector<u8> WantsHead;
	xr_hash_map<CTexture*, u32> SourceIds;
	const auto Register = [&](CTexture* Texture, bool IsWhole)
	{
		const auto [Found, IsNew] = SourceIds.try_emplace(Texture, u32(Sources.size()));
		if (IsNew)
		{
			Sources.push_back(Texture);
			WantsHead.push_back(0);
		}

		WantsHead[Found->second] |= u8(IsWhole);
		return Found->second;
	};

	for (const Caster& Item : Candidates)
	{
		Register(Item.Base, true);
	}

	xr_vector<CTexture*> DetailBases(Details.size(), nullptr);

	for (u32 Index = 0; Index < Details.size(); ++Index)
	{
		const Shader* Material = Details[Index].Material;
		const ShaderElement* Element = Material ? Material->E[ShaderVariant::NormalHQ]._get() : nullptr;
		if (!Element || Element->passes.empty() || !Element->passes[0] || !Element->passes[0]->T)
		{
			continue;
		}

		for (const auto& [Stage, Texture] : *Element->passes[0]->T)
		{
			if (Stage == BaseStage && Texture._get())
			{
				DetailBases[Index] = Texture._get();
				Register(Texture._get(), true);
			}
		}
	}

	xr_vector<Hlsl::SceneMaterial> MaterialRecords(MaterialIds.size(), EmptyMaterial());
	xr_hash_map<u32, const char*> IncompleteWhy; // of a material that is not complete: why its first missing map has no place
	xr_hash_map<u32, std::pair<const char*, const char*>> IncompleteMap; // and that map's name and texture
	xr_hash_map<CTexture*, u32> Probed; // DXGI format, 0 when the file cannot go into buckets
	xr_vector<xr_array<u32, SCENE_MATERIAL_MAPS>> MapSources(MaterialIds.size());
	u64 TailBytes = 0;
	u64 HeadBytes = 0;
	u32 Placeholders = 0;
	u32 Reported = 0;
	const auto SetMap = [](Hlsl::SceneMaterial& Record, u32 Map, const GpuTextures::Ref& Item)
	{
		u32* Words = &Record.base + Map * 4;
		Words[0] = Item.Tail;
		Words[1] = Item.Head;
		Words[2] = Item.Info;
	};
	const auto Tally = [](xr_vector<std::pair<const char*, u32>>& Kinds, const char* Why)
	{
		const auto Kind = std::find_if(Kinds.begin(), Kinds.end(), [Why](const std::pair<const char*, u32>& Seen) { return !xr_strcmp(Seen.first, Why); });
		++(Kind != Kinds.end() ? Kind->second : Kinds.emplace_back(Why, 0u).second);
	};
	const auto Join = [](const xr_vector<std::pair<const char*, u32>>& Kinds, string512& Line)
	{
		Line[0] = 0;

		for (const auto& [Why, Count] : Kinds)
		{
			string128 Part;
			xr_sprintf(Part, "%s%s %u", Line[0] ? ", " : "", Why, Count);
			xr_strcat(Line, Part);
		}
	};
	// The forward frame draws a material whose detail bump has no place in the buckets without it, not outside the batch
	constexpr u32 DetailBumpMap = 4;
	u32 DetailBumps = 0;
	u32 DetailBumpsLeft = 0;

	// The buckets go in the order of the records: the level's, the grass's, the models', the cards' last. The map is keyed
	// by address, which differs between runs: by its order the textures that find no bucket would too
	xr_vector<std::pair<Shader*, u32>> MaterialOrder(MaterialIds.begin(), MaterialIds.end());
	std::sort(MaterialOrder.begin(), MaterialOrder.end(), [&](const std::pair<Shader*, u32>& Left, const std::pair<Shader*, u32>& Right)
	{
		const bool IsLeftCard = CardShaders.contains(Left.first);
		const bool IsRightCard = CardShaders.contains(Right.first);
		return IsLeftCard != IsRightCard ? IsRightCard : Left.second < Right.second;
	});

	for (const auto& [Material, Id] : MaterialOrder)
	{
		MapSources[Id].fill(u32(-1));
		const SPass* Pass = MaterialPass(Material);
		if (!Pass)
		{
			continue;
		}

		Hlsl::SceneMaterial& Record = MaterialRecords[Id];
		CTexture* Maps[SCENE_MATERIAL_MAPS];
		ReadMaterial(*Pass, Record, Maps, CardShaders.contains(Material));

		// An XML blend's own surface (r1_) takes its base alone from the buckets: the rest of its list is bound as it is,
		// by registers the level's maps have (DrawForward)
		if (Pass->vs && IsNamed(Pass->vs->cName, "r1_"))
		{
			std::fill(std::begin(Maps) + 1, std::end(Maps), nullptr);
		}

		bool IsComplete = Maps[0] != nullptr;
		bool IsDetailBumpLeft = false;
		const char* MissingMap = "s_base";
		const char* MissingName = "not bound";
		const char* MissingWhy = "";

		for (u32 Map = 0; Map < SCENE_MATERIAL_MAPS; ++Map)
		{
			CTexture* Texture = Maps[Map];
			if (!Texture)
			{
				continue;
			}

			const auto [Found, IsNew] = Probed.try_emplace(Texture, 0u);
			u32 Width = 0;
			u32 Height = 0;
			const char* Why = "";

			if (IsNew && GpuTextures::Probe(*Texture, Found->second, Width, Height, Why))
			{
				// With mips: a third more than the top level, at 4 or 8 bits per texel
				const u64 Bits = Found->second == 71 || Found->second == 72 ? 4 : 8;
				const u64 Larger = std::max(Width, Height);
				const u64 Whole = u64(Width) * Height * Bits / 8 * 4 / 3;
				const u64 Tail = Larger > 512 ? Whole * 512 * 512 / (Larger * Larger) : Whole;
				TailBytes += Tail;
				HeadBytes += Whole - Tail;
				Placeholders += Why[0] != 0;
			}

			Record.flags |= Map == 0 && (Found->second == 71 || Found->second == 72) ? SCENE_MATERIAL_BASE_BC1 : 0u;

			if (Found->second != 0)
			{
				MapSources[Id][Map] = Register(Texture, false);
				continue;
			}

			if ((Map == DetailBumpMap || Map == DetailBumpMap + 1))
			{
				IsDetailBumpLeft = true;
				continue;
			}

			// A file the buckets cannot take is bound as it is
			const GpuTextures::Ref Bound = BindTexture(Texture, Why);
			TextureRefs[Texture] = Bound;
			SetMap(Record, Map, Bound);

			if (Bound.Tail != GpuTextures::Absent)
			{
				continue;
			}

			IsComplete = false;
			MissingMap = MapNames[Map];
			MissingName = *Texture->cName;
			MissingWhy = Why;
		}

		DetailBumps += (Record.flags & SCENE_MATERIAL_DETAIL_BUMP) != 0;

		if (IsDetailBumpLeft)
		{
			Record.flags &= ~SCENE_MATERIAL_DETAIL_BUMP;
			MapSources[Id][DetailBumpMap] = MapSources[Id][DetailBumpMap + 1] = u32(-1);
			++DetailBumpsLeft;
		}

		Record.flags |= IsComplete ? SCENE_MATERIAL_COMPLETE : 0u;
		IncompleteWhy[Id] = IsComplete ? "" : !Maps[0] ? "no base map" : MissingWhy;
		IncompleteMap[Id] = { MissingMap, MissingName };

		if (!IsComplete && Reported++ < 16)
		{
			Msg("* [scene] material %u is not complete: ps %s, %s '%s' (%s)", Id, Pass->ps ? *Pass->ps->cName : "none", MissingMap, MissingName, MissingWhy);
		}
	}

	xr_vector<GpuTextures::Ref> Refs;
	Textures.Build(Sources, WantsHead, Refs);

	for (size_t Index = 0; Index < Sources.size(); ++Index)
	{
		TextureRefs[Sources[Index]] = Refs[Index];
	}

	for (u32 Id = 0; Id < MaterialRecords.size(); ++Id)
	{
		for (u32 Map = 0; Map < SCENE_MATERIAL_MAPS; ++Map)
		{
			if (MapSources[Id][Map] != u32(-1))
			{
				SetMap(MaterialRecords[Id], Map, Refs[MapSources[Id][Map]]);
			}
		}

		Hlsl::SceneMaterial& Record = MaterialRecords[Id];

		if ((Record.flags & SCENE_MATERIAL_DETAIL_BUMP) && (Record.detail_bump == GpuTextures::Absent || Record.detail_bump_x == GpuTextures::Absent))
		{
			Record.flags &= ~SCENE_MATERIAL_DETAIL_BUMP;
			++DetailBumpsLeft;
		}
	}

	Msg("* [scene] detail bump: %u materials have one, %u of them are drawn without it (no place in the buckets)", DetailBumps, DetailBumpsLeft);

	// A texture that found no place in the buckets is bound as it is, the cards' last; a detail bump is left out as above.
	// A material with a texture that has neither stays in the graph
	for (const auto& [Material, Id] : MaterialOrder)
	{
		Hlsl::SceneMaterial& Record = MaterialRecords[Id];

		for (u32 Map = 0; Map < SCENE_MATERIAL_MAPS && (Record.flags & SCENE_MATERIAL_COMPLETE); ++Map)
		{
			const u32 Source = MapSources[Id][Map];

			if (Source == u32(-1) || Refs[Source].Tail != GpuTextures::Absent || Map == DetailBumpMap || Map == DetailBumpMap + 1)
			{
				continue;
			}

			const char* Why = "";
			const GpuTextures::Ref Bound = BindTexture(Sources[Source], Why);
			TextureRefs[Sources[Source]] = Bound;
			SetMap(Record, Map, Bound);

			if (Bound.Tail == GpuTextures::Absent)
			{
				Record.flags &= ~SCENE_MATERIAL_COMPLETE;
				IncompleteWhy[Id] = Why;
				IncompleteMap[Id] = { MapNames[Map], *Sources[Source]->cName };
			}
		}
	}

	string512 Unbucketed = {};
	u32 UnbucketedCount = 0;

	for (size_t Index = 0; Index < Sources.size(); ++Index)
	{
		if (Refs[Index].Tail == GpuTextures::Absent && UnbucketedCount++ < 8)
		{
			string128 Part;
			xr_sprintf(Part, "%s%s", Unbucketed[0] ? ", " : "", *Sources[Index]->cName);
			xr_strcat(Unbucketed, Part);
		}
	}

	Msg("* [scene] textures without a place in the buckets: %u (%s%s)", UnbucketedCount, Unbucketed, UnbucketedCount > 8 ? ", ..." : "");

	string512 BoundNames = {};

	for (size_t Index = 0; Index < std::min<size_t>(BoundTextures.size(), 8); ++Index)
	{
		string128 Part;
		xr_sprintf(Part, "%s%s", Index ? ", " : "", BoundTextures[Index]->cName.c_str());
		xr_strcat(BoundNames, Part);
	}

	Msg("* [scene] textures bound as they are: %u of %u registers (%s%s); missing files read as the placeholder: %u", u32(BoundTextures.size()), SCENE_TEXTURE_BOUND, BoundNames, BoundTextures.size() > 8 ? ", ..." : "", Placeholders);

	xr_vector<GpuTextures::Ref> AlphaRefs;
	xr_hash_map<CTexture*, u32>& AlphaIds = AlphaSlices;
	AlphaIds.clear();

	for (const Caster& Item : Candidates)
	{
		const GpuTextures::Ref& Base = Refs[SourceIds[Item.Base]];
		if (Base.Tail != GpuTextures::Absent && AlphaIds.try_emplace(Item.Base, u32(AlphaRefs.size())).second)
		{
			AlphaRefs.push_back(Base);
		}
	}

	for (CTexture* Base : DetailBases)
	{
		if (Base && Refs[SourceIds[Base]].Tail != GpuTextures::Absent && AlphaIds.try_emplace(Base, u32(AlphaRefs.size())).second)
		{
			AlphaRefs.push_back(Refs[SourceIds[Base]]);
		}
	}

	// A cut-out texture the buckets have no place for gets a slice made from its whole copy, while the array has room: the
	// level's casters', then those of the model shaders' shadows, which the moving casters are tested by
	xr_vector<CTexture*> CutOuts;

	for (const Caster& Item : Candidates)
	{
		CutOuts.push_back(Item.Base);
	}

	for (const ref_shader& Item : ModelShaders)
	{
		const ShaderElement* Shadow = Item->E[ShaderVariant::Shadow]._get();
		const SPass* Pass = Shadow && Shadow->passes.size() == 1 ? Shadow->passes[0]._get() : nullptr;

		if (!Pass || !Pass->ps || !Pass->T)
		{
			continue;
		}

		Pass->ps->CompileState.WaitCompiled();

		for (const auto& [Stage, Texture] : *Pass->T)
		{
			const auto Source = SourceIds.find(Texture._get());

			if (Stage == BaseStage && Texture._get() && Pass->ps->Texture2DSlots == (1ull << (BaseStage - CTexture::rstPixel)) && (Source == SourceIds.end() || Refs[Source->second].Tail == GpuTextures::Absent))
			{
				CutOuts.push_back(Texture._get());
			}
		}
	}

	xr_vector<IRHISurface*> AlphaWholes;
	xr_hash_set<CTexture*> Unplaced;
	xr_vector<std::pair<const char*, u32>> UnplacedWhy;

	for (CTexture* Base : CutOuts)
	{
		if (!Textures.Count() || AlphaIds.contains(Base) || !Unplaced.insert(Base).second)
		{
			continue;
		}

		u32 Format = 0;
		u32 Width = 0;
		u32 Height = 0;
		const char* Why = "";
		Tally(UnplacedWhy, GpuTextures::Probe(*Base, Format, Width, Height, Why) ? "a full bucket" : Why);

		if (AlphaRefs.size() + AlphaWholes.size() >= AlphaLayers)
		{
			continue;
		}

		Base->EnsureLoaded();
		IRHISurface* Surface = Base->surface_get();

		if (Surface && Surface->GetTextureType() == ERHI_RESOURCE_DIMENSION::TEXTURE2D && Surface->GetArraySize() == 1 && Surface->GetSampleDescCount() <= 1 &&
			nri::nriConvertDXGIFormatToNRI(u32(Surface->GetFormat())) != nri::Format::UNKNOWN)
		{
			AlphaIds.emplace(Base, u32(AlphaRefs.size() + AlphaWholes.size()));
			AlphaWholes.push_back(Surface);
		}
		else if (Surface)
		{
			Surface->Release();
		}
	}

	string512 Reasons;
	Join(UnplacedWhy, Reasons);
	Msg("* [scene] cut-out textures without a place in the buckets: %u (%s); %u of them have an alpha slice from their whole copy", u32(Unplaced.size()), Reasons, u32(AlphaWholes.size()));
	BuildAlpha(AlphaRefs, AlphaWholes);
	const bool HasModelAlpha = std::any_of(CutOuts.begin() + Candidates.size(), CutOuts.end(), [&AlphaIds](CTexture* Base) { return AlphaIds.contains(Base); });

	for (IRHISurface* Surface : AlphaWholes)
	{
		Surface->Release();
	}

	// Complete: every map in the buckets or bound as it is
	u32 Complete = 0;
	u32 WithBound = 0;
	xr_vector<std::pair<const char*, u32>> IncompleteKinds;

	for (u32 Id = 0; Id < MaterialRecords.size(); ++Id)
	{
		const Hlsl::SceneMaterial& Record = MaterialRecords[Id];
		const auto Why = IncompleteWhy.find(Id);
		bool IsBound = false;

		for (u32 Map = 0; Map < SCENE_MATERIAL_MAPS; ++Map)
		{
			const u32 Tail = (&Record.base)[Map * 4];
			IsBound |= Tail != GpuTextures::Absent && (Tail >> 16) >= SCENE_TEXTURE_BUCKETS;
		}

		if (!(Record.flags & SCENE_MATERIAL_COMPLETE))
		{
			Tally(IncompleteKinds, Why != IncompleteWhy.end() ? Why->second : "no pass that reads maps");
		}

		Complete += (Record.flags & SCENE_MATERIAL_COMPLETE) != 0;
		WithBound += (Record.flags & SCENE_MATERIAL_COMPLETE) && IsBound;
	}

	Join(IncompleteKinds, Reasons);
	Msg("* [scene] materials: %u of %u complete, %u of them with a map bound as it is; not complete by reason: %s; %u textures, tails %llu MB, heads %llu MB", Complete, u32(MaterialRecords.size()), WithBound, Reasons, u32(Probed.size()),
		TailBytes >> 20, HeadBytes >> 20);

	// The forward frame draws the level from the buckets: a texture that has a place there has no whole copy, until
	// a draw of the old path binds it. The level loads with these not loaded yet; one loaded before it is let go
	u32 Dropped = 0;

	for (size_t Index = 0; Index < Sources.size(); ++Index)
	{
		if (Refs[Index].Tail == GpuTextures::Absent)
		{
			continue;
		}

		// One a worker is still loading counts: left alone, it would finish as a whole copy
		if (Sources[Index]->pSurface || Sources[Index]->IsLoadPending())
		{
			Sources[Index]->Unload();
			++Dropped;
		}

		DroppedTextures.emplace_back(Sources[Index]);
	}

	std::sort(DroppedTextures.begin(), DroppedTextures.end(), [](const ref_texture& Left, const ref_texture& Right) { return std::less<const CTexture*>()(Left._get(), Right._get()); });
	Msg("* [scene] textures without a whole copy: %u, %u of them were loaded and let go", u32(DroppedTextures.size()), Dropped);

	const size_t OpaqueCount = Casters.size();

	for (const Caster& Item : Candidates)
	{
		const auto Slice = AlphaIds.find(Item.Base);
		if (Slice == AlphaIds.end())
		{
			LeftWhy[Item.Visual] = StaticLeft::Alpha;
			Others.push_back(Item);
			Others.back().Views = 0;
			continue;
		}

		Hlsl::SceneMaterial& Material = MaterialRecords[InstanceRecords[Item.Instance].material];
		Material.alpha = Slice->second;
		Casters.push_back(Item);
	}

	const size_t CasterCount = Casters.size();
	HasAlpha = CasterCount != OpaqueCount || HasModelAlpha;
	Casters.insert(Casters.end(), Others.begin(), Others.end());

	// The main view draws a visual from the clusters where its material has a draw of the scene, whatever its shadow
	for (Caster& Item : Casters)
	{
		const MainLeft Why = LevelBinWhy(Item.Visual->shader._get(), MaterialRecords[InstanceRecords[Item.Instance].material]);
		MainWhy[Item.Visual] = Why;
		Item.Views |= Why == MainLeft::Batched ? SCENE_CLUSTER_MAIN : 0u;
		Item.IsSorted = !(Item.Views & SCENE_CLUSTER_MAIN) && KeepSorted(*Item.Visual, MaterialRecords[InstanceRecords[Item.Instance].material]) == SortedLeft::Taken;
	}

	std::erase_if(Casters, [](const Caster& Item) { return !Item.Views && !Item.IsSorted; });

	xr_vector<Hlsl::SceneCluster> ClusterRecords;
	xr_vector<u32> ClusterCorners;
	CTimer Timer;
	Timer.Start();
	xr_vector<Hlsl::SceneClusterLod> LodRecords;
	const u32 Split = BuildClusters(Casters, InstanceRecords, ClusterRecords, LodRecords, ClusterCorners);
	const float BuildSeconds = Timer.GetElapsed_sec();

	// A sorted visual whose mesh made no clusters stays in the graph. No cull asks for the heads of the textures of those
	// kept: they are asked for always
	u32 SortedCounts[u32(SortedLeft::Count)] = {};
	u32 SortedClusters = 0;
	xr_vector<u32> SortedMissing;

	// The first few textures that kept materials of these ids out of the buckets: map, texture, why
	const auto NameMissing = [&](const xr_vector<u32>& Ids, string512& Line)
	{
		const char* Named[3] = {};
		u32 Count = 0;
		Line[0] = 0;

		for (const u32 Id : Ids)
		{
			const auto [Map, Name] = IncompleteMap[Id];
			if (Count == 3 || std::find(Named, Named + Count, Name) != Named + Count)
			{
				continue;
			}

			Named[Count++] = Name;
			const char* Why = IncompleteWhy[Id];
			string256 Part;
			xr_sprintf(Part, "%s%s '%.64s' (%.48s)", Count > 1 ? ", " : " (", Map ? Map : "no pass", Name ? Name : "none", Why ? Why : "no pass");
			xr_strcat(Line, Part);
		}

		xr_strcat(Line, Count ? ")" : "");
	};

	for (auto& [Visual, Item] : SortedStatics)
	{
		Item.Why = Item.Why == SortedLeft::Taken && !Item.Count ? SortedLeft::Mesh : Item.Why;
		++SortedCounts[u32(Item.Why)];
		SortedClusters += Item.Count;

		if (Item.Why == SortedLeft::Incomplete)
		{
			SortedMissing.push_back(MaterialIds[Visual->shader._get()]);
		}

		const Hlsl::SceneMaterial& Record = MaterialRecords[MaterialIds[Visual->shader._get()]];

		for (u32 Map = 0; Map < SCENE_MATERIAL_PLAIN_MAPS && Item.Why == SortedLeft::Taken; ++Map)
		{
			if (const u32 Stream = (&Record.base)[Map * 4 + 2] >> SCENE_TEXTURE_STREAM_SHIFT)
			{
				SortedStreams.push_back(Stream - 1);
			}
		}
	}

	std::sort(SortedStreams.begin(), SortedStreams.end());
	SortedStreams.erase(std::unique(SortedStreams.begin(), SortedStreams.end()), SortedStreams.end());
	string512 Missing;
	NameMissing(SortedMissing, Missing);
	Msg("* [scene] sorted level visuals: %u of %u drawn by the sorted list from %u clusters; left to the graph: no sorted cluster pass %u, maps neither in the buckets nor bound %u%s, an environment cube outside the array %u, no clusters of its mesh %u",
		SortedCounts[u32(SortedLeft::Taken)], u32(SortedStatics.size()), SortedClusters, SortedCounts[u32(SortedLeft::Pass)], SortedCounts[u32(SortedLeft::Incomplete)], Missing, SortedCounts[u32(SortedLeft::Env)], SortedCounts[u32(SortedLeft::Mesh)]);

	// The grass of both frames is drawn from the scene's cull: into the forward frame's colour, or into the G-buffer
	for (const GrassModel& Model : Details)
	{
		const ShaderElement* Element = Model.Material ? Model.Material->E[ShaderVariant::Cluster]._get() : nullptr;
		if (Element && !Element->passes.empty() && Element->passes.back())
		{
			GrassPass.Colour = Element->passes.back();
			GrassPass.Depth = Element->passes.size() > 1 ? Element->passes[0] : ref_pass();
			GrassPass.Source._set(Model.Material);
			break;
		}
	}

	BuildBins(InstanceRecords, ClusterRecords, MaterialRecords);

	u32 DetailCount = 0;
	xr_vector<u16> GrassIndexData;

	// A detail model's clusters are runs of its index data: the far mesh, then the whole mesh. They carry no
	// view flag, so the scene's own cull passes them by: the grass cull names them
	for (u32 Index = 0; Index < Details.size(); ++Index)
	{
		GrassModel& Model = Details[Index];
		// Its colour is drawn from the buckets: a slice of a whole copy does not do
		const auto Found = DetailBases[Index] ? AlphaIds.find(DetailBases[Index]) : AlphaIds.end();
		const auto Slice = Found != AlphaIds.end() && Found->second < AlphaRefs.size() ? Found : AlphaIds.end();
		const u32 Runs = (Model.FarCount / 3 + SCENE_CLUSTER_TRIANGLES - 1) / SCENE_CLUSTER_TRIANGLES + (Model.IndexCount / 3 + SCENE_CLUSTER_TRIANGLES - 1) / SCENE_CLUSTER_TRIANGLES;
		if (Slice == AlphaIds.end() || Runs > SCENE_GRASS_RUNS || ClusterRecords.size() + Runs > SCENE_ID_GRASS || Model.Page >= PageSlots)
		{
			Msg("! [scene] grass model %u is not drawn: %s", Index, Slice == AlphaIds.end() ? "its texture has no alpha slice" : Runs > SCENE_GRASS_RUNS ? "too many triangles" : "no room in the scene");
			continue;
		}

		MaterialRecords[InstanceRecords[DetailInstances[Index]].material].alpha = Slice->second;
		Model.Cluster = u32(ClusterRecords.size());
		GrassDraws[Model.Cluster] = { u32(GrassIndexData.size()), (Model.Page << 24) | Model.VertexBase, InstanceRecords[DetailInstances[Index]].material };

		for (u32 Map = 0; Map < SCENE_MATERIAL_PLAIN_MAPS; ++Map)
		{
			if (const u32 Stream = (&MaterialRecords[InstanceRecords[DetailInstances[Index]].material].base)[Map * 4 + 2] >> SCENE_TEXTURE_STREAM_SHIFT)
			{
				GrassStreams.push_back(Stream - 1);
			}
		}

		GrassIndexData.insert(GrassIndexData.end(), Model.Indices, Model.Indices + Model.IndexCount);
		const u32 Corners = u32(ClusterCorners.size());
		ClusterCorners.insert(ClusterCorners.end(), Model.Indices, Model.Indices + Model.IndexCount);
		++DetailCount;

		for (const u32 Count : { Model.FarCount, Model.IndexCount })
		{
			for (u32 Start = 0; Start < Count; Start += SCENE_CLUSTER_TRIANGLES * 3)
			{
				Hlsl::SceneCluster Record = {};
				Record.instance = DetailInstances[Index];
				Record.index_start = Corners + Start;
				Record.triangles = std::min(Count - Start, SCENE_CLUSTER_TRIANGLES * 3) / 3 | SCENE_CLUSTER_ALPHA;
				Record.vertices = (Model.Page << 24) | Model.VertexBase;
				ClusterRecords.push_back(Record);
			}
		}
	}

	// One draw takes all the grass: it reads the textures bound as they are when a model's material has one
	GrassPass.IsBound = std::any_of(GrassDraws.begin(), GrassDraws.end(), [&](const auto& Item) { return HasBoundMap(MaterialRecords[Item.second.Material]); });

	if (GrassPass.IsBound)
	{
		GrassPass.Colour = BoundPass(GrassPass.Colour._get());
		GrassPass.Depth = BoundPass(GrassPass.Depth._get());
	}

	if (!GrassIndexData.empty())
	{
		GrassIndices = CreateBuffer(GrassIndexData.size() * sizeof(u16), 0, nri::BufferUsageBits::INDEX, "Scene grass indices");
		nri::BufferUploadDesc Upload = {};
		Upload.data = GrassIndexData.data();
		Upload.buffer = GrassIndices;
// The engine's own INDEX_BUFFER is a macro
#pragma push_macro("INDEX_BUFFER")
#undef INDEX_BUFFER
		Upload.after.access = nri::AccessBits::INDEX_BUFFER;
#pragma pop_macro("INDEX_BUFFER")
		Help.UploadData(*GRHI->Nri->Queue, nullptr, 0, &Upload, 1);
	}

	Meshes = CreateTable(MeshRecords, "Scene meshes");
	// A bin holds materials with and without a height to march: the class of an id is told by the instance
	for (Hlsl::SceneInstance& Record : InstanceRecords)
	{
		Record.flags |= Record.material < MaterialRecords.size() && (MaterialRecords[Record.material].flags & SCENE_MATERIAL_STEEP) ? SCENE_INSTANCE_STEEP : 0u;
	}

	Instances = CreateTable(InstanceRecords, "Scene instances");
	Transforms = CreateTable(TransformRecords, "Scene transforms");
	TreeLights = CreateTable(TreeLightRecords, "Scene tree lights");

	// A cluster that is no part of a hierarchy is the one detail of its mesh
	Hlsl::SceneClusterLod Whole = {};
	Whole.errors.set(0.0f, FLT_MAX, 0.0f, 0.0f);
	LodRecords.resize(ClusterRecords.size(), Whole);

	if (!LodRecords.empty())
	{
		ClusterLods = CreateTable(LodRecords, "Scene cluster detail");
		ClusterLodView = MakeView(ClusterLods, nri::BufferView::STRUCTURED_BUFFER);
	}

	TreeLightView = MakeView(TreeLights, nri::BufferView::STRUCTURED_BUFFER);
	LevelMaterials = MaterialRecords;
	MaterialRecords.resize(MaterialRecords.size() + ModelMaterials, EmptyMaterial());
	MaterialCapacity = u32(MaterialRecords.size());
	Materials = CreateTable(MaterialRecords, "Scene materials");
	SectorCount = u32(Sectors.size());

	// A card the scene draws: its maps have a place in the buckets, its passes are those of the first card material
	xr_vector<Hlsl::SceneCard> CardRecords;
	xr_vector<FLOD*> CardGroups;
	u32 CardsLeft[3] = {}; // other passes, maps without a place, outside the heap's pages
	xr_vector<std::pair<const char*, u32>> CardMapsWhy;
	CardSource._set(Cards.empty() ? nullptr : Cards[0].first->shader._get());
	CardsBound = false;
	MakeCardPasses();

	for (const auto& [Group, Sector] : Cards)
	{
		Shader* Material = Group->shader._get();
		const auto IsSame = [&](u32 Variant)
		{
			const ShaderElement* Own = Material->E[Variant]._get();
			const ShaderElement* Ref = CardSource->E[Variant]._get();
			return Own && Ref && Own->passes.size() == 1 && Ref->passes.size() == 1 && Own->passes[0]->vs._get() == Ref->passes[0]->vs._get() && Own->passes[0]->ps._get() == Ref->passes[0]->ps._get() &&
				Own->passes[0]->state._get() == Ref->passes[0]->state._get();
		};
		const u32 Id = MaterialIds[Material];
		const u32 Why = !CardPasses[0] || !IsSame(ShaderVariant::ModelLighting) || !IsSame(ShaderVariant::NormalLQ) ? 0 : !(MaterialRecords[Id].flags & SCENE_MATERIAL_COMPLETE) ? 1 : Group->Facets.Page >= PageSlots ? 2 : 3;

		if (Why == 1)
		{
			const char* Map = IncompleteWhy[Id];
			const auto Kind = std::find_if(CardMapsWhy.begin(), CardMapsWhy.end(), [Map](const std::pair<const char*, u32>& Seen) { return !xr_strcmp(Seen.first, Map); });
			++(Kind != CardMapsWhy.end() ? Kind->second : CardMapsWhy.emplace_back(Map, 0u).second);
		}

		if (Why < 3)
		{
			++CardsLeft[Why];
			continue;
		}

		Hlsl::SceneCard Card = {};
		Card.sphere.set(Group->vis.sphere.P.x, Group->vis.sphere.P.y, Group->vis.sphere.P.z, Group->vis.sphere.R);

		for (u32 Face = 0; Face < 8; ++Face)
		{
			Card.facets[Face].set(Group->facets[Face].N.x, Group->facets[Face].N.y, Group->facets[Face].N.z, 0.0f);
		}

		Card.vertices = (Group->Facets.Page << 24) | Group->Facets.VertexBase;
		Card.material = Id;
		Card.sector = Sector;
		Card.lod_factor = Group->lod_factor;
		CardRecords.push_back(Card);
		CardGroups.push_back(Group);
	}

	CardCount = ClusterRecords.empty() ? 0 : u32(CardRecords.size());

	// One draw takes all the cards: it reads the textures bound as they are when a card's material has one
	if (std::any_of(CardRecords.begin(), CardRecords.end(), [&](const Hlsl::SceneCard& Card) { return HasBoundMap(MaterialRecords[Card.material]); }))
	{
		CardsBound = true;
		MakeCardPasses();
	}

	CreatePasses(ClusterRecords, LodRecords, ClusterCorners);

	if (CardCount)
	{
		CreateCards(CardRecords);

		for (FLOD* Group : CardGroups)
		{
			Group->IsForwardBatched = true;
		}
	}

	string256 CardReasons = {};

	for (const auto& [Why, Count] : CardMapsWhy)
	{
		string64 Part;
		xr_sprintf(Part, "%s%s %u", CardReasons[0] ? ", " : "", Why, Count);
		xr_strcat(CardReasons, Part);
	}

	Msg("* [scene] LOD cards: %u of %u drawn by the scene; left to the graph: other passes %u, maps neither in the buckets nor bound %u (%s), outside the heap's pages %u; the cards' and the grass's draws read the textures bound as they are: %s, %s",
		CardCount, u32(Cards.size()), CardsLeft[0], CardsLeft[1], CardReasons, CardsLeft[2], CardsBound ? "yes" : "no", GrassPass.IsBound ? "yes" : "no");

	u32 LeftCount = 0;
	u32 LeftCounts[u32(StaticLeft::Count)] = {};

	// A visual that was to be batched and is not had a mesh the clusters could not be made of
	if (ClusterCount)
	{
		Leftover[2].resize(SectorCount);

		for (const auto& [Visual, Sector] : Leaves)
		{
			if (!Visual->IsDepthBatched && Sector < SectorCount)
			{
				const StaticLeft Why = LeftWhy[Visual];
				++LeftCounts[u32(Why == StaticLeft::Batched ? StaticLeft::Mesh : Why)];
				Leftover[2][Sector].push_back(Visual);
				++LeftCount;
			}
		}
	}

	const u32 FirstCount = SplitLeftovers(Leftover);
	u32 MainCounts[u32(MainLeft::Count)] = {};
	xr_hash_map<const char*, u32> NoPassShaders; // by the vertex shader of the material's own pass
	MainLeftovers = 0;

	// A sorted visual is counted by the sorted list's verdict: what it draws from the clusters is not drawn by the graph
	static const MainLeft SortedWhy[u32(SortedLeft::Count)] = { MainLeft::Batched, MainLeft::Pass, MainLeft::Pass, MainLeft::Pass, MainLeft::Incomplete, MainLeft::Env, MainLeft::Mesh, MainLeft::Pass,
		MainLeft::Pass };
	xr_vector<u32> MainMissing;

	for (const auto& [Visual, Sector] : Leaves)
	{
		const auto Sorted = SortedStatics.find(Visual);
		const MainLeft Why = Visual->IsForwardBatched ? MainLeft::Batched : Sorted != SortedStatics.end() ? SortedWhy[u32(Sorted->second.Why)]
			: MainWhy[Visual] == MainLeft::Batched ? MainLeft::Mesh : MainWhy[Visual];
		++MainCounts[u32(Why)];
		MainLeftovers += Why != MainLeft::Batched;

		if (Why == MainLeft::Incomplete)
		{
			MainMissing.push_back(MaterialIds[Visual->shader._get()]);
		}

		const SPass* Pass = Why == MainLeft::Pass ? MaterialPass(Visual->shader._get()) : nullptr;

		if (Pass && Pass->vs && Pass->vs->cName.size())
		{
			++NoPassShaders[*Pass->vs->cName];
		}
	}

	xr_vector<std::pair<const char*, u32>> Shaders(NoPassShaders.begin(), NoPassShaders.end());
	std::sort(Shaders.begin(), Shaders.end(), [](const auto& Left, const auto& Right) { return Left.second > Right.second; });
	xr_sprintf(MainLeftLine, "%u of %u by reason:", MainLeftovers, u32(Leaves.size()));

	for (u32 Reason = 1; Reason < u32(MainLeft::Count); ++Reason)
	{
		string64 Part;
		xr_sprintf(Part, "%s %s %u", Reason > 1 ? "," : "", MainLeftNames[Reason], MainCounts[Reason]);
		xr_strcat(MainLeftLine, Part);
	}

	xr_strcat(MainLeftLine, "; no cluster pass by vertex shader:");

	for (size_t Index = 0; Index < std::min<size_t>(Shaders.size(), 8); ++Index)
	{
		string128 Part;
		xr_sprintf(Part, "%s %s %u", Index ? "," : "", Shaders[Index].first, Shaders[Index].second);
		xr_strcat(MainLeftLine, Part);
	}

	NameMissing(MainMissing, Missing);
	Msg("* [scene] level visuals the main view draws from the graph: %s; maps neither in the buckets nor bound:%s", MainLeftLine, MainMissing.empty() ? " none" : Missing);
	MainStatics.assign(Sectors.size(), {});
	u32 MainStaticCount = 0;

	for (IRender_Sector* Item : Sectors)
	{
		CSector* Sector = static_cast<CSector*>(Item);
		AddMainStatics(Sector->root(), MainStatics[Sector->index]);
		MainStaticCount += u32(MainStatics[Sector->index].size());
	}

	MainListed = MainStaticCount;
	Msg("* [scene] main view statics: %s; %u sorted surfaces, LOD groups and leaves left to the graph it collects without the walk", IsMainListed() ? "no sector walked" : "sectors walked", MainStaticCount);
	string512 LeftReasons = {};

	for (u32 Reason = 1; Reason < u32(StaticLeft::Count); ++Reason)
	{
		string64 Part;
		xr_sprintf(Part, "%s%s %u", Reason > 1 ? ", " : "", StaticLeftNames[Reason], LeftCounts[Reason]);
		xr_strcat(LeftReasons, Part);
	}

	Msg("* [scene] level visuals outside the depth batch: %u of %u%s", LeftCount, u32(Leaves.size()), Leftover[0].empty() ? " (sectors are still walked)" : "");
	Msg("* [scene] level visuals outside the depth batch by reason: %s; %u of them put anything into the first pass of a shadow view, %u have a shadow element (walked by a lamp with translucent shadows); shadow views that walk only their portals: of the first pass %s, lamps with translucent shadows %s",
		LeftReasons, FirstCount, ShadowLeftovers[0], ClusterCount && !FirstCount ? "yes" : "no", ClusterCount && !ShadowLeftovers[0] ? "yes" : "no");

	Msg("* [scene] tables: %u instances of %u visuals, %u meshes, %u transforms, %u materials",
		u32(InstanceRecords.size()), u32(Visuals.size()), u32(MeshRecords.size()), u32(TransformRecords.size()), u32(MaterialIds.size()));
	Msg("* [scene] detail models: %u of %u have clusters", DetailCount, u32(Details.size()));
	const u32 LateCount = u32(std::count_if(Casters.begin(), Casters.end(), [&](const Caster& Item) { return Item.Visual->IsDepthBatched && (InstanceRecords[Item.Instance].flags & SCENE_INSTANCE_TRANSLUCENT); }));
	Msg("* [scene] depth batch: %u casters (%u alpha-tested of %u candidates, %u of a later priority, which a lamp casts with translucent shadows on), %u more for the main view alone, %u clusters, %u triangles, %u meshes split, %.2f s",
		u32(CasterCount), u32(CasterCount - OpaqueCount), u32(Candidates.size()), LateCount, u32(Casters.size() - CasterCount), ClusterCount, std::accumulate(ClusterRecords.begin(), ClusterRecords.end(), 0u, [](u32 Sum, const Hlsl::SceneCluster& Record) { return Sum + (Record.triangles & SCENE_CLUSTER_COUNT_MASK); }), Split, BuildSeconds);
}
