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
namespace
{
constexpr float HeadAskMargin = 2.0f; // tail texels a pixel may take before the head is asked for
nri::Texture* HiZ = nullptr;
xr_vector<nri::Descriptor*> HizLevels;
constexpr u32 HizStep = 6; // levels a dispatch writes: HIZ_LEVELS of scene_hiz
xr_vector<nri::DescriptorSet*> HizSets;
nri::DescriptorPool* HizPool = nullptr;
nri::Descriptor* HizBlock = nullptr;
nri::AccessLayoutStage HizState = {};
u32 HizFrame = u32(-1); // the frame the depth pyramid was last built in
// The view and the projection its depth was drawn with
Fmatrix HizView = Fidentity;
Fmatrix HizProject = Fidentity;
constexpr u32 FaceCullFrames = 60;
Hlsl::SceneView FaceViews[FaceParts];
float FaceReaches[FaceParts];
u32 FaceFrames[FaceParts];
u32 TableCopied = u32(-1); // the frame whose table of moving objects is in Models
} // namespace

void ReleaseHiZ()
{
	if (!HiZ)
	{
		return;
	}

	for (nri::Descriptor* Level : HizLevels)
	{
		Api.DestroyDescriptor(Level);
	}

	Api.DestroyDescriptor(HizRead);
	HizRead = nullptr;
	Api.DestroyDescriptor(HizBlock);
	Api.DestroyDescriptorPool(HizPool);
	Api.DestroyTexture(HiZ);
	HizLevels.clear();
	HizSets.clear();
	HiZ = nullptr;
	HizWidth = 0;
	HizHeight = 0;
}

bool HizOfFrameBefore(Fmatrix& View, Fvector4& Scale, Fvector4& Depth, Fvector4& Size)
{
	if (!HizRead || HizFrame + 1 != RFrame.dwFrame || RFrame.IsCameraCut())
	{
		return false;
	}

	View = HizView;
	Scale.set(HizProject._11, HizProject._22, -HizProject._43 / HizProject._33, 0.0f);
	Depth.set(HizProject._33, HizProject._43, 0.0f, 0.0f);
	Size.set(float(HizWidth), float(HizHeight), float(HizLevels.empty() ? 0 : HizLevels.size() - 1), 0.0f);
	return true;
}

namespace
{
void CreateHiZ(u32 Width, u32 Height)
{
	nri::Device& Gpu = *GRHI->Nri->Device;
	const nri::StageBits Compute = nri::StageBits::COMPUTE_SHADER;
	const u32 Mips = u32(std::bit_width(std::max(Width, Height)));

	if (!HizLayout)
	{
		const nri::DescriptorRangeDesc Ranges[2] = { { 0, 1, nri::DescriptorType::TEXTURE, Compute, nri::DescriptorRangeBits::PARTIALLY_BOUND }, { 0, HizStep + 1, nri::DescriptorType::STORAGE_TEXTURE, Compute } };
		HizLayout = CreateLayout(Compute, Ranges, 2);
		ShaderExternalMap Defines;

		for (u32 Samples = 0; Samples < 2; ++Samples)
		{
			HizShaders[Samples] = DEV->_CreateCS("common\\scene_hiz", Defines);
			nri::ComputePipelineDesc PipelineDesc = {};
			PipelineDesc.pipelineLayout = HizLayout;
			PipelineDesc.shader = ShaderCode(HizShaders[Samples]->sh, Compute);
			Api.CreateComputePipeline(Gpu, PipelineDesc, HizPipelines[Samples]);
			Defines["USE_MSAA"] = "1";
		}
	}

	nri::TextureDesc Desc = {};
	Desc.type = nri::TextureType::TEXTURE_2D;
	Desc.usage = nri::TextureUsageBits::SHADER_RESOURCE | nri::TextureUsageBits::SHADER_RESOURCE_STORAGE;
	Desc.format = nri::Format::R32_SFLOAT;
	Desc.width = nri::Dim_t(Width);
	Desc.height = nri::Dim_t(Height);
	Desc.depth = 1;
	Desc.mipNum = nri::Dim_t(Mips);
	Desc.layerNum = 1;
	Desc.sampleNum = 1;
	Api.CreateCommittedTexture(Gpu, nri::MemoryLocation::DEVICE, 0.0f, Desc, HiZ);
	Api.SetDebugName(HiZ, "Scene depth pyramid");
	HizState = {};
	HizWidth = Width;
	HizHeight = Height;

	nri::BufferViewDesc Block = {};
	Block.buffer = StreamApi.GetStreamerConstantBuffer(*Streamer);
	Block.type = nri::BufferView::CONSTANT_BUFFER;
	Block.size = 256;
	Api.CreateBufferView(Block, HizBlock);

	nri::TextureViewDesc View = {};
	View.texture = HiZ;
	View.type = nri::TextureView::TEXTURE;
	View.mipNum = nri::REMAINING;
	View.layerNum = 1;
	Api.CreateTextureView(View, HizRead);

	nri::DescriptorPoolDesc PoolDesc = {};
	const u32 Steps = (Mips + HizStep - 1) / HizStep;
	PoolDesc.descriptorSetMaxNum = Steps;
	PoolDesc.textureMaxNum = Steps;
	PoolDesc.storageTextureMaxNum = Steps * (HizStep + 1);
	Api.CreateDescriptorPool(Gpu, PoolDesc, HizPool);
	HizLevels.resize(Mips);
	HizSets.resize(Steps);

	for (u32 Level = 0; Level < Mips; ++Level)
	{
		View.type = nri::TextureView::STORAGE_TEXTURE;
		View.mipOffset = nri::Dim_t(Level);
		View.mipNum = 1;
		Api.CreateTextureView(View, HizLevels[Level]);
	}

	// A set a dispatch: the levels it writes and the level it reads. No level is in two slots: D3D11 unbinds the
	// first of them, which left the finest level unwritten. A slot the dispatch does not use takes a level it does
	// not touch
	for (u32 Step = 0; Step < Steps; ++Step)
	{
		Api.AllocateDescriptorSets(*HizPool, *HizLayout, 0, &HizSets[Step], 1, 0);
		nri::Descriptor* Written[HizStep + 1];
		const u32 First = Step * HizStep;
		const u32 Before = Step ? First - 1 : Mips > HizStep ? Mips - 1 : 0;
		u32 Spare = 0;

		for (u32 Level = 0; Level < HizStep; ++Level)
		{
			const u32 Mip = First + Level;
			Written[Level] = HizLevels[Mip < Mips ? Mip : Spare < Before ? Spare++ : Mips - 1];
		}

		Written[HizStep] = HizLevels[Before];
		const nri::UpdateDescriptorRangeDesc Update = { HizSets[Step], 1, 0, Written, HizStep + 1 };
		Api.UpdateDescriptorRanges(&Update, 1);
	}

	const nri::UpdateDescriptorRangeDesc Read[2] = { { CullSet, 2, 0, &HizRead, 1 }, { RangeSet, 2, 0, &HizRead, 1 } };
	Api.UpdateDescriptorRanges(Read, 2);

	for (const OwnView& Own : OwnViews)
	{
		if (Own.CullSet)
		{
			const nri::UpdateDescriptorRangeDesc OwnRead[2] = { { Own.CullSet, 2, 0, &HizRead, 1 }, { Own.RangeSet, 2, 0, &HizRead, 1 } };
			Api.UpdateDescriptorRanges(OwnRead, 2);
		}
	}

	if (CardSet)
	{
		const nri::UpdateDescriptorRangeDesc Pyramid = { CardSet, 1, 0, &HizRead, 1 };
		Api.UpdateDescriptorRanges(&Pyramid, 1);
	}

	// The grass cull names the pyramid in the set of each frame it hides grass in, and a view that hides none keeps
	// what an earlier frame wrote: every set takes the new pyramid, or it holds the one just destroyed
	for (nri::DescriptorSet* Set : Grass.Sets)
	{
		if (Set)
		{
			const nri::UpdateDescriptorRangeDesc Pyramid = { Set, 2, 0, &HizRead, 1 };
			Api.UpdateDescriptorRanges(&Pyramid, 1);
		}
	}
}
} // namespace
} // namespace GpuSceneInternal

void GpuScene::BuildHiZ(GfxCmdList& Cmd, IRHIDepthStencilView* Depth, bool IsLoose)
{
	// Low culling quality tests nothing against the depth: the view's first cull draws all its frustum holds
	if (!IsForwardBinned() || ps_r_culling_quality == 0)
	{
		if (HiZ)
		{
			GRHI->DevicePtr->Flush();
			ReleaseHiZ();
		}

		return;
	}

	GPU_EVENT(SceneHiZ);
	IRHISurface* Surface = Depth->GetSurface();
	// A quarter of the depth: a cluster's sphere is tested on a level where it is two texels wide, and no cluster
	// is small enough on the screen to reach the levels above this one
	const u32 Width = std::max(std::bit_floor(Surface->GetWidth()) / 4, 1u);
	const u32 Height = std::max(std::bit_floor(Surface->GetHeight()) / 4, 1u);

	if (Width != HizWidth || Height != HizHeight)
	{
		// The old backend owns the queue: its flush covers the passes that read the old pyramid
		GRHI->DevicePtr->Flush();
		ReleaseHiZ();
		CreateHiZ(Width, Height);
	}

	// The pyramid is of the view, whatever part of the depth holds it
	u32 ViewWidth = Surface->GetWidth();
	u32 ViewHeight = Surface->GetHeight();
	ViewSize(ViewWidth, ViewHeight);

	nri::Descriptor* Source = RImplementation.NriPasses.WrapRead(Surface);
	if (!Source)
	{
		return;
	}

	const nri::UpdateDescriptorRangeDesc Update = { HizSets[0], 0, 0, &Source, 1 };
	Api.UpdateDescriptorRanges(&Update, 1);

	const nri::StageBits Compute = nri::StageBits::COMPUTE_SHADER;
	nri::TextureBarrierDesc Barrier = {};
	Barrier.texture = HiZ;
	Barrier.before = HizState;
	Barrier.after = { nri::AccessBits::SHADER_RESOURCE_STORAGE, nri::Layout::SHADER_RESOURCE_STORAGE, Compute };
	Barrier.mipNum = nri::REMAINING;
	Barrier.layerNum = nri::REMAINING;
	nri::BarrierDesc Barriers = {};
	Barriers.textures = &Barrier;
	Barriers.textureNum = 1;

	nri::CommandBuffer& Commands = *BeginNriCommands(&Surface, 1, nullptr, 0, nullptr);
	Api.CmdBarrier(Commands, Barriers);
	Api.CmdSetDescriptorPool(Commands, *HizPool);
	Api.CmdSetPipelineLayout(Commands, nri::BindPoint::COMPUTE, *HizLayout);
	Api.CmdSetPipeline(Commands, *HizPipelines[Surface->GetSampleDescCount() > 1]);
	Barrier.before = Barrier.after;

	for (u32 Step = 0; Step < HizSets.size(); ++Step)
	{
		const u32 Level = Step * HizStep;

		struct
		{
			u32 Size[4];
			float Source[4];
		} Block = { { std::max(HizWidth >> Level, 1u), std::max(HizHeight >> Level, 1u), u32(Step == 0) | (IsLoose ? 2u : 0u), std::min(HizStep, u32(HizLevels.size()) - Level) },
			{ float(ViewWidth) / float(HizWidth), float(ViewHeight) / float(HizHeight), float(ViewWidth), float(ViewHeight) } };

		nri::SetRootDescriptorDesc Root = {};
		Root.descriptor = HizBlock;
		Root.offset = StreamApi.StreamConstantData(*Streamer, &Block, sizeof(Block));
		Api.CmdSetRootDescriptor(Commands, Root);

		nri::SetDescriptorSetDesc SetDesc = {};
		SetDesc.descriptorSet = HizSets[Step];
		Api.CmdSetDescriptorSet(Commands, SetDesc);
		Api.CmdDispatch(Commands, { (Block.Size[0] + 31) / 32, (Block.Size[1] + 31) / 32, 1 });
		// The next dispatch reads what this one wrote
		Api.CmdBarrier(Commands, Barriers);
	}

	Barrier.after = { nri::AccessBits::SHADER_RESOURCE, nri::Layout::SHADER_RESOURCE, Compute };
	Api.CmdBarrier(Commands, Barriers);
	HizState = Barrier.after;
	HizFrame = RFrame.dwFrame;
	HizView = RFrame.mView;
	HizProject = RFrame.mProject;
	EndNriCommands(&Commands);
	Cmd.RestoreInput();
}

bool GpuScene::CullFace(R_dsgraph_structure& Graph, const Fvector& Eye, const Fmatrix& Transform, u32 Face)
{
	if (!IsForwardBinned())
	{
		return false;
	}

	DepthDraw Draw = {};
	Draw.Eye = &Eye;
	Draw.Transform = &Transform;
	Draw.IsSpot = true;
	Draw.IsMain = true;
	Draw.IsBinned = true;
	Draw.IsFace = true;
	Draw.Face = Face;
	Cull(Graph, Draw, 0);
	return true;
}

void GpuScene::CullCards(R_dsgraph_structure& Graph)
{
	if (!CardCount || !IsForwardBinned())
	{
		return;
	}

	DepthDraw Draw = {};
	Draw.Eye = &RFrame.vCameraPosition;
	Draw.Transform = &RFrame.mFullTransform;
	Draw.Frustum = &RImplementation.ViewBase;
	Draw.IsSpot = true;
	Draw.IsMain = true;
	Draw.IsBinned = true;
	// Against the depth pyramid where this frame built one
	Draw.HizPart = HizFrame == RFrame.dwFrame && HizRead ? 2 : 0;
	Hlsl::SceneView Block;
	MakeCullBlock(Graph, Draw, 0, 0, Block);
	Block.second.z = CardCount;
	Block.second.w = CardStart;

	GPU_EVENT(SceneCardCull);
	nri::CommandBuffer& Commands = *BeginNriCommands(nullptr, 0, nullptr, 0, nullptr);
	Move(Commands, CardArgs, nri::AccessBits::COPY_DESTINATION, nri::StageBits::COPY);
	Api.CmdZeroBuffer(Commands, *CardArgs.Buffer, 0, 4 * sizeof(u32));
	Move(Commands, CardArgs, nri::AccessBits::SHADER_RESOURCE_STORAGE, nri::StageBits::COMPUTE_SHADER);
	Move(Commands, Visible, nri::AccessBits::SHADER_RESOURCE_STORAGE, nri::StageBits::COMPUTE_SHADER);
	Api.CmdSetDescriptorPool(Commands, *CardPool);
	Api.CmdSetPipelineLayout(Commands, nri::BindPoint::COMPUTE, *CardLayout);
	Api.CmdSetPipeline(Commands, *CardCull);
	SetBlocks(Commands, StreamApi.StreamConstantData(*Streamer, &Block, sizeof(Block)));

	nri::SetDescriptorSetDesc SetDesc = {};
	SetDesc.descriptorSet = CardSet;
	Api.CmdSetDescriptorSet(Commands, SetDesc);
	Api.CmdDispatch(Commands, { (CardCount + 63) / 64, 1, 1 });
	EndNriCommands(&Commands);
	CardFrame = RFrame.dwFrame;
}

bool GpuScene::CullForward(R_dsgraph_structure& Graph, bool IsSecond)
{
	if (!ClusterCount || Bins.empty() || (IsSecond && !HiZ))
	{
		return false;
	}

	MainPackets = IsSecond ? MainPackets : u32(Graph.StaticPasses[0].size() + Graph.StaticPasses[1].size());
	DepthDraw Draw = {};
	Draw.Eye = &RFrame.vCameraPosition;
	Draw.Transform = &RFrame.mFullTransform;
	Draw.Frustum = &RImplementation.ViewBase;
	Draw.IsSpot = true;
	Draw.IsMain = true;
	Draw.IsBinned = true;
	Draw.HizPart = IsSecond ? 2 : HiZ ? 1 : 0;
	Cull(Graph, Draw, !IsSecond && ModelFrame == RFrame.dwFrame ? ModelOpaque : 0);
	return true;
}

void GpuScene::Cull(R_dsgraph_structure& Graph, const DepthDraw& Draw, u32 Casters, u32 First)
{
	Hlsl::SceneView Block;

	if (MakeCullBlock(Graph, Draw, Casters, First, Block))
	{
		Dispatch(Block);
	}
}

bool GpuScene::MakeCullBlock(R_dsgraph_structure& Graph, const DepthDraw& Draw, u32 Casters, u32 First, Hlsl::SceneView& Block)
{
	PROF_EVENT("SceneCullPrepare");
	const GraphView& View = Graph.ViewParams;
	const CPortalTraverser& Traverser = Graph.PortalTraverser;

	Block = {};
	Block.view_projection = Draw.Transform ? *Draw.Transform : View.Transform;
	const CFrustum& Frustum = Draw.Frustum ? *Draw.Frustum : View.Frustum;

	u32 Planes = std::min(u32(Frustum.p_count), SCENE_VIEW_PLANES);
	for (u32 Plane = 0; Plane < Planes; ++Plane)
	{
		const Fplane& Source = Frustum.planes[Plane];
		Block.planes[Plane].set(Source.n.x, Source.n.y, Source.n.z, Source.d);
	}

	Planes = AddCasterPlanes(Block.planes, Planes, Draw.Look);

	Block.camera.set(View.CameraPosition.x, View.CameraPosition.y, View.CameraPosition.z, View.FogDistance);
	// A cluster is a piece of a wall: dropped for its size on a face of 256 pixels, it leaves a hole the sun shows through
	Block.thresholds.set(Draw.IsFace ? 0.0f : View.SsaDiscard, float(SCENE_TEXTURE_TAIL) / (HeadAskMargin * RFrame.mProject._22 * 0.5f * float(DevicePtr->TargetHeight)), std::bit_cast<float>(u32(RFrame.dwFrame)), -1.0f);
	// The casters of a later priority: a shadow view draws them where its graph collects the second priority, as only a
	// lamp's does with r4_translucent_shadows
	const bool IsTranslucent = Draw.IsMain || Graph.pmask[1];
	Block.counts = { ClusterCount, (Draw.IsKept ? SCENE_VIEW_KEPT : 0u) | (Draw.IsMain ? SCENE_VIEW_MAIN : 0u) | (Draw.IsBinned ? SCENE_VIEW_BINS : 0u) | (IsTranslucent ? SCENE_VIEW_TRANSLUCENT : 0u) |
		(Draw.HizPart == 1 ? SCENE_VIEW_SEEN : Draw.HizPart == 2 ? SCENE_VIEW_UNSEEN : 0u) | (Casters << SCENE_VIEW_DYNAMIC_SHIFT), Planes, GetStaticCullSteps(!Draw.IsMain, View.Fov, Block.steps) };

	Fvector4 Wave, Wind;
	float SwayScale;
	GetTreeWave(Wave, Wind, SwayScale);
	Block.wave = Wave;
	Block.wind.set(Wind.x, Wind.y, Wind.z, SwayScale);

	// What the frame before had: its frame can have taken any time
	static Fvector4 Waves[2], Winds[2];
	static u32 WaveFrame = u32(-1);

	if (WaveFrame != RFrame.dwFrame)
	{
		WaveFrame = RFrame.dwFrame;
		Waves[1] = Waves[0];
		Winds[1] = Winds[0];
		Waves[0] = Block.wave;
		Winds[0] = Block.wind;
	}

	Block.wave_old = Waves[1];
	Block.wind_old = Winds[1];
	Block.lod.set(Draw.IsMain ? (Graph.OwnsView ? View.SsaLodB : r_ssaLOD_B) : 0.0f, Draw.IsMain ? (Graph.OwnsView ? View.SsaLodA : r_ssaLOD_A) : 0.0f, 0.0f, 0.0f);

	// Every view draws the detail the main view asks for: a shadow belongs to what is on the screen
	const Fvector& Eye = RFrame.vCameraPosition;
	Block.detail.set(Eye.x, Eye.y, Eye.z, ps_r_lod_error > 0.0f ? RFrame.mProject._22 * 0.5f * float(DevicePtr->TargetHeight) / ps_r_lod_error : 0.0f);

	// A face of the cube asks for the detail its own 256 pixels show, a quarter turn wide, with r_reflections_lod_error
	// times the error the screen is allowed
	if (Draw.IsFace && ps_r_lod_error > 0.0f)
	{
		Block.detail.w = 0.5f * 256.0f / (ps_r_lod_error * ps_r_reflections_lod_error);
	}

	if (Draw.HizPart)
	{
		// The first part tests nothing of the level against the pyramid: it has the one the frame before left, and its
		// models are tested against that, as that frame saw them (hiz.w). A model that comes out from behind a wall is
		// drawn a frame late; its sphere shows before it does
		const bool IsLate = Draw.HizPart == 1;
		const bool IsModelTest = IsLate && HizFrame + 1 == RFrame.dwFrame && !RFrame.IsCameraCut() && !DevicePtr->IsEditorMode();
		const Fmatrix& Project = IsLate ? HizProject : RFrame.mProject;
		Block.view = IsLate ? HizView : RFrame.mView;
		Block.hiz.set(Project._11, Project._22, -Project._43 / Project._33, IsModelTest ? 1.0f : 0.0f);
		Block.hiz_depth.set(Project._33, Project._43, 0.0f, 0.0f);
		Block.hiz_size.set(float(HizWidth), float(HizHeight), float(HizLevels.empty() ? 0 : HizLevels.size() - 1), 0.0f);
		Block.second = { Draw.HizPart == 2 ? u32(Bins.size()) : 0u, Draw.HizPart == 2 ? ClusterCount + ModelClusters : 0u, 0, 0 };
	}

	// A shadow view's dynamic count is of the frame's table from First
	if (!Draw.IsBinned)
	{
		Block.second.z = First;
		Block.second.w = SCENE_OBJECT_NONE;
	}

	Block.material.set(ps_r2_def_aref_quality / 255.0f, 0.0f, Draw.IsMain ? ps_r_taa_jitter.x : 0.0f, Draw.IsMain ? ps_r_taa_jitter.y : 0.0f);

	for (u32 Sector = 0; Sector < std::min(SectorCount, SCENE_VIEW_SECTORS); ++Sector)
	{
		// The main view does not traverse a level of one sector
		const bool IsVisited = Draw.Sectors ? (Draw.Sectors[Sector >> 5] & (1u << (Sector & 31))) != 0 : Traverser.IsVisited(Sector);

		if (IsVisited || (Draw.IsMain && SectorCount <= 1))
		{
			(&Block.sectors[Sector >> 7].x)[(Sector >> 5) & 3] |= 1u << (Sector & 31);
		}
	}

	if (Draw.IsFace)
	{
		const float Reach = ps_r_reflections_cull_reach;
		Block.second = { u32(Bins.size()) * (2 + Draw.Face), (ClusterCount + ModelClusters) * (2 + Draw.Face) + SCENE_DYNAMIC_CLUSTERS, 0, 0 };

		for (u32 Plane = 0; Plane < Planes; ++Plane)
		{
			Block.planes[Plane].w -= Reach;
		}

		const Hlsl::SceneView& Kept = FaceViews[Draw.Face];
		const Fvector Moved = { Block.camera.x - Kept.camera.x, Block.camera.y - Kept.camera.y, Block.camera.z - Kept.camera.z };

		if (IsFaceCulled[Draw.Face] && RFrame.dwFrame - FaceFrames[Draw.Face] < FaceCullFrames && FaceReaches[Draw.Face] == Reach && Kept.detail.w == Block.detail.w && Moved.square_magnitude() < Reach * Reach && !memcmp(Kept.sectors, Block.sectors, sizeof(Block.sectors)))
		{
			return false;
		}

		FaceViews[Draw.Face] = Block;
		FaceFrames[Draw.Face] = RFrame.dwFrame;
		FaceReaches[Draw.Face] = Reach;
		IsFaceCulled[Draw.Face] = true;
	}

	return true;
}

void GpuScene::Dispatch(const Hlsl::SceneView& Block)
{
	GPU_EVENT(SceneCull);

	if (!IsDensityBuilt)
	{
		for (u32 Page = 0; Page < RImplementation.Geometry.PageCount(); ++Page)
		{
			if (IRHIBuffer* Source = RImplementation.Geometry.VertexBuffer(Page))
			{
				GRHI->DevicePtr->ReadExternalBuffer(Source);
			}
		}
	}

	const CullBlocks Blocks = StreamCull(Block);
	ForwardViewOffset = (Block.counts.y & SCENE_VIEW_SEEN) ? Blocks.Range : ForwardViewOffset;
	ViewOffset = Blocks.Bands[Blocks.BandCount - 1];

	nri::CommandBuffer& Commands = *BeginNriCommands(nullptr, 0, nullptr, 0, nullptr);
	PrepareCull(Commands, Block);
	RecordCull(Commands, SharedLists(), Block, Blocks);
	EndNriCommands(&Commands);
}

void GpuScene::PrepareCull(nri::CommandBuffer& Commands, const Hlsl::SceneView& Block)
{
	if (!IsDensityBuilt)
	{
		IsDensityBuilt = true;
		Move(Commands, Feedback, nri::AccessBits::COPY_DESTINATION, nri::StageBits::COPY);
		Api.CmdZeroBuffer(Commands, *Feedback.Buffer, 0, FeedbackBytes);
		Move(Commands, Density, nri::AccessBits::SHADER_RESOURCE_STORAGE, nri::StageBits::COMPUTE_SHADER);
		Api.CmdSetDescriptorPool(Commands, *Pool);
		Api.CmdSetPipelineLayout(Commands, nri::BindPoint::COMPUTE, *DensityLayout);
		Api.CmdSetPipeline(Commands, *DensityPipeline);
		nri::SetDescriptorSetDesc DensityDesc = {};
		DensityDesc.descriptorSet = DensitySet;
		Api.CmdSetDescriptorSet(Commands, DensityDesc);
		Api.CmdDispatch(Commands, { (ClusterCount + 63) / 64, 1, 1 });
		Move(Commands, Density, nri::AccessBits::SHADER_RESOURCE, nri::StageBits::COMPUTE_SHADER);
	}

	// The frame's table whole out of the upload buffer of its slot, once a frame, for the first cull that reads it
	const u32 Taken = (Block.counts.y & ~SCENE_VIEW_MODES) >> SCENE_VIEW_DYNAMIC_SHIFT;

	if (Taken && TableCopied != RFrame.dwFrame)
	{
		TableCopied = RFrame.dwFrame;
		Move(Commands, Models, nri::AccessBits::COPY_DESTINATION, nri::StageBits::COPY);
		Api.CmdCopyBuffer(Commands, *Models.Buffer, 0, *ModelUploads[RFrame.dwFrame % FrameCount], 0, ModelRecords.size() * sizeof(Hlsl::SceneModel));
		Move(Commands, Models, nri::AccessBits::SHADER_RESOURCE, nri::StageBits::COMPUTE_SHADER | nri::StageBits::VERTEX_SHADER);
	}

	Move(Commands, Feedback, nri::AccessBits::SHADER_RESOURCE_STORAGE, nri::StageBits::COMPUTE_SHADER);

	if (!IsSeenCleared)
	{
		IsSeenCleared = true;
		Move(Commands, Seen, nri::AccessBits::COPY_DESTINATION, nri::StageBits::COPY);
		Api.CmdZeroBuffer(Commands, *Seen.Buffer, 0, u64(ClusterCount) * sizeof(u32));
	}

	Move(Commands, Seen, nri::AccessBits::SHADER_RESOURCE_STORAGE, nri::StageBits::COMPUTE_SHADER);
}
