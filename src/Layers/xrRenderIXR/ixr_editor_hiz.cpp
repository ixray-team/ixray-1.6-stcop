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

#include "ixr_editor_hiz.h"
#include "NriPass.h"
#include "dxRenderDeviceRender.h"

// The engine defines Device as a macro; NRI and NriDevice use it as a name
#undef Device
#include "../../xrRHI/Nri/NriDevice.h"

#include <NRI.h>

namespace
{
	// The depth is a few frames old: any camera move uncovers what it hid, so the data is used
	// only while the camera stands still (then it shows the frame being drawn)
	constexpr float MaxCameraShift = 0.01f;
	constexpr float MinCameraDirectionDot = 0.99999f;

	const nri::AccessLayoutStage GridCopied = { nri::AccessBits::COPY_SOURCE, nri::Layout::COPY_SOURCE, nri::StageBits::COPY };
}

CEditorHiZ::~CEditorHiZ()
{
	Destroy();
}

void CEditorHiZ::ReleaseResources()
{
	if (!Grid)
	{
		return;
	}

	// The frames in flight still write the grid and its read-backs
	for (SReadback& Readback : Readbacks)
	{
		GRHI->Nri->Retire(Readback.Buffer);
		Readback = SReadback();
	}

	GRHI->Nri->Retire(Pool);
	GRHI->Nri->Retire(GridView);
	GRHI->Nri->Retire(Grid);
	Pool = nullptr;
	GridView = nullptr;
	Grid = nullptr;
	IsGridWritten = false;
	HasData = false;
	Usable = false;
}

void CEditorHiZ::Destroy()
{
	ReleaseResources();

	if (Layout)
	{
		GRHI->Nri->Retire(Pipeline);
		GRHI->Nri->Retire(Layout);
		Pipeline = nullptr;
		Layout = nullptr;
	}

	Shader = nullptr;
	ViewWidth = 0;
	ViewHeight = 0;
	GridHeight = 0;
}

bool CEditorHiZ::CreateResources(u32 Width, u32 Height)
{
	if (Grid && Width == ViewWidth && Height == ViewHeight)
	{
		return true;
	}

	ReleaseResources();
	nri::Device& Gpu = *GRHI->Nri->Device;
	const nri::CoreInterface& Api = GRHI->Nri->CoreApi();
	const nri::StageBits Compute = nri::StageBits::COMPUTE_SHADER;

	// The range of s_position is written each frame, and the depth is written again while a set holding it is bound
	const nri::DescriptorRangeDesc Ranges[2] = { { ShaderBindings::Texture("s_position"), 1, nri::DescriptorType::TEXTURE, Compute, nri::DescriptorRangeBits::ALLOW_UPDATE_AFTER_SET }, { 0, 1, nri::DescriptorType::STORAGE_TEXTURE, Compute } };

	if (!Layout)
	{
		Shader = DEV->_CreateCS("editor_hiz");

		nri::DescriptorSetDesc Set = {};
		Set.ranges = Ranges;
		Set.rangeNum = 2;
		const nri::RootConstantDesc Block = { CBEditor::Slot, sizeof(CBEditor), Compute };

		nri::PipelineLayoutDesc LayoutDesc = {};
		LayoutDesc.rootConstants = &Block;
		LayoutDesc.rootConstantNum = 1;
		LayoutDesc.descriptorSets = &Set;
		LayoutDesc.descriptorSetNum = 1;
		LayoutDesc.shaderStages = Compute;

		const void* Code = nullptr;
		size_t Size = 0;
		if (!Shader->sh || !GRHI->DevicePtr->GetShaderCode(Shader->sh, Code, Size) || Api.CreatePipelineLayout(Gpu, LayoutDesc, Layout) != nri::Result::SUCCESS)
		{
			return false;
		}

		nri::ComputePipelineDesc PipelineDesc = {};
		PipelineDesc.pipelineLayout = Layout;
		PipelineDesc.shader = { Compute, Code, Size };
		Api.CreateComputePipeline(Gpu, PipelineDesc, Pipeline);
	}

	ViewWidth = Width;
	ViewHeight = Height;
	GridHeight = std::clamp(u32(float(GridWidth) * float(Height) / float(Width) + 0.5f), 1u, GridWidth);

	nri::TextureDesc Desc = {};
	Desc.type = nri::TextureType::TEXTURE_2D;
	Desc.usage = nri::TextureUsageBits::SHADER_RESOURCE_STORAGE;
	Desc.format = nri::Format::R32_SFLOAT;
	Desc.width = nri::Dim_t(GridWidth);
	Desc.height = nri::Dim_t(GridHeight);
	Desc.depth = 1;
	Desc.mipNum = 1;
	Desc.layerNum = 1;
	Desc.sampleNum = 1;

	if (!Pipeline || Api.CreateCommittedTexture(Gpu, nri::MemoryLocation::DEVICE, 0.0f, Desc, Grid) != nri::Result::SUCCESS)
	{
		return false;
	}

	Api.SetDebugName(Grid, "Editor occlusion grid");
	nri::TextureViewDesc View = {};
	View.texture = Grid;
	View.type = nri::TextureView::STORAGE_TEXTURE;
	View.mipNum = 1;
	View.layerNum = 1;
	Api.CreateTextureView(View, GridView);

	nri::DescriptorPoolDesc PoolDesc = {};
	PoolDesc.flags = nri::DescriptorPoolBits::ALLOW_UPDATE_AFTER_SET;
	PoolDesc.descriptorSetMaxNum = ReadbackCount;
	PoolDesc.textureMaxNum = ReadbackCount;
	PoolDesc.storageTextureMaxNum = ReadbackCount;
	Api.CreateDescriptorPool(Gpu, PoolDesc, Pool);

	nri::BufferDesc Copy = {};
	Copy.size = u64(GridWidth) * sizeof(float) * GridHeight;

	for (SReadback& Readback : Readbacks)
	{
		Api.CreateCommittedBuffer(Gpu, nri::MemoryLocation::HOST_READBACK, 0.0f, Copy, Readback.Buffer);
		Api.AllocateDescriptorSets(*Pool, *Layout, 0, &Readback.Set, 1, 0);
		const nri::UpdateDescriptorRangeDesc Update = { Readback.Set, 1, 0, &GridView, 1 };
		Api.UpdateDescriptorRanges(&Update, 1);
	}

	// CPU pyramid down to one cell
	Levels.clear();
	u32 LevelWidth = GridWidth;
	u32 LevelHeight = GridHeight;
	while (true)
	{
		SLevel& Level = Levels.emplace_back();
		Level.Width = LevelWidth;
		Level.Height = LevelHeight;
		Level.Depth.resize(LevelWidth * LevelHeight);

		if (LevelWidth == 1 && LevelHeight == 1)
		{
			break;
		}

		LevelWidth = (LevelWidth + 1) / 2;
		LevelHeight = (LevelHeight + 1) / 2;
	}

	return true;
}

void CEditorHiZ::Build(GfxCmdList& Cmd)
{
	const u32 Width = (u32)GfxCmdList::GetWidth();
	const u32 Height = (u32)GfxCmdList::GetHeight();
	if (Width == 0 || Height == 0 || !CreateResources(Width, Height))
	{
		return;
	}

	IRHISurface* Surface = RImplementation.Target->rt_Position->pSurface;
	nri::Descriptor* Source = RImplementation.NriPasses.WrapRead(Surface);
	if (!Source)
	{
		return;
	}

	GPU_EVENT(editor_hiz);
	const nri::CoreInterface& Api = GRHI->Nri->CoreApi();
	const u32 Frame = DevicePtr->dwFrame;
	SReadback& Readback = Readbacks[Frame % ReadbackCount];

	// A second build of the frame reads the same depth through the set the first one bound
	if (!Readback.Pending || Readback.Frame != Frame)
	{
		const nri::UpdateDescriptorRangeDesc Update = { Readback.Set, 0, 0, &Source, 1 };
		Api.UpdateDescriptorRanges(&Update, 1);
	}

	nri::TextureBarrierDesc Barrier = {};
	Barrier.texture = Grid;
	Barrier.before = IsGridWritten ? GridCopied : nri::AccessLayoutStage{};
	Barrier.after = { nri::AccessBits::SHADER_RESOURCE_STORAGE, nri::Layout::SHADER_RESOURCE_STORAGE, nri::StageBits::COMPUTE_SHADER };
	Barrier.mipNum = nri::REMAINING;
	Barrier.layerNum = nri::REMAINING;
	nri::BarrierDesc Barriers = {};
	Barriers.textures = &Barrier;
	Barriers.textureNum = 1;

	CBEditor Block = {};
	Block.editor_hiz_params.set(float(Width), float(Height), 0.f, 0.f);
	nri::SetRootConstantsDesc Constants = {};
	Constants.data = &Block;
	Constants.size = sizeof(Block);
	nri::SetDescriptorSetDesc SetDesc = {};
	SetDesc.descriptorSet = Readback.Set;

	nri::CommandBuffer& Commands = *BeginNriCommands(&Surface, 1, nullptr, 0, nullptr);
	Api.CmdBarrier(Commands, Barriers);
	Api.CmdSetDescriptorPool(Commands, *Pool);
	Api.CmdSetPipelineLayout(Commands, nri::BindPoint::COMPUTE, *Layout);
	Api.CmdSetPipeline(Commands, *Pipeline);
	Api.CmdSetRootConstants(Commands, Constants);
	Api.CmdSetDescriptorSet(Commands, SetDesc);
	Api.CmdDispatch(Commands, { (GridWidth + 7) / 8, (GridHeight + 7) / 8, 1 });

	Barrier.before = Barrier.after;
	Barrier.after = GridCopied;
	Api.CmdBarrier(Commands, Barriers);

	// A row is 1 KB: a multiple of the row alignment of every API's copies
	const nri::TextureDataLayoutDesc Rows = { 0, GridWidth * u32(sizeof(float)), GridWidth * u32(sizeof(float)) * GridHeight };
	const nri::TextureRegionDesc Region = { 0, 0, 0, nri::WHOLE_SIZE, nri::WHOLE_SIZE, nri::WHOLE_SIZE, 0, 0, nri::PlaneBits::ALL };
	Api.CmdReadbackTextureToBuffer(Commands, *Readback.Buffer, Rows, *Grid, Region);
	EndNriCommands(&Commands);
	Cmd.RestoreInput();

	IsGridWritten = true;
	Readback.Frame = Frame;
	Readback.ViewProject = RFrame.mFullTransform;
	Readback.CameraPosition = RFrame.vCameraPosition;
	Readback.CameraDirection = RFrame.vCameraDirection;
	Readback.Pending = true;
}

void CEditorHiZ::Fetch()
{
	// Written ReadbackCount - 1 frames ago, or earlier when frames built none
	SReadback& Readback = Readbacks[(DevicePtr->dwFrame + 1) % ReadbackCount];

	if (Readback.Pending)
	{
		const nri::CoreInterface& Api = GRHI->Nri->CoreApi();
		SLevel& Level = Levels[0];
		const u64 Bytes = Level.Depth.size() * sizeof(float);
		std::memcpy(Level.Depth.data(), Api.MapBuffer(*Readback.Buffer, 0, Bytes), Bytes);
		Api.UnmapBuffer(*Readback.Buffer);

		Readback.Pending = false;
		ViewProject = Readback.ViewProject;
		CameraPosition = Readback.CameraPosition;
		CameraDirection = Readback.CameraDirection;
		HasData = true;

		BuildMips();
	}

	Usable = HasData
		&& RFrame.vCameraPosition.distance_to_sqr(CameraPosition) < _sqr(MaxCameraShift)
		&& RFrame.vCameraDirection.dotproduct(CameraDirection) > MinCameraDirectionDot;
}

void CEditorHiZ::BuildMips()
{
	for (size_t Index = 1; Index < Levels.size(); ++Index)
	{
		const SLevel& Src = Levels[Index - 1];
		SLevel& Dst = Levels[Index];

		for (u32 Y = 0; Y < Dst.Height; ++Y)
		{
			const u32 Y0 = Y * 2;
			const u32 Y1 = std::min(Y0 + 1, Src.Height - 1);

			for (u32 X = 0; X < Dst.Width; ++X)
			{
				const u32 X0 = X * 2;
				const u32 X1 = std::min(X0 + 1, Src.Width - 1);

				const float* Row0 = Src.Depth.data() + Y0 * Src.Width;
				const float* Row1 = Src.Depth.data() + Y1 * Src.Width;
				Dst.Depth[Y * Dst.Width + X] = std::max(std::max(Row0[X0], Row0[X1]), std::max(Row1[X0], Row1[X1]));
			}
		}
	}
}

bool CEditorHiZ::IsVisible(const Fbox& Box) const
{
	if (!Usable)
	{
		return true;
	}

	float MinX = flt_max, MinY = flt_max, MaxX = -flt_max, MaxY = -flt_max, MinZ = flt_max;
	const Fmatrix& M = ViewProject;

	for (u32 Corner = 0; Corner < 8; ++Corner)
	{
		const float X = (Corner & 1) ? Box.max.x : Box.min.x;
		const float Y = (Corner & 2) ? Box.max.y : Box.min.y;
		const float Z = (Corner & 4) ? Box.max.z : Box.min.z;

		const float W = X * M._14 + Y * M._24 + Z * M._34 + M._44;

		// Crosses the near plane: cannot be tested in screen space
		if (W < EPS_L)
		{
			return true;
		}

		const float InvW = 1.f / W;
		const float PX = (X * M._11 + Y * M._21 + Z * M._31 + M._41) * InvW;
		const float PY = (X * M._12 + Y * M._22 + Z * M._32 + M._42) * InvW;
		const float PZ = (X * M._13 + Y * M._23 + Z * M._33 + M._43) * InvW;

		MinX = std::min(MinX, PX);
		MaxX = std::max(MaxX, PX);
		MinY = std::min(MinY, PY);
		MaxY = std::max(MaxY, PY);
		MinZ = std::min(MinZ, PZ);
	}

	// Partly outside the read-back view: that part was not tested in that frame and can be on
	// screen now (the camera turned)
	if (MinX < -1.f || MaxX > 1.f || MinY < -1.f || MaxY > 1.f)
	{
		return true;
	}

	// Grid cells under the box (y goes down in the grid)
	const SLevel& Base = Levels[0];
	const float U0 = std::clamp((MinX * 0.5f + 0.5f) * float(Base.Width), 0.f, float(Base.Width - 1));
	const float U1 = std::clamp((MaxX * 0.5f + 0.5f) * float(Base.Width), 0.f, float(Base.Width - 1));
	const float V0 = std::clamp((0.5f - MaxY * 0.5f) * float(Base.Height), 0.f, float(Base.Height - 1));
	const float V1 = std::clamp((0.5f - MinY * 0.5f) * float(Base.Height), 0.f, float(Base.Height - 1));

	// Coarsest level that keeps the box within a few cells
	const float Extent = std::max(U1 - U0, V1 - V0);
	u32 LevelIndex = 0;
	while (LevelIndex + 1 < Levels.size() && Extent > float(2u << LevelIndex))
	{
		++LevelIndex;
	}

	const SLevel& Level = Levels[LevelIndex];
	const u32 X0 = std::min(u32(U0) >> LevelIndex, Level.Width - 1);
	const u32 X1 = std::min(u32(U1) >> LevelIndex, Level.Width - 1);
	const u32 Y0 = std::min(u32(V0) >> LevelIndex, Level.Height - 1);
	const u32 Y1 = std::min(u32(V1) >> LevelIndex, Level.Height - 1);

	for (u32 Y = Y0; Y <= Y1; ++Y)
	{
		const float* Row = Level.Depth.data() + Y * Level.Width;
		for (u32 X = X0; X <= X1; ++X)
		{
			// Something under the box is farther than its nearest point
			if (MinZ <= Row[X])
			{
				return true;
			}
		}
	}

	return false;
}
