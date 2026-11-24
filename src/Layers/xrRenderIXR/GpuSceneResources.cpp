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
nri::Buffer* ClusterIndices = nullptr;
ref_cs DensityShader;
ref_cs RangeShader;

constexpr u32 AlphaSize = 1024;
constexpr u32 AlphaMips = 9;
nri::Descriptor* AlphaMipViews[AlphaMips] = {};
nri::Buffer* AlphaSources = nullptr;
nri::PipelineLayout* AlphaLayout = nullptr;
nri::Pipeline* AlphaPipeline = nullptr;
nri::Pipeline* AlphaWholePipeline = nullptr; // a slice from the whole copy of a texture the buckets have no place for
nri::DescriptorPool* AlphaPool = nullptr;
ref_cs AlphaBuildShader;
constexpr u32 ModelBins = 64;
// The cubes the materials mirror at load, as one cube array the forward draw reads by SceneMaterial.env: a material
// that mirrors a cube the array does not hold stays in the graph
constexpr u32 EnvSize = 256; // the largest face of the array
nri::Texture* EnvArray = nullptr;
nri::PipelineLayout* EnvLayout = nullptr;
nri::Pipeline* EnvPipeline = nullptr;
nri::DescriptorPool* EnvPool = nullptr;
nri::Descriptor* FeedbackView = nullptr;
ref_cs CullShader;
} // namespace

// The alpha of the alpha-tested textures as one single-channel array: a depth pass tests it with one
// plain sample, whatever buckets a texture's head and tail are in. Wholes: the textures of the slices after those of
// Refs, read from their whole copies
void BuildAlpha(const xr_vector<GpuTextures::Ref>& Refs, const xr_vector<IRHISurface*>& Wholes)
{
	if ((Refs.empty() && Wholes.empty()) || !Textures.Count())
	{
		return;
	}

	nri::Device& Gpu = *GRHI->Nri->Device;
	const nri::StageBits Compute = nri::StageBits::COMPUTE_SHADER;
	xr_vector<Hlsl::SceneMaterial> Sources;

	for (const GpuTextures::Ref& Item : Refs)
	{
		Sources.push_back({ Item.Tail, Item.Head, Item.Info, 0 });
	}

	AlphaSources = CreateTable(Sources, "Scene alpha sources");

	nri::TextureDesc Desc = {};
	Desc.type = nri::TextureType::TEXTURE_2D;
	Desc.usage = nri::TextureUsageBits::SHADER_RESOURCE | nri::TextureUsageBits::SHADER_RESOURCE_STORAGE;
	Desc.format = nri::Format::R8_UNORM;
	Desc.width = nri::Dim_t(AlphaSize);
	Desc.height = nri::Dim_t(AlphaSize);
	Desc.depth = 1;
	Desc.mipNum = nri::Dim_t(AlphaMips);
	Desc.layerNum = nri::Dim_t(Refs.size() + Wholes.size());
	Desc.sampleNum = 1;
	Require(Api.CreateCommittedTexture(Gpu, nri::MemoryLocation::DEVICE, 0.0f, Desc, Alpha), "The graphics device could not create the scene alpha array.");
	Api.SetDebugName(Alpha, "Scene alpha");

	const nri::DescriptorRangeDesc Ranges[3] = { { 0, 1, nri::DescriptorType::STRUCTURED_BUFFER, Compute }, { 1, GpuTextures::MaxBuckets, nri::DescriptorType::TEXTURE, Compute }, { 0, 1, nri::DescriptorType::STORAGE_TEXTURE, Compute } };
	AlphaLayout = CreateLayout(Compute, Ranges, 3);
	AlphaBuildShader = DEV->_CreateCS("common\\scene_alpha_build");
	nri::ComputePipelineDesc PipelineDesc = {};
	PipelineDesc.pipelineLayout = AlphaLayout;
	PipelineDesc.shader = ShaderCode(AlphaBuildShader->sh, Compute);
	Require(Api.CreateComputePipeline(Gpu, PipelineDesc, AlphaPipeline), "The graphics device could not create the scene alpha pipeline.");

	if (!Wholes.empty())
	{
		ShaderExternalMap Defines;
		Defines["USE_WHOLE"] = "1";
		const ref_cs WholeShader = DEV->_CreateCS("common\\scene_alpha_build", Defines);
		PipelineDesc.shader = ShaderCode(WholeShader->sh, Compute);
		Require(Api.CreateComputePipeline(Gpu, PipelineDesc, AlphaWholePipeline), "The graphics device could not create the scene alpha pipeline.");
	}

	// A set a mip of the whole array, then one a mip of each slice of a whole copy
	const u32 Sets = AlphaMips * (1 + u32(Wholes.size()));
	nri::DescriptorPoolDesc PoolDesc = {};
	PoolDesc.descriptorSetMaxNum = Sets;
	PoolDesc.structuredBufferMaxNum = Sets;
	PoolDesc.textureMaxNum = Sets * GpuTextures::MaxBuckets;
	PoolDesc.storageTextureMaxNum = Sets;
	Api.CreateDescriptorPool(Gpu, PoolDesc, AlphaPool);

	nri::Descriptor* SourceView = MakeView(AlphaSources, nri::BufferView::STRUCTURED_BUFFER);
	nri::Descriptor* Buckets[GpuTextures::MaxBuckets];

	for (u32 Bucket = 0; Bucket < GpuTextures::MaxBuckets; ++Bucket)
	{
		Buckets[Bucket] = Textures.View(Bucket);
	}

	nri::TextureBarrierDesc Barrier = {};
	Barrier.texture = Alpha;
	Barrier.after = { nri::AccessBits::SHADER_RESOURCE_STORAGE, nri::Layout::SHADER_RESOURCE_STORAGE, Compute };
	Barrier.mipNum = nri::REMAINING;
	Barrier.layerNum = nri::REMAINING;
	nri::BarrierDesc Barriers = {};
	Barriers.textures = &Barrier;
	Barriers.textureNum = 1;

	nri::CommandBuffer& Commands = *BeginNriCommands(Wholes.data(), u32(Wholes.size()), nullptr, 0, nullptr);
	Api.CmdBarrier(Commands, Barriers);
	Api.CmdSetDescriptorPool(Commands, *AlphaPool);
	Api.CmdSetPipelineLayout(Commands, nri::BindPoint::COMPUTE, *AlphaLayout);
	Api.CmdSetPipeline(Commands, *AlphaPipeline);

	for (u32 Mip = 0; Mip < AlphaMips; ++Mip)
	{
		nri::TextureViewDesc View = {};
		View.texture = Alpha;
		View.type = nri::TextureView::STORAGE_TEXTURE_ARRAY;
		View.mipOffset = nri::Dim_t(Mip);
		View.mipNum = 1;
		View.layerNum = nri::REMAINING;
		Api.CreateTextureView(View, AlphaMipViews[Mip]);

		nri::DescriptorSet* Set = nullptr;
		Api.AllocateDescriptorSets(*AlphaPool, *AlphaLayout, 0, &Set, 1, 0);
		const nri::UpdateDescriptorRangeDesc Updates[3] = { { Set, 0, 0, &SourceView, 1 }, { Set, 1, 0, Buckets, GpuTextures::MaxBuckets }, { Set, 2, 0, &AlphaMipViews[Mip], 1 } };
		Api.UpdateDescriptorRanges(Updates, 3);

		nri::SetDescriptorSetDesc SetDesc = {};
		SetDesc.descriptorSet = Set;
		Api.CmdSetDescriptorSet(Commands, SetDesc);
		const u32 Groups = ((AlphaSize >> Mip) + 7) / 8;

		if (!Refs.empty())
		{
			Api.CmdDispatch(Commands, { Groups, Groups, u32(Refs.size()) });
		}
	}

	// A whole copy is read as the texture it is, into a view that starts at its slice
	nri::Descriptor* Inputs[GpuTextures::MaxBuckets];
	std::copy(std::begin(Buckets), std::end(Buckets), Inputs);
	xr_vector<nri::Texture*> Wrapped;
	xr_vector<nri::Descriptor*> Made;

	if (AlphaWholePipeline)
	{
		Api.CmdSetPipeline(Commands, *AlphaWholePipeline);
	}

	for (u32 Index = 0; Index < Wholes.size(); ++Index)
	{
		nri::TextureViewDesc Read = {};
		Read.texture = Wrapped.emplace_back(WrapNriSurface(Wholes[Index]));
		Read.type = nri::TextureView::TEXTURE;
		Read.mipNum = nri::REMAINING;
		Read.layerNum = 1;
		Api.CreateTextureView(Read, Inputs[0]);
		Made.push_back(Inputs[0]);

		for (u32 Mip = 0; Mip < AlphaMips; ++Mip)
		{
			nri::TextureViewDesc Write = {};
			Write.texture = Alpha;
			Write.type = nri::TextureView::STORAGE_TEXTURE_ARRAY;
			Write.mipOffset = nri::Dim_t(Mip);
			Write.mipNum = 1;
			Write.layerOffset = nri::Dim_t(Refs.size() + Index);
			Write.layerNum = 1;
			nri::Descriptor* Layer = nullptr;
			Api.CreateTextureView(Write, Layer);
			Made.push_back(Layer);

			nri::DescriptorSet* Set = nullptr;
			Api.AllocateDescriptorSets(*AlphaPool, *AlphaLayout, 0, &Set, 1, 0);
			const nri::UpdateDescriptorRangeDesc Updates[3] = { { Set, 0, 0, &SourceView, 1 }, { Set, 1, 0, Inputs, GpuTextures::MaxBuckets }, { Set, 2, 0, &Layer, 1 } };
			Api.UpdateDescriptorRanges(Updates, 3);

			nri::SetDescriptorSetDesc SetDesc = {};
			SetDesc.descriptorSet = Set;
			Api.CmdSetDescriptorSet(Commands, SetDesc);
			const u32 Groups = ((AlphaSize >> Mip) + 7) / 8;
			Api.CmdDispatch(Commands, { Groups, Groups, 1 });
		}
	}

	Barrier.before = Barrier.after;
	Barrier.after = { nri::AccessBits::SHADER_RESOURCE, nri::Layout::SHADER_RESOURCE, nri::StageBits::FRAGMENT_SHADER };
	Api.CmdBarrier(Commands, Barriers);
	EndNriCommands(&Commands);

	// The sets hold what they were given, and the commands ran or were recorded with them
	for (nri::Descriptor* Item : Made)
	{
		Api.DestroyDescriptor(Item);
	}

	for (nri::Texture* Item : Wrapped)
	{
		Api.DestroyTexture(Item);
	}

	nri::TextureViewDesc View = {};
	View.texture = Alpha;
	View.type = nri::TextureView::TEXTURE_ARRAY;
	View.mipNum = nri::REMAINING;
	View.layerNum = nri::REMAINING;
	Api.CreateTextureView(View, AlphaView);
	const u64 Slices = Refs.size() + Wholes.size();
	Msg("* [scene] alpha array: %u slices of %u (%u of whole copies), %u MB", u32(Slices), AlphaSize, u32(Wholes.size()), u32(Slices * AlphaSize * AlphaSize * 4 / 3 >> 20));
}

// The cards' table and their cull; the visible list has their room past everything else (CreatePasses)
void CreateCards(const xr_vector<Hlsl::SceneCard>& Records)
{
	nri::Device& Gpu = *GRHI->Nri->Device;
	const nri::StageBits Compute = nri::StageBits::COMPUTE_SHADER;
	CardTable = CreateTable(Records, "Scene LOD cards");
	CardArgs.Buffer = CreateBuffer(4 * sizeof(u32), 0, nri::BufferUsageBits::SHADER_RESOURCE_STORAGE | nri::BufferUsageBits::ARGUMENT, "Scene LOD card draw");

	const nri::DescriptorRangeDesc Ranges[3] = { { 0, 1, nri::DescriptorType::STRUCTURED_BUFFER, Compute }, { 1, 1, nri::DescriptorType::TEXTURE, Compute, nri::DescriptorRangeBits::PARTIALLY_BOUND },
		{ 0, 2, nri::DescriptorType::STORAGE_BUFFER, Compute } };
	CardLayout = CreateLayout(Compute, Ranges, 3);
	const ref_cs Cull = DEV->_CreateCS("common\\scene_cards");
	nri::ComputePipelineDesc PipelineDesc = {};
	PipelineDesc.pipelineLayout = CardLayout;
	PipelineDesc.shader = ShaderCode(Cull->sh, Compute);
	Require(Api.CreateComputePipeline(Gpu, PipelineDesc, CardCull), "The graphics device could not create the scene card pipeline.");

	nri::DescriptorPoolDesc PoolDesc = {};
	PoolDesc.descriptorSetMaxNum = 1;
	PoolDesc.structuredBufferMaxNum = 1;
	PoolDesc.textureMaxNum = 1;
	PoolDesc.storageBufferMaxNum = 2;
	Api.CreateDescriptorPool(Gpu, PoolDesc, CardPool);
	Api.AllocateDescriptorSets(*CardPool, *CardLayout, 0, &CardSet, 1, 0);

	nri::Descriptor* Table = MakeView(CardTable, nri::BufferView::STRUCTURED_BUFFER);
	nri::Descriptor* Outputs[2] = { MakeView(CardArgs.Buffer, nri::BufferView::STORAGE_BUFFER, nri::Format::R32_UINT), MakeView(Visible.Buffer, nri::BufferView::STORAGE_BUFFER, nri::Format::R32_UINT) };
	const nri::UpdateDescriptorRangeDesc Updates[2] = { { CardSet, 0, 0, &Table, 1 }, { CardSet, 2, 0, Outputs, 2 } };
	Api.UpdateDescriptorRanges(Updates, 2);

	// A pyramid made before the level loaded holds for it too
	if (HizRead)
	{
		const nri::UpdateDescriptorRangeDesc Pyramid = { CardSet, 1, 0, &HizRead, 1 };
		Api.UpdateDescriptorRanges(&Pyramid, 1);
	}
}

// The cubes as one cube array of the largest face up to EnvSize, every mip of a cube taken from the mip of its source
// that matches (scene_env_build.cs.hlsl). A cube that cannot be read as six faces of a plain format gets no slot
void BuildEnv(const xr_vector<CTexture*>& Cubes)
{
	xr_vector<IRHISurface*> Surfaces;
	u32 Size = 16;

	for (CTexture* Cube : Cubes)
	{
		Cube->EnsureLoaded();
		IRHISurface* Surface = Cube->surface_get();

		if (Surface && Surface->GetArraySize() == 6 && Surface->GetWidth() == Surface->GetHeight() && Surface->GetSampleDescCount() <= 1 &&
			nri::nriConvertDXGIFormatToNRI(u32(Surface->GetFormat())) != nri::Format::UNKNOWN)
		{
			EnvSlots.emplace(Cube, u32(Surfaces.size()));
			Surfaces.push_back(Surface);
			Size = std::max(Size, std::min(std::bit_floor(Surface->GetWidth()), EnvSize));
		}
		else if (Surface)
		{
			Surface->Release();
		}
	}

	Msg("* [scene] environment cubes: %u of %u in the cube array, %u texels a face", u32(Surfaces.size()), u32(Cubes.size()), Surfaces.empty() ? 0u : Size);

	if (Surfaces.empty())
	{
		return;
	}

	nri::Device& Gpu = *GRHI->Nri->Device;
	const nri::StageBits Compute = nri::StageBits::COMPUTE_SHADER;
	const u32 Mips = u32(std::bit_width(Size));
	nri::TextureDesc Desc = {};
	Desc.type = nri::TextureType::TEXTURE_2D;
	Desc.usage = nri::TextureUsageBits::SHADER_RESOURCE | nri::TextureUsageBits::SHADER_RESOURCE_STORAGE;
	Desc.format = nri::Format::RGBA16_SFLOAT;
	Desc.width = nri::Dim_t(Size);
	Desc.height = nri::Dim_t(Size);
	Desc.depth = 1;
	Desc.mipNum = nri::Dim_t(Mips);
	Desc.layerNum = nri::Dim_t(Surfaces.size() * 6);
	Desc.sampleNum = 1;
	Require(Api.CreateCommittedTexture(Gpu, nri::MemoryLocation::DEVICE, 0.0f, Desc, EnvArray), "The graphics device could not create the scene environment array.");
	Api.SetDebugName(EnvArray, "Scene environment cubes");

	const nri::DescriptorRangeDesc Ranges[2] = { { 0, 1, nri::DescriptorType::TEXTURE, Compute }, { 0, 1, nri::DescriptorType::STORAGE_TEXTURE, Compute } };
	EnvLayout = CreateLayout(Compute, Ranges, 2);
	const ref_cs Build = DEV->_CreateCS("common\\scene_env_build");
	nri::ComputePipelineDesc PipelineDesc = {};
	PipelineDesc.pipelineLayout = EnvLayout;
	PipelineDesc.shader = ShaderCode(Build->sh, Compute);
	Require(Api.CreateComputePipeline(Gpu, PipelineDesc, EnvPipeline), "The graphics device could not create the scene environment pipeline.");

	const u32 Sets = Mips * u32(Surfaces.size());
	nri::DescriptorPoolDesc PoolDesc = {};
	PoolDesc.descriptorSetMaxNum = Sets;
	PoolDesc.textureMaxNum = Sets;
	PoolDesc.storageTextureMaxNum = Sets;
	Api.CreateDescriptorPool(Gpu, PoolDesc, EnvPool);

	nri::TextureBarrierDesc Barrier = {};
	Barrier.texture = EnvArray;
	Barrier.after = { nri::AccessBits::SHADER_RESOURCE_STORAGE, nri::Layout::SHADER_RESOURCE_STORAGE, Compute };
	Barrier.mipNum = nri::REMAINING;
	Barrier.layerNum = nri::REMAINING;
	nri::BarrierDesc Barriers = {};
	Barriers.textures = &Barrier;
	Barriers.textureNum = 1;

	nri::CommandBuffer& Commands = *BeginNriCommands(Surfaces.data(), u32(Surfaces.size()), nullptr, 0, nullptr);
	Api.CmdBarrier(Commands, Barriers);
	Api.CmdSetDescriptorPool(Commands, *EnvPool);
	Api.CmdSetPipelineLayout(Commands, nri::BindPoint::COMPUTE, *EnvLayout);
	Api.CmdSetPipeline(Commands, *EnvPipeline);
	xr_vector<nri::Texture*> Wrapped;
	xr_vector<nri::Descriptor*> Made;

	for (u32 Slot = 0; Slot < Surfaces.size(); ++Slot)
	{
		nri::TextureViewDesc Read = {};
		Read.texture = Wrapped.emplace_back(WrapNriSurface(Surfaces[Slot]));
		Read.type = nri::TextureView::TEXTURE_ARRAY;
		Read.mipNum = nri::REMAINING;
		Read.layerNum = 6;
		nri::Descriptor* Source = nullptr;
		Api.CreateTextureView(Read, Source);
		Made.push_back(Source);

		for (u32 Mip = 0; Mip < Mips; ++Mip)
		{
			nri::TextureViewDesc Write = {};
			Write.texture = EnvArray;
			Write.type = nri::TextureView::STORAGE_TEXTURE_ARRAY;
			Write.mipOffset = nri::Dim_t(Mip);
			Write.mipNum = 1;
			Write.layerOffset = nri::Dim_t(Slot * 6);
			Write.layerNum = 6;
			nri::Descriptor* Faces = nullptr;
			Api.CreateTextureView(Write, Faces);
			Made.push_back(Faces);

			nri::DescriptorSet* Set = nullptr;
			Api.AllocateDescriptorSets(*EnvPool, *EnvLayout, 0, &Set, 1, 0);
			const nri::UpdateDescriptorRangeDesc Updates[2] = { { Set, 0, 0, &Source, 1 }, { Set, 1, 0, &Faces, 1 } };
			Api.UpdateDescriptorRanges(Updates, 2);

			nri::SetDescriptorSetDesc SetDesc = {};
			SetDesc.descriptorSet = Set;
			Api.CmdSetDescriptorSet(Commands, SetDesc);
			const u32 Groups = ((Size >> Mip) + 7) / 8;
			Api.CmdDispatch(Commands, { Groups, Groups, 6 });
		}
	}

	Barrier.before = Barrier.after;
	Barrier.after = { nri::AccessBits::SHADER_RESOURCE, nri::Layout::SHADER_RESOURCE, nri::StageBits::FRAGMENT_SHADER };
	Api.CmdBarrier(Commands, Barriers);
	EndNriCommands(&Commands);

	// The sets hold what they were given, and the commands ran or were recorded with them
	for (nri::Descriptor* Item : Made)
	{
		Api.DestroyDescriptor(Item);
	}

	for (nri::Texture* Item : Wrapped)
	{
		Api.DestroyTexture(Item);
	}

	for (IRHISurface* Surface : Surfaces)
	{
		Surface->Release();
	}

	nri::TextureViewDesc View = {};
	View.texture = EnvArray;
	View.type = nri::TextureView::TEXTURE_CUBE_ARRAY;
	View.mipNum = nri::REMAINING;
	View.layerNum = nri::REMAINING;
	Api.CreateTextureView(View, EnvArrayView);
}
} // namespace GpuSceneInternal

void GpuScene::CreatePasses(const xr_vector<Hlsl::SceneCluster>& Records, const xr_vector<Hlsl::SceneClusterLod>& Lods, const xr_vector<u32>& Words)
{
	if (Records.empty())
	{
		return;
	}

	ClusterCount = u32(Records.size());
	nri::Device& Gpu = *GRHI->Nri->Device;

	const nri::StageBits Compute = nri::StageBits::COMPUTE_SHADER;
	const nri::StageBits Vertex = nri::StageBits::VERTEX_SHADER;
	const nri::StageBits Pixel = nri::StageBits::FRAGMENT_SHADER;
	const nri::DescriptorRangeDesc CullRanges[6] = { { 0, 4, nri::DescriptorType::STRUCTURED_BUFFER, Compute }, { 0, 5, nri::DescriptorType::STORAGE_BUFFER, Compute }, { 4, 1, nri::DescriptorType::TEXTURE, Compute, nri::DescriptorRangeBits::PARTIALLY_BOUND },
		{ 5, 1, nri::DescriptorType::STRUCTURED_BUFFER, Compute }, { 6, 2, nri::DescriptorType::BUFFER, Compute }, { 8, 1, nri::DescriptorType::STRUCTURED_BUFFER, Compute } };
	const nri::DescriptorRangeDesc DensityRanges[4] = { { 0, 2, nri::DescriptorType::STRUCTURED_BUFFER, Compute }, { 10, 1, nri::DescriptorType::BUFFER, Compute }, { 64 - PageSlots, PageSlots, nri::DescriptorType::STRUCTURED_BUFFER, Compute },
		{ 0, 1, nri::DescriptorType::STORAGE_BUFFER, Compute } };
	const nri::DescriptorRangeDesc ChunkRanges[4] = { { 0, 1, nri::DescriptorType::STRUCTURED_BUFFER, Compute }, { 5, 2, nri::DescriptorType::STORAGE_BUFFER, Compute }, { 1, 1, nri::DescriptorType::TEXTURE, Compute, nri::DescriptorRangeBits::PARTIALLY_BOUND },
		{ 2, 1, nri::DescriptorType::STRUCTURED_BUFFER, Compute } };
	// The visible lists and the skinned vertices are written again while a set with them is still on the command list
	const nri::DescriptorRangeDesc DrawRanges[4] = { { 0, 5 + PageSlots, nri::DescriptorType::STRUCTURED_BUFFER, Vertex, nri::DescriptorRangeBits::ALLOW_UPDATE_AFTER_SET }, { 5 + PageSlots, 2 + PageSlots, nri::DescriptorType::BUFFER, Vertex, nri::DescriptorRangeBits::ALLOW_UPDATE_AFTER_SET }, { 0, 1, nri::DescriptorType::TEXTURE, Pixel },
		{ 7 + 2 * PageSlots, 1, nri::DescriptorType::STRUCTURED_BUFFER, Vertex } };

	CullLayout = CreateLayout(Compute, CullRanges, 6);
	RangeLayout = CreateLayout(Compute, ChunkRanges, 4);
	DensityLayout = CreateLayout(Compute, DensityRanges, 4);
	DrawLayout = CreateLayout(Vertex | Pixel, DrawRanges, 4);

	if (Api.GetDeviceDesc(Gpu).features.meshShader && nri::nriGetInterface(Gpu, NRI_INTERFACE(nri::MeshShaderInterface), &MeshApi) == nri::Result::SUCCESS)
	{
		// The same ranges for the other stage: a set of DrawLayout is bound to either
		nri::DescriptorRangeDesc MeshRanges[4];
		std::copy_n(DrawRanges, 4, MeshRanges);

		for (nri::DescriptorRangeDesc& Range : MeshRanges)
		{
			Range.shaderStages = Range.shaderStages == Vertex ? nri::StageBits::MESH_SHADER : Range.shaderStages;
		}

		MeshLayout = CreateLayout(nri::StageBits::MESH_SHADER | Pixel, MeshRanges, 4);
	}

	CullShader = DEV->_CreateCS("common\\scene_cull");
	nri::ComputePipelineDesc CullDesc = {};
	CullDesc.pipelineLayout = CullLayout;
	CullDesc.shader = ShaderCode(CullShader->sh, Compute);
	Api.CreateComputePipeline(Gpu, CullDesc, CullPipeline);

	RangeShader = DEV->_CreateCS("common\\scene_ranges");
	CullDesc.pipelineLayout = RangeLayout;
	CullDesc.shader = ShaderCode(RangeShader->sh, Compute);
	Api.CreateComputePipeline(Gpu, CullDesc, RangePipeline);

	DensityShader = DEV->_CreateCS("common\\scene_density");
	CullDesc.pipelineLayout = DensityLayout;
	CullDesc.shader = ShaderCode(DensityShader->sh, Compute);
	Api.CreateComputePipeline(Gpu, CullDesc, DensityPipeline);

	nri::StreamerDesc StreamerDesc = {};
	StreamerDesc.constantBufferMemoryLocation = nri::MemoryLocation::HOST_UPLOAD;
	StreamerDesc.constantBufferSize = 8 << 20;
	StreamerDesc.dynamicBufferMemoryLocation = nri::MemoryLocation::HOST_UPLOAD;
	// Its dynamic buffer only feeds texture uploads, which D3D11 makes by reading the buffer back on the CPU: with any
	// usage NRI makes it a write-only buffer there, and a streamed head then took 28 to 130 ms of the frame to read
	StreamerDesc.dynamicBufferDesc.usage = nri::BufferUsageBits::NONE;
	StreamerDesc.queuedFrameNum = FrameCount - 1;
	StreamApi.CreateStreamer(Gpu, StreamerDesc, Streamer);

	nri::BufferViewDesc Block = {};
	Block.buffer = StreamApi.GetStreamerConstantBuffer(*Streamer);
	Block.type = nri::BufferView::CONSTANT_BUFFER;
	Block.size = (sizeof(Hlsl::SceneView) + 255) & ~255u;
	Api.CreateBufferView(Block, ViewBlock);

	Clusters = CreateTable(Records, "Scene clusters");

	// A run of clusters of one instance, and the sphere the first level of the cull tests in their place: an
	// instance's own sphere does not hold its clusters. A mesh's clusters are all its levels of detail, one after
	// another: a run ends where the errors leave a factor of two, so that a view drops the levels it does not draw
	// by the run. Where it is cut changes only how much is dropped: the bounds of a run hold whatever is in it. A
	// thread of the first level walks a run: no more clusters than a mask of two words marks (SCENE_CHUNK_CLUSTERS)
	const u32 RunClusters = 32;
	xr_vector<Hlsl::SceneRange> RangeRecords;
	// A list entry a cluster, the dynamic meshes of a depth view or the models of the forward frame's main view
	// among them, after the two counts
	const u32 ChunkCount = 2 + SCENE_MODELS + u32(Records.size());

	for (u32 First = 0; First < Records.size();)
	{
		u32 Last = First;
		Fvector3 Centre = { 0.0f, 0.0f, 0.0f };
		float Own[2] = { FLT_MAX, 0.0f };
		float Coarser[2] = { FLT_MAX, 0.0f };

		for (; Last < Records.size() && Records[Last].instance == Records[First].instance; ++Last)
		{
			const auto& Errors = Lods[Last].errors;
			const float Spans[4] = { std::min(Own[0], Errors.x), std::max(Own[1], Errors.x), std::min(Coarser[0], Errors.y), std::max(Coarser[1], Errors.y) };

			if (Last - First == SCENE_CHUNK_CLUSTERS || (Last - First >= RunClusters && (Spans[1] > 2.0f * Spans[0] || Spans[3] > 2.0f * Spans[2])))
			{
				break;
			}

			Own[0] = Spans[0];
			Own[1] = Spans[1];
			Coarser[0] = Spans[2];
			Coarser[1] = Spans[3];
			Centre.add(Fvector3().set(Records[Last].bounds.x, Records[Last].bounds.y, Records[Last].bounds.z));
		}

		Centre.div(float(Last - First));
		float Radius = 0.0f;
		// How much nearer than the run's centre its own spheres reach, and how much farther the coarser ones
		float Nearer = -FLT_MAX;
		float Farther = 0.0f;

		for (u32 Index = First; Index < Last; ++Index)
		{
			const Hlsl::SceneClusterLod& Lod = Lods[Index];
			Radius = std::max(Radius, Centre.distance_to(Fvector3().set(Records[Index].bounds.x, Records[Index].bounds.y, Records[Index].bounds.z)) + Records[Index].bounds.w);
			Nearer = std::max(Nearer, Centre.distance_to(Fvector3().set(Lod.self.x, Lod.self.y, Lod.self.z)) - Lod.self.w);
			Farther = std::max(Farther, Centre.distance_to(Fvector3().set(Lod.parent.x, Lod.parent.y, Lod.parent.z)) + Lod.parent.w);
		}

		Hlsl::SceneRange Range = {};
		Range.bounds.set(Centre.x, Centre.y, Centre.z, Radius);
		Range.detail.set(Own[0], Nearer, Coarser[1], Farther);
		Range.first = First;
		Range.count = Last - First;
		Range.flags = (Records[First].triangles & SCENE_CLUSTER_ALPHA) ? SCENE_CHUNK_ALPHA : 0u;
		RangeRecords.push_back(Range);
		First = Last;
	}

	// A chunk names its first cluster below SCENE_CHUNK_SHIFT, the frame's models after the level's
	R_ASSERT(Records.size() + SCENE_MODELS <= (1u << SCENE_CHUNK_SHIFT));
	RangeCount = u32(RangeRecords.size());
	Ranges = CreateTable(RangeRecords, "Scene cluster ranges");
	ClusterIndices = CreateTable(Words.data(), Words.size() * sizeof(u32), 0, "Scene cluster pages");

	const nri::BufferUsageBits Storage = nri::BufferUsageBits::SHADER_RESOURCE_STORAGE;
	Counters.Buffer = CreateBuffer((SCENE_COUNTER_WORDS + (Bins.size() + ModelBins) * 4 * (2 + FaceParts)) * sizeof(u32), 0, Storage | nri::BufferUsageBits::ARGUMENT, "Scene counters");
	CardStart = u32(u64(ClusterCount + ModelClusters) * (2 + FaceParts) + SCENE_DYNAMIC_CLUSTERS);
	SortedStart = u32(u64(CardStart) + u64(CardCount) * SCENE_CARD_WORDS);
	Visible.Buffer = CreateBuffer((u64(SortedStart) + SortedEntries) * sizeof(u32), 0, Storage | nri::BufferUsageBits::SHADER_RESOURCE, "Scene visible clusters");
	Seen.Buffer = CreateBuffer(u64(ClusterCount + ModelClusters) * sizeof(u32), 0, Storage, "Scene seen clusters");
	IsSeenCleared = false;
	Chunks.Buffer = CreateBuffer(u64(ChunkCount) * sizeof(u32), 0, Storage | nri::BufferUsageBits::SHADER_RESOURCE, "Scene cull chunks");
	CullGroups.Buffer = CreateBuffer(8 * sizeof(u32), 0, Storage | nri::BufferUsageBits::ARGUMENT, "Scene cull groups");
	Density.Buffer = CreateBuffer(u64(ClusterCount + ModelClusters) * sizeof(float), 0, Storage | nri::BufferUsageBits::SHADER_RESOURCE, "Scene cluster texel density");
	IsDensityBuilt = false;

	Feedback.Buffer = CreateBuffer(FeedbackBytes, 0, Storage, "Scene texture feedback");
	nri::BufferViewDesc FeedbackDesc = {};
	FeedbackDesc.buffer = Feedback.Buffer;
	FeedbackDesc.type = nri::BufferView::STORAGE_BUFFER;
	FeedbackDesc.format = nri::Format::R32_UINT;
	Api.CreateBufferView(FeedbackDesc, FeedbackView);

	nri::BufferDesc CopyDesc = {};
	CopyDesc.size = FeedbackBytes;

	for (nri::Buffer*& Copy : FeedbackCopies)
	{
		Api.CreateCommittedBuffer(Gpu, nri::MemoryLocation::HOST_READBACK, 0.0f, CopyDesc, Copy);
		Api.SetDebugName(Copy, "Scene texture feedback copy");
		std::memset(Api.MapBuffer(*Copy, 0, FeedbackBytes), 0, FeedbackBytes);
		Api.UnmapBuffer(*Copy);
	}

	VisibleAlpha.Buffer = CreateBuffer(u64(ClusterCount + ModelClusters) * sizeof(u32), 0, Storage | nri::BufferUsageBits::SHADER_RESOURCE, "Scene visible alpha clusters");
	ModelCapacity = SCENE_MODELS;
	Models.Buffer = CreateBuffer(u64(ModelCapacity) * sizeof(Hlsl::SceneModel), sizeof(Hlsl::SceneModel), nri::BufferUsageBits::SHADER_RESOURCE, "Scene models");
	ModelView = MakeView(Models.Buffer, nri::BufferView::STRUCTURED_BUFFER);
	// The culls of the shadow views read it before the main view first copies into it: that read is its state
	Models.State = { nri::AccessBits::SHADER_RESOURCE, nri::StageBits::COMPUTE_SHADER };

	nri::Descriptor* ClusterView = MakeView(Clusters, nri::BufferView::STRUCTURED_BUFFER);
	nri::Descriptor* InstanceView = MakeView(Instances, nri::BufferView::STRUCTURED_BUFFER);
	nri::Descriptor* CullOutputs[3] = { MakeView(Counters.Buffer, nri::BufferView::STORAGE_BUFFER, nri::Format::R32_UINT), MakeView(Visible.Buffer, nri::BufferView::STORAGE_BUFFER, nri::Format::R32_UINT), MakeView(VisibleAlpha.Buffer, nri::BufferView::STORAGE_BUFFER, nri::Format::R32_UINT) };
	nri::Descriptor* DrawTables[5 + PageSlots] = { ClusterView, MakeView(Transforms, nri::BufferView::STRUCTURED_BUFFER), InstanceView, MakeView(Materials, nri::BufferView::STRUCTURED_BUFFER) };
	nri::Descriptor* CullTables[4] = { InstanceView, ClusterView, DrawTables[3], TreeLightView };
	nri::Descriptor* DrawLists[2 + PageSlots] = { MakeView(ClusterIndices, nri::BufferView::BUFFER, nri::Format::R32_UINT), MakeView(Visible.Buffer, nri::BufferView::BUFFER, nri::Format::R32_UINT) };
	nri::Descriptor* AlphaLists[2 + PageSlots] = { DrawLists[0], MakeView(VisibleAlpha.Buffer, nri::BufferView::BUFFER, nri::Format::R32_UINT) };
	std::copy(DrawTables, DrawTables + 4, SceneTableViews);
	ClusterIndexView = DrawLists[0];
	WrapPages();
	nri::Descriptor* const* Opened = std::find_if(VertexPageViews, VertexPageViews + PageSlots, [](nri::Descriptor* View) { return View != nullptr; });
	nri::Descriptor* Filler = Opened != VertexPageViews + PageSlots ? *Opened : nullptr;

	// A page that is not open is never read, but its slot has to hold a descriptor
	std::replace(VertexPageViews, VertexPageViews + PageSlots, static_cast<nri::Descriptor*>(nullptr), Filler);
	std::replace(IndexPageViews, IndexPageViews + PageSlots, static_cast<nri::Descriptor*>(nullptr), DrawLists[0]);
	std::copy(VertexPageViews, VertexPageViews + PageSlots, DrawTables + 4);
	DrawTables[4 + PageSlots] = Filler;
	std::copy(IndexPageViews, IndexPageViews + PageSlots, DrawLists + 2);
	std::copy(IndexPageViews, IndexPageViews + PageSlots, AlphaLists + 2);
	SkinFiller = Filler;
	BinLimit = Bins.size() + ModelBins;

	nri::DescriptorPoolDesc PoolDesc = {};
	PoolDesc.descriptorSetMaxNum = 5;
	PoolDesc.structuredBufferMaxNum = 10 + PageSlots + 2 * (6 + PageSlots);
	PoolDesc.bufferMaxNum = 3 + 2 * (2 + PageSlots);
	PoolDesc.storageBufferMaxNum = 8;
	PoolDesc.textureMaxNum = 4;
	Api.CreateDescriptorPool(Gpu, PoolDesc, Pool);

	Api.AllocateDescriptorSets(*Pool, *CullLayout, 0, &CullSet, 1, 0);
	Api.AllocateDescriptorSets(*Pool, *RangeLayout, 0, &RangeSet, 1, 0);
	Api.AllocateDescriptorSets(*Pool, *DensityLayout, 0, &DensitySet, 1, 0);
	Api.AllocateDescriptorSets(*Pool, *DrawLayout, 0, &DrawSet, 1, 0);
	Api.AllocateDescriptorSets(*Pool, *DrawLayout, 0, &AlphaSet, 1, 0);

	const nri::UpdateDescriptorRangeDesc Updates[10] = {
		{ CullSet, 0, 0, CullTables, 4 }, { CullSet, 1, 0, CullOutputs, 3 },
		{ DrawSet, 0, 0, DrawTables, 5 + PageSlots }, { DrawSet, 1, 0, DrawLists, 2 + PageSlots },
		{ AlphaSet, 0, 0, DrawTables, 5 + PageSlots }, { AlphaSet, 1, 0, AlphaLists, 2 + PageSlots },
		{ DrawSet, 3, 0, &ModelView, 1 }, { AlphaSet, 3, 0, &ModelView, 1 },
		{ DrawSet, 2, 0, &AlphaView, 1 }, { AlphaSet, 2, 0, &AlphaView, 1 } };
	Api.UpdateDescriptorRanges(Updates, AlphaView ? 10 : 8);

	nri::Descriptor* SeenOutput = MakeView(Seen.Buffer, nri::BufferView::STORAGE_BUFFER, nri::Format::R32_UINT);
	const nri::UpdateDescriptorRangeDesc SeenUpdates[3] = { { CullSet, 1, 3, &SeenOutput, 1 }, { CullSet, 3, 0, &ClusterLodView, 1 }, { CullSet, 5, 0, &ModelView, 1 } };
	Api.UpdateDescriptorRanges(SeenUpdates, 3);

	nri::Descriptor* RangeView = MakeView(Ranges, nri::BufferView::STRUCTURED_BUFFER);
	nri::Descriptor* ChunkRead = MakeView(Chunks.Buffer, nri::BufferView::BUFFER, nri::Format::R32_UINT);
	nri::Descriptor* ChunkOutputs[2] = { MakeView(Chunks.Buffer, nri::BufferView::STORAGE_BUFFER, nri::Format::R32_UINT), MakeView(CullGroups.Buffer, nri::BufferView::STORAGE_BUFFER, nri::Format::R32_UINT) };
	nri::Descriptor* CullLists[2] = { ChunkRead, MakeView(Density.Buffer, nri::BufferView::BUFFER, nri::Format::R32_SFLOAT) };
	nri::Descriptor* DensityTables[2] = { ClusterView, DrawTables[1] };
	nri::Descriptor* DensityOutput = MakeView(Density.Buffer, nri::BufferView::STORAGE_BUFFER, nri::Format::R32_SFLOAT);
	nri::Descriptor* DensityPages[PageSlots];

	// vertex.hlsli counts its pages down from the last register
	for (u32 Page = 0; Page < PageSlots; ++Page)
	{
		DensityPages[Page] = VertexPageViews[PageSlots - 1 - Page];
	}

	const nri::UpdateDescriptorRangeDesc ChunkUpdates[9] = { { RangeSet, 3, 0, &ClusterLodView, 1 }, { CullSet, 4, 0, CullLists, 2 }, { RangeSet, 0, 0, &RangeView, 1 }, { RangeSet, 1, 0, ChunkOutputs, 2 }, { CullSet, 1, 4, &FeedbackView, 1 },
		{ DensitySet, 0, 0, DensityTables, 2 }, { DensitySet, 1, 0, &ClusterIndexView, 1 }, { DensitySet, 2, 0, DensityPages, PageSlots }, { DensitySet, 3, 0, &DensityOutput, 1 } };
	Api.UpdateDescriptorRanges(ChunkUpdates, 9);

	// The forward frame's sun cascades and lamp pages cull into lists of their own, read by sets of their own: written here
	// once, as the shared ones, and again only where those are (WrapPages, WrapSkin, CreateHiZ). Seen and Feedback are
	// bound, never written: a shadow view has no pipeline lists and no second part
	PoolDesc.descriptorSetMaxNum = 4 * OwnViewCount;
	PoolDesc.structuredBufferMaxNum = (8 + 2 * (6 + PageSlots)) * OwnViewCount;
	PoolDesc.bufferMaxNum = (2 + 2 * (2 + PageSlots)) * OwnViewCount;
	PoolDesc.storageBufferMaxNum = 7 * OwnViewCount;
	PoolDesc.textureMaxNum = 4 * OwnViewCount;
	Api.CreateDescriptorPool(Gpu, PoolDesc, OwnPool);

	for (OwnView& Own : OwnViews)
	{
		Own.Counters.Buffer = CreateBuffer((SCENE_COUNTER_WORDS + BinLimit * 4) * sizeof(u32), 0, Storage | nri::BufferUsageBits::ARGUMENT, "Scene shadow view counters");
		Own.Visible.Buffer = CreateBuffer(u64(ClusterCount + ModelClusters) * sizeof(u32), 0, Storage | nri::BufferUsageBits::SHADER_RESOURCE, "Scene shadow view visible clusters");
		Own.VisibleAlpha.Buffer = CreateBuffer(u64(ClusterCount + ModelClusters) * sizeof(u32), 0, Storage | nri::BufferUsageBits::SHADER_RESOURCE, "Scene shadow view visible alpha clusters");
		Own.Chunks.Buffer = CreateBuffer(u64(ChunkCount) * sizeof(u32), 0, Storage | nri::BufferUsageBits::SHADER_RESOURCE, "Scene shadow view cull chunks");
		Own.CullGroups.Buffer = CreateBuffer(8 * sizeof(u32), 0, Storage | nri::BufferUsageBits::ARGUMENT, "Scene shadow view cull groups");
		Api.AllocateDescriptorSets(*OwnPool, *CullLayout, 0, &Own.CullSet, 1, 0);
		Api.AllocateDescriptorSets(*OwnPool, *RangeLayout, 0, &Own.RangeSet, 1, 0);
		Api.AllocateDescriptorSets(*OwnPool, *DrawLayout, 0, &Own.DrawSet, 1, 0);
		Api.AllocateDescriptorSets(*OwnPool, *DrawLayout, 0, &Own.AlphaSet, 1, 0);

		nri::Descriptor* Outputs[5] = { MakeView(Own.Counters.Buffer, nri::BufferView::STORAGE_BUFFER, nri::Format::R32_UINT), MakeView(Own.Visible.Buffer, nri::BufferView::STORAGE_BUFFER, nri::Format::R32_UINT),
			MakeView(Own.VisibleAlpha.Buffer, nri::BufferView::STORAGE_BUFFER, nri::Format::R32_UINT), SeenOutput, FeedbackView };
		nri::Descriptor* Reads[2] = { MakeView(Own.Chunks.Buffer, nri::BufferView::BUFFER, nri::Format::R32_UINT), CullLists[1] };
		nri::Descriptor* Writes[2] = { MakeView(Own.Chunks.Buffer, nri::BufferView::STORAGE_BUFFER, nri::Format::R32_UINT), MakeView(Own.CullGroups.Buffer, nri::BufferView::STORAGE_BUFFER, nri::Format::R32_UINT) };
		nri::Descriptor* OwnLists[2 + PageSlots];
		nri::Descriptor* OwnAlphaLists[2 + PageSlots];
		std::copy(DrawLists, DrawLists + 2 + PageSlots, OwnLists);
		std::copy(AlphaLists, AlphaLists + 2 + PageSlots, OwnAlphaLists);
		OwnLists[1] = MakeView(Own.Visible.Buffer, nri::BufferView::BUFFER, nri::Format::R32_UINT);
		OwnAlphaLists[1] = MakeView(Own.VisibleAlpha.Buffer, nri::BufferView::BUFFER, nri::Format::R32_UINT);

		const nri::UpdateDescriptorRangeDesc OwnUpdates[16] = { { Own.RangeSet, 3, 0, &ClusterLodView, 1 }, { Own.CullSet, 0, 0, CullTables, 4 }, { Own.CullSet, 1, 0, Outputs, 5 }, { Own.CullSet, 3, 0, &ClusterLodView, 1 }, { Own.CullSet, 4, 0, Reads, 2 },
			{ Own.CullSet, 5, 0, &ModelView, 1 }, { Own.RangeSet, 0, 0, &RangeView, 1 }, { Own.RangeSet, 1, 0, Writes, 2 }, { Own.DrawSet, 0, 0, DrawTables, 5 + PageSlots }, { Own.DrawSet, 1, 0, OwnLists, 2 + PageSlots },
			{ Own.DrawSet, 3, 0, &ModelView, 1 }, { Own.AlphaSet, 0, 0, DrawTables, 5 + PageSlots }, { Own.AlphaSet, 1, 0, OwnAlphaLists, 2 + PageSlots }, { Own.AlphaSet, 3, 0, &ModelView, 1 },
			{ Own.DrawSet, 2, 0, &AlphaView, 1 }, { Own.AlphaSet, 2, 0, &AlphaView, 1 } };
		Api.UpdateDescriptorRanges(OwnUpdates, AlphaView ? 16 : 14);
	}
}

void GpuScene::Unload()
{
	ForgetIcons();

	if (!Meshes)
	{
		return;
	}

	// What the old path bound again is the measure of what still draws outside the buckets
	LogWholeCopies();
	DroppedTextures.clear();

	// The old backend owns the queue: its flush covers the passes recorded into its lists
	GRHI->DevicePtr->Flush();

	if (ClusterCount)
	{
		Api.DestroyPipeline(CullPipeline);
		Api.DestroyPipeline(RangePipeline);
		Api.DestroyPipeline(DensityPipeline);

		for (const DepthPipelines& Item : Pipelines)
		{
			Api.DestroyPipeline(Item.Opaque);

			if (Item.Mesh)
			{
				Api.DestroyPipeline(Item.Mesh);
			}

			if (Item.Alpha)
			{
				Api.DestroyPipeline(Item.Alpha);
			}

			for (nri::Pipeline* Tiled : { Item.Tiled, Item.TiledAlpha })
			{
				if (Tiled)
				{
					Api.DestroyPipeline(Tiled);
				}
			}
		}

		Pipelines.clear();
		Api.DestroyPipelineLayout(CullLayout);
		Api.DestroyPipelineLayout(RangeLayout);
		Api.DestroyPipelineLayout(DensityLayout);
		Api.DestroyPipelineLayout(DrawLayout);

		if (MeshLayout)
		{
			Api.DestroyPipelineLayout(MeshLayout);
			MeshLayout = nullptr;
		}
		Api.DestroyDescriptorPool(Pool);

		for (nri::Descriptor* View : Views)
		{
			Api.DestroyDescriptor(View);
		}

		Api.DestroyDescriptor(ViewBlock);

		ReleaseGrass(Grass);
		ReleaseResolve();

		if (GrassLayout)
		{
			Api.DestroyPipeline(GrassCullPipeline);
			Api.DestroyPipelineLayout(GrassLayout);
			Api.DestroyDescriptorPool(GrassPool);
			Api.DestroyDescriptor(GrassBlock);
			GrassBlock = nullptr;
			Api.DestroyDescriptorPool(GrassDrawPool);
			GrassLayout = nullptr;

			for (u32 Slot = 0; Slot < FrameCount && GrassRunCapacity; ++Slot)
			{
				Api.DestroyDescriptor(GrassRunViews[Slot]);
				Api.DestroyBuffer(GrassRunBuffers[Slot]);
				GrassRunBuffers[Slot] = nullptr;
			}

			GrassRunCapacity = 0;
			GrassRunFrame = u32(-1);
			GrassCullShader = nullptr;

			for (const GrassPipeline& Item : GrassPipelines)
			{
				Api.DestroyPipeline(Item.Pipeline);
			}

			GrassPipelines.clear();
			GrassVertex = nullptr;
			GrassPixels = nullptr;
		}

		std::fill(std::begin(IndexPageBuffers), std::end(IndexPageBuffers), nullptr);
		std::fill(std::begin(IndexPageViews), std::end(IndexPageViews), nullptr);
		std::fill(std::begin(VertexPageViews), std::end(VertexPageViews), nullptr);
		std::fill(std::begin(PageSources), std::end(PageSources), nullptr);
		DrawSet = nullptr;
		SkinView = nullptr;
		StreamApi.DestroyStreamer(Streamer);

		for (nri::Buffer* Buffer : { Clusters, ClusterIndices, ClusterLods, Counters.Buffer, Visible.Buffer, VisibleAlpha.Buffer, Models.Buffer, Seen.Buffer, Ranges, Chunks.Buffer, CullGroups.Buffer, Density.Buffer, Feedback.Buffer, FeedbackCopies[0], FeedbackCopies[1], FeedbackCopies[2], FeedbackCopies[3], FeedbackCopies[4], FeedbackCopies[5], FeedbackCopies[6], FeedbackCopies[7] })
		{
			Api.DestroyBuffer(Buffer);
		}

		for (nri::Buffer* Page : Borrowed)
		{
			Api.DestroyBuffer(Page);
		}

		for (OwnView& Own : OwnViews)
		{
			for (nri::Buffer* Buffer : { Own.Counters.Buffer, Own.Visible.Buffer, Own.VisibleAlpha.Buffer, Own.Chunks.Buffer, Own.CullGroups.Buffer })
			{
				if (Buffer)
				{
					Api.DestroyBuffer(Buffer);
				}
			}

			Own.Counters = Own.Visible = Own.VisibleAlpha = Own.Chunks = Own.CullGroups = {};
			Own.CullSet = Own.RangeSet = Own.DrawSet = Own.AlphaSet = nullptr;
		}

		if (OwnPool)
		{
			Api.DestroyDescriptorPool(OwnPool);
			OwnPool = nullptr;
		}

		Views.clear();
		Borrowed.clear();
		Counters = {};
		Seen = {};
		Chunks = {};
		CullGroups = {};
		Density = {};
		Feedback = {};
		Api.DestroyDescriptor(FeedbackView);
		RangeShader = nullptr;
		DensityShader = nullptr;
		GrassStreams.clear();
		ReleaseHiZ();

		if (HizLayout)
		{
			for (u32 Samples = 0; Samples < 2; ++Samples)
			{
				Api.DestroyPipeline(HizPipelines[Samples]);
				HizPipelines[Samples] = nullptr;
				HizShaders[Samples] = nullptr;
			}

			Api.DestroyPipelineLayout(HizLayout);
			HizLayout = nullptr;
		}

		ReleaseForward();

		if (CullStatsCopy)
		{
			Api.DestroyBuffer(CullStatsCopy);
			CullStatsCopy = nullptr;
			CullStatsStep = 0;
		}

		Visible = {};
		VisibleAlpha = {};
		Models = {};
		ModelView = nullptr;
		ModelCapacity = 0;
		Streamer = nullptr;
		CullShader = nullptr;
		std::fill(std::begin(DepthShaders), std::end(DepthShaders), nullptr);
		AlphaPixels = nullptr;
		TileCount = 0;
		ClusterCount = 0;
	}

	if (Alpha)
	{
		for (nri::Descriptor*& View : AlphaMipViews)
		{
			Api.DestroyDescriptor(View);
			View = nullptr;
		}

		Api.DestroyDescriptor(AlphaView);
		Api.DestroyDescriptorPool(AlphaPool);
		Api.DestroyPipeline(AlphaPipeline);

		if (AlphaWholePipeline)
		{
			Api.DestroyPipeline(AlphaWholePipeline);
			AlphaWholePipeline = nullptr;
		}

		Api.DestroyPipelineLayout(AlphaLayout);
		Api.DestroyTexture(Alpha);
		Api.DestroyBuffer(AlphaSources);
		AlphaBuildShader = nullptr;
		AlphaView = nullptr;
		Alpha = nullptr;
	}

	Textures.Destroy();
	Api.DestroyBuffer(Materials);
	Api.DestroyBuffer(Meshes);
	Api.DestroyBuffer(Instances);
	Api.DestroyBuffer(Transforms);

	for (auto& List : Leftover)
	{
		List.clear();
	}
	MainStatics.clear();
	ModelBinIds.clear();
	ModelRejects.clear();
	ModelShaders.clear();
	MaterialIds.clear();
	TextureRefs.clear();
	BoundTextures.clear();
	AlphaSlices.clear();
	EnvSlots.clear();

	if (EnvArray)
	{
		Api.DestroyDescriptor(EnvArrayView);
		Api.DestroyDescriptorPool(EnvPool);
		Api.DestroyPipeline(EnvPipeline);
		Api.DestroyPipelineLayout(EnvLayout);
		Api.DestroyTexture(EnvArray);
		EnvArrayView = nullptr;
		EnvArray = nullptr;
	}
	ModelRecords.clear();
	ModelWorlds[0].clear();
	ModelWorlds[1].clear();
	ModelFrame = u32(-1);
	Meshes = nullptr;
	Instances = nullptr;
	Transforms = nullptr;
	DepthState = nullptr;
	WaveShader = nullptr;
	WaveAlphaShader = nullptr;
}
