#include "stdafx.h"
#include "GpuScene.h"

#include "FHierrarhyVisual.h"
#include "FVisual.h"
#include "FTreeVisual.h"
#include "SkeletonCustom.h"
#include "SkeletonX.h"
#include "NriPass.h"
#include "GpuTextures.h"
#include "dxRenderDeviceRender.h"
#include "r__sector.h"

#include <meshoptimizer.h>

// The engine defines Device as a macro; NRI and NriDevice use it as a name
#undef Device
#include "../../xrRHI/Nri/NriDevice.h"

#include <NRI.h>
#include <Extensions/NRIHelper.h>
#include <Extensions/NRIStreamer.h>
#include <Extensions/NRIWrapperD3D11.h>
#include <Extensions/NRIWrapperD3D12.h>

u32 GetStaticCullSteps(bool Shadow, float Fov, Hlsl::float4* Steps);
void GetTreeWave(Fvector4& Wave, Fvector4& Wind, float& Scale);

namespace
{
constexpr u32 FrameCount = 4;
constexpr u32 VertexStride = 32;
constexpr u32 PageSlots = GeometryHeap::MaxPages;
constexpr size_t ClusterVertices = 192;
// How far the wind may carry a vertex out of its cluster's bounds
constexpr float SwayReach = 1.0f;
constexpr u32 GrassItemSize = 48;
constexpr u32 GrassVariants = 2;

// ERHI_COMPARISON keeps the D3D numbering, which starts at 1
const nri::CompareOp Compare[] = { nri::CompareOp::NONE, nri::CompareOp::NEVER, nri::CompareOp::LESS, nri::CompareOp::EQUAL, nri::CompareOp::LESS_EQUAL, nri::CompareOp::GREATER, nri::CompareOp::NOT_EQUAL, nri::CompareOp::GREATER_EQUAL, nri::CompareOp::ALWAYS };

struct Tracked
{
	nri::Buffer* Buffer = nullptr;
	nri::AccessStage State = {};
};

nri::CoreInterface Api;
nri::HelperInterface Help;
nri::StreamerInterface StreamApi;

nri::PipelineLayout* CullLayout = nullptr;
nri::PipelineLayout* DrawLayout = nullptr;
nri::Pipeline* CullPipeline = nullptr;
struct DepthPipelines
{
	nri::Format Format;
	bool IsSpot;
	nri::Pipeline* Opaque;
	nri::Pipeline* Alpha;
};
xr_vector<DepthPipelines> Pipelines;
nri::DescriptorPool* Pool = nullptr;
nri::DescriptorSet* CullSet = nullptr;
nri::DescriptorSet* DrawSet = nullptr;
nri::DescriptorSet* AlphaSet = nullptr;
nri::Streamer* Streamer = nullptr;
nri::Descriptor* ViewBlock = nullptr;
nri::Descriptor* DynamicBlock = nullptr;
// The skinned output is the old backend's and is replaced when it grows
IRHIBuffer* SkinSource = nullptr;
nri::Buffer* SkinBuffer = nullptr;
nri::Descriptor* SkinView = nullptr;
nri::Descriptor* SkinFiller = nullptr;
nri::Buffer* Clusters = nullptr;
nri::Buffer* ClusterIndices = nullptr;
nri::Buffer* Materials = nullptr;
Tracked Counters, Visible, VisibleAlpha;
GpuTextures Textures;

struct GrassSet
{
	IRHIBuffer* Source = nullptr; // the old backend uploads the instances
	nri::Buffer* Items = nullptr;
	Tracked Args, Visible;
	nri::Descriptor* Views[4] = {}; // instances; arguments and visible list written; visible list read
	nri::DescriptorSet* Set = nullptr;
	u32 Capacity = 0;
	u32 Frame = u32(-1);
};
GrassSet Grass[GrassVariants];
void ReleaseGrass(GrassSet& Set);
nri::Descriptor* MakeSliceView(nri::Texture* Texture, u32 Slice);
nri::PipelineLayout* GrassLayout = nullptr;
nri::Pipeline* GrassCullPipeline = nullptr;
nri::DescriptorPool* GrassPool = nullptr;
nri::Descriptor* GrassBlock = nullptr;
ref_cs GrassCullShader;

nri::Descriptor* VertexPageViews[PageSlots] = {};
nri::Buffer* IndexPageBuffers[PageSlots] = {};

constexpr u32 MaxBlocks = 16;
nri::PipelineLayout* GrassDrawLayout = nullptr;
nri::DescriptorPool* GrassDrawPool = nullptr;
nri::Descriptor* GrassBlockViews[MaxBlocks] = {};
u32 GrassBlockCount = 0;
ref_vs GrassShader;

struct GrassPipeline
{
	nri::Format Format;
	nri::CompareOp Compare;
	RHIObject* Pixels;
	nri::Pipeline* Pipeline;
};
xr_vector<GrassPipeline> GrassPipelines;

struct GrassDrawSet
{
	const GrassSet* Owner;
	IRHISurface* Surface;
	nri::Texture* Texture;
	nri::Descriptor* View;
	nri::DescriptorSet* Set;
};
xr_vector<GrassDrawSet> GrassDrawSets;
bool HasAlpha = false;
xr_vector<nri::Descriptor*> Views;
xr_vector<nri::Buffer*> Borrowed;
ref_cs CullShader;
ref_vs DepthShader, AlphaShader;
ref_ps AlphaPixels;
ref_vs WaveShader, WaveAlphaShader;
u32 ViewOffset = 0;
u32 DynamicOffset = 0;
SState* DepthState = nullptr;

nri::Buffer* CreateBuffer(u64 Size, u32 Stride, nri::BufferUsageBits Usage, const char* Name)
{
	nri::BufferDesc Desc = {};
	Desc.size = Size;
	Desc.structureStride = Stride;
	Desc.usage = Usage;

	nri::Buffer* Buffer = nullptr;
	Api.CreateCommittedBuffer(*GRHI->Nri->Device, nri::MemoryLocation::DEVICE, 0.0f, Desc, Buffer);
	Api.SetDebugName(Buffer, Name);
	return Buffer;
}

// Stride 0 makes a buffer that is read through a typed view
nri::Buffer* CreateTable(const void* Data, size_t Bytes, u32 Stride, const char* Name)
{
	nri::Buffer* Buffer = CreateBuffer(std::max<u64>(Bytes, std::max(Stride, 4u)), Stride, nri::BufferUsageBits::SHADER_RESOURCE, Name);

	if (Bytes)
	{
		nri::BufferUploadDesc Upload = {};
		Upload.data = Data;
		Upload.buffer = Buffer;
		Upload.after.access = nri::AccessBits::SHADER_RESOURCE;
		Help.UploadData(*GRHI->Nri->Queue, nullptr, 0, &Upload, 1);
	}

	return Buffer;
}

template <class T>
nri::Buffer* CreateTable(const xr_vector<T>& Records, const char* Name)
{
	return CreateTable(Records.data(), Records.size() * sizeof(T), sizeof(T), Name);
}

nri::Buffer* WrapBuffer(IRHIBuffer* Source, u32 Stride)
{
	void* Native = GRHI->DevicePtr->ReadExternalBuffer(Source);

	nri::Buffer* Buffer = nullptr;

	if (GRHI->APILevel == ERHI_API_LAYER::D3D12)
	{
		nri::WrapperD3D12Interface Wrapper;
		nri::nriGetInterface(*GRHI->Nri->Device, NRI_INTERFACE(nri::WrapperD3D12Interface), &Wrapper);
		nri::BufferD3D12Desc Desc = {};
		Desc.d3d12Resource = static_cast<ID3D12Resource*>(Native);
		Desc.structureStride = Stride;
		Wrapper.CreateBufferD3D12(*GRHI->Nri->Device, Desc, Buffer);
	}
	else
	{
		nri::WrapperD3D11Interface Wrapper;
		nri::nriGetInterface(*GRHI->Nri->Device, NRI_INTERFACE(nri::WrapperD3D11Interface), &Wrapper);
		nri::BufferD3D11Desc Desc = {};
		Desc.d3d11Resource = static_cast<ID3D11Resource*>(Native);
		Wrapper.CreateBufferD3D11(*GRHI->Nri->Device, Desc, Buffer);
	}

	Borrowed.push_back(Buffer);
	return Buffer;
}

nri::Descriptor* MakeView(nri::Buffer* Buffer, nri::BufferView Type, nri::Format Format = nri::Format::UNKNOWN, u32 Stride = 0)
{
	nri::BufferViewDesc Desc = {};
	Desc.buffer = Buffer;
	Desc.type = Type;
	Desc.format = Format;
	Desc.structureStride = Stride;

	nri::Descriptor* View = nullptr;
	Api.CreateBufferView(Desc, View);
	Views.push_back(View);
	return View;
}

nri::PipelineLayout* CreateLayout(nri::StageBits Stages, const nri::DescriptorRangeDesc* Ranges, u32 RangeCount)
{
	nri::RootDescriptorDesc Roots[2] = {};
	for (u32 Root = 0; Root < 2; ++Root)
	{
		Roots[Root].registerIndex = Root;
		Roots[Root].descriptorType = nri::DescriptorType::CONSTANT_BUFFER;
		Roots[Root].shaderStages = Stages;
	}

	nri::RootSamplerDesc Sampler = {};
	Sampler.desc.filters = { nri::Filter::LINEAR, nri::Filter::LINEAR, nri::Filter::LINEAR };
	Sampler.desc.anisotropy = 1;
	Sampler.desc.mipMax = 16.0f;
	Sampler.desc.addressModes = { nri::AddressMode::REPEAT, nri::AddressMode::REPEAT, nri::AddressMode::REPEAT };
	Sampler.shaderStages = nri::StageBits::FRAGMENT_SHADER;

	nri::DescriptorSetDesc SetDesc = {};
	SetDesc.ranges = Ranges;
	SetDesc.rangeNum = RangeCount;

	nri::PipelineLayoutDesc Desc = {};
	Desc.rootDescriptors = Roots;
	Desc.rootDescriptorNum = 2;
	Desc.rootSamplers = &Sampler;
	Desc.rootSamplerNum = (u32(Stages) & u32(nri::StageBits::FRAGMENT_SHADER)) != 0;
	Desc.descriptorSets = &SetDesc;
	Desc.descriptorSetNum = 1;
	Desc.shaderStages = Stages;

	nri::PipelineLayout* Layout = nullptr;
	Api.CreatePipelineLayout(*GRHI->Nri->Device, Desc, Layout);
	return Layout;
}

nri::ShaderDesc ShaderCode(RHIObject* Shader, nri::StageBits Stage)
{
	const void* Code = nullptr;
	size_t Size = 0;
	const bool HasCode = GRHI->DevicePtr->GetShaderCode(Shader, Code, Size);
	R_ASSERT(HasCode);

	nri::ShaderDesc Desc = {};
	Desc.stage = Stage;
	Desc.bytecode = Code;
	Desc.size = Size;
	return Desc;
}

nri::Pipeline* CreateDepthPipeline(nri::Format Format, bool IsAlpha, bool IsSpot)
{
	static const nri::CullMode Culls[] = { nri::CullMode::NONE, nri::CullMode::NONE, nri::CullMode::FRONT, nri::CullMode::BACK };
	const RHIRasterizerDesc& Raster = DepthState->state->Raster;

	nri::ShaderDesc Shaders[2] = {};

	if (IsAlpha)
	{
		ShaderExternalMap Defines;
		Defines["USE_AREF"] = "1";
		AlphaShader = DEV->_CreateVS("scene_depth", Defines);
		AlphaPixels = DEV->_CreatePS("scene_depth");
		Shaders[0] = ShaderCode(AlphaShader->vs, nri::StageBits::VERTEX_SHADER);
		Shaders[1] = ShaderCode(AlphaPixels->ps, nri::StageBits::FRAGMENT_SHADER);
	}
	else
	{
		DepthShader = DEV->_CreateVS("scene_depth");
		Shaders[0] = ShaderCode(DepthShader->vs, nri::StageBits::VERTEX_SHADER);
	}

	nri::GraphicsPipelineDesc Desc = {};
	Desc.pipelineLayout = DrawLayout;
	Desc.inputAssembly.topology = nri::Topology::TRIANGLE_LIST;
	Desc.rasterization.depthBias = { float(Raster.DepthBias), Raster.DepthBiasClamp, Raster.SlopeScaledDepthBias };
	// A spot shadow map overrides these two (SetupSpotShadowTarget)
	Desc.rasterization.cullMode = IsSpot ? nri::CullMode::BACK : Culls[u32(Raster.CullMode)];
	Desc.rasterization.frontCounterClockwise = Raster.FrontCounterClockwise != 0;
	Desc.rasterization.depthClamp = !Raster.DepthClipEnable;
	Desc.outputMerger.depthStencilFormat = Format;
	Desc.outputMerger.depth.compareOp = IsSpot ? nri::CompareOp::LESS_EQUAL : Compare[u32(DepthState->state->Depth.DepthFunc)];
	Desc.outputMerger.depth.write = true;
	Desc.shaders = Shaders;
	Desc.shaderNum = IsAlpha ? 2 : 1;

	nri::Pipeline* Pipeline = nullptr;
	Api.CreateGraphicsPipeline(*GRHI->Nri->Device, Desc, Pipeline);
	return Pipeline;
}

// +1 when a cluster facing along the view shows only culled faces, -1 when against it, 0 when
// nothing is culled. A cluster's facing is the normal of clockwise triangles, the engine's front.
float ConeSign(bool IsSpot)
{
	const RHIRasterizerDesc& Raster = DepthState->state->Raster;
	if (!IsSpot && Raster.CullMode != RHI_CULL_BACK && Raster.CullMode != RHI_CULL_FRONT)
	{
		return 0.0f;
	}

	return (IsSpot || Raster.CullMode == RHI_CULL_BACK) != (Raster.FrontCounterClockwise != 0) ? 1.0f : -1.0f;
}

void SetBlocks(nri::CommandBuffer& Commands)
{
	nri::SetRootDescriptorDesc Root = {};
	Root.descriptor = ViewBlock;
	Root.offset = ViewOffset;
	Api.CmdSetRootDescriptor(Commands, Root);
	Root.rootDescriptorIndex = 1;
	Root.descriptor = DynamicBlock;
	Root.offset = DynamicOffset;
	Api.CmdSetRootDescriptor(Commands, Root);
}

void Move(nri::CommandBuffer& Commands, Tracked& Item, nri::AccessBits Access, nri::StageBits Stages)
{
	nri::BufferBarrierDesc Barrier = {};
	Barrier.buffer = Item.Buffer;
	Barrier.before = Item.State;
	Item.State = { Access, Stages };
	Barrier.after = Item.State;

	nri::BarrierDesc Desc = {};
	Desc.buffers = &Barrier;
	Desc.bufferNum = 1;
	Api.CmdBarrier(Commands, Desc);
}

enum class ECaster : u8
{
	None,
	Opaque,
	Alpha
};

ECaster ClassifyCaster(const dxRender_Visual& Visual, const char* VertexName, CTexture*& Base)
{
	const u32 BaseSlot = ShaderBindings::Texture("s_base");
	const ShaderElement* Element = Visual.shader->E[ShaderVariant::Shadow]._get();
	if (!Element || Element->passes.size() != 1 || !Element->passes[0] || Element->flags.iPriority / 2 != 0 || Element->flags.bStrictB2F)
	{
		return ECaster::None;
	}

	const SPass& Pass = *Element->passes[0];
	if (!Pass.vs || !Pass.ps || !Pass.state || (Pass.ps->Texture2DSlots & ~(1ull << BaseSlot)) || (Pass.iPriority != u8(-1) && Pass.iPriority / 2 != 0))
	{
		return ECaster::None;
	}

	Base = nullptr;

	if (Pass.ps->Texture2DSlots && Pass.T)
	{
		for (const auto& [Stage, Texture] : *Pass.T)
		{
			if (Stage == CTexture::rstPixel + BaseSlot)
			{
				Base = Texture._get();
			}
		}
	}

	if (Pass.ps->Texture2DSlots && !Base)
	{
		return ECaster::None;
	}

	const auto IsNamed = [](const shared_str& Name, const char* Prefix) { return Name.size() && !strncmp(*Name, Prefix, xr_strlen(Prefix)); };
	if (!IsNamed(Pass.vs->cName, VertexName) || !IsNamed(Pass.ps->cName, "shadow_base"))
	{
		return ECaster::None;
	}

	if (!DepthState)
	{
		DepthState = Pass.state._get();
	}

	if (DepthState != Pass.state._get())
	{
		return ECaster::None;
	}

	return Base ? ECaster::Alpha : ECaster::Opaque;
}

bool IsDynamicCaster(const R_dsgraph::DynamicPacket& Packet)
{
	const SPass& Pass = *Packet.Pass;
	const u32 Type = Packet.Visual->Type;
	if ((Type != MT_NORMAL && Type != MT_SKELETON_GEOMDEF_ST) || !DepthState || !Pass.vs || !Pass.ps || !Pass.state || Pass.ps->Texture2DSlots)
	{
		return false;
	}

	if ((Pass.gs && Pass.gs->gs) || (Pass.hs && Pass.hs->sh) || (Pass.ds && Pass.ds->sh))
	{
		return false;
	}

	const auto IsNamed = [](const shared_str& Name, const char* Prefix) { return Name.size() && !strncmp(*Name, Prefix, xr_strlen(Prefix)); };
	const RHIRasterizerDesc& Raster = Pass.state->state->Raster;
	const RHIRasterizerDesc& Batch = DepthState->state->Raster;

	return IsNamed(Pass.vs->cName, "shadow_model") && IsNamed(Pass.ps->cName, "shadow_base") &&
		Raster.CullMode == Batch.CullMode && Raster.FrontCounterClockwise == Batch.FrontCounterClockwise && Raster.DepthBias == Batch.DepthBias &&
		Raster.SlopeScaledDepthBias == Batch.SlopeScaledDepthBias && Raster.DepthClipEnable == Batch.DepthClipEnable &&
		Pass.state->state->Depth.DepthFunc == DepthState->state->Depth.DepthFunc;
}

u32 TakeDynamic(GfxCmdList& Cmd, R_dsgraph_structure& Graph, Hlsl::SceneDynamic* Records)
{
	CSkeletonX* Skinned[SCENE_DYNAMIC_MESHES];
	u32 Count = 0;

	std::erase_if(Graph.DynamicPasses[0], [&](const R_dsgraph::DynamicPacket& Packet)
	{
		const IRender_Mesh* Mesh = dynamic_cast<const IRender_Mesh*>(Packet.Visual);
		if (Count == SCENE_DYNAMIC_MESHES || !IsDynamicCaster(Packet) || !Mesh || !Mesh->dwPrimitives || Mesh->dwPrimitives > SCENE_DYNAMIC_RUNS * SCENE_CLUSTER_TRIANGLES)
		{
			return false;
		}

		Fmatrix World = Packet.Matrix;
		Skinned[Count] = nullptr;

		if (Packet.Visual->Type == MT_SKELETON_GEOMDEF_ST)
		{
			CSkeletonX& Part = *static_cast<CSkeletonX*>(Packet.Visual);
			if (!Part.Parent)
			{
				return false;
			}

			if (Part.IsSkinned())
			{
				RImplementation.Skinning.Request(Part, Packet.Matrix);
				Skinned[Count] = &Part;
			}
			else
			{
				RImplementation.Skinning.Snapshot(*Part.Parent, Packet.Matrix);
				World.mul_43(Packet.Matrix, Part.Parent->LL_GetBoneInstance(u16(Part.RMS_boneid)).mRenderTransform_tmp);
			}
		}

		Hlsl::SceneDynamic& Record = Records[Count++];
		Record.world = World;
		Record.index_start = Mesh->iBase;
		Record.triangle_count = Mesh->dwPrimitives;
		Record.vertices = Mesh->vBase;
		Record.source = Mesh->Alloc.Page;
		return true;
	});

	RImplementation.Skinning.Flush(Cmd);

	for (u32 Index = 0; Index < Count; ++Index)
	{
		if (Skinned[Index])
		{
			Records[Index].vertices = Skinned[Index]->SkinOutBase;
			Records[Index].source |= SCENE_DYNAMIC_SKINNED;
		}
	}

	return Count;
}

struct MeshKey
{
	u32 Page;
	u32 VertexBase;
	u32 IndexStart;
	u32 IndexCount;

	bool operator==(const MeshKey&) const = default;
};

struct Caster
{
	dxRender_Visual* Visual;
	const IRender_Mesh* Mesh;
	u32 Instance;
	u32 Transform;
	const Fmatrix* World;
	bool IsWaving;
	bool IsAlpha;
	CTexture* Base;
};

struct ClusterCache
{
	// Changes with the layout below and with how a mesh is split
	static constexpr u32 Version = 0x434C5301;

	struct Mesh
	{
		u64 Id;
		u32 First;
		u32 Count;
	};

	struct Cluster
	{
		float Bounds[4];
		float Cone[4];
		u32 IndexStart;
		u32 TriangleCount;
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

u32 BuildClusters(const xr_vector<Caster>& Casters, xr_vector<Hlsl::SceneInstance>& Instances, xr_vector<Hlsl::SceneCluster>& Records, xr_vector<u16>& Indices)
{
	CRender& Render = RImplementation;
	xr_hash_map<u32, const xr_array<u32, 7>*> Sources;

	for (const auto& [Key, Block] : Render.LevelMeshes)
	{
		Sources.emplace(Block.Ref, &Key);
	}

	string_path Name;
	xr_strconcat(Name, "level_cache\\", FS.get_path("$level$")->m_Add, "clusters.cache");
	ClusterCache Stored, Used;
	Stored.Load(Name);

	xr_hash_map<u64, u32> StoredIds;

	for (u32 Index = 0; Index < Stored.Meshes.size(); ++Index)
	{
		StoredIds.emplace(Stored.Meshes[Index].Id, Index);
	}

	xr_hash_map<u32, u32> UsedIds;
	xr_vector<u32> Wide;
	xr_vector<meshopt_Meshlet> Meshlets;
	xr_vector<u32> Corners;
	xr_vector<u8> Triangles;
	u32 Split = 0;

	for (const Caster& Item : Casters)
	{
		const GeometryAlloc& Block = Item.Mesh->Alloc;
		const auto Source = Sources.find(Block.Ref);
		if (Source == Sources.end())
		{
			continue;
		}

		const auto [Slot, IsNew] = UsedIds.try_emplace(Block.Ref, u32(Used.Meshes.size()));
		if (IsNew)
		{
			const xr_array<u32, 7>& Key = *Source->second;
			const CRender::LevelVertexChunk& Chunk = Render.LevelVertices[Key[0]][Key[1]];
			const u8* Vertices = Chunk.Data.data() + size_t(Key[2]) * Chunk.Stride;
			const float* Positions = reinterpret_cast<const float*>(Vertices + Chunk.Position);
			const u16* Narrow = Render.LevelIndices[Key[0]][Key[4]].data() + Key[5];
			const u32 Count = std::min(Key[6], Item.Mesh->dwPrimitives * 3);

			ClusterCache::Mesh Mesh = { u64(crc32(Narrow, Count * sizeof(u16))) << 32 | crc32(Vertices, Key[3] * Chunk.Stride), u32(Used.Clusters.size()), 0 };
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
			}
			else
			{
				Wide.assign(Narrow, Narrow + Count);
				const size_t Bound = meshopt_buildMeshletsBound(Count, ClusterVertices, SCENE_CLUSTER_TRIANGLES);
				Meshlets.resize(Bound);
				Corners.resize(Bound * ClusterVertices);
				Triangles.resize(Bound * SCENE_CLUSTER_TRIANGLES * 3);
				Mesh.Count = u32(meshopt_buildMeshlets(Meshlets.data(), Corners.data(), Triangles.data(), Wide.data(), Count, Positions, Key[3], Chunk.Stride, ClusterVertices, SCENE_CLUSTER_TRIANGLES, 0.25f));
				for (u32 Index = 0; Index < Mesh.Count; ++Index)
				{
					const meshopt_Meshlet& Meshlet = Meshlets[Index];
					const u32* Local = Corners.data() + Meshlet.vertex_offset;
					const u8* Corner = Triangles.data() + Meshlet.triangle_offset;
					const meshopt_Bounds Bounds = meshopt_computeMeshletBounds(Local, Corner, Meshlet.triangle_count, Positions, Key[3], Chunk.Stride);
					Used.Clusters.push_back({ { Bounds.center[0], Bounds.center[1], Bounds.center[2], Bounds.radius }, { Bounds.cone_axis[0], Bounds.cone_axis[1], Bounds.cone_axis[2], Bounds.cone_cutoff }, u32(Used.Indices.size()), Meshlet.triangle_count });

					for (u32 Vertex = 0; Vertex < Meshlet.triangle_count * 3; ++Vertex)
					{
						Used.Indices.push_back(u16(Local[Corner[Vertex]]));
					}
				}

				++Split;
			}

			Used.Meshes.push_back(Mesh);
		}

		const ClusterCache::Mesh& Mesh = Used.Meshes[Slot->second];
		for (u32 Index = 0; Index < Mesh.Count; ++Index)
		{
			const ClusterCache::Cluster& Cluster = Used.Clusters[Mesh.First + Index];
			Hlsl::SceneCluster Record = {};
			Record.bounds.set(Cluster.Bounds[0], Cluster.Bounds[1], Cluster.Bounds[2], Cluster.Bounds[3]);
			Record.cone.set(Cluster.Cone[0], Cluster.Cone[1], Cluster.Cone[2], Cluster.Cone[3]);

			if (Item.World)
			{
				const Fmatrix& World = *Item.World;
				const float Scales[3] = { World.i.magnitude(), World.j.magnitude(), World.k.magnitude() };
				const float Largest = std::max({ Scales[0], Scales[1], Scales[2] });
				Fvector Centre, Axis, Normal;
				World.transform_tiny(Centre, Fvector().set(Cluster.Bounds[0], Cluster.Bounds[1], Cluster.Bounds[2]));
				World.transform_dir(Axis, Fvector().set(Cluster.Cone[0], Cluster.Cone[1], Cluster.Cone[2]));
				Record.bounds.set(Centre.x, Centre.y, Centre.z, Cluster.Bounds[3] * Largest + (Item.IsWaving ? SwayReach : 0.0f));

				// Facing survives a rotation and an even scale only; 2 is a limit no view reaches
				const bool IsEven = std::min({ Scales[0], Scales[1], Scales[2] }) > Largest * 0.99f && Normal.crossproduct(World.i, World.j).dotproduct(World.k) > 0.0f;
				Axis.normalize_safe();
				Record.cone.set(Axis.x, Axis.y, Axis.z, IsEven ? Cluster.Cone[3] : 2.0f);
			}

			Record.instance = Item.Instance;
			Record.index_start = Cluster.IndexStart;
			Record.triangles = Cluster.TriangleCount | (Item.IsWaving ? SCENE_CLUSTER_WAVE : 0u) | (Item.IsAlpha ? SCENE_CLUSTER_ALPHA : 0u) | (Item.Transform << SCENE_CLUSTER_TRANSFORM_SHIFT);
			Record.vertices = (Block.Page << 24) | Block.VertexBase;
			Records.push_back(Record);
		}

		Instances[Item.Instance].flags |= SCENE_INSTANCE_DEPTH_BATCH;
		Item.Visual->IsDepthBatched = true;
		Item.Visual->IsBatchedTree = (Instances[Item.Instance].flags & SCENE_INSTANCE_TREE) != 0;
	}

	if (Split || Used.Meshes.size() != Stored.Meshes.size())
	{
		Used.Save(Name);
	}

	Indices.swap(Used.Indices);
	return Split;
}
} // namespace

template <>
struct std::hash<MeshKey>
{
	size_t operator()(const MeshKey& Key) const noexcept
	{
		return size_t(crc32(&Key, sizeof(Key)));
	}
};

void GpuScene::Load(const xr_vector<dxRender_Visual*>& Visuals, const xr_vector<IRender_Sector*>& Sectors)
{
	nri::nriGetInterface(*GRHI->Nri->Device, NRI_INTERFACE(nri::CoreInterface), &Api);
	nri::nriGetInterface(*GRHI->Nri->Device, NRI_INTERFACE(nri::HelperInterface), &Help);
	nri::nriGetInterface(*GRHI->Nri->Device, NRI_INTERFACE(nri::StreamerInterface), &StreamApi);

	xr_vector<Hlsl::SceneMesh> MeshRecords;
	xr_vector<Hlsl::SceneInstance> InstanceRecords;
	xr_vector<Hlsl::SceneTransform> TransformRecords;
	xr_hash_map<MeshKey, u32> MeshIds;
	xr_hash_map<Shader*, u32> MaterialIds;
	xr_vector<Caster> Casters, Candidates;

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
		const MeshKey Key = { Mesh.Alloc.Page, Mesh.vBase, Mesh.iBase, Mesh.dwPrimitives * 3 };
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
	bool IsFlat = true;

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

			if (FHierrarhyVisual* Node = dynamic_cast<FHierrarhyVisual*>(Visual))
			{
				// A group that shows one of its children by distance cannot be flattened
				IsFlat &= Visual->Type != MT_MESH_LODS;
				Stack.insert(Stack.end(), Node->children.begin(), Node->children.end());
				continue;
			}

			Leaves.emplace_back(Visual, Sector->index);
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
				TransformRecords.push_back(Placement);

				CTexture* Base = nullptr;
				const ECaster Kind = Material ? ClassifyCaster(*Visual, "shadow_lod", Base) : ECaster::None;
				if (Kind != ECaster::None)
				{
					const bool IsWaving = Material->E[ShaderVariant::Shadow]->passes[0]->vs._get() == (Kind == ECaster::Alpha ? WaveAlphaShader : WaveShader)._get();
					(Kind == ECaster::Alpha ? Candidates : Casters).push_back({ Visual, Mesh, u32(InstanceRecords.size()), Record.transform, &Tree->GetTransform(), IsWaving, Kind == ECaster::Alpha, Base });
				}
			}
			else if (Fvisual* Static = dynamic_cast<Fvisual*>(Visual))
			{
				const bool HasFast = Material && Static->m_fast && Static->m_fast->dwPrimitives && Static->fast_usable();
				if (HasFast)
				{
					Record.mesh_depth = FindMesh(*Static->m_fast);
				}

				if (Visual->Type == MT_PROGRESSIVE)
				{
					Record.flags |= SCENE_INSTANCE_PROGRESSIVE;
				}

				// A progressive mesh without the simplified one is drawn at a distance-picked detail
				CTexture* Base = nullptr;
				const ECaster Kind = Material && (HasFast || Visual->Type == MT_NORMAL) ? ClassifyCaster(*Visual, "shadow_base", Base) : ECaster::None;

				// The simplified mesh has no texture coordinates
				if (Kind == ECaster::Opaque)
				{
					Casters.push_back({ Visual, HasFast ? Static->m_fast : Mesh, u32(InstanceRecords.size()), 0, nullptr, false, false, nullptr });
				}
				else if (Kind == ECaster::Alpha && Visual->Type == MT_NORMAL)
				{
					Candidates.push_back({ Visual, Mesh, u32(InstanceRecords.size()), 0, nullptr, false, true, Base });
				}
			}

			InstanceRecords.push_back(Record);
		}
	}

	xr_vector<Hlsl::SceneMaterial> MaterialRecords(MaterialIds.size(), Hlsl::SceneMaterial{ SCENE_TEXTURE_ABSENT, SCENE_TEXTURE_ABSENT, 0, 0 });
	xr_vector<CTexture*> Bases;
	xr_vector<GpuTextures::Ref> Refs;
	xr_hash_map<CTexture*, u32> BaseIds;

	for (const Caster& Item : Candidates)
	{
		if (BaseIds.try_emplace(Item.Base, u32(Bases.size())).second)
		{
			Bases.push_back(Item.Base);
		}
	}

	Textures.Build(Bases, Refs);
	const size_t OpaqueCount = Casters.size();

	for (const Caster& Item : Candidates)
	{
		const GpuTextures::Ref& Base = Refs[BaseIds[Item.Base]];
		if (Base.Tail != GpuTextures::Absent)
		{
			Hlsl::SceneMaterial& Material = MaterialRecords[InstanceRecords[Item.Instance].material];
			Material.base = Base.Tail;
			Material.base_head = Base.Head;
			Material.base_info = Base.Info;
			Casters.push_back(Item);
		}
	}

	HasAlpha = Casters.size() != OpaqueCount;

	xr_vector<Hlsl::SceneCluster> ClusterRecords;
	xr_vector<u16> ClusterCorners;
	CTimer Timer;
	Timer.Start();
	const u32 Split = BuildClusters(Casters, InstanceRecords, ClusterRecords, ClusterCorners);
	const float BuildSeconds = Timer.GetElapsed_sec();

	Meshes = CreateTable(MeshRecords, "Scene meshes");
	Instances = CreateTable(InstanceRecords, "Scene instances");
	Transforms = CreateTable(TransformRecords, "Scene transforms");
	Materials = CreateTable(MaterialRecords, "Scene materials");
	SectorCount = u32(Sectors.size());
	CreatePasses(ClusterRecords, ClusterCorners);

	u32 LeftCount = 0;

	if (IsFlat && ClusterCount)
	{
		Leftover.resize(SectorCount);

		for (const auto& [Visual, Sector] : Leaves)
		{
			if (!Visual->IsDepthBatched && Sector < SectorCount)
			{
				Leftover[Sector].push_back(Visual);
				++LeftCount;
			}
		}
	}

	Msg("* [scene] level visuals outside the depth batch: %u of %u%s", LeftCount, u32(Leaves.size()), Leftover.empty() ? " (sectors are still walked)" : "");

	Msg("* [scene] tables: %u instances of %u visuals, %u meshes, %u transforms, %u materials",
		u32(InstanceRecords.size()), u32(Visuals.size()), u32(MeshRecords.size()), u32(TransformRecords.size()), u32(MaterialIds.size()));
	Msg("* [scene] depth batch: %u casters (%u alpha-tested of %u candidates), %u clusters, %u triangles, %u meshes split, %.2f s",
		u32(Casters.size()), u32(Casters.size() - OpaqueCount), u32(Candidates.size()), ClusterCount, u32(ClusterCorners.size() / 3), Split, BuildSeconds);
}

void GpuScene::CreatePasses(const xr_vector<Hlsl::SceneCluster>& Records, const xr_vector<u16>& Indices)
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
	const nri::DescriptorRangeDesc CullRanges[2] = { { 0, 2, nri::DescriptorType::STRUCTURED_BUFFER, Compute }, { 0, 3, nri::DescriptorType::STORAGE_BUFFER, Compute } };
	const nri::DescriptorRangeDesc DrawRanges[3] = { { 0, 5 + PageSlots, nri::DescriptorType::STRUCTURED_BUFFER, Vertex }, { 5 + PageSlots, 2 + PageSlots, nri::DescriptorType::BUFFER, Vertex }, { 0, GpuTextures::MaxBuckets, nri::DescriptorType::TEXTURE, Pixel } };

	CullLayout = CreateLayout(Compute, CullRanges, 2);
	DrawLayout = CreateLayout(Vertex | Pixel, DrawRanges, 3);

	CullShader = DEV->_CreateCS("scene_cull");
	nri::ComputePipelineDesc CullDesc = {};
	CullDesc.pipelineLayout = CullLayout;
	CullDesc.shader = ShaderCode(CullShader->sh, Compute);
	Api.CreateComputePipeline(Gpu, CullDesc, CullPipeline);

	nri::StreamerDesc StreamerDesc = {};
	StreamerDesc.constantBufferMemoryLocation = nri::MemoryLocation::HOST_UPLOAD;
	StreamerDesc.constantBufferSize = 8 << 20;
	StreamerDesc.dynamicBufferMemoryLocation = nri::MemoryLocation::HOST_UPLOAD;
	StreamerDesc.dynamicBufferDesc.usage = nri::BufferUsageBits::VERTEX;
	StreamerDesc.queuedFrameNum = FrameCount - 1;
	StreamApi.CreateStreamer(Gpu, StreamerDesc, Streamer);

	nri::BufferViewDesc Block = {};
	Block.buffer = StreamApi.GetStreamerConstantBuffer(*Streamer);
	Block.type = nri::BufferView::CONSTANT_BUFFER;
	Block.size = (sizeof(Hlsl::SceneView) + 255) & ~255u;
	Api.CreateBufferView(Block, ViewBlock);
	Block.size = sizeof(Hlsl::SceneDynamic) * SCENE_DYNAMIC_MESHES;
	Api.CreateBufferView(Block, DynamicBlock);

	Clusters = CreateTable(Records, "Scene clusters");
	ClusterIndices = CreateTable(Indices.data(), Indices.size() * sizeof(u16), 0, "Scene cluster indices");

	const nri::BufferUsageBits Storage = nri::BufferUsageBits::SHADER_RESOURCE_STORAGE;
	Counters.Buffer = CreateBuffer(SCENE_COUNTER_WORDS * sizeof(u32), 0, Storage | nri::BufferUsageBits::ARGUMENT, "Scene counters");
	Visible.Buffer = CreateBuffer((u64(ClusterCount) + SCENE_DYNAMIC_CLUSTERS) * sizeof(u32), 0, Storage | nri::BufferUsageBits::SHADER_RESOURCE, "Scene visible clusters");
	VisibleAlpha.Buffer = CreateBuffer(u64(ClusterCount) * sizeof(u32), 0, Storage | nri::BufferUsageBits::SHADER_RESOURCE, "Scene visible alpha clusters");

	nri::Descriptor* ClusterView = MakeView(Clusters, nri::BufferView::STRUCTURED_BUFFER);
	nri::Descriptor* InstanceView = MakeView(Instances, nri::BufferView::STRUCTURED_BUFFER);
	nri::Descriptor* CullTables[2] = { InstanceView, ClusterView };
	nri::Descriptor* CullOutputs[3] = { MakeView(Counters.Buffer, nri::BufferView::STORAGE_BUFFER, nri::Format::R32_UINT), MakeView(Visible.Buffer, nri::BufferView::STORAGE_BUFFER, nri::Format::R32_UINT), MakeView(VisibleAlpha.Buffer, nri::BufferView::STORAGE_BUFFER, nri::Format::R32_UINT) };
	nri::Descriptor* DrawTables[5 + PageSlots] = { ClusterView, MakeView(Transforms, nri::BufferView::STRUCTURED_BUFFER), InstanceView, MakeView(Materials, nri::BufferView::STRUCTURED_BUFFER) };
	nri::Descriptor* DrawLists[2 + PageSlots] = { MakeView(ClusterIndices, nri::BufferView::BUFFER, nri::Format::R16_UINT), MakeView(Visible.Buffer, nri::BufferView::BUFFER, nri::Format::R32_UINT) };
	nri::Descriptor* AlphaLists[2 + PageSlots] = { DrawLists[0], MakeView(VisibleAlpha.Buffer, nri::BufferView::BUFFER, nri::Format::R32_UINT) };
	nri::Descriptor* Buckets[GpuTextures::MaxBuckets];

	for (u32 Bucket = 0; Bucket < GpuTextures::MaxBuckets; ++Bucket)
	{
		Buckets[Bucket] = Textures.View(Bucket);
	}

	nri::Descriptor* Filler = nullptr;

	for (u32 Page = 0; Page < RImplementation.Geometry.PageCount(); ++Page)
	{
		if (IRHIBuffer* Source = RImplementation.Geometry.VertexBuffer(Page))
		{
			DrawTables[4 + Page] = MakeView(WrapBuffer(Source, VertexStride), nri::BufferView::STRUCTURED_BUFFER, nri::Format::UNKNOWN, VertexStride);
			Filler = DrawTables[4 + Page];
			IndexPageBuffers[Page] = WrapBuffer(RImplementation.Geometry.IndexBuffer(Page), 0);
			DrawLists[2 + Page] = MakeView(IndexPageBuffers[Page], nri::BufferView::BUFFER, nri::Format::R16_UINT);
		}
	}

	// A page that is not open is never read, but its slot has to hold a descriptor
	std::replace(DrawTables + 4, DrawTables + 5 + PageSlots, static_cast<nri::Descriptor*>(nullptr), Filler);
	std::replace(DrawLists + 2, DrawLists + 2 + PageSlots, static_cast<nri::Descriptor*>(nullptr), DrawLists[0]);
	std::copy(DrawLists + 2, DrawLists + 2 + PageSlots, AlphaLists + 2);

	SkinFiller = Filler;
	std::copy(DrawTables + 4, DrawTables + 4 + PageSlots, VertexPageViews);

	nri::DescriptorPoolDesc PoolDesc = {};
	PoolDesc.descriptorSetMaxNum = 3;
	PoolDesc.structuredBufferMaxNum = 2 + 2 * (5 + PageSlots);
	PoolDesc.bufferMaxNum = 2 * (2 + PageSlots);
	PoolDesc.storageBufferMaxNum = 3;
	PoolDesc.textureMaxNum = 2 * GpuTextures::MaxBuckets;
	Api.CreateDescriptorPool(Gpu, PoolDesc, Pool);

	Api.AllocateDescriptorSets(*Pool, *CullLayout, 0, &CullSet, 1, 0);
	Api.AllocateDescriptorSets(*Pool, *DrawLayout, 0, &DrawSet, 1, 0);
	Api.AllocateDescriptorSets(*Pool, *DrawLayout, 0, &AlphaSet, 1, 0);

	const nri::UpdateDescriptorRangeDesc Updates[8] = {
		{ CullSet, 0, 0, CullTables, 2 }, { CullSet, 1, 0, CullOutputs, 3 },
		{ DrawSet, 0, 0, DrawTables, 5 + PageSlots }, { DrawSet, 1, 0, DrawLists, 2 + PageSlots },
		{ AlphaSet, 0, 0, DrawTables, 5 + PageSlots }, { AlphaSet, 1, 0, AlphaLists, 2 + PageSlots },
		{ DrawSet, 2, 0, Buckets, GpuTextures::MaxBuckets }, { AlphaSet, 2, 0, Buckets, GpuTextures::MaxBuckets } };
	Api.UpdateDescriptorRanges(Updates, Textures.Count() ? 8 : 6);
}

void GpuScene::Unload()
{
	if (!Meshes)
	{
		return;
	}

	// The old backend owns the queue: its flush covers the passes recorded into its lists
	GRHI->DevicePtr->Flush();

	if (ClusterCount)
	{
		Api.DestroyPipeline(CullPipeline);

		for (const DepthPipelines& Item : Pipelines)
		{
			Api.DestroyPipeline(Item.Opaque);

			if (Item.Alpha)
			{
				Api.DestroyPipeline(Item.Alpha);
			}
		}

		Pipelines.clear();
		Api.DestroyPipelineLayout(CullLayout);
		Api.DestroyPipelineLayout(DrawLayout);
		Api.DestroyDescriptorPool(Pool);

		for (nri::Descriptor* View : Views)
		{
			Api.DestroyDescriptor(View);
		}

		Api.DestroyDescriptor(ViewBlock);
		Api.DestroyDescriptor(DynamicBlock);

		for (GrassSet& Set : Grass)
		{
			ReleaseGrass(Set);
		}

		if (GrassLayout)
		{
			Api.DestroyPipeline(GrassCullPipeline);
			Api.DestroyPipelineLayout(GrassLayout);
			Api.DestroyDescriptorPool(GrassPool);
			Api.DestroyDescriptor(GrassBlock);
			GrassLayout = nullptr;
			GrassCullShader = nullptr;
		}

		if (GrassDrawLayout)
		{
			for (const GrassPipeline& Item : GrassPipelines)
			{
				Api.DestroyPipeline(Item.Pipeline);
			}

			GrassPipelines.clear();

			for (u32 Block = 0; Block < GrassBlockCount; ++Block)
			{
				Api.DestroyDescriptor(GrassBlockViews[Block]);
			}

			GrassBlockCount = 0;
			Api.DestroyPipelineLayout(GrassDrawLayout);
			Api.DestroyDescriptorPool(GrassDrawPool);
			GrassDrawLayout = nullptr;
			GrassShader = nullptr;
		}

		std::fill(std::begin(IndexPageBuffers), std::end(IndexPageBuffers), nullptr);

		if (SkinBuffer)
		{
			Api.DestroyDescriptor(SkinView);
			Api.DestroyBuffer(SkinBuffer);
			SkinBuffer = nullptr;
			SkinSource = nullptr;
		}

		StreamApi.DestroyStreamer(Streamer);

		for (nri::Buffer* Buffer : { Clusters, ClusterIndices, Counters.Buffer, Visible.Buffer, VisibleAlpha.Buffer })
		{
			Api.DestroyBuffer(Buffer);
		}

		for (nri::Buffer* Page : Borrowed)
		{
			Api.DestroyBuffer(Page);
		}

		Views.clear();
		Borrowed.clear();
		Counters = {};
		Visible = {};
		VisibleAlpha = {};
		Streamer = nullptr;
		CullShader = nullptr;
		DepthShader = nullptr;
		AlphaShader = nullptr;
		AlphaPixels = nullptr;
		ClusterCount = 0;
	}

	Textures.Destroy();
	Api.DestroyBuffer(Materials);
	Api.DestroyBuffer(Meshes);
	Api.DestroyBuffer(Instances);
	Api.DestroyBuffer(Transforms);

	Leftover.clear();
	Meshes = nullptr;
	Instances = nullptr;
	Transforms = nullptr;
	DepthState = nullptr;
	WaveShader = nullptr;
	WaveAlphaShader = nullptr;
}

namespace
{
nri::Descriptor* MakeSliceView(nri::Texture* Texture, u32 Slice)
{
	nri::TextureViewDesc Desc = {};
	Desc.texture = Texture;
	Desc.type = nri::TextureView::DEPTH_STENCIL_ATTACHMENT;
	Desc.mipNum = 1;
	Desc.layerOffset = nri::Dim_t(Slice);
	Desc.layerNum = 1;

	nri::Descriptor* View = nullptr;
	Api.CreateTextureView(Desc, View);
	return View;
}

void ReleaseGrass(GrassSet& Set)
{
	if (!Set.Source)
	{
		return;
	}

	std::erase_if(GrassDrawSets, [&Set](const GrassDrawSet& Item)
	{
		if (Item.Owner != &Set)
		{
			return false;
		}

		Api.DestroyDescriptor(Item.View);
		Api.DestroyTexture(Item.Texture);
		return true;
	});

	for (nri::Descriptor* View : Set.Views)
	{
		Api.DestroyDescriptor(View);
	}

	Api.DestroyBuffer(Set.Items);
	Api.DestroyBuffer(Set.Args.Buffer);
	Api.DestroyBuffer(Set.Visible.Buffer);
	_RELEASE(Set.Source);
	Set = {};
}

void ReserveGrass(GrassSet& Set, u32 Count)
{
	const u32 Capacity = std::max(Count, std::max(Set.Capacity * 2, 16384u));
	ReleaseGrass(Set);
	Set.Capacity = Capacity;

	RHIBufferDesc Desc = {};
	Desc.Size = Capacity * GrassItemSize;
	Desc.Usage = ERHI_USAGE::USAGE_DEFAULT;
	Desc.Type = ERHI_BUFFER_TYPE::STRUCTURED;
	Desc.StructureByteStride = GrassItemSize;
	Set.Source = GRHI->CreateBuffer(Desc);
	Set.Items = WrapBuffer(Set.Source, GrassItemSize);
	Borrowed.pop_back();

	const nri::BufferUsageBits Storage = nri::BufferUsageBits::SHADER_RESOURCE_STORAGE;
	Set.Args.Buffer = CreateBuffer(SCENE_GRASS_BINS * SCENE_GRASS_ARGS * sizeof(u32), 0, Storage | nri::BufferUsageBits::ARGUMENT, "Scene grass arguments");
	Set.Visible.Buffer = CreateBuffer(u64(Capacity) * sizeof(u32), 0, Storage | nri::BufferUsageBits::SHADER_RESOURCE, "Scene visible grass");

	nri::BufferViewDesc View = {};
	View.buffer = Set.Items;
	View.type = nri::BufferView::STRUCTURED_BUFFER;
	View.structureStride = GrassItemSize;
	Api.CreateBufferView(View, Set.Views[0]);
	View = {};
	View.type = nri::BufferView::STORAGE_BUFFER;
	View.format = nri::Format::R32_UINT;
	View.buffer = Set.Args.Buffer;
	Api.CreateBufferView(View, Set.Views[1]);
	View.buffer = Set.Visible.Buffer;
	Api.CreateBufferView(View, Set.Views[2]);
	View.type = nri::BufferView::BUFFER;
	Api.CreateBufferView(View, Set.Views[3]);

	Api.AllocateDescriptorSets(*GrassPool, *GrassLayout, 0, &Set.Set, 1, 0);
	const nri::UpdateDescriptorRangeDesc Updates[2] = { { Set.Set, 0, 0, &Set.Views[0], 1 }, { Set.Set, 1, 0, &Set.Views[1], 2 } };
	Api.UpdateDescriptorRanges(Updates, 2);
}
} // namespace

namespace
{
// Root descriptors in the order of the engine's blocks; All false streams the two that differ per model
void StreamBlocks(nri::CommandBuffer& Commands, const CShaderConstants& Constants, bool All)
{
	u32 Root = 0;

	Constants.ForEachBlock([&](u32 Slot, const void* Data, u32 Size)
	{
		if (All || Slot == CBDraw::Slot || Slot == CBDetail::Slot)
		{
			nri::SetRootDescriptorDesc Desc = {};
			Desc.rootDescriptorIndex = Root;
			Desc.descriptor = GrassBlockViews[Root];
			Desc.offset = StreamApi.StreamConstantData(*Streamer, Data, Size);
			Api.CmdSetRootDescriptor(Commands, Desc);
		}

		++Root;
	});
}

void CreateGrassDraw(const CShaderConstants& Constants)
{
	const nri::StageBits Vertex = nri::StageBits::VERTEX_SHADER;
	const nri::StageBits Pixel = nri::StageBits::FRAGMENT_SHADER;

	nri::RootDescriptorDesc Roots[MaxBlocks] = {};
	nri::BufferViewDesc Block = {};
	Block.buffer = StreamApi.GetStreamerConstantBuffer(*Streamer);
	Block.type = nri::BufferView::CONSTANT_BUFFER;

	Constants.ForEachBlock([&](u32 Slot, const void*, u32 Size)
	{
		R_ASSERT(GrassBlockCount < MaxBlocks);
		Roots[GrassBlockCount] = { Slot, nri::DescriptorType::CONSTANT_BUFFER, Vertex | Pixel };
		Block.size = (Size + 255) & ~255u;
		Api.CreateBufferView(Block, GrassBlockViews[GrassBlockCount++]);
	});

	nri::RootSamplerDesc Sampler = {};
	Sampler.registerIndex = ShaderBindings::Sampler("smp_linear");
	Sampler.desc.filters = { nri::Filter::LINEAR, nri::Filter::LINEAR, nri::Filter::LINEAR };
	Sampler.desc.anisotropy = 1;
	Sampler.desc.mipMax = 16.0f;
	Sampler.desc.addressModes = { nri::AddressMode::REPEAT, nri::AddressMode::REPEAT, nri::AddressMode::REPEAT };
	Sampler.shaderStages = Pixel;

	// The registers of deffer_detail.vs.hlsl and shadow_base.ps.hlsl: the instances, the visible
	// list, the vertex pages (page 4 first), the base texture
	const nri::DescriptorRangeDesc Ranges[4] = {
		{ ShaderBindings::Texture("detail_buffer"), 1, nri::DescriptorType::STRUCTURED_BUFFER, Vertex },
		{ 56, 1, nri::DescriptorType::BUFFER, Vertex },
		{ ShaderBindings::Texture("g_RingVertices"), 2 + PageSlots, nri::DescriptorType::STRUCTURED_BUFFER, Vertex },
		{ ShaderBindings::Texture("s_base"), 1, nri::DescriptorType::TEXTURE, Pixel } };

	nri::DescriptorSetDesc SetDesc = {};
	SetDesc.ranges = Ranges;
	SetDesc.rangeNum = 4;

	nri::PipelineLayoutDesc Desc = {};
	Desc.rootDescriptors = Roots;
	Desc.rootDescriptorNum = GrassBlockCount;
	Desc.rootSamplers = &Sampler;
	Desc.rootSamplerNum = 1;
	Desc.descriptorSets = &SetDesc;
	Desc.descriptorSetNum = 1;
	Desc.shaderStages = Vertex | Pixel;
	Api.CreatePipelineLayout(*GRHI->Nri->Device, Desc, GrassDrawLayout);

	nri::DescriptorPoolDesc PoolDesc = {};
	PoolDesc.descriptorSetMaxNum = 64;
	PoolDesc.structuredBufferMaxNum = 64 * (3 + PageSlots);
	PoolDesc.bufferMaxNum = 64;
	PoolDesc.textureMaxNum = 64;
	Api.CreateDescriptorPool(*GRHI->Nri->Device, PoolDesc, GrassDrawPool);

	ShaderExternalMap Defines;

	for (const char* Name : { "DISABLE_MOTION_VECTORS", "DETAIL_SHADOW_PASS", "USE_AREF", "USE_TREEWAVE", "DETAIL_VISIBLE_LIST" })
	{
		Defines[Name] = "1";
	}

	GrassShader = DEV->_CreateVS("deffer_detail", Defines);
}

nri::Pipeline* FindGrassPipeline(nri::Format Format, nri::CompareOp Depth, const SPass& Pass)
{
	for (const GrassPipeline& Item : GrassPipelines)
	{
		if (Item.Format == Format && Item.Compare == Depth && Item.Pixels == Pass.ps->ps)
		{
			return Item.Pipeline;
		}
	}

	const RHIRasterizerDesc& Raster = Pass.state->state->Raster;
	const nri::ShaderDesc Shaders[2] = { ShaderCode(GrassShader->vs, nri::StageBits::VERTEX_SHADER), ShaderCode(Pass.ps->ps, nri::StageBits::FRAGMENT_SHADER) };

	nri::GraphicsPipelineDesc Desc = {};
	Desc.pipelineLayout = GrassDrawLayout;
	Desc.inputAssembly.topology = nri::Topology::TRIANGLE_LIST;
	Desc.rasterization.depthBias = { float(Raster.DepthBias), Raster.DepthBiasClamp, Raster.SlopeScaledDepthBias };
	// Grass is drawn from both sides (hw_Render)
	Desc.rasterization.cullMode = nri::CullMode::NONE;
	Desc.rasterization.depthClamp = !Raster.DepthClipEnable;
	Desc.outputMerger.depthStencilFormat = Format;
	Desc.outputMerger.depth.compareOp = Depth;
	Desc.outputMerger.depth.write = true;
	Desc.shaders = Shaders;
	Desc.shaderNum = 2;

	nri::Pipeline* Pipeline = nullptr;
	Api.CreateGraphicsPipeline(*GRHI->Nri->Device, Desc, Pipeline);
	GrassPipelines.push_back({ Format, Depth, Pass.ps->ps, Pipeline });
	return Pipeline;
}

nri::DescriptorSet* FindGrassDrawSet(const GrassSet& Owner, IRHISurface* Surface)
{
	for (const GrassDrawSet& Item : GrassDrawSets)
	{
		if (Item.Owner == &Owner && Item.Surface == Surface)
		{
			return Item.Set;
		}
	}

	GrassDrawSet Item = { &Owner, Surface, WrapNriSurface(Surface), nullptr, nullptr };

	nri::TextureViewDesc View = {};
	View.texture = Item.Texture;
	View.type = nri::TextureView::TEXTURE;
	View.mipNum = nri::REMAINING;
	View.layerNum = nri::REMAINING;
	Api.CreateTextureView(View, Item.View);

	// Page 4 has the lowest register
	// The vertex function names the ring and the skinned output too, though grass reads neither
	nri::Descriptor* Pages[2 + PageSlots] = { VertexPageViews[0], VertexPageViews[0] };
	std::reverse_copy(VertexPageViews, VertexPageViews + PageSlots, Pages + 2);

	Api.AllocateDescriptorSets(*GrassDrawPool, *GrassDrawLayout, 0, &Item.Set, 1, 0);
	const nri::UpdateDescriptorRangeDesc Updates[4] = { { Item.Set, 0, 0, &Owner.Views[0], 1 }, { Item.Set, 1, 0, &Owner.Views[3], 1 }, { Item.Set, 2, 0, Pages, 2 + PageSlots }, { Item.Set, 3, 0, &Item.View, 1 } };
	Api.UpdateDescriptorRanges(Updates, 4);
	GrassDrawSets.push_back(Item);

	return Item.Set;
}
} // namespace

bool GpuScene::DrawGrass(GfxCmdList& Cmd, u32 Variant, u32 Frame, const GrassBin* Bins, u32 BinCount, const CFrustum& Frustum, const DepthDraw& Draw)
{
	IRHISurface* Surface = Draw.Target->GetSurface();
	const u32 Format = NriDepthFormat(Surface->GetFormat());
	if (!ClusterCount || !BinCount || Variant >= GrassVariants || BinCount > SCENE_GRASS_BINS || !Format)
	{
		return false;
	}

	const u32 BaseStage = CTexture::rstPixel + ShaderBindings::Texture("s_base");
	IRHISurface* Bases[SCENE_GRASS_BINS];
	u32 First[SCENE_GRASS_BINS];
	u32 Total = 0;

	for (u32 Bin = 0; Bin < BinCount; ++Bin)
	{
		const SPass* Pass = Bins[Bin].Pass;
		Bases[Bin] = nullptr;

		if (Pass && Pass->ps && Pass->state && Pass->T)
		{
			for (const auto& [Stage, Texture] : *Pass->T)
			{
				if (Stage == BaseStage && Texture._get())
				{
					Bases[Bin] = Texture->pSurface;
				}
			}
		}

		if (!Bases[Bin] || Bins[Bin].Page >= PageSlots || !IndexPageBuffers[Bins[Bin].Page])
		{
			return false;
		}

		First[Bin] = Total;
		Total += Bins[Bin].Count;
	}

	GPU_EVENT(SceneGrass);
	CullGrass(Variant, Frame, Bins, BinCount, Frustum);

	if (!GrassDrawLayout)
	{
		CreateGrassDraw(Cmd.Constants);
	}

	GrassSet& Set = Grass[Variant];
	const nri::Format Typed = nri::nriConvertDXGIFormatToNRI(Format);

	nri::Texture* Texture = WrapNriSurface(Surface, Format);
	nri::Descriptor* SliceView = MakeSliceView(Texture, Draw.Slice);

	for (u32 Page = 0; Page < RImplementation.Geometry.PageCount(); ++Page)
	{
		if (IRHIBuffer* Source = RImplementation.Geometry.VertexBuffer(Page))
		{
			GRHI->DevicePtr->ReadExternalBuffer(Source);
			GRHI->DevicePtr->ReadExternalBuffer(RImplementation.Geometry.IndexBuffer(Page));
		}
	}

	GRHI->DevicePtr->ReadExternalBuffer(Set.Source);

	nri::CommandBuffer& Commands = *BeginNriCommands(Bases, BinCount, nullptr, 0, Draw.Target);
	Move(Commands, Set.Args, nri::AccessBits::ARGUMENT_BUFFER, nri::StageBits::INDIRECT);
	Move(Commands, Set.Visible, nri::AccessBits::SHADER_RESOURCE, nri::StageBits::VERTEX_SHADER);

	Api.CmdSetDescriptorPool(Commands, *GrassDrawPool);
	Api.CmdSetPipelineLayout(Commands, nri::BindPoint::GRAPHICS, *GrassDrawLayout);
	nri::Pipeline* Bound = FindGrassPipeline(Typed, Draw.IsSpot ? nri::CompareOp::LESS_EQUAL : Compare[u32(Bins[0].Pass->state->state->Depth.DepthFunc)], *Bins[0].Pass);
	Api.CmdSetPipeline(Commands, *Bound);
	StreamBlocks(Commands, Cmd.Constants, true);

	const RHIViewport Whole = { 0.0f, 0.0f, float(Surface->GetWidth()), float(Surface->GetHeight()), 0.0f, 1.0f };
	const RHIViewport& Drawn = Draw.Area ? *Draw.Area : Whole;
	const nri::Viewport Viewport = { Drawn.TopLeftX, Drawn.TopLeftY, Drawn.Width, Drawn.Height, Drawn.MinDepth, Drawn.MaxDepth };
	const nri::Rect Scissor = { s16(Drawn.TopLeftX), s16(Drawn.TopLeftY), nri::Dim_t(Drawn.Width), nri::Dim_t(Drawn.Height) };
	Api.CmdSetViewports(Commands, &Viewport, 1);
	Api.CmdSetScissors(Commands, &Scissor, 1);

	nri::RenderingDesc Rendering = {};
	Rendering.depth.descriptor = SliceView;
	Rendering.depth.loadOp = nri::LoadOp::LOAD;
	Rendering.depth.storeOp = nri::StoreOp::STORE;
	Api.CmdBeginRendering(Commands, Rendering);

	nri::DescriptorSet* BoundSet = nullptr;

	for (u32 Bin = 0; Bin < BinCount; ++Bin)
	{
		const SPass& Pass = *Bins[Bin].Pass;
		nri::Pipeline* Pipeline = FindGrassPipeline(Typed, Draw.IsSpot ? nri::CompareOp::LESS_EQUAL : Compare[u32(Pass.state->state->Depth.DepthFunc)], Pass);
		if (Pipeline != Bound)
		{
			Bound = Pipeline;
			Api.CmdSetPipeline(Commands, *Bound);
		}

		nri::DescriptorSet* Wanted = FindGrassDrawSet(Set, Bases[Bin]);
		if (Wanted != BoundSet)
		{
			BoundSet = Wanted;
			nri::SetDescriptorSetDesc SetDesc = {};
			SetDesc.descriptorSet = BoundSet;
			Api.CmdSetDescriptorSet(Commands, SetDesc);
		}

		CBDraw& Base = Cmd.Constants.Write<CBDraw>();
		Base.draw_vertex_base = Bins[Bin].VertexBase;
		Base.draw_source_base = 0;
		Base.draw_flags = DRAW_SOURCE_STATIC | (Bins[Bin].Page << DRAW_PAGE_SHIFT);
		Cmd.Constants.Write<CBDetail>().detail_first = First[Bin];
		StreamBlocks(Commands, Cmd.Constants, false);
		Api.CmdSetIndexBuffer(Commands, *IndexPageBuffers[Bins[Bin].Page], 0, nri::IndexType::UINT16);
		Api.CmdDrawIndexedIndirect(Commands, *Set.Args.Buffer, Bin * SCENE_GRASS_ARGS * sizeof(u32), 1, SCENE_GRASS_ARGS * sizeof(u32), nullptr, 0);
	}

	Api.CmdEndRendering(Commands);
	EndNriCommands(&Commands);

	Api.DestroyDescriptor(SliceView);
	Api.DestroyTexture(Texture);
	Cmd.RestoreInput();
	return true;
}

void GpuScene::CullGrass(u32 Variant, u32 Frame, const GrassBin* Bins, u32 BinCount, const CFrustum& Frustum)
{
	GPU_EVENT(SceneGrassCull);

	if (!GrassLayout)
	{
		const nri::StageBits Compute = nri::StageBits::COMPUTE_SHADER;
		const nri::DescriptorRangeDesc Ranges[2] = { { 0, 1, nri::DescriptorType::STRUCTURED_BUFFER, Compute }, { 0, 2, nri::DescriptorType::STORAGE_BUFFER, Compute } };

		GrassLayout = CreateLayout(Compute, Ranges, 2);
		GrassCullShader = DEV->_CreateCS("scene_grass_cull");
		nri::ComputePipelineDesc Desc = {};
		Desc.pipelineLayout = GrassLayout;
		Desc.shader = ShaderCode(GrassCullShader->sh, Compute);
		Api.CreateComputePipeline(*GRHI->Nri->Device, Desc, GrassCullPipeline);

		nri::BufferViewDesc Block = {};
		Block.buffer = StreamApi.GetStreamerConstantBuffer(*Streamer);
		Block.type = nri::BufferView::CONSTANT_BUFFER;
		Block.size = (sizeof(Hlsl::SceneGrassView) + 255) & ~255u;
		Api.CreateBufferView(Block, GrassBlock);

		nri::DescriptorPoolDesc PoolDesc = {};
		PoolDesc.descriptorSetMaxNum = 32;
		PoolDesc.structuredBufferMaxNum = 32;
		PoolDesc.storageBufferMaxNum = 64;
		Api.CreateDescriptorPool(*GRHI->Nri->Device, PoolDesc, GrassPool);
	}

	Hlsl::SceneGrassView Block = {};
	u32 Total = 0;

	for (u32 Bin = 0; Bin < BinCount; ++Bin)
	{
		Block.bins[Bin] = { Total, Bins[Bin].Count, Bins[Bin].Radius, Bins[Bin].IndexCount, Bins[Bin].IndexStart, Bins[Bin].VertexBase, Bins[Bin].Page, 0 };
		Total += Bins[Bin].Count;
	}

	GrassSet& Set = Grass[Variant];
	if (Total > Set.Capacity)
	{
		// The old backend owns the queue: its flush covers the passes that read the old buffers
		GRHI->DevicePtr->Flush();
		ReserveGrass(Set, Total);
	}

	if (Set.Frame != Frame)
	{
		Set.Frame = Frame;

		for (u32 Bin = 0; Bin < BinCount; ++Bin)
		{
			Set.Source->UpdateSubresource(Bins[Bin].Items, Bins[Bin].Count * GrassItemSize, Block.bins[Bin].first * GrassItemSize);
		}
	}

	const u32 Planes = std::min(u32(Frustum.p_count), SCENE_VIEW_PLANES);
	for (u32 Plane = 0; Plane < Planes; ++Plane)
	{
		const Fplane& Source = Frustum.planes[Plane];
		Block.planes[Plane].set(Source.n.x, Source.n.y, Source.n.z, Source.d);
	}

	Block.counts = { Total, BinCount, Planes, 0 };

	GRHI->DevicePtr->ReadExternalBuffer(Set.Source);
	nri::CommandBuffer& Commands = *BeginNriCommands(nullptr, 0, nullptr, 0, nullptr);
	Move(Commands, Set.Args, nri::AccessBits::COPY_DESTINATION, nri::StageBits::COPY);
	Api.CmdZeroBuffer(Commands, *Set.Args.Buffer, 0, SCENE_GRASS_BINS * SCENE_GRASS_ARGS * sizeof(u32));
	Move(Commands, Set.Args, nri::AccessBits::SHADER_RESOURCE_STORAGE, nri::StageBits::COMPUTE_SHADER);
	Move(Commands, Set.Visible, nri::AccessBits::SHADER_RESOURCE_STORAGE, nri::StageBits::COMPUTE_SHADER);

	Api.CmdSetDescriptorPool(Commands, *GrassPool);
	Api.CmdSetPipelineLayout(Commands, nri::BindPoint::COMPUTE, *GrassLayout);
	Api.CmdSetPipeline(Commands, *GrassCullPipeline);

	nri::SetRootDescriptorDesc Root = {};
	Root.descriptor = GrassBlock;
	Root.offset = StreamApi.StreamConstantData(*Streamer, &Block, sizeof(Block));
	Api.CmdSetRootDescriptor(Commands, Root);

	nri::SetDescriptorSetDesc SetDesc = {};
	SetDesc.descriptorSet = Set.Set;
	Api.CmdSetDescriptorSet(Commands, SetDesc);
	const nri::DispatchDesc Groups = { (Total + 63) / 64, 1, 1 };
	Api.CmdDispatch(Commands, Groups);
	EndNriCommands(&Commands);
}

void GpuScene::EndFrame()
{
	if (Streamer)
	{
		StreamApi.EndStreamerFrame(*Streamer);
	}
}

bool GpuScene::DrawDepth(GfxCmdList& Cmd, R_dsgraph_structure& Graph, const DepthDraw& Draw)
{
	IRHIDepthStencilView* Target = Draw.Target;
	IRHISurface* Surface = Target->GetSurface();
	const u32 Format = NriDepthFormat(Surface->GetFormat());
	if (!ClusterCount || !Format)
	{
		return false;
	}

	GPU_EVENT(SceneDepth);
	Hlsl::SceneDynamic Dynamic[SCENE_DYNAMIC_MESHES] = {};
	const u32 DynamicCount = TakeDynamic(Cmd, Graph, Dynamic);
	if (Draw.IsDynamicOnly && !DynamicCount)
	{
		return false;
	}

	if (IRHIBuffer* Output = RImplementation.Skinning.GetOutput(); Output != SkinSource)
	{
		if (SkinBuffer)
		{
			Api.DestroyDescriptor(SkinView);
			Api.DestroyBuffer(SkinBuffer);
		}

		SkinSource = Output;
		SkinBuffer = WrapBuffer(Output, VertexStride);
		Borrowed.pop_back();

		nri::BufferViewDesc Desc = {};
		Desc.buffer = SkinBuffer;
		Desc.type = nri::BufferView::STRUCTURED_BUFFER;
		Desc.structureStride = VertexStride;
		Api.CreateBufferView(Desc, SkinView);
		const nri::UpdateDescriptorRangeDesc Updates[2] = { { DrawSet, 0, 4 + PageSlots, &SkinView, 1 }, { AlphaSet, 0, 4 + PageSlots, &SkinView, 1 } };
		Api.UpdateDescriptorRanges(Updates, 2);
	}

	Cull(Graph, Draw, Dynamic, DynamicCount);
	const nri::Format Typed = nri::nriConvertDXGIFormatToNRI(Format);
	const bool IsSpot = Draw.IsSpot;
	auto Found = std::find_if(Pipelines.begin(), Pipelines.end(), [Typed, IsSpot](const DepthPipelines& Item) { return Item.Format == Typed && Item.IsSpot == IsSpot; });
	if (Found == Pipelines.end())
	{
		Pipelines.push_back({ Typed, IsSpot, CreateDepthPipeline(Typed, false, IsSpot), HasAlpha ? CreateDepthPipeline(Typed, true, IsSpot) : nullptr });
		Found = Pipelines.end() - 1;
	}

	// The wrapper of the old backend's surface lives for this pass: targets are bound when recorded
	nri::Texture* Texture = WrapNriSurface(Surface, Format);
	nri::Descriptor* SliceView = MakeSliceView(Texture, Draw.Slice);

	// The geometry pages and the skinned output belong to the old backend, which has to hand them
	// over readable
	for (u32 Page = 0; Page < RImplementation.Geometry.PageCount(); ++Page)
	{
		if (IRHIBuffer* Source = RImplementation.Geometry.VertexBuffer(Page))
		{
			GRHI->DevicePtr->ReadExternalBuffer(Source);
			GRHI->DevicePtr->ReadExternalBuffer(RImplementation.Geometry.IndexBuffer(Page));
		}
	}

	if (SkinSource)
	{
		GRHI->DevicePtr->ReadExternalBuffer(SkinSource);
	}

	nri::CommandBuffer& Commands = *BeginNriCommands(nullptr, 0, nullptr, 0, Target);
	Move(Commands, Counters, nri::AccessBits::ARGUMENT_BUFFER, nri::StageBits::INDIRECT);
	Move(Commands, Visible, nri::AccessBits::SHADER_RESOURCE, nri::StageBits::VERTEX_SHADER);
	Move(Commands, VisibleAlpha, nri::AccessBits::SHADER_RESOURCE, nri::StageBits::VERTEX_SHADER);

	Api.CmdSetDescriptorPool(Commands, *Pool);
	Api.CmdSetPipelineLayout(Commands, nri::BindPoint::GRAPHICS, *DrawLayout);
	Api.CmdSetPipeline(Commands, *Found->Opaque);
	SetBlocks(Commands);

	const RHIViewport Whole = { 0.0f, 0.0f, float(Surface->GetWidth()), float(Surface->GetHeight()), 0.0f, 1.0f };
	const RHIViewport& Drawn = Draw.Area ? *Draw.Area : Whole;
	const nri::Viewport Viewport = { Drawn.TopLeftX, Drawn.TopLeftY, Drawn.Width, Drawn.Height, Drawn.MinDepth, Drawn.MaxDepth };
	const nri::Rect Scissor = { s16(Drawn.TopLeftX), s16(Drawn.TopLeftY), nri::Dim_t(Drawn.Width), nri::Dim_t(Drawn.Height) };
	Api.CmdSetViewports(Commands, &Viewport, 1);
	Api.CmdSetScissors(Commands, &Scissor, 1);

	nri::RenderingDesc Rendering = {};
	Rendering.depth.descriptor = SliceView;
	Rendering.depth.loadOp = nri::LoadOp::LOAD;
	Rendering.depth.storeOp = nri::StoreOp::STORE;
	Api.CmdBeginRendering(Commands, Rendering);

	nri::SetDescriptorSetDesc SetDesc = {};
	SetDesc.descriptorSet = DrawSet;
	Api.CmdSetDescriptorSet(Commands, SetDesc);
	Api.CmdDrawIndirect(Commands, *Counters.Buffer, 0, 1, sizeof(nri::DrawDesc), nullptr, 0);

	if (HasAlpha && !Draw.IsDynamicOnly)
	{
		Api.CmdSetPipeline(Commands, *Found->Alpha);
		SetDesc.descriptorSet = AlphaSet;
		Api.CmdSetDescriptorSet(Commands, SetDesc);
		Api.CmdDrawIndirect(Commands, *Counters.Buffer, SCENE_COUNTER_ALPHA * sizeof(u32), 1, sizeof(nri::DrawDesc), nullptr, 0);
	}

	Api.CmdEndRendering(Commands);
	EndNriCommands(&Commands);

	Api.DestroyDescriptor(SliceView);
	Api.DestroyTexture(Texture);
	Cmd.RestoreInput();
	return true;
}

void GpuScene::Cull(R_dsgraph_structure& Graph, const DepthDraw& Draw, const Hlsl::SceneDynamic* Dynamic, u32 DynamicCount)
{
	GPU_EVENT(SceneCull);

	const GraphView& View = Graph.ViewParams;
	const CPortalTraverser& Traverser = Graph.PortalTraverser;

	// Without clusters the threads of the cull start at the dynamic meshes
	const u32 Clusters = Draw.IsDynamicOnly ? 0 : ClusterCount;
	const float Facing = ConeSign(Draw.IsSpot);
	Fvector Look = Draw.Look;
	Look.normalize_safe().mul(Facing);

	if (Draw.Eye)
	{
		Look = *Draw.Eye;
	}

	Hlsl::SceneView Block = {};
	Block.view_projection = View.Transform;

	const u32 Planes = std::min(u32(View.Frustum.p_count), SCENE_VIEW_PLANES);
	for (u32 Plane = 0; Plane < Planes; ++Plane)
	{
		const Fplane& Source = View.Frustum.planes[Plane];
		Block.planes[Plane].set(Source.n.x, Source.n.y, Source.n.z, Source.d);
	}

	Block.camera.set(View.CameraPosition.x, View.CameraPosition.y, View.CameraPosition.z, View.FogDistance);
	Block.thresholds.set(View.SsaDiscard, Look.x, Look.y, Look.z);
	Block.counts = { Clusters, (Facing != 0.0f ? SCENE_VIEW_FACING : 0u) | (Draw.Eye ? SCENE_VIEW_PERSPECTIVE : 0u) | (Draw.IsKept ? SCENE_VIEW_KEPT : 0u) | (DynamicCount << SCENE_VIEW_DYNAMIC_SHIFT), Planes, GetStaticCullSteps(true, View.Fov, Block.steps) };

	Fvector4 Wave, Wind;
	float SwayScale;
	GetTreeWave(Wave, Wind, SwayScale);
	Block.wave = Wave;
	Block.wind.set(Wind.x, Wind.y, Wind.z, SwayScale);
	Block.material.set(ps_r2_def_aref_quality / 255.0f, Facing, 0.0f, 0.0f);

	for (u32 Sector = 0; Sector < std::min(SectorCount, SCENE_VIEW_SECTORS); ++Sector)
	{
		if (Traverser.IsVisited(Sector))
		{
			(&Block.sectors[Sector >> 7].x)[(Sector >> 5) & 3] |= 1u << (Sector & 31);
		}
	}

	nri::CommandBuffer& Commands = *BeginNriCommands(nullptr, 0, nullptr, 0, nullptr);
	Move(Commands, Counters, nri::AccessBits::COPY_DESTINATION, nri::StageBits::COPY);
	Api.CmdZeroBuffer(Commands, *Counters.Buffer, 0, SCENE_COUNTER_WORDS * sizeof(u32));
	Move(Commands, Counters, nri::AccessBits::SHADER_RESOURCE_STORAGE, nri::StageBits::COMPUTE_SHADER);
	Move(Commands, Visible, nri::AccessBits::SHADER_RESOURCE_STORAGE, nri::StageBits::COMPUTE_SHADER);
	Move(Commands, VisibleAlpha, nri::AccessBits::SHADER_RESOURCE_STORAGE, nri::StageBits::COMPUTE_SHADER);

	Api.CmdSetDescriptorPool(Commands, *Pool);
	Api.CmdSetPipelineLayout(Commands, nri::BindPoint::COMPUTE, *CullLayout);
	Api.CmdSetPipeline(Commands, *CullPipeline);
	ViewOffset = StreamApi.StreamConstantData(*Streamer, &Block, sizeof(Block));
	DynamicOffset = StreamApi.StreamConstantData(*Streamer, Dynamic, sizeof(Hlsl::SceneDynamic) * SCENE_DYNAMIC_MESHES);
	SetBlocks(Commands);

	nri::SetDescriptorSetDesc SetDesc = {};
	SetDesc.descriptorSet = CullSet;
	Api.CmdSetDescriptorSet(Commands, SetDesc);
	const nri::DispatchDesc Groups = { (Clusters + DynamicCount + 63) / 64, 1, 1 };
	Api.CmdDispatch(Commands, Groups);
	EndNriCommands(&Commands);
}
