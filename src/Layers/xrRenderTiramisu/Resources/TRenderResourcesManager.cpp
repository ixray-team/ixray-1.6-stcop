#include "TRenderResourcesManager.h"

#include <RedImage/RedImage.hpp>

#include "RenderVertexTypes.h"
#include "Materials/TRenderMaterialsManager.h"
#include "Scene/TRenderScene.h"
#include "Shaders/Defines/TShaderDefinesManager.h"
#include "Shaders/Global/TGlobalShadersManager.h"

void TRenderResourcesManager::CreateSamplers()
{
	NRI_CHECK(GRenderDevice.CoreInterface.CreateSampler(*GRenderDevice.Device, {{nri::Filter::LINEAR, nri::Filter::LINEAR}}, LinearSampler));
	
	const nri::Descriptor* samplers[] = 
	{
		LinearSampler,
		LinearSampler,
		LinearSampler,
		LinearSampler,
	};

	const nri::UpdateDescriptorRangeDesc UpdateDescriptorRangeDescription[] = 
	{
		{SamplerDescriptorSet, 0, 0, samplers, 4},
	};
		
	GRenderDevice.CoreInterface.UpdateDescriptorRanges(UpdateDescriptorRangeDescription,1);
}

void TRenderResourcesManager::CreateQuadBuffer()
{
	static const FUIVertex QuadVertexData[] =
	{
		{{-1.f, -1.f ,0 },0xFFFFFFFF, {0.0f, 0.0f}},
		{{1.f, -1.f,0 },0xFFFFFFFF, {1.0f, 0.0f}},
		{{-1.f, 1.f,0 },0xFFFFFFFF, {0.0f, 1.0f}},
		{{1.f, 1.f,0 },0xFFFFFFFF, {1.0f, 1.0f}},
	};
	static const uint16_t QuadIndexData[] = { 0, 1, 2, 1, 3, 2 };


	const uint64_t IndexDataSize = sizeof(QuadIndexData);
	const uint64_t IndexDataAlignedSize = Align(IndexDataSize, 16);
	const uint64_t VertexDataSize = sizeof(QuadVertexData);

	{ // Geometry buffer
		nri::BufferDesc bufferDesc = {};
		bufferDesc.size = IndexDataAlignedSize + VertexDataSize;
		bufferDesc.usage = nri::BufferUsageBits::VERTEX_BUFFER | nri::BufferUsageBits::INDEX_BUFFER;
		NRI_CHECK(GRenderDevice.CoreInterface.CreateCommittedBuffer(*GRenderDevice.Device,nri::MemoryLocation::DEVICE , 1.0f, bufferDesc, QuadGeometryBuffer));
	}
	QuadGeometryOffset = IndexDataAlignedSize;

	
	xr_vector<uint8_t> geometryBufferData(IndexDataAlignedSize + VertexDataSize);
	memcpy(&geometryBufferData[0], QuadIndexData, IndexDataSize);
	memcpy(&geometryBufferData[IndexDataAlignedSize], QuadVertexData, VertexDataSize);


	nri::BufferUploadDesc BufferUploadData = {};
	BufferUploadData.buffer = QuadGeometryBuffer;
	BufferUploadData.data = geometryBufferData.data();
	BufferUploadData.after = { nri::AccessBits::INDEX_BUFFER | nri::AccessBits::VERTEX_BUFFER };

	NRI_CHECK(GRenderDevice.HelperInterface.UploadData(*GRenderDevice.GraphicsQueue, nullptr, 0, &BufferUploadData, 1));
}

TRenderResourcesManager::TRenderResourcesManager()
{
	{
		nri::DescriptorPoolDesc DescriptorPoolDescription = {};
		DescriptorPoolDescription.mutableMaxNum = 2048;
		DescriptorPoolDescription.samplerMaxNum = 4;
		DescriptorPoolDescription.descriptorSetMaxNum = 2 + 1;
		DescriptorPoolDescription.constantBufferMaxNum = 1;
		NRI_CHECK(GRenderDevice.CoreInterface.CreateDescriptorPool(*GRenderDevice.Device, DescriptorPoolDescription, GlobalDescriptorPool));
	}
	 
	{ 
		nri::DescriptorRangeDesc DescriptorRangeDescriptions[2] = {
			{ // Resource heap
				0, // VK binding for "-fvk-bind-resource-heap"
				2048,
				nri::DescriptorType::MUTABLE,
				nri::StageBits::VERTEX_SHADER | nri::StageBits::FRAGMENT_SHADER,
				nri::DescriptorRangeBits::ARRAY | nri::DescriptorRangeBits::PARTIALLY_BOUND,
			},
			{ // Sampler heap
				1, // VK binding for "-fvk-bind-sampler-heap"
				4,
				nri::DescriptorType::SAMPLER,
				nri::StageBits::VERTEX_SHADER | nri::StageBits::FRAGMENT_SHADER,
				nri::DescriptorRangeBits::ARRAY | nri::DescriptorRangeBits::PARTIALLY_BOUND,
			},
		};

		
		nri::DescriptorRangeDesc SetConstantBuffer = {0, 1, nri::DescriptorType::CONSTANT_BUFFER, nri::StageBits::ALL};
		
		nri::DescriptorSetDesc GlobalDescriptorSetDescription[] = {
			{0, DescriptorRangeDescriptions + 0, 1},
			{1, DescriptorRangeDescriptions + 1, 1},
			{2,&SetConstantBuffer,1}};

		nri::PipelineLayoutDesc pipelineLayoutDesc = {};
		pipelineLayoutDesc.descriptorSetNum = 3;
		pipelineLayoutDesc.descriptorSets = GlobalDescriptorSetDescription;
		pipelineLayoutDesc.shaderStages =  nri::StageBits::VERTEX_SHADER | nri::StageBits::FRAGMENT_SHADER;
		pipelineLayoutDesc.flags = nri::PipelineLayoutBits::RESOURCE_HEAP_DIRECTLY_INDEXED | nri::PipelineLayoutBits::SAMPLER_HEAP_DIRECTLY_INDEXED;
		
		if (GRenderDevice.GraphicsApi == nri::GraphicsAPI::D3D12)
		{
			pipelineLayoutDesc.flags |= nri::PipelineLayoutBits::ENABLE_DRAW_PARAMETERS_EMULATION;
		}

		NRI_CHECK(GRenderDevice.CoreInterface.CreatePipelineLayout(*GRenderDevice.Device, pipelineLayoutDesc, GlobalPipelineLayout));
	}
	{
		NRI_CHECK(GRenderDevice.CoreInterface.AllocateDescriptorSets(*GlobalDescriptorPool, *GlobalPipelineLayout, 0, &ResourcesDescriptorSet, 1, 0));
		NRI_CHECK(GRenderDevice.CoreInterface.AllocateDescriptorSets(*GlobalDescriptorPool, *GlobalPipelineLayout, 1, &SamplerDescriptorSet, 1, 0));
		{
			uint32_t resourceHeapOffset = uint32_t(-1);
			uint32_t samplerHeapOffset = uint32_t(-1);
			GRenderDevice.CoreInterface.GetDescriptorSetOffsets(*ResourcesDescriptorSet, resourceHeapOffset, samplerHeapOffset);
			R_ASSERT (resourceHeapOffset == 0 && samplerHeapOffset == 0);
		}
		{
			uint32_t resourceHeapOffset = uint32_t(-1);
			uint32_t samplerHeapOffset = uint32_t(-1);
			GRenderDevice.CoreInterface.GetDescriptorSetOffsets(*SamplerDescriptorSet, resourceHeapOffset, samplerHeapOffset);
			R_ASSERT (resourceHeapOffset == 0 && samplerHeapOffset == 0);
		}
	}
	CreateSamplers();
	CreateQuadBuffer();
}
extern u32 UIShaderCounter;
TRenderResourcesManager::~TRenderResourcesManager()
{
	R_ASSERT(!IsRenderThreadRunning());
	delete RenderScene;
	MaterialsManager->Free(DefaultMaterial);
	delete MaterialsManager;
	VERIFY(UIShaderCounter == 0);
	delete TexturesManager;
	delete WhiteTexture;
	delete BlackTexture;
	Tiramisu::RenderCommands::FlushRenderCommands();
	delete DescriptorHeapAllocator;
	delete GlobalShadersManager;
	delete ShaderDefinesManager;
	
	if (QuadGeometryBuffer)
	{
		GRenderDevice.CoreInterface.DestroyBuffer(QuadGeometryBuffer);
	}
	
	
	if (GlobalPipelineLayout)
	{
		GRenderDevice.CoreInterface.DestroyPipelineLayout(GlobalPipelineLayout);
	}
	
	if (GlobalDescriptorPool)
	{
		GRenderDevice.CoreInterface.DestroyDescriptorPool(GlobalDescriptorPool);
	}
	
	if (LinearSampler)
	{
		GRenderDevice.CoreInterface.DestroyDescriptor(LinearSampler);
	}
	
}

void TRenderResourcesManager::Initialize()
{
	R_ASSERT(!IsRenderThreadRunning());
	VERIFY(ShaderDefinesManager == nullptr && GlobalShadersManager == nullptr);
	
	{
		BlackTexture = new TRenderTexture2D;
		RedImageTool::RedImage BlackImage;
		BlackImage.Create(1,1,1,1,RedImageTool::RedTexturePixelFormat::R8G8B8A8);
		BlackImage.Fill({0.f,0.f,0.f,1.f});
		R_ASSERT(BlackTexture->LoadFromImage(BlackImage,false));
	}
	{
		WhiteTexture = new TRenderTexture2D;
		RedImageTool::RedImage WhiteImage;
		WhiteImage.Create(1,1,1,1,RedImageTool::RedTexturePixelFormat::R8G8B8A8);
		WhiteImage.Fill({1.f,1.f,1.f,1.f});
		R_ASSERT(WhiteTexture->LoadFromImage(WhiteImage,false));
	}
	ShaderDefinesManager = new TShaderDefinesManager; 
	GlobalShadersManager = new TGlobalShadersManager(GRenderDevice.GraphicsApi,strstr(Core.Params,"-shader_pdb") || strstr(Core.Params,"-shader_debug"),strstr(Core.Params,"-shader_debug"));
	DescriptorHeapAllocator = new TRenderDescriptorHeapAllocator;
	TexturesManager = new TRenderTexturesManager;
	MaterialsManager = new TRenderMaterialsManager;
	DefaultMaterial = MaterialsManager->Get("default");
	RenderScene = new TRenderScene;
	VERIFY(!IsRenderThreadRunning());
}

bool TRenderResourcesManager::IsCookedMode()
{
	return false;
}

void TRenderResourcesManager::FlushNextFrame()
{
	TexturesManager->FlushNextFrame();
}

void TRenderResourcesManager::FlushNextFrame_RenderThread()
{
	DescriptorHeapAllocator->FlushNextFrame_RenderThread();
}

TRenderResourcesManager* GRenderResourcesManager = nullptr;
