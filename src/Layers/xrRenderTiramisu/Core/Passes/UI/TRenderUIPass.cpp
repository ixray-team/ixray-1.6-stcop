#include "TRenderUIPass.h"

#include "src/xrEngine/IGame_Persistent.h"

TRenderUIPass::TRenderUIPass()
{
	
	{ 
		nri::BufferDesc BufferDescription = {};
		BufferDescription.size = 16 * 1024* 6* sizeof(FUIVertex);
		BufferDescription.usage = nri::BufferUsageBits::VERTEX_BUFFER;
		NRI_CHECK(GRenderDevice.CoreInterface.CreateCommittedBuffer(*GRenderDevice.Device,nri::MemoryLocation::DEVICE ,0.f, BufferDescription,GeometryBuffer));
		
	}
	 
	{ 
		nri::BufferDesc BufferDescription = {};
		BufferDescription.size = 16 * 1024* 6* sizeof(FUIVertex);
		NRI_CHECK(GRenderDevice.CoreInterface.CreateCommittedBuffer(*GRenderDevice.Device,nri::MemoryLocation::DEVICE_UPLOAD ,0, BufferDescription, UploadBuffer));
	}
	
	{
		nri::VertexStreamDesc vertexStreamDesc = {};
		vertexStreamDesc.bindingSlot = 0;

		nri::VertexInputDesc vertexInputDesc = {};
		vertexInputDesc.attributes = FUIVertex::VertexAttributeDescription;
		vertexInputDesc.attributeNum = 3;
		vertexInputDesc.streams = &vertexStreamDesc;
		vertexInputDesc.streamNum = 1;

		nri::InputAssemblyDesc inputAssemblyDesc = {};
		inputAssemblyDesc.topology = nri::Topology::TRIANGLE_LIST;

		nri::RasterizationDesc rasterizationDesc = {};
		rasterizationDesc.fillMode = nri::FillMode::SOLID;
		rasterizationDesc.cullMode = nri::CullMode::NONE;

		nri::ColorAttachmentDesc colorAttachmentDesc = {};
		colorAttachmentDesc.format = nri::Format::RGBA8_UNORM;
		colorAttachmentDesc.colorWriteMask = nri::ColorWriteBits::RGBA;
		colorAttachmentDesc.blendEnabled = true;
		colorAttachmentDesc.colorBlend = { nri::BlendFactor::SRC_ALPHA, nri::BlendFactor::ONE_MINUS_SRC_ALPHA, nri::BlendOp::ADD};

		nri::OutputMergerDesc OutputMergerDescription = {};
		OutputMergerDescription.colors = &colorAttachmentDesc;
		OutputMergerDescription.depthStencilFormat = nri::Format::D24_UNORM_S8_UINT;
		OutputMergerDescription.depth = {nri::CompareOp::ALWAYS,false,false};
		OutputMergerDescription.colorNum = 1;
		

		TShaderDefinesContainer ShaderDefinesContainer;

		nri::ShaderDesc shaderStages[2] = {};
		{
			const xr_vector<char>& ShaderCode = GRenderResourcesManager->GlobalShadersManager->GetShader("ui_screen_transform", EShaderType::Vertex, ShaderDefinesContainer);
			shaderStages[0].stage = nri::StageBits::VERTEX_SHADER;
			shaderStages[0].bytecode = ShaderCode.data();
			shaderStages[0].size = ShaderCode.size();
			shaderStages[0].entryPointName = "Main";
		}
		{
			const xr_vector<char>& ShaderCode = GRenderResourcesManager->GlobalShadersManager->GetShader("ui",EShaderType::Pixel,ShaderDefinesContainer);
			shaderStages[1].stage = nri::StageBits::FRAGMENT_SHADER;
			shaderStages[1].bytecode = ShaderCode.data();
			shaderStages[1].size = ShaderCode.size();
			shaderStages[1].entryPointName = "Main";
		}
		
		nri::GraphicsPipelineDesc graphicsPipelineDesc = {};
		graphicsPipelineDesc.pipelineLayout = GRenderResourcesManager->GlobalPipelineLayout;
		graphicsPipelineDesc.vertexInput = &vertexInputDesc;
		graphicsPipelineDesc.inputAssembly = inputAssemblyDesc;
		graphicsPipelineDesc.rasterization = rasterizationDesc;
		graphicsPipelineDesc.outputMerger = OutputMergerDescription;
		graphicsPipelineDesc.shaders = shaderStages;
		graphicsPipelineDesc.shaderNum = 2;
		graphicsPipelineDesc.cache = nullptr;
		NRI_CHECK(GRenderDevice.CoreInterface.CreateGraphicsPipeline(*GRenderDevice.Device, graphicsPipelineDesc, Pipeline));

	}
	
	BufferBarrierDescription.buffer = GeometryBuffer;
	BufferBarrierDescription.before = {nri::AccessBits::NONE, nri::StageBits::NONE};
	BufferBarrierDescription.after = {nri::AccessBits::NONE, nri::StageBits::NONE};
}

TRenderUIPass::~TRenderUIPass()
{
	GRenderDevice.CoreInterface.DestroyBuffer(GeometryBuffer);
	GRenderDevice.CoreInterface.DestroyBuffer(UploadBuffer);
	
	GRenderDevice.CoreInterface.DestroyPipeline(Pipeline);
}

void TRenderUIPass::Upload(nri::CommandBuffer& CurrentCommandBuffer)
{
	g_pGamePersistent->OnRenderPPUI_main();	// PP-UI
	
	if (GUIRender.Vertexes.size() >= 16 * 1024* 6)
	{
		return;
	}
	{
		if (FUIVertex* UploadVertexes = static_cast<FUIVertex*>(GRenderDevice.CoreInterface.MapBuffer(*UploadBuffer, 0, nri::WHOLE_SIZE)))
		{
			memcpy(UploadVertexes, GUIRender.Vertexes.data(), GUIRender.Vertexes.size() * sizeof(FUIVertex));
			GRenderDevice.CoreInterface.UnmapBuffer(*UploadBuffer);
		}
	}
	
	{
		nri::BarrierDesc BarrierDescription = {};
		BarrierDescription.bufferNum = 1;
		BarrierDescription.buffers  = &BufferBarrierDescription;
		
		
		BufferBarrierDescription.before = BufferBarrierDescription.after;
		BufferBarrierDescription.after = {nri::AccessBits::COPY_DESTINATION, nri::StageBits::COPY};
		GRenderDevice.CoreInterface.CmdBarrier(CurrentCommandBuffer,BarrierDescription);
		
		GRenderDevice.CoreInterface.CmdCopyBuffer(CurrentCommandBuffer,*GeometryBuffer,0,*UploadBuffer,0,nri::WHOLE_SIZE);
		
		BufferBarrierDescription.before = BufferBarrierDescription.after;
		BufferBarrierDescription.after = {nri::AccessBits::VERTEX_BUFFER, nri::StageBits::VERTEX_SHADER};
		GRenderDevice.CoreInterface.CmdBarrier(CurrentCommandBuffer,BarrierDescription);
	}
}

void TRenderUIPass::Render(nri::CommandBuffer& CurrentCommandBuffer)
{
	GRenderDevice.CoreInterface.CmdBeginAnnotation(CurrentCommandBuffer,"UI",nri::BGRA_UNUSED);
	if (GUIRender.Vertexes.size() >= 16 * 1024* 6)
	{
		GUIRender.Flush();
		GRenderDevice.CoreInterface.CmdEndAnnotation(CurrentCommandBuffer);
		return;
	}
	
	GRenderDevice.CoreInterface.CmdSetPipeline(CurrentCommandBuffer, *Pipeline);

	nri::VertexBufferDesc VertexBufferDescription = {};
	VertexBufferDescription.buffer = GeometryBuffer;
	VertexBufferDescription.offset = 0;
	VertexBufferDescription.stride = sizeof(FUIVertex);
	GRenderDevice.CoreInterface.CmdSetVertexBuffers(CurrentCommandBuffer, 0, &VertexBufferDescription, 1);
	
	for (const FXRayUIPrimitive& Primitve :GUIRender.Primitivs)
	{
		if (Primitve.VertexCount == 0)
		{
			continue;
		}
		GRenderDevice.CoreInterface.CmdDraw(CurrentCommandBuffer, {Primitve.VertexCount , 1, Primitve.VertexOffset,  Primitve.Texture->GetOrCreateHeapIndex()});
	}
	
	GUIRender.Flush();
	
	GRenderDevice.CoreInterface.CmdEndAnnotation(CurrentCommandBuffer);
}


