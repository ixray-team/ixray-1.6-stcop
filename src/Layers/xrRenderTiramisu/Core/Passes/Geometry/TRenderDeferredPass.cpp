#include "TRenderDeferredPass.h"

#include "Resources/LegacyScene/TRenderLegacyScene.h"

TRenderDeferredPass::TRenderDeferredPass()
{
	auto LambdaCreatePipeline = [&](const char* PixelShaderName, const char* VertexShaderName,nri::VertexAttributeDesc* Attributes)
    {
		nri::VertexStreamDesc vertexStreamDesc = {};
		vertexStreamDesc.bindingSlot = 0;

		nri::VertexInputDesc VertexInputDescription = {};
		VertexInputDescription.attributes = Attributes;
		VertexInputDescription.attributeNum = 6;
		VertexInputDescription.streams = &vertexStreamDesc;
		VertexInputDescription.streamNum = 1;

		nri::InputAssemblyDesc InputAssemblyDescription = {};
		InputAssemblyDescription.topology = nri::Topology::TRIANGLE_LIST;

		nri::RasterizationDesc RasterizationDescription = {};
		RasterizationDescription.fillMode = nri::FillMode::SOLID;
		RasterizationDescription.cullMode = nri::CullMode::BACK;

		nri::ColorAttachmentDesc colorAttachmentDesc = {};
		colorAttachmentDesc.format = nri::Format::RGBA8_UNORM;
		colorAttachmentDesc.colorWriteMask = nri::ColorWriteBits::RGBA;
		colorAttachmentDesc.blendEnabled = false;
		colorAttachmentDesc.colorBlend = { nri::BlendFactor::SRC_ALPHA, nri::BlendFactor::ONE_MINUS_SRC_ALPHA, nri::BlendOp::ADD};

		nri::OutputMergerDesc OutputMergerDescription = {};
		OutputMergerDescription.colors = &colorAttachmentDesc;
    	OutputMergerDescription.depthStencilFormat = nri::Format::D24_UNORM_S8_UINT;
    	OutputMergerDescription.depth = {nri::CompareOp::LESS,true,false};
		OutputMergerDescription.colorNum = 1;

		XRayShaderDefinesContainer ShaderDefinesContainer;

		nri::ShaderDesc ShaderStagesDescription[2] = {};
		{
			const xr_vector<char>& ShaderCode = GRenderResourcesManager->GlobalShadersManager->GetShader(PixelShaderName, EXRayShaderType::Vertex, ShaderDefinesContainer);
			ShaderStagesDescription[0].stage = nri::StageBits::VERTEX_SHADER;
			ShaderStagesDescription[0].bytecode = ShaderCode.data();
			ShaderStagesDescription[0].size = ShaderCode.size();
			ShaderStagesDescription[0].entryPointName = "Main";
		}
		{
			const xr_vector<char>& ShaderCode = GRenderResourcesManager->GlobalShadersManager->GetShader(VertexShaderName,EXRayShaderType::Pixel,ShaderDefinesContainer);
			ShaderStagesDescription[1].stage = nri::StageBits::FRAGMENT_SHADER;
			ShaderStagesDescription[1].bytecode = ShaderCode.data();
			ShaderStagesDescription[1].size = ShaderCode.size();
			ShaderStagesDescription[1].entryPointName = "Main";
		}
		
		nri::GraphicsPipelineDesc GraphicsPipelineDescription = {};
		GraphicsPipelineDescription.pipelineLayout = GRenderResourcesManager->GlobalPipelineLayout;
		GraphicsPipelineDescription.vertexInput = &VertexInputDescription;
		GraphicsPipelineDescription.inputAssembly = InputAssemblyDescription;
		GraphicsPipelineDescription.rasterization = RasterizationDescription;
		GraphicsPipelineDescription.outputMerger = OutputMergerDescription;
		GraphicsPipelineDescription.shaders = ShaderStagesDescription;
		GraphicsPipelineDescription.shaderNum = 2;
		GraphicsPipelineDescription.cache = nullptr;
		nri::Pipeline* OutPipeline = nullptr;
		NRI_CHECK(GRenderDevice.CoreInterface.CreateGraphicsPipeline(*GRenderDevice.Device, GraphicsPipelineDescription, OutPipeline));
		return OutPipeline;
	};
	Pipeline_LightVertex = LambdaCreatePipeline("scene_vertex","scene_vertex",FXRayLegacyLevelVertex_BaseWithLightColor::VertexAttributeDescription);
	Pipeline_LightMap = LambdaCreatePipeline("scene_lmap","scene_lmap",FXRayLegacyLevelVertex_BaseWithLightMap::VertexAttributeDescription);
}

TRenderDeferredPass::~TRenderDeferredPass()
{
	if (Pipeline_LightVertex)
	{
		GRenderDevice.CoreInterface.DestroyPipeline(Pipeline_LightVertex);
	}
	if (Pipeline_LightMap)
	{
		GRenderDevice.CoreInterface.DestroyPipeline(Pipeline_LightMap);
	}
}

void TRenderDeferredPass::Render(nri::CommandBuffer& CurrentCommandBuffer)
{
    GRenderDevice.CoreInterface.CmdBeginAnnotation(CurrentCommandBuffer,"DeferredPass",nri::BGRA_UNUSED);
    TLegacyRenderGraph::TRenderList RenderItems = GRenderResourcesManager->LegacyScene->GetRenderGraph().RenderList;
    for (const FLegacyVisualRenderItem& RenderItem :RenderItems)
    {
    	if (RenderItem.SceneVertexBuffer.VertexType == EXRayLegacyLevelVertexType::BaseWithLightColor)
    	{
    		GRenderDevice.CoreInterface.CmdSetPipeline(CurrentCommandBuffer, *Pipeline_LightVertex);
    	}
    	else if (RenderItem.SceneVertexBuffer.VertexType == EXRayLegacyLevelVertexType::BaseWithLightMap)
    	{
    		GRenderDevice.CoreInterface.CmdSetPipeline(CurrentCommandBuffer, *Pipeline_LightMap);
    	}

    	nri::VertexBufferDesc VertexBufferDescription = {};
    	VertexBufferDescription.buffer = RenderItem.VertexBuffer;
    	VertexBufferDescription.offset = RenderItem.SceneVertexBuffer.Offset;
    	VertexBufferDescription.stride = RenderItem.SceneVertexBuffer.Stride;
    	GRenderDevice.CoreInterface.CmdSetVertexBuffers(CurrentCommandBuffer, 0, &VertexBufferDescription, 1);
    	GRenderDevice.CoreInterface.CmdSetIndexBuffer(CurrentCommandBuffer,*RenderItem.IndexBuffer,RenderItem.SceneIndexBuffer.Offset,nri::IndexType::UINT16);
    	GRenderDevice.CoreInterface.CmdDrawIndexed(CurrentCommandBuffer, {RenderItem.CountIndex , 1, RenderItem.OffsetIndex, static_cast<int32_t>(RenderItem.OffsetVertex),RenderItem.Texture->GetOrCreateHeapIndex()});
    	
	
    }
    GRenderDevice.CoreInterface.CmdEndAnnotation(CurrentCommandBuffer);
}
