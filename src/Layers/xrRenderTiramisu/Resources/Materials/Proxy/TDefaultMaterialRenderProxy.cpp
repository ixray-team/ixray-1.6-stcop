#include "TDefaultMaterialRenderProxy.h"

TDefaultMaterialRenderProxy::TDefaultMaterialRenderProxy()
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

		TShaderDefinesContainer ShaderDefinesContainer;

		nri::ShaderDesc ShaderStagesDescription[2] = {};
		{
			const xr_vector<char>& ShaderCode = GRenderResourcesManager->GlobalShadersManager->GetShader(PixelShaderName, EShaderType::Vertex, ShaderDefinesContainer);
			ShaderStagesDescription[0].stage = nri::StageBits::VERTEX_SHADER;
			ShaderStagesDescription[0].bytecode = ShaderCode.data();
			ShaderStagesDescription[0].size = ShaderCode.size();
			ShaderStagesDescription[0].entryPointName = "Main";
		}
		{
			const xr_vector<char>& ShaderCode = GRenderResourcesManager->GlobalShadersManager->GetShader(VertexShaderName,EShaderType::Pixel,ShaderDefinesContainer);
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
	Pipelines.insert({EVertexType::BaseWithLightColor,LambdaCreatePipeline("scene_vertex","scene_vertex",FLegacyLevelVertex_BaseWithLightColor::VertexAttributeDescription)});
	Pipelines.insert({EVertexType::BaseWithLightMap,LambdaCreatePipeline("scene_lmap","scene_lmap",FLegacyLevelVertex_BaseWithLightMap::VertexAttributeDescription)});

}

TDefaultMaterialRenderProxy::~TDefaultMaterialRenderProxy()
{
    for (auto& [VertexType, Pipeline] : Pipelines)
    {
        GRenderDevice.CoreInterface.DestroyPipeline(Pipeline);
    }
}

const xr_map<EVertexType, nri::Pipeline*>& TDefaultMaterialRenderProxy::GetPipelines() const
{
    return Pipelines;
}

TRenderTextureResourceProxy* TDefaultMaterialRenderProxy::GetTexture() const
{
    VERIFY(TextureResourceProxy);
    return TextureResourceProxy;
}
