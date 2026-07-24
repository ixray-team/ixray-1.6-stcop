#include "TiramisuDefaultMaterialRenderProxy.h"
#include "Resources/Materials/TiramisuRenderMaterialPipelineRegistry.h"
#include "Resources/Materials/TiramisuRenderMaterialShaderLibrary.h"

namespace
{
const char* GetLegacyVertexFactoryName(const EVertexType VertexType)
{
    switch (VertexType)
    {
    case EVertexType::BaseWithLightMap: return "legacy_level_lmap";
    case EVertexType::BaseWithLightColor: return "legacy_level_color";
    default: return "legacy_unknown";
    }
}

u64 MakeLegacyPipelineKey(const FMaterialAssetId& AssetReference,
    const EVertexType VertexType)
{
    FMaterialPipelineKey Key;
    Key.MasterMaterial = AssetReference;
    Key.VertexFactory = GetLegacyVertexFactoryName(VertexType);
    Key.RenderPassSignature = "legacy_geometry:rgba8:d24s8";
    Key.Backend = GRenderDevice.GraphicsApi == nri::GraphicsAPI::D3D12 ? "d3d12" : "vulkan";
    Key.ShaderModel = "6.6";
    Key.CompilerOptions = "legacy_global_shader;descriptor_heap_indexing";
    const u64 Hash = Key.StableHash();
    return Hash != 0 ? Hash : 1;
}
} // namespace

TiramisuDefaultMaterialRenderProxy::TiramisuDefaultMaterialRenderProxy(
    FMaterialAssetId InAssetReference)
{
    CheckIsGameThread();
    AssetReference = InAssetReference.Value == "default" ?
        FMaterialAssetId{"ee5ffbc0-bd24-4aa8-9e16-50651ca1c269"} :
        std::move(InAssetReference);
    ENQUEUE_RENDER_COMMAND(TiramisuDefaultMaterialRenderProxy::Initialize)(
        [this, AssetReference = AssetReference]
        {
            Initialize_RenderThread(AssetReference);
        });
}

void TiramisuDefaultMaterialRenderProxy::Initialize_RenderThread(
    const FMaterialAssetId& AssetReference)
{
    CheckIsRenderThread();
    VERIFY(Pipelines.empty());

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

		TiramisuShaderDefinesContainer ShaderDefinesContainer;

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

    auto RegisterPipeline = [&](const EVertexType VertexType, const char* ShaderName,
        nri::VertexAttributeDesc* Attributes)
    {
        nri::Pipeline* Pipeline = LambdaCreatePipeline(ShaderName, ShaderName, Attributes);
        FPipelineEntry Entry;
        Entry.Handle = GRenderResourcesManager->MaterialPipelineRegistry->RegisterPipeline_RenderThread(Pipeline, true);
        Entry.PipelineKey = MakeLegacyPipelineKey(AssetReference, VertexType);
        Entry.VertexFactory = GetLegacyVertexFactoryName(VertexType);
        VERIFY(Entry.Handle.IsValid());
        Pipelines.emplace(FPipelineMapKey{EMaterialPass::GBuffer, VertexType},
            std::move(Entry));
    };

	RegisterPipeline(EVertexType::BaseWithLightColor, "scene_vertex",
        FLegacyLevelVertex_BaseWithLightColor::VertexAttributeDescription);
	RegisterPipeline(EVertexType::BaseWithLightMap, "scene_lmap",
        FLegacyLevelVertex_BaseWithLightMap::VertexAttributeDescription);

    const auto Program = GRenderResourcesManager->MaterialShaderLibrary->Find_RenderThread(
        AssetReference, EMaterialPass::Validation);
    if (Program && Program->IsComplete())
    {
        nri::VertexStreamDesc VertexStreamDescription = {};
        VertexStreamDescription.bindingSlot = 0;

        nri::VertexInputDesc VertexInputDescription = {};
        VertexInputDescription.attributes = FStaticMeshVertex::VertexAttributeDescription;
        VertexInputDescription.attributeNum = 6;
        VertexInputDescription.streams = &VertexStreamDescription;
        VertexInputDescription.streamNum = 1;

        nri::InputAssemblyDesc InputAssemblyDescription = {};
        InputAssemblyDescription.topology = nri::Topology::TRIANGLE_LIST;

        nri::RasterizationDesc RasterizationDescription = {};
        RasterizationDescription.fillMode = nri::FillMode::SOLID;
        RasterizationDescription.cullMode = nri::CullMode::BACK;

        nri::ColorAttachmentDesc ColorAttachmentDescription = {};
        ColorAttachmentDescription.format = nri::Format::RGBA8_UNORM;
        ColorAttachmentDescription.colorWriteMask = nri::ColorWriteBits::RGBA;

        nri::OutputMergerDesc OutputMergerDescription = {};
        OutputMergerDescription.colors = &ColorAttachmentDescription;
        OutputMergerDescription.colorNum = 1;

        nri::ShaderDesc ShaderDescriptions[2] = {};
        ShaderDescriptions[0].stage = nri::StageBits::VERTEX_SHADER;
        ShaderDescriptions[0].bytecode = Program->Vertex->Bytecode.data();
        ShaderDescriptions[0].size = Program->Vertex->Bytecode.size();
        ShaderDescriptions[0].entryPointName = Program->Vertex->EntryPoint.c_str();
        ShaderDescriptions[1].stage = nri::StageBits::FRAGMENT_SHADER;
        ShaderDescriptions[1].bytecode = Program->Pixel->Bytecode.data();
        ShaderDescriptions[1].size = Program->Pixel->Bytecode.size();
        ShaderDescriptions[1].entryPointName = Program->Pixel->EntryPoint.c_str();

        nri::GraphicsPipelineDesc PipelineDescription = {};
        PipelineDescription.pipelineLayout = GRenderResourcesManager->GlobalPipelineLayout;
        PipelineDescription.vertexInput = &VertexInputDescription;
        PipelineDescription.inputAssembly = InputAssemblyDescription;
        PipelineDescription.rasterization = RasterizationDescription;
        PipelineDescription.outputMerger = OutputMergerDescription;
        PipelineDescription.shaders = ShaderDescriptions;
        PipelineDescription.shaderNum = 2;

        nri::Pipeline* Pipeline = nullptr;
        NRI_CHECK(GRenderDevice.CoreInterface.CreateGraphicsPipeline(
            *GRenderDevice.Device, PipelineDescription, Pipeline));

        FPipelineEntry Entry;
        Entry.Handle = GRenderResourcesManager->MaterialPipelineRegistry->
            RegisterPipeline_RenderThread(Pipeline, true);
        Entry.PipelineKey = Program->Vertex->PipelineKey;
        Entry.VertexFactory = Program->Vertex->VertexFactory;
        VERIFY(Entry.Handle.IsValid());
        Pipelines.emplace(FPipelineMapKey{EMaterialPass::Validation,
            EVertexType::StaticMesh}, std::move(Entry));
    }

}

TiramisuDefaultMaterialRenderProxy::~TiramisuDefaultMaterialRenderProxy()
{
    CheckIsRenderThread();
    for (auto& [Key, Pipeline] : Pipelines)
    {
        GRenderResourcesManager->MaterialPipelineRegistry->ReleasePipeline_RenderThread(Pipeline.Handle);
    }
}

xr_optional<FMaterialPassProxy> TiramisuDefaultMaterialRenderProxy::ResolvePass(
    const EMaterialPass Pass, const EVertexType VertexType) const
{
    CheckIsRenderThread();
    const auto Pipeline = Pipelines.find(FPipelineMapKey{Pass, VertexType});
    if (Pipeline == Pipelines.end())
        return std::nullopt;

    FMaterialPassProxy Proxy;
    Proxy.Pass = Pass;
    Proxy.PipelineKey = Pipeline->second.PipelineKey;
    Proxy.Pipeline = Pipeline->second.Handle;
    Proxy.VertexFactory = Pipeline->second.VertexFactory;
    Proxy.Revision = 1;
    return Proxy;
}

TiramisuRenderTextureResourceProxy* TiramisuDefaultMaterialRenderProxy::GetTexture() const
{
    CheckIsRenderThread();
    VERIFY(TextureResourceProxy);
    return TextureResourceProxy;
}

const FMaterialAssetId&
TiramisuDefaultMaterialRenderProxy::GetAssetReference() const
{
    CheckIsRenderThread();
    return AssetReference;
}

xr_span<const FMaterialTextureParameterBinding>
TiramisuDefaultMaterialRenderProxy::GetTextureParameters() const
{
    CheckIsRenderThread();
    return {};
}
