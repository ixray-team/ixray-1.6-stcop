#include "XRayRender.h"

#include "Passes/Geometry/TRenderDeferredPass.h"
#include "Passes/UI/XRayRenderUIPass.h"
#include "Resources/XRayRenderResourcesManager.h"
#include "Resources/Shaders/XRayShaderType.h"
#include "Resources/Shaders/Defines/XRayShaderDefinesContainer.h"
#include "Resources/Shaders/Global/XRayGlobalShadersManager.h"
#include "Resources/Textures/RenderTargets/XRayRenderTarget2D.h"
XRayRender* GRender = nullptr;

XRayRender::XRayRender() 
{
}

XRayRender::~XRayRender()
{
}



void XRayRender::Initialize()
{
    Destroy();
    
    NRI_CHECK(GRenderDevice.CoreInterface.CreateFence(*GRenderDevice.Device, nri::SWAPCHAIN_SEMAPHORE, WaitSemaphore));
    NRI_CHECK(GRenderDevice.CoreInterface.CreateFence(*GRenderDevice.Device, nri::SWAPCHAIN_SEMAPHORE, SignalSemaphore));
    NRI_CHECK(GRenderDevice.CoreInterface.CreateFence(*GRenderDevice.Device, 0, FrameFence));
    	
    QueuedFrames.resize(1);
    for (FXRayQueuedFrame& QueuedFrame : QueuedFrames) 
    {
        NRI_CHECK(GRenderDevice.CoreInterface.CreateCommandAllocator(*GRenderDevice.GraphicsQueue, QueuedFrame.CommandAllocator));
        NRI_CHECK(GRenderDevice.CoreInterface.CreateCommandBuffer(*QueuedFrame.CommandAllocator, QueuedFrame.CommandBuffer));
    }
    
    CreateGlobalConstantBuffer();
    
    OutputRenderTarget = new XRayRenderTarget2D(1024, 768, nri::Format::RGBA8_UNORM,{} ,"Output");
    OutputRenderTarget->GetOrCreateHeapIndex();
    
    DepthRenderTarget = new XRayRenderTarget2D(1024, 768, nri::Format::D24_UNORM_S8_UINT,{} ,"Depth");
    
    UIPass = new XRayRenderUIPass;
    GeometryPass = new TRenderDeferredPass;
}

void XRayRender::Destroy()
{
    WaitGPU();
 
    {
        if (GlobalConstantDescriptor)
        {
            GRenderDevice.CoreInterface.DestroyDescriptor(GlobalConstantDescriptor);
            GlobalConstantDescriptor = nullptr;
        }
        if (GlobalConstantBuffer)
        {
            GRenderDevice.CoreInterface.DestroyBuffer(GlobalConstantBuffer);
            GlobalConstantBuffer = nullptr;
        }
        if (GlobalConstantBufferMemory)
        {
            GRenderDevice.CoreInterface.FreeMemory(GlobalConstantBufferMemory);
            GlobalConstantBufferMemory = nullptr;
        }
        GlobalConstantDescriptorSet  = nullptr;
    }
    
    if (OutputRenderTarget)
    {
        delete OutputRenderTarget;
        OutputRenderTarget = nullptr;
    }
    
    if (DepthRenderTarget)
    {
        delete DepthRenderTarget;
        DepthRenderTarget = nullptr;
    }
    
    if (UIPass)
    {
        delete UIPass;
        UIPass = nullptr;  
    }
    
    if (GeometryPass)
    {
        delete GeometryPass;
        GeometryPass = nullptr;
    }

    if (Pipeline)
    {
        GRenderDevice.CoreInterface.DestroyPipeline(Pipeline);
        Pipeline = nullptr;
    }
    
    if (TestPipeline)
    {
        GRenderDevice.CoreInterface.DestroyPipeline(TestPipeline);
        TestPipeline = nullptr;
    }
   
    if (WaitSemaphore)
    {   
        GRenderDevice.CoreInterface.DestroyFence(WaitSemaphore);
        WaitSemaphore = nullptr;
    }
    
    if (SignalSemaphore)
    {
        GRenderDevice.CoreInterface.DestroyFence(SignalSemaphore);
        SignalSemaphore = nullptr;
    }
    
    if (FrameFence)
    {
        GRenderDevice.CoreInterface.DestroyFence(FrameFence);
        FrameFence = nullptr;
    }
    
    for (FXRayQueuedFrame& QueuedFrame : QueuedFrames)
    {
        if (QueuedFrame.CommandBuffer)
        {
            GRenderDevice.CoreInterface.DestroyCommandBuffer(QueuedFrame.CommandBuffer);
            QueuedFrame.CommandBuffer = nullptr;
        }
        if (QueuedFrame.CommandAllocator)
        {
            GRenderDevice.CoreInterface.DestroyCommandAllocator(QueuedFrame.CommandAllocator);
            QueuedFrame.CommandAllocator = nullptr;
        }
    }
    FrameIndex = 0;
}



void XRayRender::Submit(XRayRenderViewport* ToViewport)
{
    if(!IsWaitSubmit)
    {
        return;
    }
    // Pipeline
    if (!Pipeline)
    {
        nri::VertexStreamDesc vertexStreamDesc = {};
        vertexStreamDesc.bindingSlot = 0;

        nri::VertexInputDesc vertexInputDesc = {};
        vertexInputDesc.attributes = FXRayUIVertex::VertexAttributeDescription;
        vertexInputDesc.attributeNum = 3;
        vertexInputDesc.streams = &vertexStreamDesc;
        vertexInputDesc.streamNum = 1;

        nri::InputAssemblyDesc inputAssemblyDesc = {};
        inputAssemblyDesc.topology = nri::Topology::TRIANGLE_LIST;

        nri::RasterizationDesc rasterizationDesc = {};
        rasterizationDesc.fillMode = nri::FillMode::SOLID;
        rasterizationDesc.cullMode = nri::CullMode::FRONT;

        nri::ColorAttachmentDesc colorAttachmentDesc = {};
        colorAttachmentDesc.format = ToViewport->GetSwapChainFormat();
        colorAttachmentDesc.colorWriteMask = nri::ColorWriteBits::RGBA;
        colorAttachmentDesc.blendEnabled = true;
        colorAttachmentDesc.colorBlend = { nri::BlendFactor::SRC_ALPHA, nri::BlendFactor::ONE_MINUS_SRC_ALPHA, nri::BlendOp::ADD};

        nri::OutputMergerDesc outputMergerDesc = {};
        outputMergerDesc.colors = &colorAttachmentDesc;
        outputMergerDesc.colorNum = 1;

        XRayShaderDefinesContainer ShaderDefinesContainer;

        nri::ShaderDesc shaderStages[2] = {};
        {
            const xr_vector<char>& ShaderCode = GRenderResourcesManager->GlobalShadersManager->GetShader("ui_no_transform", EXRayShaderType::Vertex, ShaderDefinesContainer);
            shaderStages[0].stage = nri::StageBits::VERTEX_SHADER;
            shaderStages[0].bytecode = ShaderCode.data();
            shaderStages[0].size = ShaderCode.size();
            shaderStages[0].entryPointName = "Main";
        }
        {
            const xr_vector<char>& ShaderCode = GRenderResourcesManager->GlobalShadersManager->GetShader("output",EXRayShaderType::Pixel,ShaderDefinesContainer);
            shaderStages[1].stage = nri::StageBits::FRAGMENT_SHADER;
            shaderStages[1].bytecode = ShaderCode.data();
            shaderStages[1].size = ShaderCode.size();
            shaderStages[1].entryPointName = "Main";
        }
        
        nri::GraphicsPipelineDesc graphicsPipelineDesc = {};
        graphicsPipelineDesc.pipelineLayout =  GRenderResourcesManager->GlobalPipelineLayout;
        graphicsPipelineDesc.vertexInput = &vertexInputDesc;
        graphicsPipelineDesc.inputAssembly = inputAssemblyDesc;
        graphicsPipelineDesc.rasterization = rasterizationDesc;
        graphicsPipelineDesc.outputMerger = outputMergerDesc;
        graphicsPipelineDesc.shaders = shaderStages;
        graphicsPipelineDesc.shaderNum = 2;
        graphicsPipelineDesc.cache = nullptr;
        NRI_CHECK(GRenderDevice.CoreInterface.CreateGraphicsPipeline(*GRenderDevice.Device, graphicsPipelineDesc, Pipeline));

    }
    
    
    ToViewport->BeginRender( GRenderResourcesManager->GlobalDescriptorPool);
    {
        nri::Dim_t w = (nri::Dim_t)ToViewport->GetWidth();
        nri::Dim_t h = (nri::Dim_t)ToViewport->GetHeight();

        // helper::Annotation annotation(NRI, CommandBuffer, "Triangle");
        
        nri::CommandBuffer& CommandBuffer = ToViewport->GetCurrentCommandBuffer();
        
        GRenderDevice.CoreInterface.CmdSetPipelineLayout(CommandBuffer, nri::BindPoint::GRAPHICS, *GRenderResourcesManager->GlobalPipelineLayout);
       
        {
            const nri::Viewport viewport = {0.0f, 0.0f, (float)w, (float)h, 0.0f, 1.0f};
            GRenderDevice.CoreInterface.CmdSetViewports(CommandBuffer, &viewport, 1);
            const nri::Rect ScissorRect = {0,0,w,h};
            GRenderDevice.CoreInterface.CmdSetScissors(CommandBuffer,&ScissorRect,1);
        }
        
        GRenderDevice.CoreInterface.CmdSetPipeline(CommandBuffer, *Pipeline);
        
        nri::SetDescriptorSetDesc descriptorSet0 = {0, GRenderResourcesManager->ResourcesDescriptorSet};
        nri::SetDescriptorSetDesc descriptorSet1 = {1, GRenderResourcesManager->SamplerDescriptorSet};
        nri::SetDescriptorSetDesc descriptorSet2 = {2, GlobalConstantDescriptorSet};
        GRenderDevice.CoreInterface.CmdSetDescriptorSet(CommandBuffer,descriptorSet0);
        GRenderDevice.CoreInterface.CmdSetDescriptorSet(CommandBuffer,descriptorSet1);
        GRenderDevice.CoreInterface.CmdSetDescriptorSet(CommandBuffer,descriptorSet2);
        
        GRenderDevice.CoreInterface.CmdSetIndexBuffer(CommandBuffer, *GRenderResourcesManager->QuadGeometryBuffer, 0, nri::IndexType::UINT16);

        nri::VertexBufferDesc vertexBufferDesc = {};
        vertexBufferDesc.buffer = GRenderResourcesManager->QuadGeometryBuffer;
        vertexBufferDesc.offset = GRenderResourcesManager->QuadGeometryOffset;
        vertexBufferDesc.stride = sizeof(FXRayUIVertex);
        GRenderDevice.CoreInterface.CmdSetVertexBuffers(CommandBuffer, 0, &vertexBufferDesc, 1);

        
        GRenderDevice.CoreInterface.CmdDrawIndexed(CommandBuffer, {6, 1, 0, 0, OutputRenderTarget->GetOrCreateHeapIndex()});
    }
    ToViewport->EndRender(SignalSemaphore, nullptr);
    IsWaitSubmit = false;
}



void XRayRender::Render()
{
    VERIFY(IsWaitSubmit == false);
    IsWaitSubmit = true;
    
    GRenderDevice.CoreInterface.Wait(*FrameFence, FrameIndex >= QueuedFrames.size() ? 1 + FrameIndex - QueuedFrames.size() : 0);
    
    UpdateGlobalConstantBuffer();
    GRenderResourcesManager->FlushNextFrame();
    
    uint32_t QueuedFrameIndex = FrameIndex % QueuedFrames.size();
    const FXRayQueuedFrame& QueuedFrame = QueuedFrames[QueuedFrameIndex];
    
    nri::CommandBuffer& CurrentCommandBuffer = *QueuedFrame.CommandBuffer;
    
    GRenderDevice.CoreInterface.ResetCommandAllocator(*QueuedFrame.CommandAllocator);
    GRenderDevice.CoreInterface.BeginCommandBuffer(CurrentCommandBuffer, GRenderResourcesManager->GlobalDescriptorPool);
    
    GRenderDevice.CoreInterface.CmdBeginAnnotation(CurrentCommandBuffer,"Main",nri::BGRA_UNUSED);
    {
        {
            UIPass->Upload(CurrentCommandBuffer);
        }
        {
            nri::TextureBarrierDesc TextureBarrierDescription[2] = {};
            TextureBarrierDescription[0].texture = OutputRenderTarget->Texture;
            TextureBarrierDescription[0].layerNum = 1;
            TextureBarrierDescription[0].mipNum = 1;
            OutputRenderTarget->SetNewAccessLayoutStage(TextureBarrierDescription[0],{nri::AccessBits::COLOR_ATTACHMENT, nri::Layout::COLOR_ATTACHMENT});
        
            TextureBarrierDescription[1].texture = DepthRenderTarget->Texture;
            TextureBarrierDescription[1].layerNum = 1;
            TextureBarrierDescription[1].mipNum = 1;
            DepthRenderTarget->SetNewAccessLayoutStage(TextureBarrierDescription[1],{nri::AccessBits::DEPTH_STENCIL_ATTACHMENT_WRITE, nri::Layout::DEPTH_STENCIL_ATTACHMENT});
        
            
            nri::BarrierDesc BarrierDescription = {};
            BarrierDescription.textureNum = 2;
            BarrierDescription.textures  = TextureBarrierDescription;
            GRenderDevice.CoreInterface.CmdBarrier(CurrentCommandBuffer,BarrierDescription);
        }
        {
            nri::AttachmentDesc ColorAttachmentDescription = {};
            ColorAttachmentDescription.descriptor =  OutputRenderTarget->DescriptorAttachment;
            ColorAttachmentDescription.clearValue.color.f = {0.0f, 0.0f, 0.0f, 1.0f};
            ColorAttachmentDescription.loadOp = nri::LoadOp::CLEAR;

            nri::RenderingDesc RenderingDescription = {};
            RenderingDescription.colorNum = 1;
            RenderingDescription.colors = &ColorAttachmentDescription;
            RenderingDescription.depth.clearValue = {1,0x0};
            RenderingDescription.depth.loadOp = nri::LoadOp::CLEAR;
            RenderingDescription.depth.descriptor = DepthRenderTarget->DescriptorAttachment;
            
            GRenderDevice.CoreInterface.CmdBeginRendering(CurrentCommandBuffer, RenderingDescription);
        }
        {
            GRenderDevice.CoreInterface.CmdSetPipelineLayout(CurrentCommandBuffer, nri::BindPoint::GRAPHICS, * GRenderResourcesManager->GlobalPipelineLayout);
            nri::SetDescriptorSetDesc descriptorSet0 = {0, GRenderResourcesManager->ResourcesDescriptorSet};
            nri::SetDescriptorSetDesc descriptorSet1 = {1, GRenderResourcesManager->SamplerDescriptorSet};
            nri::SetDescriptorSetDesc descriptorSet2 = {2, GlobalConstantDescriptorSet};
            GRenderDevice.CoreInterface.CmdSetDescriptorSet(CurrentCommandBuffer,descriptorSet0);
            GRenderDevice.CoreInterface.CmdSetDescriptorSet(CurrentCommandBuffer,descriptorSet1);
            GRenderDevice.CoreInterface.CmdSetDescriptorSet(CurrentCommandBuffer,descriptorSet2);
        }
        {
            const nri::Viewport viewport = {0.0f, 0.0f, (float)OutputRenderTarget->TextureDescription.width, (float)OutputRenderTarget->TextureDescription.height, 0.0f, 1.0f};
            GRenderDevice.CoreInterface.CmdSetViewports(CurrentCommandBuffer, &viewport, 1);
                
            const nri::Rect ScissorRect = {0,0,OutputRenderTarget->TextureDescription.width,OutputRenderTarget->TextureDescription.height};
            GRenderDevice.CoreInterface.CmdSetScissors(CurrentCommandBuffer,&ScissorRect,1);
        }
        
        {
            GeometryPass->Render(CurrentCommandBuffer);
            UIPass->Render(CurrentCommandBuffer);
        }
        
       
        GRenderDevice.CoreInterface.CmdEndRendering(CurrentCommandBuffer);
        {
            nri::TextureBarrierDesc TextureBarrierDescription = {};
            TextureBarrierDescription.texture = OutputRenderTarget->Texture;
            TextureBarrierDescription.layerNum = 1;
            TextureBarrierDescription.mipNum = 1;
            OutputRenderTarget->SetNewAccessLayoutStage(TextureBarrierDescription, {nri::AccessBits::SHADER_RESOURCE, nri::Layout::SHADER_RESOURCE});

            nri::BarrierDesc BarrierDescription = {};
            BarrierDescription.textureNum = 1;
            BarrierDescription.textures = &TextureBarrierDescription;
            GRenderDevice.CoreInterface.CmdBarrier(CurrentCommandBuffer, BarrierDescription);
        }
    }
    
    GRenderDevice.CoreInterface.CmdEndAnnotation(CurrentCommandBuffer);
    GRenderDevice.CoreInterface.EndCommandBuffer(CurrentCommandBuffer);
    
    {
        nri::FenceSubmitDesc SignalFencesSubmitDescription[2] = {};
        SignalFencesSubmitDescription[0].fence = SignalSemaphore;
        SignalFencesSubmitDescription[1].fence = FrameFence;
        SignalFencesSubmitDescription[1].value = ++FrameIndex;
        
        nri::QueueSubmitDesc QueueSubmitDescription = {};
		
        QueueSubmitDescription.commandBuffers = &QueuedFrame.CommandBuffer;
        QueueSubmitDescription.commandBufferNum = 1;
		
        QueueSubmitDescription.signalFences = SignalFencesSubmitDescription;
        QueueSubmitDescription.signalFenceNum = 2;

        GRenderDevice.CoreInterface.QueueSubmit(*GRenderDevice.GraphicsQueue, QueueSubmitDescription);
    }
    GRenderResourcesManager->DescriptorHeapAllocator->UpdateDescriptorRanges();
    
}

void XRayRender::WaitGPU()
{
    GRenderDevice.CoreInterface.QueueWaitIdle(GRenderDevice.GraphicsQueue);

    if (GRenderResourcesManager)
    {
        GRenderResourcesManager->FlushNextFrame();
        GRenderResourcesManager->DescriptorHeapAllocator->UpdateDescriptorRanges();
    }
}

void XRayRender::ResizeRenderTarget(uint32_t InWidth, uint32_t InHeight)
{
    WaitGPU();
    if (OutputRenderTarget)
    {
        delete OutputRenderTarget;
    }
    OutputRenderTarget = new XRayRenderTarget2D(InWidth, InHeight, nri::Format::RGBA8_UNORM,{} ,"Output");
    OutputRenderTarget->GetOrCreateHeapIndex();
    
    if (DepthRenderTarget)
    {
        delete DepthRenderTarget;
    }
    DepthRenderTarget = new XRayRenderTarget2D(InWidth, InHeight, nri::Format::D24_UNORM_S8_UINT,{} ,"Depth");
}

void XRayRender::CreateGlobalConstantBuffer()
{
    { // Constant buffer
        nri::BufferDesc BufferDescription = {};
        BufferDescription.size = Align( sizeof(FXRayRenderConstantBuffer), GRenderDevice.DeviceDescription.memoryAlignment.constantBufferOffset );
        BufferDescription.usage = nri::BufferUsageBits::CONSTANT_BUFFER;
        NRI_CHECK(GRenderDevice.CoreInterface.CreateBuffer(*GRenderDevice.Device, BufferDescription, GlobalConstantBuffer));
    }
    {
        nri::ResourceGroupDesc ResourceGroupDescription = {};
        ResourceGroupDescription.memoryLocation = nri::MemoryLocation::HOST_UPLOAD;
        ResourceGroupDescription.bufferNum = 1;
        ResourceGroupDescription.buffers = &GlobalConstantBuffer;

        NRI_CHECK(GRenderDevice.HelperInterface.AllocateAndBindMemory(*GRenderDevice.Device, ResourceGroupDescription, &GlobalConstantBufferMemory));
    }
    
    { 
        nri::BufferViewDesc BufferViewDescription = {};
        BufferViewDescription.buffer = GlobalConstantBuffer;
        BufferViewDescription.type = nri::BufferView::CONSTANT_BUFFER;
        BufferViewDescription.offset = 0;
        BufferViewDescription.size =  Align( sizeof(FXRayRenderConstantBuffer), GRenderDevice.DeviceDescription.memoryAlignment.constantBufferOffset );
        NRI_CHECK(GRenderDevice.CoreInterface.CreateBufferView(BufferViewDescription, GlobalConstantDescriptor));
    }
    {
        NRI_CHECK(GRenderDevice.CoreInterface.AllocateDescriptorSets(*GRenderResourcesManager->GlobalDescriptorPool, *GRenderResourcesManager->GlobalPipelineLayout, 2, &GlobalConstantDescriptorSet, 1, 0));
        nri::UpdateDescriptorRangeDesc UpdateDescriptorRangeDescription = {GlobalConstantDescriptorSet, 0, 0, &GlobalConstantDescriptor, 1};
        GRenderDevice.CoreInterface.UpdateDescriptorRanges(&UpdateDescriptorRangeDescription, 1);
    }
  
}

void XRayRender::UpdateGlobalConstantBuffer()
{
    // Update constants
    if (FXRayRenderConstantBuffer* ConstantBuffer = (FXRayRenderConstantBuffer*)GRenderDevice.CoreInterface.MapBuffer(*GlobalConstantBuffer, 0, sizeof(FXRayRenderConstantBuffer))) 
    {
        ConstantBuffer->SceneView = {(float)OutputRenderTarget->TextureDescription.width,(float)OutputRenderTarget->TextureDescription.height,1.f/OutputRenderTarget->TextureDescription.width,1.f/OutputRenderTarget->TextureDescription.height};
        ConstantBuffer->ViewProjection = DevicePtr->mFullTransform;
        GRenderDevice.CoreInterface.UnmapBuffer(*GlobalConstantBuffer);
    }
    
    
}
