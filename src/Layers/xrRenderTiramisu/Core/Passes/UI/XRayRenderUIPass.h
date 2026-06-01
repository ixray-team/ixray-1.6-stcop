#pragma once

class XRayRenderUIPass
{
public:
    XRayRenderUIPass();
    ~XRayRenderUIPass();
    
    
    void Upload(nri::CommandBuffer& CurrentCommandBuffer);
    void Render(nri::CommandBuffer& CurrentCommandBuffer);
    
    nri::Buffer*    GeometryBuffer = nullptr;
    nri::Memory*    GeometryMemory = nullptr;
    
    
    nri::Buffer*    UploadBuffer = nullptr;
    nri::Memory*    UploadBufferMemory = nullptr;
    
    
    nri::Pipeline*  Pipeline = nullptr;
    
    nri::BufferBarrierDesc BufferBarrierDescription = {};
};
