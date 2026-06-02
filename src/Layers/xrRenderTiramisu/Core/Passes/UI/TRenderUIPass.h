#pragma once

class TRenderUIPass
{
public:
    TRenderUIPass();
    ~TRenderUIPass();
    
    
    void Upload(nri::CommandBuffer& CurrentCommandBuffer);
    void Render(nri::CommandBuffer& CurrentCommandBuffer);
    
    nri::Buffer*    GeometryBuffer = nullptr;
    
    
    nri::Buffer*    UploadBuffer = nullptr;
    
    
    nri::Pipeline*  Pipeline = nullptr;
    
    nri::BufferBarrierDesc BufferBarrierDescription = {};
};
