#pragma once

class TRenderUIPass
{
public:
    TRenderUIPass();
    ~TRenderUIPass();
    
    
    void Upload(nri::CommandBuffer& CurrentCommandBuffer);
    void Render(nri::CommandBuffer& CurrentCommandBuffer,const nri::Viewport& Viewport);
    
    nri::Buffer*    GeometryBuffer = nullptr;
    
    
    nri::Buffer*    UploadBuffer = nullptr;
    
    
    nri::Pipeline*  Pipeline = nullptr;
    
    nri::BufferBarrierDesc BufferBarrierDescription = {};
    
    
    xr_vector<FUIVertex>			Vertexes;
    xr_vector<FXRayUIPrimitive>		Primitivs;
};
