#pragma once

#include "TiramisuRenderTypes.h"

// Записывает UI draw data в отдельный pass поверх результата сцены.
class TiramisuRenderUIPass
{
public:
    TiramisuRenderUIPass();
    ~TiramisuRenderUIPass();
    
    
    void Upload(nri::CommandBuffer& CurrentCommandBuffer);
    void Render(nri::CommandBuffer& CurrentCommandBuffer,const nri::Viewport& Viewport);
    
    nri::Buffer*    GeometryBuffer = nullptr;
    
    
    nri::Buffer*    UploadBuffer = nullptr;
    
    
    nri::Pipeline*  Pipeline = nullptr;
    
    nri::BufferBarrierDesc BufferBarrierDescription = {};
    
    
    xr_vector<FUIVertex>			Vertexes;
    xr_vector<FXRayUIPrimitive>		Primitivs;
};
