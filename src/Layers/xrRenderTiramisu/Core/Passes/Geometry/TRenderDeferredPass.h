#pragma once

class TRenderDeferredPass
{
public:
    TRenderDeferredPass();
    ~TRenderDeferredPass();
    void Render(nri::CommandBuffer& CurrentCommandBuffer);
    
    nri::Pipeline* Pipeline_LightVertex = nullptr;
    nri::Pipeline* Pipeline_LightMap = nullptr;
};
