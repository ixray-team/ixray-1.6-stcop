#pragma once

class TRenderDeferredPass
{
public:
    TRenderDeferredPass();
    ~TRenderDeferredPass();
    void Render(nri::CommandBuffer& CurrentCommandBuffer);
};
