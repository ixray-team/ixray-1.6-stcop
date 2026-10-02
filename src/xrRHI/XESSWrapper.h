#pragma once
class IRHISurface;

class RHI_API XeSSWrapper
{
public:
    struct ContextParameters
    {
        u32 outputWidth = 0;
        u32 outputHeight = 0;
        u32 preset = 5;
        float scale = 1;
    };
    struct DrawParameters
    {
        IRHISurface* pColorTexture = nullptr;
        IRHISurface* pVelocityTexture = nullptr;
        IRHISurface* pDepthTexture = nullptr;
        IRHISurface* pOutputTexture = nullptr;
        float jitterOffsetX = 0;
        float jitterOffsetY = 0;
        u32 inputWidth = 0;
        u32 inputHeight = 0;
    };
    ~XeSSWrapper();
    bool Create(const ContextParameters& params);
    void Destroy();
    bool Draw(const DrawParameters& params);
    bool GetRenderScale(float& out_scale, u32 preset, float scale, u32 width, u32 height);
    bool IsCreated() const { return _context != nullptr; }

private:
    void* _context = nullptr;
    ContextParameters _params;
    bool _isD3D12 = false;
    bool _resetHistory = true;
};

extern RHI_API XeSSWrapper g_XESSWrapper;
