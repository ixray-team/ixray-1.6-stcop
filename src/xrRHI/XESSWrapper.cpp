#include "RHI.h"
#include "XESSWrapper.h"
#include "DLSSWrapper.h"
#if defined(IXR_WINDOWS) && defined(IXR_X64)
#include <xess/xess.h>
#include <xess/xess_d3d11.h>
#include "D3D12/xess_d3d12.h"
#include "D3D12/Device.h"
#endif

XeSSWrapper g_XESSWrapper;

XeSSWrapper::~XeSSWrapper()
{
    Destroy();
}

#if defined(IXR_WINDOWS) && defined(IXR_X64)
struct XeSSCommonAPI
{
    decltype(&xessDestroyContext) Destroy;
    decltype(&xessGetInputResolution) GetInputResolution;
    decltype(&xessSetVelocityScale) SetVelocityScale;
};

static const XeSSCommonAPI& XeSSAPI(bool isD3D12)
{
    auto load = [](const wchar_t* name)
    {
        const auto module = GetModuleHandleW(name);
        R_ASSERT(module);
        XeSSCommonAPI api;
        api.Destroy = reinterpret_cast<decltype(api.Destroy)>(GetProcAddress(module, "xessDestroyContext"));
        api.GetInputResolution = reinterpret_cast<decltype(api.GetInputResolution)>(GetProcAddress(module, "xessGetInputResolution"));
        api.SetVelocityScale = reinterpret_cast<decltype(api.SetVelocityScale)>(GetProcAddress(module, "xessSetVelocityScale"));
        R_ASSERT(api.Destroy && api.GetInputResolution && api.SetVelocityScale);
        return api;
    };
    static const XeSSCommonAPI apis[] = { load(L"libxess_dx11.dll"), load(L"libxess.dll") };
    return apis[isD3D12];
}

static xess_quality_settings_t XeSSQuality(u32 preset, float scale)
{
    static constexpr xess_quality_settings_t quality[] =
    {
        XESS_QUALITY_SETTING_AA,
        XESS_QUALITY_SETTING_QUALITY,
        XESS_QUALITY_SETTING_BALANCED,
        XESS_QUALITY_SETTING_PERFORMANCE,
        XESS_QUALITY_SETTING_ULTRA_PERFORMANCE
    };
    const u32 index = g_DLSSWrapper.GetOptimalPresetForScale(scale, preset);
    R_ASSERT(index < std::size(quality));
    return quality[index];
}
#endif

bool XeSSWrapper::Create(const ContextParameters& params)
{
    Destroy();
#if defined(IXR_WINDOWS) && defined(IXR_X64)
    if (!params.outputWidth || !params.outputHeight)
    {
        return false;
    }
    _isD3D12 = GRHI->APILevel == ERHI_API_LAYER::D3D12;
    xess_context_handle_t context = nullptr;
    xess_result_t result = _isD3D12 ?
        xessD3D12CreateContext(static_cast<ID3D12Device*>(GRHI->DevicePtr->RawDevice), &context) :
        xessD3D11CreateContext(static_cast<ID3D11Device*>(GRHI->DevicePtr->RawDevice), &context);
    if (result < XESS_RESULT_SUCCESS)
    {
        Msg("! XeSS context creation failed (%d)", result);
        return false;
    }
    _context = context;
    if (_isD3D12)
    {
        xess_d3d12_init_params_t desc = {};
        desc.outputResolution = { params.outputWidth, params.outputHeight };
        desc.qualitySetting = XeSSQuality(params.preset, params.scale);
        desc.initFlags = XESS_INIT_FLAG_ENABLE_AUTOEXPOSURE;
        desc.creationNodeMask = 1;
        desc.visibleNodeMask = 1;
        result = xessD3D12Init(context, &desc);
    }
    else
    {
        xess_d3d11_init_params_t desc = {};
        desc.outputResolution = { params.outputWidth, params.outputHeight };
        desc.qualitySetting = XeSSQuality(params.preset, params.scale);
        desc.initFlags = XESS_INIT_FLAG_ENABLE_AUTOEXPOSURE;
        result = xessD3D11Init(context, &desc);
    }
    if (result < XESS_RESULT_SUCCESS)
    {
        Msg("! XeSS initialization failed (%d)", result);
        Destroy();
        return false;
    }
    _params = params;
    _resetHistory = true;
    return true;
#else
    return false;
#endif
}

void XeSSWrapper::Destroy()
{
#if defined(IXR_WINDOWS) && defined(IXR_X64)
    if (_context)
    {
        if (_isD3D12)
        {
            static_cast<InternalDevice12*>(GRHI->DevicePtr)->Flush();
        }
        XeSSAPI(_isD3D12).Destroy(static_cast<xess_context_handle_t>(_context));
        _context = nullptr;
    }
#endif
}

bool XeSSWrapper::GetRenderScale(float& out_scale, u32 preset, float scale, u32 width, u32 height)
{
#if defined(IXR_WINDOWS) && defined(IXR_X64)
    if (!_context || _params.outputWidth != width || _params.outputHeight != height ||
        _params.preset != preset || _params.scale != scale)
    {
        if (!Create({ width, height, preset, scale }))
        {
            return false;
        }
    }
    xess_2d_t output = { width, height };
    xess_2d_t input = {};
    const auto result = XeSSAPI(_isD3D12).GetInputResolution(static_cast<xess_context_handle_t>(_context), &output,
        XeSSQuality(preset, scale), &input);
    if (result < XESS_RESULT_SUCCESS || !input.x || !input.y || !height)
    {
        return false;
    }
    out_scale = float(input.y) / height;
    return true;
#else
    return false;
#endif
}

#if defined(IXR_WINDOWS) && defined(IXR_X64)
template<typename T, typename Execute>
static void FillXeSSParameters(Execute& desc, const XeSSWrapper::DrawParameters& params, bool reset)
{
    desc.pColorTexture = static_cast<T*>(params.pColorTexture->GetRawTexture());
    desc.pVelocityTexture = static_cast<T*>(params.pVelocityTexture->GetRawTexture());
    desc.pDepthTexture = static_cast<T*>(params.pDepthTexture->GetRawTexture());
    desc.pOutputTexture = static_cast<T*>(params.pOutputTexture->GetRawTexture());
    desc.jitterOffsetX = params.jitterOffsetX;
    desc.jitterOffsetY = params.jitterOffsetY;
    desc.exposureScale = 1;
    desc.resetHistory = reset;
    desc.inputWidth = params.inputWidth;
    desc.inputHeight = params.inputHeight;
}
#endif

bool XeSSWrapper::Draw(const DrawParameters& params)
{
#if defined(IXR_WINDOWS) && defined(IXR_X64)
    if (!_context || !params.pColorTexture || !params.pVelocityTexture || !params.pDepthTexture || !params.pOutputTexture)
    {
        return false;
    }
    auto context = static_cast<xess_context_handle_t>(_context);
    xess_result_t result = XeSSAPI(_isD3D12).SetVelocityScale(context, -float(params.inputWidth) * 0.5f, float(params.inputHeight) * 0.5f);
    if (result < XESS_RESULT_SUCCESS)
    {
        return false;
    }
    if (_isD3D12)
    {
        auto& device = *static_cast<InternalDevice12*>(GRHI->DevicePtr);
        xrCriticalSectionGuard guard(device.ContextMutex());
        device.PrepareUpscale(params.pColorTexture, false);
        device.PrepareUpscale(params.pVelocityTexture, false);
        device.PrepareUpscale(params.pDepthTexture, false);
        device.PrepareUpscale(params.pOutputTexture, true);
        xess_d3d12_execute_params_t desc = {};
        FillXeSSParameters<ID3D12Resource>(desc, params, _resetHistory);
        result = xessD3D12Execute(context, device.Commands(), &desc);
        device.UAVBarrier(desc.pOutputTexture);
        device.InvalidateBindings();
    }
    else
    {
        xess_d3d11_execute_params_t desc = {};
        FillXeSSParameters<ID3D11Resource>(desc, params, _resetHistory);
        result = xessD3D11Execute(context, &desc);
    }
    if (result < XESS_RESULT_SUCCESS)
    {
        Msg("! XeSS execution failed (%d)", result);
        return false;
    }
    _resetHistory = false;
    return true;
#else
    return false;
#endif
}
