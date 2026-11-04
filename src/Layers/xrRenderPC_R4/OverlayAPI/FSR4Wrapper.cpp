#include "stdafx.h"
#include "FSR4Wrapper.h"

extern ENGINE_API u32   ps_render_scale_preset;
extern ENGINE_API float ps_render_scale;

FSR4Wrapper g_Fsr4Wrapper;

static constexpr float kPresetRatios[] = { 1.0f, 1.5f, 1.7f, 2.0f, 3.0f };

FSR4Wrapper::~FSR4Wrapper()
{
    Destroy();
}

bool FSR4Wrapper::GetRenderScale(float& RenderScale)
{
    if (ContextParams.displaySize.width == 0 || ContextParams.displaySize.height == 0)
    {
        Msg("! [FSR4] Display size not set. Fallback!");
        return false;
    }

    u32 PresetID = ps_render_scale_preset;
    if (PresetID == 5)
    {
        PresetID = 0;
        if (ps_render_scale < 0.9f) ++PresetID;
        if (ps_render_scale < 0.7f) ++PresetID;
        if (ps_render_scale < 0.6f) ++PresetID;
        if (ps_render_scale < 0.5f) ++PresetID;
    }

    const float Ratio    = PresetID < 5 ? kPresetRatios[PresetID] : 1.0f;
    const u32   DisplayW = ContextParams.displaySize.width;
    const u32   DisplayH = ContextParams.displaySize.height;
    const u32   RenderW  = (u32)ceilf((float)DisplayW / Ratio);
    const u32   RenderH  = (u32)ceilf((float)DisplayH / Ratio);

    Msg("* [FSR4] Resolution Target - %ux%u (preset %u)", RenderW, RenderH, PresetID);
    RenderScale = float(RenderH) / float(DisplayH);
    Msg("* [FSR4] RenderScale %.4f", RenderScale);
    return true;
}

bool FSR4Wrapper::Create(ContextParameters params)
{
    Destroy();

    if (RFeatureLevel < D3D_FEATURE_LEVEL_11_0 || !params.device ||
        !params.maxRenderSize.width || !params.displaySize.width)
        return false;

    ContextParams = params;

    wchar_t exePath[MAX_PATH] = {};
    GetModuleFileNameW(nullptr, exePath, MAX_PATH);
    if (wchar_t* slash = wcsrchr(exePath, L'\\'))
        *slash = 0;

    if (!Backend.Initialize(params.device, exePath))
        return false;

    Created = true;
    return true;
}

void FSR4Wrapper::Destroy()
{
    if (!Created)
        return;

    Backend.Shutdown();
    Created = false;
    ContextParams = {};
}

bool FSR4Wrapper::Draw(const DrawParameters& params)
{
    if (!Created)
        return false;

    return Backend.DrawFrame(params);
}