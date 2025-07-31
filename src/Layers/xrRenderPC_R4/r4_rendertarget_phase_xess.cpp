#include "stdafx.h"

#include "OverlayAPI\XESSWrapper.h"

extern Fvector3 ps_r_taa_jitter_full;

void CRenderTarget::init_xess()
{
    g_XESSWrapper.Destroy();

    XeSSWrapper::ContextParameters initParams = {};

    // Устанавливаем выходное разрешение (целевое разрешение экрана)
    initParams.outputWidth = (u32)RCache.get_target_width();
    initParams.outputHeight = (u32)RCache.get_target_height();

    // Устанавливаем качество (можно изменить на XESS_QUALITY_SETTING_BALANCED и т.д.)
    initParams.qualitySetting = XESS_QUALITY_SETTING_QUALITY;

    // Флаги инициализации (можно добавить XESS_INIT_FLAG_HIGH_RES_MV и другие при необходимости)
    initParams.initFlags = XESS_INIT_FLAG_NONE;

    initParams.device = RDevice;

    g_XESSWrapper.Create(initParams);
}

bool CRenderTarget::phase_xess()
{
    PIX_EVENT(XESS);

    XeSSWrapper::DrawParameters xessParams = {};
    xessParams.deviceContext = RContext;

    // Устанавливаем входные ресурсы
    xessParams.pColorTexture = rt_Generic_0->pSurface;
    xessParams.pVelocityTexture = rt_Velocity->pSurface;
    xessParams.pDepthTexture = rt_Position->pSurface;

    // Опциональные ресурсы (можно оставить nullptr)
    xessParams.pExposureScaleTexture = nullptr;
    xessParams.pResponsivePixelMaskTexture = nullptr;

    // Выходной ресурс
    xessParams.pOutputTexture = rt_Generic->pSurface;

    // Разрешение исходного рендера
    xessParams.inputWidth = (u32)RCache.get_width();
    xessParams.inputHeight = (u32)RCache.get_height();

    // Jitter камеры
    xessParams.jitterOffsetX = ps_r_taa_jitter_full.x;
    xessParams.jitterOffsetY = ps_r_taa_jitter_full.y;

    // Параметры камеры
    xessParams.nearPlane = Device.fViewportNear;
    xessParams.farPlane = g_pGamePersistent->Environment().CurrentEnv->far_plane;
    xessParams.fovH = deg2rad(Device.fFOV); // XeSS использует горизонтальный FOV в радианах

    // Сброс истории (при необходимости)
    xessParams.resetHistory = 0;

    // Масштаб экспозиции (по умолчанию 1.0)
    xessParams.exposureScale = 1.0f;

    return g_XESSWrapper.Draw(xessParams);
}

