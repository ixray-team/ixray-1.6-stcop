#pragma once

#include <d3d11_4.h>
#include <wrl/client.h>

#include <FidelityFX/api/include/ffx_api.h>
#include <FidelityFX/api/include/ffx_api_types.h>

/**
 * @file Fsr4BridgeCommon.h
 * @brief Общие типы, разделяемые между `FSR4Wrapper` (публичный фасад) и
 *        `Fsr4Backend` (внутренняя реализация).
 *
 * Здесь живёт только то, что не зависит от конкретного API-провайдера:
 *  - enum статусов (`Fsr4Status`);
 *  - собственные биты флагов (`Fsr4Flags`);
 *  - публичные структуры параметров (`Fsr4ContextParameters`, `Fsr4DrawParameters`);
 *  - константы, используемые обеими сторонами (`Fsr4PerDispatchFlagsMask`,
 *    `Fsr4FfxSehCrashCode`).
 *
 * FFX-специфичные типы (`ffxContext`, `ffxDispatchDescUpscale` и т.п.)
 * сюда **не** попадают — они изолированы в `Fsr4Backend` и `Fsr4SharedTexture`.
 */

/**
 * @brief Текущий статус backend'а FSR 4.
 *
 * Возвращается через `FSR4Wrapper::GetStatus()` / `Fsr4Backend::GetStatus()`
 * и предназначен для логов, консольных команд и UI-индикаторов.
 */
enum class Fsr4Status : int
{
    Ok                 = 0,  ///< Кадр обработан штатно (или ожидание первого кадра).
    NotInitialized     = 1,  ///< `Initialize()` ещё не вызывался.
    LoaderMissing      = 2,  ///< Не найдена `amd_fidelityfx_loader_dx12.dll`.
    EntrypointsMissing = 3,  ///< DLL загружена, но `ffxCreateContext/Destroy/Query/Dispatch` не найдены.
    DeviceFailed       = 4,  ///< Не удалось создать D3D12-устройство или interop-инфраструктуру.
    InteropUnsupported = 5,  ///< Нет D3D11.1/5 или `ID3D11DeviceContext4` (требуется Win10 1709+).
    NoVersions         = 6,  ///< FFX не вернул список версий провайдера.
    ContextFailed      = 7,  ///< `ffxCreateContext` провалился.
    DispatchFailed     = 8,  ///< `ffxDispatch` провалился (не SEH — обычная ошибка).
    DeviceLost         = 9,  ///< D3D12-устройство удалено (TDR, смена драйвера и т.п.).
};

/**
 * @brief Собственные флаги FSR 4, независимые от FFX.
 *
 * Биты 0..6 — **контекстные**: при их изменении требуется пересоздать FFX-контекст.
 * Биты 7..11 — **per-dispatch**: не влияют на контекст и транслируются в
 * `ffxDispatchDescUpscale` каждый кадр.
 *
 * Диапазоны разделены через `Fsr4PerDispatchFlagsMask` — это позволяет
 * `Fsr4Backend::NeedsContextRebuild` игнорировать per-dispatch изменения.
 */
enum Fsr4Flags : int
{
    // ---- Контекстные (биты 0..6) ----
    Fsr4Flag_Hdr            = 1 << 0,  ///< HDR-вход (FFX_UPSCALE_ENABLE_HIGH_DYNAMIC_RANGE).
    Fsr4Flag_DepthInverted  = 1 << 1,  ///< Инвертированная глубина (1 = ближе).
    Fsr4Flag_DepthInfinite  = 1 << 2,  ///< Бесконечный far-plane.
    Fsr4Flag_AutoExposure   = 1 << 3,  ///< Берём экспозицию из игры.
    Fsr4Flag_DebugChecking  = 1 << 4,  ///< Включить debug-проверки FFX.
    Fsr4Flag_DynamicRes     = 1 << 5,  ///< Разрешить менять render-size без пересоздания контекста.
    Fsr4Flag_PreferFsr4     = 1 << 6,  ///< Приоритет FSR4 над 3.1.x.

    // ---- Per-dispatch (биты 7..11) ----
    Fsr4Flag_Reset          = 1 << 7,  ///< Принудительный сброс temporal-истории (camera cut).
    Fsr4Flag_MvJitterCancel = 1 << 8,  ///< Компенсация jitter'а в motion vectors.
    Fsr4Flag_Reactive       = 1 << 9,  ///< Использовать реактивную маску.
    Fsr4Flag_ModelFix       = 1 << 10, ///< Зарезервировано (спец-кейсы рендера).
    Fsr4Flag_Optic          = 1 << 11, ///< Зарезервировано (оптика/оптический прицел).
};

/**
 * @brief Маска per-dispatch флагов (биты 7..11).
 *
 * Используется в `NeedsContextRebuild`: изменения в этих битах **не**
 * требуют пересоздания контекста.
 */
inline constexpr int Fsr4PerDispatchFlagsMask = Fsr4Flag_Reset | Fsr4Flag_Reactive | Fsr4Flag_Optic;

/**
 * @brief Сентинел-код возврата SEH-шимов `Fsr4Backend`.
 *
 * Возвращается обёртками `SafeQuery`/`SafeCreateContext`/`SafeDispatch`,
 * если внутри FFX DLL произошло структурное исключение (AV и т.п.).
 * Позволяет отличить «FFX реально упал» от обычного `ffxReturnCode_t`.
 */
inline constexpr ffxReturnCode_t Fsr4FfxSehCrashCode = 0x0BADC0DE;

/**
 * @brief Параметры инициализации FSR 4 (вход `FSR4Wrapper::Create`).
 *
 * Заполняется рендер-таргетом при `init_fsr4()`.
 */
struct Fsr4ContextParameters
{
    /** @brief Контекстные флаги (`Fsr4Flags`, только биты 0..6). */
    u32                 flags         = 0;

    /** @brief Максимальный render-size (входное разрешение; display при dynamic-res). */
    FfxApiDimensions2D  maxRenderSize = { 0, 0 };

    /** @brief Разрешение дисплея (output / upscale). */
    FfxApiDimensions2D  displaySize   = { 0, 0 };

    /** @brief Игровое D3D11-устройство. */
    ID3D11Device*       device        = nullptr;
};

/**
 * @brief Параметры одного кадра FSR 4 (вход `FSR4Wrapper::Draw`).
 *
 * Игровые ресурсы передаются как `ID3D11Resource*` (без привязки к типу),
 * `Fsr4Backend::ResolveFrameTextures` приводит их к `ID3D11Texture2D`.
 * Все входы должны принадлежать тому же D3D11-устройству, что и в
 * `Fsr4ContextParameters::device`.
 */
struct Fsr4DrawParameters
{
    /** @brief Immediate-контекст игрового устройства (для копий/синхронизации). */
    ID3D11DeviceContext* deviceContext = nullptr;

    // ---- Входы (render-resolution, game-owned) ----
    ID3D11Resource* unresolvedColorResource            = nullptr; ///< Цвет до resolve (обязательно).
    ID3D11Resource* motionvectorResource               = nullptr; ///< Motion vectors (обязательно).
    ID3D11Resource* depthbufferResource                = nullptr; ///< Глубина (обязательно).
    ID3D11Resource* reactiveMapResource                = nullptr; ///< Reactive mask (опционально).
    ID3D11Resource* transparencyAndCompositionResource = nullptr; ///< Зарезервировано (не используется FSR4).

    // ---- Выход (display-resolution, game-owned) ----
    ID3D11Resource* resolvedColorResource = nullptr; ///< Куда положить результат (обязательно).

    // ---- Размеры ----
    u32 renderWidth   = 0; ///< Ширина входов (render-res).
    u32 renderHeight  = 0; ///< Высота входов (render-res).
    u32 displayWidth  = 0; ///< Ширина выхода (display-res).
    u32 displayHeight = 0; ///< Высота выхода (display-res).

    // ---- Камера ----
    bool  cameraReset   = false; ///< Сбросить историю в этом кадре (camera cut / телепорт).
    float cameraJitterX = 0.f;   ///< Смещение jitter'а по X (в пикселях render-res).
    float cameraJitterY = 0.f;   ///< Смещение jitter'а по Y.

    // ---- Sharpening ----
    bool  enableSharpening = true; ///< Включить RCAS-sharpening.
    float sharpness        = 0.f;  ///< 0 = максимальный sharpening, 1 = без sharpening (FFX-семантика).

    // ---- Timing и фрустум ----
    float frameTimeDelta = 0.f;    ///< Дельта времени кадра (мс) для motion-компенсации.
    float nearPlane      = 1.f;    ///< Near-plane в мировых единицах.
    float farPlane       = 10.f;   ///< Far-plane в мировых единицах.
    float fovYRadians    = 1.57f;  ///< Вертикальный FOV в радианах (не горизонтальный!).

    /** @brief Битовая маска `Fsr4Flags`. */
    int flags = 0;
};

/**
 * @brief Преобразует `Fsr4Status` в человекочитаемую строку для логов/UI.
 *
 * @param s Статус.
 * @return Статическая C-строка (никогда не nullptr).
 */
inline const char* Fsr4StatusToString(Fsr4Status s)
{
    switch (s)
    {
        case Fsr4Status::Ok:                 return "OK";
        case Fsr4Status::NotInitialized:     return "Not initialized";
        case Fsr4Status::LoaderMissing:      return "FFX loader DLL missing";
        case Fsr4Status::EntrypointsMissing: return "FFX entry points missing";
        case Fsr4Status::DeviceFailed:       return "D3D12 device failed";
        case Fsr4Status::InteropUnsupported: return "Interop unsupported";
        case Fsr4Status::NoVersions:         return "No FFX providers";
        case Fsr4Status::ContextFailed:      return "Context creation failed";
        case Fsr4Status::DispatchFailed:     return "Dispatch failed";
        case Fsr4Status::DeviceLost:         return "Device lost";
    }
    return "Unknown";
}