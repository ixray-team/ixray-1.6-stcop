#pragma once

#include "Fsr4BridgeCommon.h"
#include "Fsr4SharedTexture.h"

#include <FidelityFX/upscalers/include/ffx_upscale.h>


class Fsr4Backend
{
public:
    Fsr4Backend();
    ~Fsr4Backend();

    Fsr4Backend(const Fsr4Backend&) = delete;
    Fsr4Backend& operator=(const Fsr4Backend&) = delete;

    /**
     * @brief Набор D3D11-текстур, разрешённых из `Fsr4DrawParameters` на текущем кадре.
     *
     * Все указатели — ComPtr'ы, чтобы держать ресурсы живыми до конца кадра.
     * `reactive` может быть nullptr, если игра не предоставила реактивную маску.
     */
    struct FrameTextures
    {
        Microsoft::WRL::ComPtr<ID3D11Texture2D> color;    ///< unresolved color (render-res)
        Microsoft::WRL::ComPtr<ID3D11Texture2D> depth;    ///< depth (render-res)
        Microsoft::WRL::ComPtr<ID3D11Texture2D> mvec;     ///< motion vectors (render-res)
        Microsoft::WRL::ComPtr<ID3D11Texture2D> reactive; ///< reactive mask (render-res, опционально)
        Microsoft::WRL::ComPtr<ID3D11Texture2D> output;   ///< resolved color (display-res)
    };

    /**
     * @brief Ленивая инициализация: валидация D3D11.4 и загрузка FFX-библиотек.
     *
     * Сохраняет `d3d11Device` и его immediate context, проверяет доступность
     * `ID3D11Device1/5` и `ID3D11DeviceContext4` (Win10 1709+), загружает
     * `amd_fidelityfx_loader_dx12.dll` и `amd_fidelityfx_upscaler_dx12.dll`.
     *
     * @param d3d11Device Игровое D3D11-устройство.
     * @param ffxDllDir   Директория с FFX DLL (обычно рядом с exe).
     *
     * @return true  — предпосылки выполнены, можно вызывать `DrawFrame()`;
     * @return false — nullptr-входы; D3D11.4 недоступен
     *                 (`InteropUnsupported`); DLL/экспорты не найдены
     *                 (`LoaderMissing` / `EntrypointsMissing`).
     *
     * @note D3D12-устройство и FFX-контекст здесь ещё не создаются — ленивая
     *       инициализация в первом `DrawFrame()`.
     */
    bool Initialize(ID3D11Device* d3d11Device, const wchar_t* ffxDllDir);

    /**
     * @brief Полный сброс: ожидание GPU, освобождение FFX-контекста, shared-текстур,
     *        command ring'а, depth-copy pipeline и D3D12-устройства.
     *
     * @note Идемпотентна; безопасна при частичной инициализации.
     * @note После `Shutdown()` допускается повторный `Initialize()`.
     */
    void Shutdown();

    /**
     * @brief Главная точка входа: обработка одного кадра FSR 4 (или fallback 3.1.x).
     *
     * Полный пайплайн (кратко):
     *  1. ResolveFrameTextures — `ID3D11Resource*` → `ID3D11Texture2D`.
     *  2. EnsureDevices — ленивое создание/проверка D3D12-устройства.
     *  3. EnsureSharedTextures — (пере)создание shared-текстур.
     *  4. NeedsContextRebuild / EnsureContext — (пере)создание FFX-контекста.
     *  5. CopyInputsToShared — color/mvec/reactive/depth в shared.
     *  6. RecordAndSubmit — запись dispatch + submit + GPU-wait.
     *  7. CopyOutputToGame — результат обратно в игровую текстуру.
     *
     * @param params Параметры кадра (см. `Fsr4DrawParameters`).
     *
     * @return true при успешном кадре; false при любой ошибке
     *         (статус — в `GetStatus()`).
     *
     * @note После успешного кадра статус сбрасывается в `Ok`, даже если
     *       transient-ошибка была в предыдущем кадре.
     */
    bool DrawFrame(const Fsr4DrawParameters& params);

    /** @brief Текущий статус backend'а (см. `Fsr4Status`). */
    Fsr4Status GetStatus() const { return m_Status; }

    /** @brief true, если активный FFX-контекст реально использует FSR 4 (не fallback). */
    bool IsFsr4Active() const { return m_ChosenIsFsr4; }

    /** @brief Строка версии активного провайдера (заполняется при создании контекста). */
    const xr_string& GetActiveVersionString() const { return m_ActiveVersionName; }

private:
    // ===================== Frame pipeline steps =====================

    /**
     * @brief Разрешает `ID3D11Resource*` из `Fsr4DrawParameters` в конкретные `ID3D11Texture2D`.
     *
     * @return true, если все 5 ресурсов успешно прошли `QueryInterface` к
     *         `ID3D11Texture2D`; false при nullptr или неверном типе ресурса.
     */
    static bool ResolveFrameTextures(const Fsr4DrawParameters& p, FrameTextures& out);

    /**
     * @brief Ленивое создание/проверка D3D12-устройства и interop-инфраструктуры.
     *
     * Быстрый путь: если `anyTexture == m_LastAnyTexture` и D3D12-устройство
     * не device-removed — пропуск без `GetDevice()`.
     *
     * Иначе — полное пересоздание: D3D12-устройство на адаптере игры, очередь,
     * shared fence, command ring.
     *
     * @param anyTexture Любая D3D11-текстура кадра (для детекта смены устройства).
     * @return true при готовом D3D12-устройстве; false при ошибке (статус
     *         `DeviceFailed` / `InteropUnsupported`).
     */
    bool EnsureDevices(ID3D11Texture2D* anyTexture);

    /**
     * @brief Проверяет/создаёт shared-текстуры под текущие игровые ресурсы.
     *
     * Пересоздаёт shared-текстуру, если сменился source-указатель/размер/формат.
     * depth создаётся как R32_FLOAT (`isDepthIntermediate`), output — с UAV-флагом.
     *
     * @return true при успехе всех 5 текстур; false при неудаче
     *         (`DeviceFailed` / `DeviceLost`).
     */
    bool EnsureSharedTextures(const FrameTextures& tex);

    /**
     * @brief Решает, нужно ли пересоздать FFX-контекст на этом кадре.
     *
     * Триггеры: нет контекста; сменилось D3D12-устройство; изменились
     * размеры дисплея; изменились контекстные флаги; изменился render-size,
     * **и** контекст не в dynamic-res режиме.
     */
    bool NeedsContextRebuild(const Fsr4DrawParameters& p) const;

    /**
     * @brief (Пере)создаёт FFX-контекст с учётом провайдера и флагов.
     *
     * Учитывает override версии (`ffxOverrideVersion`), SEH-crash на FSR4
     * (→ fallback на 3.1.x), ошибку override (→ retry без override).
     * Заполняет `m_ActiveVersionName` через `ffxQueryGetProviderVersion`.
     *
     * @return true при успехе; false при провале (статус `ContextFailed`).
     */
    bool EnsureContext(const Fsr4DrawParameters& p);

    /**
     * @brief Копирует color/mvec/reactive/depth из игровых в shared-текстуры.
     *
     * color/mvec/reactive — прямым `CopyResource`; depth — через `CopyDepthToShared`
     * (полноэкранный проход D24/D32→R32_FLOAT). Ошибка depth-копии не фатальна.
     */
    void CopyInputsToShared(const FrameTextures& tex);

    /**
     * @brief Заполняет `ffxDispatchDescUpscale`, записывает dispatch, submit'ит и ждёт GPU.
     *
     * `reset = m_FirstDispatch || cameraReset || Fsr4Flag_Reset` — критично для
     * первого кадра и для camera-cut'ов. `motionVectorScale` учитывает инверсию Y
     * и половину render-res.
     *
     * @return true при успехе; false при SEH-crash (переключение на 3.1.x)
     *         или dispatch-ошибке.
     */
    bool RecordAndSubmit(const Fsr4DrawParameters& p);

    /**
     * @brief Копирует результат из shared-выхода обратно в игровую текстуру.
     *
     * @note GPU-side: копия встанет в очередь после GPU-wait'а из
     *       `WaitD3D11Output`, поэтому вызывается сразу после `RecordAndSubmit`.
     */
    void CopyOutputToGame(const FrameTextures& tex) const;

    /**
     * @brief Обрабатывает провал dispatch'а — переключается на провайдер 3.1.x.
     *
     * При SEH-crash и активном FSR4: ставит `m_ForceProvider3x = true`,
     * уничтожает контекст, возвращает false — на следующем кадре
     * `EnsureContext` создаст 3.1.x. При прочих ошибках — статус `DispatchFailed`.
     *
     * @return Всегда false (используется как ранний выход из `RecordAndSubmit`).
     */
    bool HandleDispatchFailure(ffxReturnCode_t rc);

    // ===================== Depth-copy pipeline =====================

    /**
     * @brief Компилирует и создаёт D3D11-конвейер D24→R32_FLOAT.
     *
     * Fullscreen-triangle без vertex buffer'а (3 вершины). Создаёт VS/PS/DSS/BS/RS/сэмплер.
     *
     * @return true, если pipeline готов под текущее D3D11-устройство.
     * @note Идемпотентно для того же `m_D3D11Device`.
     */
    bool InitDepthCopyPipeline();

    /** @brief Освобождает все ресурсы depth-copy pipeline. */
    void ReleaseDepthCopyPipeline();

    /**
     * @brief Копирует игровую depth в shared R32_FLOAT полноэкранным проходом.
     *
     * Сохраняет/восстанавливает всё состояние D3D11-контекста (IA/VS/PS/SRV/
     * сэмплер/RTV/DSV/DSS/BS/RS/viewports/scissors/VB), чтобы не сломать
     * игровой рендер. Поддерживает D24S8/D32S8/D32F/D16.
     *
     * @param gameDepth Игровая depth-текстура (например, `rt_Position`).
     * @return true при успехе; false при ошибке SRV/RTV или если `m_Depth` не готова.
     */
    bool CopyDepthToShared(ID3D11Texture2D* gameDepth);

    // ===================== Device management =====================

    /**
     * @brief Создаёт D3D12-устройство на указанном адаптере.
     *
     * Пытается через Agility SDK v616, при неудаче — системный `D3D12CreateDevice`.
     * Требует feature level 12.0.
     */
    bool CreateD3D12Device(IDXGIAdapter* adapter);

    /**
     * @brief Создаёт D3D12 fence и открывает его на D3D11 как `ID3D11Fence` + событие.
     *
     * Нужно для cross-API синхронизации (D3D11 сигналит после копий, D3D12
     * ждёт перед dispatch, затем D3D11 ждёт результата).
     */
    bool ShareFenceToD3D11();

    /** @brief Создаёт `kRingSize` allocator'ов и один command list. */
    bool InitCommandRing();

    /** @brief Освобождает allocator'ы и command list кольца. */
    void ReleaseCommandRing();

    /** @brief Освобождает D3D12-устройство/очередь/fence/событие и сбрасывает кэш версий. */
    void ReleaseDevices();

    /** @brief Освобождает все shared-текстуры (color/depth/mvec/output/reactive). */
    void ReleaseSharedTextures();

    /**
     * @brief Освобождает FFX-контекст, shared-текстуры и всё state, зависящее от устройства.
     *
     * Нужна при смене размеров, смене D3D11-устройства и при переключении
     * провайдера (FSR4↔3.1.x).
     */
    void ReleaseUpscalerState();

    /** @brief true, если текущее D3D12-устройство в состоянии device-removed. */
    bool IsDeviceLost() const;

    // ===================== FFX context =====================

    /**
     * @brief Загружает `amd_fidelityfx_upscaler_dx12.dll` и `amd_fidelityfx_loader_dx12.dll`,
     *        резолвит указатели `ffxCreateContext/DestroyContext/Query/Dispatch`.
     *
     * @return true, если все 4 указателя найдены (в loader, иначе в upscaler).
     */
    bool LoadFfxLibrary(const wchar_t* dllDir);

    /**
     * @brief Однократно запрашивает у FFX список версий провайдера.
     *
     * Заполняет `m_VersionIds`/`m_VersionNames`, определяет `m_VersionIdFsr4`
     * (макс. 4.x) и `m_VersionId3x` (макс. 3.1.x). При неудаче — `m_VersionsFailed`.
     */
    void QueryVersionsOnce();

    /**
     * @brief Выбирает ID версии провайдера для создания контекста.
     *
     * @param preferFsr4 Если true и FSR4 доступна и не форсирован fallback → FSR4.
     * @return ID версии (0 = пусть FFX выберет дефолт).
     * @note Побочно выставляет `m_ChosenIsFsr4`.
     */
    u64 PickProviderVersionId(bool preferFsr4);

    /**
     * @brief Транслирует собственные `Fsr4Flags` (контекстные биты) в `FFX_UPSCALE_ENABLE_*`.
     */
    static u32 TranslateFfxFlags(int ourFlags);

    /** @brief Уничтожает текущий FFX-контекст (SEH-безопасно). */
    void DestroyContext();

    // ===================== Command list / sync =====================

    /**
     * @brief Начинает новый командный буфер из кольца, при необходимости ждёт GPU.
     *
     * @param waitForD3D11Value Значение D3D11-fence'а (0 = не ждать).
     * @return Command list или nullptr (таймаут кольца / ошибка Reset).
     */
    ID3D12GraphicsCommandList* BeginFrame(UINT64 waitForD3D11Value);

    /** @brief Отменяет текущий кадр: закрывает command list без submit'а. */
    void AbortFrame();

    /** @brief Закрывает command list, submit'ит его в очередь и сигналит fence. */
    UINT64 EndFrameAndSubmit();

    /**
     * @brief Сигналит shared fence со стороны D3D11 после копирования входов.
     *
     * @return Значение fence'а (передаётся в `BeginFrame`).
     * @note Всегда делает `Flush()` — иначе D3D12-очередь может простаивать.
     */
    UINT64 SignalD3D11Inputs();

    /** @brief GPU-side ожидание на D3D11 завершения D3D12-команды (не блокирует CPU). */
    void WaitD3D11Output(UINT64 value);

    /** @brief Блокирующее ожидание завершения всей работы D3D12-очереди (таймаут 5с). */
    void WaitIdle();

    // ===================== FFX dispatch =====================

    /**
     * @brief Выполняет dispatch upscale в указанный command list.
     *
     * @return `FFX_API_RETURN_OK` при успехе; `Fsr4FfxSehCrashCode` при SEH.
     */
    ffxReturnCode_t DispatchUpscale(ID3D12GraphicsCommandList* cmd, ffxDispatchDescUpscale* desc);

    // ===================== SEH shims =====================
    // Все обёртки ниже ловят структурные исключения внутри FFX DLL и
    // возвращают единый код `Fsr4FfxSehCrashCode` (0x0BADC0DE).

    /** @brief SEH-защищённый `ffxQuery`. */
    ffxReturnCode_t SafeQuery(ffxContext* ctx, ffxQueryDescHeader* desc);

    /** @brief SEH-защищённый `ffxCreateContext`. */
    ffxReturnCode_t SafeCreateContext(ffxContext* ctx, ffxCreateContextDescHeader* desc);

    /** @brief SEH-защищённый `ffxDispatch`. */
    ffxReturnCode_t SafeDispatch(ffxContext* ctx, ffxDispatchDescHeader* desc);

    /** @brief SEH-защищённый `ffxDestroyContext` (глотает исключения). */
    void SafeDestroyContext(ffxContext* ctx);

    // ===================== State =====================

    /** @brief Текущий статус backend'а. Сбрасывается в `Ok` после успешного кадра. */
    Fsr4Status m_Status = Fsr4Status::NotInitialized;

    // --- D3D11 ---
    Microsoft::WRL::ComPtr<ID3D11Device>         m_D3D11Device;   ///< игровое устройство
    Microsoft::WRL::ComPtr<ID3D11Device1>        m_D3D11Device1;  ///< для OpenSharedResource1
    Microsoft::WRL::ComPtr<ID3D11Device5>        m_D3D11Device5;  ///< для OpenSharedFence
    Microsoft::WRL::ComPtr<ID3D11DeviceContext>  m_D3D11Context;  ///< immediate context
    Microsoft::WRL::ComPtr<ID3D11DeviceContext4> m_D3D11Context4; ///< для Signal/Wait на fence

    // --- D3D12 ---
    Microsoft::WRL::ComPtr<ID3D12Device>       m_D3D12Device; ///< приватное D3D12-устройство
    Microsoft::WRL::ComPtr<ID3D12CommandQueue> m_D3D12Queue;  ///< DIRECT-очередь
    Microsoft::WRL::ComPtr<ID3D12Fence>        m_D3D12Fence;  ///< shared timeline fence
    Microsoft::WRL::ComPtr<ID3D11Fence>        m_D3D11Fence;  ///< D3D11-сторона того же fence
    UINT64 m_FenceValue = 0;   ///< текущее значение fence'а
    HANDLE m_FenceEvent = nullptr; ///< событие для SetEventOnCompletion

    // --- Command ring ---
    /** @brief Размер кольца allocator'ов: 4 слоя, чтобы CPU мог убежать на 3 кадра вперёд. */
    static constexpr int kRingSize = 4;
    Microsoft::WRL::ComPtr<ID3D12CommandAllocator>    m_Alloc[kRingSize];      ///< кольцо allocator'ов
    UINT64                                            m_AllocFence[kRingSize] = {}; ///< последний fence каждого слота
    int                                               m_AllocIdx = 0;         ///< следующий слот кольца
    int                                               m_CurrentIdx = -1;      ///< слот текущего открытого кадра
    Microsoft::WRL::ComPtr<ID3D12GraphicsCommandList> m_CmdList;              ///< один командный лист на кадр

    // --- FFX libraries ---
    HMODULE m_LoaderModule   = nullptr; ///< amd_fidelityfx_loader_dx12.dll
    HMODULE m_UpscalerModule = nullptr; ///< amd_fidelityfx_upscaler_dx12.dll
    PfnFfxCreateContext  m_pfnCreateContext  = nullptr; ///< ffxCreateContext
    PfnFfxDestroyContext m_pfnDestroyContext = nullptr; ///< ffxDestroyContext
    PfnFfxQuery          m_pfnQuery          = nullptr; ///< ffxQuery
    PfnFfxDispatch       m_pfnDispatch       = nullptr; ///< ffxDispatch

    // --- Versions ---
    bool                  m_VersionsQueried = false; ///< одноразовый запрос версий выполнен
    bool                  m_VersionsFailed  = false; ///< запрос версий провалился
    xr_vector<u64>        m_VersionIds;              ///< ID всех провайдеров
    xr_vector<xr_string>  m_VersionNames;            ///< имена всех провайдеров
    u64                   m_VersionIdFsr4   = 0;     ///< ID максимальной 4.x
    u64                   m_VersionId3x     = 0;     ///< ID максимальной 3.x
    bool                  m_ForceProvider3x = false; ///< после SEH-crash FSR4 больше не пробуем
    bool                  m_ChosenIsFsr4    = false; ///< какой провайдер реально выбран
    xr_string             m_ActiveVersionName = "none"; ///< имя активного провайдера

    // --- FFX context ---
    ffxContext                            m_Context = nullptr; ///< активный FFX-контекст
    Microsoft::WRL::ComPtr<ID3D12Device>  m_ContextDevice;     ///< D3D12-устройство, на котором создан контекст

    // --- Shared textures ---
    Fsr4SharedTexture m_Color;    ///< unresolved color
    Fsr4SharedTexture m_Depth;    ///< depth (R32_FLOAT, заполняется CopyDepthToShared)
    Fsr4SharedTexture m_Mvec;     ///< motion vectors
    Fsr4SharedTexture m_Output;   ///< resolved color (UAV-таргет для FSR)
    Fsr4SharedTexture m_Reactive; ///< reactive mask (опционально, копируется как есть)

    // --- Context sizing / state ---
    UINT m_CtxOutW    = 0;    ///< display width, для которого создан контекст
    UINT m_CtxOutH    = 0;    ///< display height
    UINT m_CtxRenderW = 0;    ///< render width
    UINT m_CtxRenderH = 0;    ///< render height
    int  m_CtxFlags   = -1;   ///< контекстные флаги (без per-dispatch битов)
    bool m_FirstDispatch = true; ///< первый dispatch после (пере)создания → reset

    // --- Depth-copy pipeline ---
    Microsoft::WRL::ComPtr<ID3D11VertexShader>      m_DepthCopyVS;      ///< fullscreen-triangle VS
    Microsoft::WRL::ComPtr<ID3D11PixelShader>       m_DepthCopyPS;      ///< depth→float PS
    Microsoft::WRL::ComPtr<ID3D11DepthStencilState> m_DepthCopyDSS;     ///< depth disabled
    Microsoft::WRL::ComPtr<ID3D11BlendState>        m_DepthCopyBS;      ///< blend disabled
    Microsoft::WRL::ComPtr<ID3D11RasterizerState>   m_DepthCopyRS;      ///< cull none, no scissor
    Microsoft::WRL::ComPtr<ID3D11SamplerState>      m_DepthCopySampler; ///< point sampler
    Microsoft::WRL::ComPtr<ID3D11Device>            m_DepthCopyDevice;  ///< устройство, под которое создан pipeline
    bool                                            m_DepthCopyReady = false; ///< pipeline готов

    /** @brief Кэш для fast-path в `EnsureDevices` (та же anyTexture → skip `GetDevice`). */
    ID3D11Texture2D* m_LastAnyTexture = nullptr;
};