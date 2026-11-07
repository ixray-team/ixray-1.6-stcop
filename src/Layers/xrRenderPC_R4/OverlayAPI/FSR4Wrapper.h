#pragma once

#if defined(IXR_APPLE_SERIES)
class FSR4Wrapper
{
public:
	bool GetRenderScale(float&) { return false; }
	void Destroy() {}
};
inline FSR4Wrapper g_Fsr4Wrapper;
#else
#include "fsr4bridge/Fsr4Backend.h"
#include <FidelityFX/api/include/ffx_api_types.h>

class FSR4Wrapper
{
public:
    /** @brief Параметры инициализации (device, displaySize, maxRenderSize, flags). */
    using ContextParameters = Fsr4ContextParameters;

    /** @brief Параметры одного кадра (текстуры, размеры, камера, jitter, timing). */
    using DrawParameters    = Fsr4DrawParameters;

    /**
     * @brief Возвращает рассчитанный render-scale (renderHeight / displayHeight).
     *
     * Переводит `ps_render_scale_preset` в коэффициент соотношения рендер→дисплей.
     * Для пресета Custom (ID == 5) каскадно подбирает коэффициент по
     * `ps_render_scale` через пороги 0.9 / 0.7 / 0.6 / 0.5.
     *
     * @param[out] RenderScale Итоговый коэффициент (0..1], например 0.6667 для Quality.
     *
     * @return true при успехе; false — если в `ContextParams` не заданы размеры
     *         дисплея (обёртка ещё не инициализирована).
     *
     * @note Только чтение: не меняет состояние обёртки.
     */
    bool GetRenderScale(float& RenderScale);

    /**
     * @brief Инициализирует обёртку: валидирует входные параметры и загружает FFX DLL.
     *
     * Сначала освобождает предыдущее состояние (`Destroy`), затем проверяет
     * feature level ≥ 11.0, наличие `device` и ненулевые размеры, определяет
     * директорию exe и вызывает `Backend.Initialize()`.
     *
     * @param params Параметры: device, displaySize, maxRenderSize, flags.
     *
     * @return true  — обёртка готова к `Draw()`;
     * @return false — валидация или `Backend.Initialize()` провалились
     *                 (детали в `GetStatus()`).
     *
     * @note FFX-контекст здесь **не создаётся** — это происходит лениво в
     *       первом `Draw()` (`Fsr4Backend::EnsureContext`), чтобы переживать
     *       смену размеров без полного пересоздания backend'а.
     * @note Безопасно вызывать повторно — старый контекст сначала уничтожается.
     */
    bool Create(ContextParameters params);

    /**
     * @brief Освобождает все ресурсы и возвращает обёртку в неинициализированное состояние.
     *
     * Вызывает `Backend.Shutdown()` (ожидание GPU, освобождение FFX-контекста,
     * D3D12-устройства, shared-текстур, command ring'а, depth-copy pipeline)
     * и обнуляет `ContextParams`.
     *
     * @note Идемпотентна: повторный вызов при `Created == false` — no-op.
     * @note После `Destroy()` допускается повторный `Create()`.
     */
    void Destroy();

    /**
     * @brief Выполняет один кадр апскейла (FSR 4 с автоматическим fallback на 3.1.x).
     *
     * Делегирует в `Fsr4Backend::DrawFrame()`. Backend сам решает: нужно ли
     * (пере)создать D3D12-устройство, shared-текстуры и FFX-контекст.
     *
     * @param params Описание кадра: входы/выходы, размеры, jitter, sharpening, timing.
     *
     * @return true  — кадр обработан и результат скопирован в `resolvedColorResource`;
     * @return false — обёртка не инициализирована, текстуры не разрешены,
     *                 либо dispatch провалился (см. `GetStatus()`).
     *
     * @note CPU-неблокирующая: синхронизация полностью GPU-side
     *       (`SignalD3D11Inputs` / `WaitD3D11Output`).
     */
    bool Draw(const DrawParameters& params);

    /** @brief true, если `Create()` успешно завершился и `Destroy()` ещё не вызывался. */
    bool IsCreated() const { return Created; }

    /** @brief Размеры дисплея (output), переданные в `Create()`. */
    FfxApiDimensions2D GetDisplaySize() const { return ContextParams.displaySize; }

    /** @brief true, если активный FFX-контекст реально использует FSR 4 (не fallback на 3.1.x). */
    bool             IsFsr4Active()            const { return Backend.IsFsr4Active(); }

    /** @brief Текущий статус backend'а (см. `Fsr4Status`). */
    Fsr4Status       GetStatus()               const { return Backend.GetStatus(); }

    /** @brief Строка версии активного провайдера FFX (например, `"4.0.0"` или `"3.1.4"`). */
    const xr_string& GetActiveVersionString()  const { return Backend.GetActiveVersionString(); }

    /**
     * @brief Деструктор: гарантированно вызывает `Destroy()`.
     *
     * @note Порядок разрушения безопасен даже при частичной инициализации —
     *       `Destroy()` идемпотентен.
     */
    ~FSR4Wrapper();

private:
    /** @brief Флаг завершённой инициализации. См. `IsCreated()`. */
    bool              Created = false;

    /** @brief Внутренний backend — вся реальная работа с FFX и D3D12. */
    Fsr4Backend       Backend;

    /** @brief Параметры, сохранённые из `Create()` (нужны для `GetDisplaySize` и `GetRenderScale`). */
    ContextParameters ContextParams;
};

/**
 * @brief Глобальный экземпляр обёртки, используемый движком.
 *
 * Подключается в рендер-таргетах (`r4_rendertarget_phase_fsr4.cpp`)
 * как `extern FSR4Wrapper g_Fsr4Wrapper`.
 */
extern FSR4Wrapper g_Fsr4Wrapper;
#endif
