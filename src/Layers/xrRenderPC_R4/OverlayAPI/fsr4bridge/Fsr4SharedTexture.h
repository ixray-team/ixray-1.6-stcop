#pragma once

#include <d3d11_4.h>
#include <d3d12.h>
#include <wrl/client.h>

/**
 * @file Fsr4SharedTexture.h
 * @brief Shared-текстура между игровым D3D11-устройством и приватным D3D12-устройством FSR 4.
 *
 * -------------------------------------------------------------------------
 * Назначение
 * -------------------------------------------------------------------------
 * FSR 4 работает через FFX на D3D12, а игра рендерит в D3D11. Нужна общая
 * GPU-память, к которой обе стороны имеют доступ без CPU-копирования.
 * `Fsr4SharedTexture` инкапсулирует пару:
 *
 *   [ ID3D12Resource ]  <── shared NT-handle ──  [ ID3D11Texture2D ]
 *        (D3D12)                                         (D3D11)
 *
 * Ресурс создаётся на приватном D3D12-устройстве backend'а с
 * `HEAP_FLAG_SHARED`, открывается в игровое D3D11 через `OpenSharedResource1`.
 *
 * -------------------------------------------------------------------------
 * Особый случай: depth
 * -------------------------------------------------------------------------
 * Игровой depth обычно `D24_UNORM_S8_UINT` / `D32_FLOAT_S8X24_UINT`.
 * Такие форматы нельзя надёжно шарить напрямую через NT-handle (или нельзя
 * открыть как RTV/UAV на D3D12-стороне). Поэтому:
 *
 *  - `isDepthIntermediate = true` → создаётся обычная `R32_FLOAT`-текстура;
 *  - игровая depth копируется в неё D3D11-проходом `CopyDepthToShared`
 *    (fullscreen-triangle, PS читает depth как SRV и пишет float).
 *
 * -------------------------------------------------------------------------
 * Универсальность
 * -------------------------------------------------------------------------
 * Класс source-agnostic: используется и для color/mvec/output/reactive, где
 * достаточно обычного `CopyResource`. Выбор режима — через параметры
 * `needUav` / `isDepthIntermediate`.
 */
class Fsr4SharedTexture
{
public:
    /**
     * @brief Создаёт (или переиспользует) shared-ресурс под данную игровую текстуру.
     *
     * Алгоритм:
     *  1. Если `Matches(gameTexture)` — переиспользуем существующий ресурс.
     *  2. Отбраковываем MSAA (не поддерживается).
     *  3. Определяем целевой формат:
     *     - `isDepthIntermediate` → `R32_FLOAT`;
     *     - иначе → `CoerceTypedFormat(desc.Format)` (TYPELESS → типизированный).
     *  4. Пробуем 3 комбинации `D3D12_RESOURCE_FLAGS` (RT+UAV / plain / RT+SA)
     *     в порядке от наиболее совместимой.
     *  5. Для удачной комбинации:
     *     `CreateCommittedResource(SHARED)` → `CreateSharedHandle` →
     *     `OpenSharedResource1` в D3D11.
     *
     * @param d3d11Device1       Игровое D3D11.1-устройство (для `OpenSharedResource1`).
     * @param d3d12Device        Приватное D3D12-устройство backend'а.
     * @param gameTexture        Игровая текстура-источник (только для получения desc).
     * @param needUav            true для output-текстур, куда FSR пишет через UAV.
     * @param isDepthIntermediate true для промежуточной `R32_FLOAT` depth-текстуры.
     * @param debugName          Имя для логов (`"color"`, `"depth"`, `"mvec"`, …).
     *
     * @return true при успехе; false — если MSAA или все комбинации провалились.
     *         При неудаче `IsValid()` останется `false`.
     *
     * @note Побочно обновляет `m_SourcePtr`/`m_Width`/`m_Height`/`m_SourceFormat`/
     *       `m_SharedFormat`/`m_IsDepthIntermediate` — они кэшируются для `Matches`.
     */
    bool Ensure(ID3D11Device1*   d3d11Device1,
                ID3D12Device*    d3d12Device,
                ID3D11Texture2D* gameTexture,
                bool             needUav,
                bool             isDepthIntermediate,
                const char*      debugName);

    /**
     * @brief Освобождает обе стороны (D3D11 и D3D12) и сбрасывает кэш source-указателя.
     *
     * @note Идемпотентна.
     */
    void Destroy();

    /** @brief true, если обе стороны (D3D11 и D3D12) валидны. */
    bool IsValid() const { return m_Tex11 && m_Res12; }

    /** @brief Возвращает D3D11-сторону (shared-view на приватный D3D12-ресурс). */
    ID3D11Texture2D* GetD3D11Texture()  const { return m_Tex11.Get(); }

    /** @brief Возвращает D3D12-сторону — приватный `ID3D12Resource`, на котором работает FFX. */
    ID3D12Resource*  GetD3D12Resource() const { return m_Res12.Get(); }

    /** @brief true, если текстура — промежуточный `R32_FLOAT` depth-таргет. */
    bool IsDepthIntermediate() const { return m_IsDepthIntermediate; }

private:
    /**
     * @brief Проверяет, можно ли переиспользовать текущий ресурс под данную игровую текстуру.
     *
     * Сравнивает `m_SourcePtr`, ширину, высоту и `m_SourceFormat`
     * (**формат источника**, не shared-формата).
     *
     * @param gameTexture Игровая текстура.
     * @param desc        Её DESC (передаётся, чтобы не вызывать `GetDesc` повторно).
     * @return true, если параметры совпадают и ресурс можно не пересоздавать.
     */
    bool Matches(ID3D11Texture2D* gameTexture, const D3D11_TEXTURE2D_DESC& desc) const
    {
        return m_SourcePtr           == gameTexture
            && m_Width               == desc.Width
            && m_Height              == desc.Height
            && m_SourceFormat        == desc.Format;
    }

    Microsoft::WRL::ComPtr<ID3D11Texture2D> m_Tex11; ///< D3D11-сторона (open shared)
    Microsoft::WRL::ComPtr<ID3D12Resource>  m_Res12; ///< D3D12-сторона (source of truth)

    ID3D11Texture2D* m_SourcePtr           = nullptr;            ///< игровая текстура-источник (слабая ссылка)
    UINT             m_Width               = 0;                  ///< ширина ресурса
    UINT             m_Height              = 0;                  ///< высота ресурса
    DXGI_FORMAT      m_SourceFormat        = DXGI_FORMAT_UNKNOWN;///< формат игрового источника
    DXGI_FORMAT      m_SharedFormat        = DXGI_FORMAT_UNKNOWN;///< формат shared-ресурса (typed)
    bool             m_IsDepthIntermediate = false;              ///< true, если это R32_FLOAT depth-таргет
};