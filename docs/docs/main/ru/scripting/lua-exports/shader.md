# Шейдерная Lua VM

> [!IMPORTANT]
> **Статус**: Поддерживается<br>
> **Минимальная версия**: 1.4

## Обзор

Это регистрации для `.lua`-скриптов шейдеров. Ссылки на `dx11ResourceManager_Scripting.cpp` относятся к R4, на `dx9ResourceManager_Scripting.cpp` — к рендерам Direct3D 9. Шейдеры используют свою Lua VM.

## Синтаксис

| Вызов | Аргументы и результат |
| --- | --- |
| `GetShaderOption(name)` | Проверяет наличие ключа в глобальных `[shaders_options]` из `engine_external.ltx`. Возвращает `boolean`. |
| `AddShaderOption(name, value)` | Передаёт имя и строковое значение макроса в набор параметров компиляции рендера R4. |
| `ClearAllShaderOptions()` | Очищает текущий набор макросов рендера. Глобальная таблица опций сохраняется. |

## Примеры

### Передать макрос в проход прицела

Фрагмент штатного `models_zoom_lerp.lua`. Функция `l_special` выполняется в шейдерной Lua VM. `USE_LENSE_LERP` задаётся перед `shader:begin`.

```lua
function l_special(shader, t_base, t_second, t_detail)
    AddShaderOption("USE_LENSE_LERP", "1")
    shader:begin("scope_lense_mask", "scope_lense_mask")
        :zb(false, true):scope(true):fog(false)
    shader:dx10texture("s_position", "$user$position")
    shader:dx10texture("s_base", t_base)
    shader:dx10sampler("smp_base")
end
```

## Регистрации

### Глобальная область

| Вызов или имя | Тип | Описание | Регистрация |
| --- | --- | --- | --- |
| `AddShaderOption(name, value)` | Функция | Добавляет макрос в текущие опции компиляции R4. Имя и значение передаются строками. Вызывайте до создания прохода. | [dx11ResourceManager_Scripting.cpp:244](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/Layers/xrRenderDX10/dx11ResourceManager_Scripting.cpp#L244) |
| `ClearAllShaderOptions()` | Функция | Очищает текущие макросы рендера. Глобальная секция `[shaders_options]` остаётся доступна через `GetShaderOption`. | [dx11ResourceManager_Scripting.cpp:246](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/Layers/xrRenderDX10/dx11ResourceManager_Scripting.cpp#L246) |
| `GetShaderOption(name)` | Функция | Возвращает `boolean` по наличию ключа в глобальной секции `[shaders_options]` файла `engine_external.ltx`. | [dx11ResourceManager_Scripting.cpp:245](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/Layers/xrRenderDX10/dx11ResourceManager_Scripting.cpp#L245) |
| `log(message)` | Функция | Пишет строку в журнал движка из Lua VM шейдеров. | [dx11ResourceManager_Scripting.cpp:242](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/Layers/xrRenderDX10/dx11ResourceManager_Scripting.cpp#L242), [dx9ResourceManager_Scripting.cpp:185](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/Layers/xrRenderDX9/dx9ResourceManager_Scripting.cpp#L185) |

### `_compiler`

| Вызов или имя | Тип | Описание | Регистрация |
| --- | --- | --- | --- |
| `_compiler:iblend(target_index, enabled, src, dst)` | Метод | Настраивает смешивание для указанного Render Target. `src` и `dst` — коэффициенты смешивания. Возвращает компилятор для цепочки вызовов. | [dx11ResourceManager_Scripting.cpp:187](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/Layers/xrRenderDX10/dx11ResourceManager_Scripting.cpp#L187) |
| `_compiler:scope(enabled)` | Метод | Включает или выключает флаг маски прицела для шейдера. Возвращает компилятор. | [dx11ResourceManager_Scripting.cpp:180](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/Layers/xrRenderDX10/dx11ResourceManager_Scripting.cpp#L180), [dx9ResourceManager_Scripting.cpp:160](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/Layers/xrRenderDX9/dx9ResourceManager_Scripting.cpp#L160) |
| `_compiler:scope_postprocess(enabled)` | Метод | Включает или выключает флаг постобработки прицела для шейдера. Возвращает компилятор. | [dx11ResourceManager_Scripting.cpp:181](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/Layers/xrRenderDX10/dx11ResourceManager_Scripting.cpp#L181), [dx9ResourceManager_Scripting.cpp:161](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/Layers/xrRenderDX9/dx9ResourceManager_Scripting.cpp#L161) |

## См. также

- [Новые экспорты Lua](../lua-exports.md)
- [Опции шейдеров](../../graphics/shader-options.md)
