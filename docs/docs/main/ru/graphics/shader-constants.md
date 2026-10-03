# Шейдерные константы

`b0`–`b5` (`cb_frame`, `cb_view`, `cb_object`, `cb_material`, `cb_light`, `cb_pass`) объявлены в `gamedata/shaders/d3d11/shared/fixed_cb.hlsli`. Не занимайте эти слоты в своём шейдере, если он включает `common.hlsli`. Запись по имени (`set_c`) и `RegisterConstantSetup` по-прежнему попадают в отражённые константы, в том числе внутри фиксированных буферов. Свои константы вне этих шести буферов остаются на старом отражённом пути.

Проходные буферы занимают `b6`–`b10` и не привязаны все сразу. Скиннинг — `b6` (`sbones_array`, и `sbones_array_old`, если нет `DISABLE_MOTION_VECTORS`). Примятие травы — `b6`, ветер — `b7`. Константы bloom и tonemap, которые раньше лежали в `$Globals`, теперь явные буферы `b6` в этих проходах (`cb_bloom_down`, `cb_bloom_up`, `cb_bloom_adapt`, `cb_tonemap`).

> [!IMPORTANT]  
> **Статус**: Поддерживается <br>
> **Минимальная версия**: 1.0

```hlsl
float4 rain_params;
x // rainDensity 
y // rainWetness 
```

> [!IMPORTANT]  
> **Статус**: Поддерживается <br>
> **Минимальная версия**: 1.3

```hlsl
float4 m_timearrow;
float4 m_timearrow2;
```
