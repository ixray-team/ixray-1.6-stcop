# Шейдерные константы

`b0`–`b5` (`cb_frame`, `cb_view`, `cb_object`, `cb_material`, `cb_light`, `cb_pass`) объявлены в `gamedata/shaders/d3d11/shared/fixed_cb.hlsli`. Не занимайте эти слоты в своём шейдере, если он включает `common.hlsli`. Запись по имени (`set_c`) по-прежнему пишет фиксированные буферы напрямую. У этих шести буферов больше нет отражённых переменных и setup-обработчиков. Регистры текстур и сэмплеров назначает проход: `shader:dx10texture(name, texture)` берёт следующий слот `t`, `shader:dx10texture(name, texture, slot)` задаёт слот явно, `shader:dx10sampler(name)` берёт следующий слот `s`. Привязки делаются только для pixel- и compute-проходов, не больше 16 текстур и 16 сэмплеров на проход; не поместившийся запрос пишется в лог и пропускается.

Проходные буферы занимают `b6`–`b10` и не привязаны все сразу. Скиннинг — `b6` (`sbones_array`, и `sbones_array_old`, если нет `DISABLE_MOTION_VECTORS`). Примятие травы — `b6`, ветер — `b7`. Константы bloom и tonemap, которые раньше лежали в `$Globals`, теперь явные буферы `b6` в этих проходах (`cb_bloom_down`, `cb_bloom_up`, `cb_bloom_adapt`, `cb_tonemap`). Глубина резкости (`cb_dof`) — `b7`; GTAO, лужи, sharpening, цвет UI и compute-дождь используют `b6`.

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
