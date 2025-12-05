# Шейдерные опции
> [!IMPORTANT]  
> **Статус**: Поддерживается <br>
> **Минимальная версия**: 1.3

# Общее
Опциональные настройки для шейдеров находятся в [gamedata/configs/engine_external.ltx](https://github.com/ixray-team/ixray-1.6-stcop/blob/default/gamedata/configs/engine_external.ltx). 

Стандартный режим выглядит так:
```ini

[shaders_options]
USE_LEGACY_LIGHT = 1

; Hozar: Use brga skycolor format
; USE_BGRA_SKYCOLOR = 1;

; Hozar: Use original GSC format for sky ToneMapping
; USE_LEGACY_SKY_TONEMAP = 1

; Hozar: Use normal on Hemi calc
; USE_NORMAL_HEMI_DISTRIBUTION = 1

; Hozar: For PBS pipeline
; IBL_FAKE_IRRADANCE = 1
; IBL_REMAP_IRRADANCE = 1
; IBL_REMAP_REFLECTIONS = 1
; IBL_REMAP_POSITIVE_Y = 1
; IBL_MAX_LOD = 10
; USE_FULL_SKY_SPHERE = 1
```

# Параметры
### USE_LEGACY_LIGHT
* Оригинальная модель просчёта освещения (Diffuse + Specular)

### USE_BGRA_SKYCOLOR 
* Расчёт цвета неба из погодных конфигов с использованием `BRGA` расстановки каналов 
> В выключенном варианте используется `RGBA`

### USE_LEGACY_SKY_TONEMAP
* Использование оригинального алгоритма цветокоррекции неба

## Опции IBL / PBS в рендере
Ниже приведено описание доступных дефайнов и их влияния на рендеринг.

### USE_NORMAL_HEMI_DISTRIBUTION
* Включает влияние нормали поверхности на затенение окружения (hemi lighting).  

### IBL_FAKE_IRRADANCE
**(PBS Only)**  
* Костыль для работы PBS с оригинальными текстурами неба (у которых отсутствуют мип-уровни).  

### IBL_REMAP_IRRADANCE
* Перераспределение (`remap`) **Diffuse Irradiance** текстуры.  
> Делает освещение более "физически корректным", но результат может выглядеть не всегда красивым.  

### IBL_REMAP_REFLECTIONS
* Аналогичный `remap` для **Specular Reflections** (актуально только для PBS Only).  

### IBL_REMAP_POSITIVE_Y
* Используется при расчёте **спекулярных отражений**.  
> Особенность GSC-скайкубов: игнорирует туман в отражениях.  

### IBL_MAX_LOD
* _[Опциональный параметр]_ Определяет количество **mip-уровней** в скайкубах.  

## USE_FULL_SKY_SPHERE
* Включает использование классических **скайкубов** без растягивания.  
