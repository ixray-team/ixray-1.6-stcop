# Погода
## Обзор

> [!IMPORTANT]
> **Статус**: Поддерживается <br>
> **Минимальная версия**: 1.1

Различные погодные изменения.

## Погодный цикл

```ini
tree_amplitude_intensity = 12.0; (0...250) Наклон деревьев при ветре
```

## Rain

> [!IMPORTANT]
> **Статус**: Поддерживается <br>
> **Минимальная версия**: 1.3

```ini
[rain]
max_desired_items                 = 2500
source_offset                     = 40.0
drop_angle                        = 3.0
drop_max_angle                    = 10.0
drop_max_wind_vel                 = 20.0
max_particles                     = 1000
particles_cache                   = 400
particles_time                    = 0.3
source_rain_radius_render         = 12.5
add_const_dist_coefficient        = 30.0
add_const_dist_coefficient_render = 40.0
```
