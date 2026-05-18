> [!IMPORTANT]
> **Статус**: Поддерживается <br>
> **Минимальная версия**: 1.4

# Радиальные индикаторы

## Обзор

`CUIProgressShape` позволяет рисовать круговой прогресс для бустеров и других таймеров.

## Ключевые параметры

1. `sector_count` количество сегментов круга.
2. `begin_angle` начальный угол.
3. `end_angle` конечный угол.
4. `clockwise` направление заполнения.
5. `blend` режим смешивания.
6. `back` и `front` фон и активный слой.

## Пример

```xml
<indicator_booster_health x="0" y="0" width="32" height="32" sector_count="16" begin_angle="0" end_angle="360">
  <back>...</back>
  <front>...</front>
</indicator_booster_health>
```

## Применение

1. Добавьте узел в XML HUD.
2. Настройте `sector_count` и углы.
3. Проверьте чтение на разных масштабах UI.

Смежный материал: [общая информация](general-information.md), [обзор UI](ui-advanced-features.md).
