> [!IMPORTANT]
> **Статус**: Поддерживается <br>
> **Минимальная версия**: 1.4

# Панель быстрого доступа

## Обзор

Панель содержит 4 быстрых слота и поддерживает автоскрытие, плавное появление и красную подсветку пустых ячеек.

## Готовый XML: поведение панели

```xml
<quick_slots_panel
  show_speed="3.0"
  hide_speed="4.0"
  hide_delay="2.0"
  empty_red_glow_counter="1"
  empty_red_glow_icon="1"
  empty_red_intensity="1.0"
/>
```
## Применение

1. Добавьте блоки в `configs/ui/maingame.xml`.
2. Проверьте `hud_hide_quick_slots`.
3. При необходимости привяжите `show_quick_slots` в `configs/ui/ui_keybinding.xml`.

Смежный материал: [обзор UI](ui-advanced-features.md).
