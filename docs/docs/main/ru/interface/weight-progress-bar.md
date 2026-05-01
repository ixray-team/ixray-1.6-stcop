> [!IMPORTANT]
> **Статус**: Поддерживается <br>
> **Минимальная версия**: 1.4

# Прогресс бар веса

## Обзор

В `actor_menu.xml` можно включить строку веса через контейнер `actor_weight_row`. Внутри строки поддерживаются два варианта: прогресс бар веса или текстовое значение веса.

## Полная логика actor_weight_row

1. Движок сначала ищет узел `actor_weight_row`.
2. Если узел найден:
   1. создается контейнер строки веса
   2. обязательно создается `actor_weight_row:actor_weight_caption`
   3. затем проверяется `actor_weight_row:weight_status_bar`
3. Если `weight_status_bar` найден:
   1. создается `CUIProgressBar`
   2. прогресс бар включается и показывается
   3. текстовый `actor_weight_row:actor_weight` не создается
4. Если `weight_status_bar` не найден:
   1. создается `actor_weight_row:actor_weight` как fallback
5. В обоих случаях обязательно создается `actor_weight_row:actor_weight_max`.
6. Если `actor_weight_row` отсутствует полностью, движок переходит в старый layout и ищет корневые узлы `actor_weight_caption`, `actor_weight`, `actor_weight_max`.

## Логика тултипа для weight_status_bar

1. Тултип работает только если есть `weight_status_bar`.
2. Тултип показывается при наведении курсора на бар.
3. Есть задержка перед показом 700 мс после получения фокуса.
4. Если в данный момент открыт другой hint, новый не показывается.
5. Текст тултипа содержит:
   1. текущий суммарный вес инвентаря
   2. максимальный переносимый вес
6. Позиция тултипа подбирается рядом с курсором в пределах экрана.

## Готовый XML

```xml
<actor_weight_row x="810" y="736" width="210" height="16">
  <actor_weight_caption x="0" y="0" width="71" height="16">
    <text font="ui_font_arial_14" align="l" color="ui_1">ui_inv_weight</text>
  </actor_weight_caption>

  <weight_status_bar x="73" y="4" width="90" height="8" horz="1" min="0" max="100" pos="0">
    <background><texture>ui_inGame2_hint_wnd_bar_16</texture></background>
    <progress><texture a="180">ui_inGame2_hint_wnd_bar_alfa_line_16</texture></progress>
    <color_less color="pda_green"/>
    <color_more color="pda_red"/>
  </weight_status_bar>

  <actor_weight_max x="166" y="0" width="44" height="16">
    <text font="ui_font_arial_14" align="r" color="ui_1"/>
  </actor_weight_max>
</actor_weight_row>
```

## XML fallback без progress bar

```xml
<actor_weight_row x="810" y="736" width="210" height="16">
  <actor_weight_caption x="0" y="0" width="71" height="16">
    <text font="ui_font_arial_14" align="l" color="ui_1">ui_inv_weight</text>
  </actor_weight_caption>

  <actor_weight x="73" y="0" width="44" height="16">
    <text font="ui_font_arial_14" align="r" color="ui_1"/>
  </actor_weight>

  <actor_weight_max x="121" y="0" width="44" height="16">
    <text font="ui_font_arial_14" align="r" color="ui_1"/>
  </actor_weight_max>
</actor_weight_row>
```

## Применение

1. Добавьте блок в `configs/ui/actor_menu.xml`.
2. Для режима прогресс бара добавьте `weight_status_bar` и проверьте диапазон `min` и `max`.
3. Для режима текста не добавляйте `weight_status_bar`, оставьте `actor_weight`.
4. Проверьте:
   1. отображение в норме
   2. отображение при перегрузе
   3. тултип при наведении на progress bar

Смежный материал: [обзор UI](ui-advanced-features.md).
