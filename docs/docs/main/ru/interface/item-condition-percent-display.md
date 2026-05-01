> [!IMPORTANT]
> **Статус**: Поддерживается <br>
> **Минимальная версия**: 1.4

# Состояние предмета и проценты

## Обзор

`CUIItemStateDisplay` поддерживает два базовых режима: полоса и текстовый процентный вывод.

## Режимы

1. Полоса:
   1. не задается `percent_display`
   2. используется `CUIProgressBar`
2. Текст:
   1. задается `percent_display`
   2. текст настраивается через `percent_display:text`

## Форматы вывода

1. `percent` число и знак процента
2. `number` число без знака процента
3. `fraction` дробь с `fraction_max`
4. `portion` счетчик частей предмета

## Пример

```xml
<percent_display>
  <background stretch="1">
    <texture>ui_inv_condition_back</texture>
  </background>
  <text format="percent" align="c" vert_align="c" font="letterica16"/>
</percent_display>
```

## Применение

1. Добавьте блок в нужный узел элемента UI.
2. Выберите формат `format`.
3. При необходимости задайте `min_color`, `middle_color`, `max_color`.

Смежный материал: [параметры предметов](item-parameters.md), [обзор UI](ui-advanced-features.md).
