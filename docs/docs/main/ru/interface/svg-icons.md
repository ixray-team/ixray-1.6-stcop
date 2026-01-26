> [!IMPORTANT]
> **Статус**: Поддерживается  <br>
> **Минимальная версия**: IX-Ray Platform 1.4

# SVG-иконки

**SVG (Scalable Vector Graphics)** — это формат изображений, основанный на **векторной графике**, а не на пикселях.  

## Основные преимущества SVG-иконок

- 🔍 **Масштабируемость**  
  Не теряют качество при любом размере (от маленьких 16×16 до 4K).
- 🧩 **Текстовый формат**  
  Это XML, который легко читать, редактировать и генерировать.
- 🌐 **Удобство для веба и UI**  
  Подходят для кнопок, меню, тулбаров, HUD’ов и других элементов интерфейса.

## Использование
### Инвентарные предметы
Достаточно просто указать имя `SVG` файла в папке `textures/ui/`
```ini
inv_vector_icon = ui_inv_ak74.svg
```

### UI
Принцип схож с инвентарным, только тут указывается:

* аттрибут `svg` для худовых индикаторов
```xml
<indicator_bleeding 
  x="980" 
  y="590" 
  width="26" 
  height="35" 
  stretch="1" 
  svg="ui_hud_bleeding_indicator.svg"
/>
```

* Тег `svg` для остальных типов
```xml
</treasure_spot>
  <treasure_spot_mini width="17" height="17" alignment="c" stretch="1">
      <texture>ui_inGame2_PDA_icon_secret</texture>
      <svg color="green">ui_map_legend_icon_secret.svg</svg>
  </treasure_spot_mini>
```
