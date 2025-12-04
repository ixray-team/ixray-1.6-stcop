# XLMOverride
> [!IMPORTANT]
> **Статус**: Поддерживается <br>
> **Минимальная версия**: 1.0

## Вводная часть
**XMLOverride** - система кастомизации XML через отдельный файл без изменения существующего файла. Выглядит следующий образом: 
```
mm_ui_test.xml         <-- Orig
mod_ui_test_myedit.xml <-- Mod file override
```
Файл перегрузки строится следующим образом:
>mod_(filename)_(editname).xml

Перегрузка блоков осуществляется с помощью атрибута `override`, который имеет три значения:
* add
* replace
* remove

## Пример
Имеем следующий код в оригинальном файле: 
```xml
<w>
      <background x="0" y="0" width="1024" height="768">
	<auto_static x="500" y="130" width="345" height="160" stretch="1">
		<texture width="432" height="160">ui\video_voroni_crop</texture>
	</auto_static>
	<auto_static x="432" y="353" width="460" height="416" stretch="1">
		<texture x="0" y="0" width="576" height="416">ui\video_water_crop</texture>
	</auto_static>
	<auto_static x="0" y="0" width="104" height="768" stretch="1">
		<texture>ui_inGame2_left_widepanel</texture>
	</auto_static>
	<auto_static x="920" y="0" width="104" height="768" stretch="1">
		<texture>ui_inGame2_right_widepanel</texture>
	</auto_static>
      </background>
</w>
```
Допустим мы хотим переопределить:
```xml
	<auto_static x="500" y="130" width="345" height="160" stretch="1">
		<texture width="432" height="160">ui\video_voroni_crop</texture>
	</auto_static>
```
Пишем следующее в mod-файл: 
```xml
<w>
      <background x="0" y="0" width="1024" height="768">
	<auto_static x="500" y="130" width="345" height="160" stretch="1" override="replace"> <--! Устанавливаем атрибут override со значением replace -->
		<texture width="432" height="160">ui\video_voroni_crop</texture> <--! Сюда можем ставить своё значение -->
	</auto_static>
      </background>
</w>
```
