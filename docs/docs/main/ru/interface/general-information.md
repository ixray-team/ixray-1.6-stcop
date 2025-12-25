# Общие сведения
## Общее

### XML Масштабирование
> [!IMPORTANT]  
> **Статус**: Поддерживается <br>
> **Минимальная версия**: 1.2.2
* Добавлена возможность настройки масштабирования текстур UI в XML с помощью параметра `scale`.

Пример реализации:
```xml
    <file name="ui\ui_mainMenu2" scale="2">
```

### CUIStackPanel
> [!IMPORTANT]  
> **Статус**: Поддерживается <br>
> **Минимальная версия**: 1.3
* Простейшая реализация вертикальной `StackPanel`. Размещает видимые элементы один за другим автоматически.
![image](https://github.com/user-attachments/assets/086aa7e8-8c8f-45ca-81a1-17c20c00397a)

```xml
<indicator_start_line x="568" y="689" width="0" height="0" stretch="1" right="true"> <!-- right="true": выравнивание справа -->
 <!-- Elements -->
</indicator_start_line>
```

* tag `spacing="2` - отступ в 2 пикселя (опционально)

```lua
local sp =  xml:InitStackPanel("indicator_start_line", self) --> Создаём панель (родитель: CUIWindows)
sp:SetRightAlign(true)            --// установить выравнивание по правой стороне 
local IsRight = sp:IsAlignRight() --// получить выравнивание
```

### CUICursor
> [!IMPORTANT]  
> **Статус**: Поддерживается <br>
> **Минимальная версия**: 1.0
* Добавлена возможность настройки UI элемента. Подробности смотреть в ```cursor.xml```

### CUIDoubleProgressBar 
> [!IMPORTANT]  
> **Статус**: Поддерживается <br>
> **Минимальная версия**: 1.0
* Поддержка установки диапазона минимального <-> максимального цвета 
```xml
<first_min_color color="pda_red" />
<first_middle_color color="pda_green" />
<first_max_color color="pda_red" />
```

### CUIMainInGameWnd
> [!IMPORTANT]  
> **Статус**: Поддерживается <br>
> **Минимальная версия**: 1.3
* Поддержка установки собственных цветов для активного/неактивного типа патронов (static_fmj_ammo, static_ap_ammo и так далее)
```xml
<inactive_ammo_color color="pda_green" />
<active_ammo_color color="pda_red" />
```

## Ползунки

### CUITrackBar
> [!IMPORTANT]  
> **Статус**: Поддерживается <br>
> **Минимальная версия**: 1.0
* Добавлена возможность настройки UI элемента. Подробности смотреть в ```trackbar.xml```
* Поддержка вывода цифрового показателя значения справа от слайдера при указании атрибута `show_value="1"`
![image](https://github.com/ixray-team/ixray-1.6-stcop/assets/13867290/d53f08a2-1d18-4942-b669-fd7eb132956f)

### CUICustomSpin
> [!IMPORTANT]  
> **Статус**: Поддерживается <br>
> **Минимальная версия**: 1.0
* Добавлена возможность настройки UI элемента. Подробности смотреть в ```custom_spin.xml```
* Добавлена поддержка горизонтальных ползунков при указании атрибута `horz="1"`. Горизонтальные ползунки настраиваются в файле ```custom_spin_horz.xml```

## Прочее

### CUIStatic | CUITextWnd

> [!IMPORTANT]  
> **Статус**: Поддерживается <br>
> **Минимальная версия**: 1.3

* Добавлена поддержка смены цвета при наведении курсора на текст, как в билдах ТЧ.

* ```highlight_text``` - Включить подсветку текста.
* ```hA``` - альфа-канал подсветки
* ```hR``` - красный цвет у подсветки
* ```hG``` - зелёный цвет у подсветки
* ```hB``` - синий цвет у подсветки

Пример реализации:

```xml
        <cap_screenshot_format x="26" y="3" width="108" height="24" highlight_text="1" hA="255" hR="255" hG="0" hB="0">
            <text r="170" g="170" b="170" font="letterica16" align="r" vert_align="c">ui_mm_screenshot_format</text>
        </cap_screenshot_format>
```
> [!IMPORTANT]  
> **Статус**: Поддерживается <br>
> **Минимальная версия**: 1.3
* Добавлена поддержка градиентов для шрифтов при указании аттрибута `gradient="1"`.

Также можно менять направление градиента с помощью аттрибута `gradient_mode`. Принимаемые значения: 

* ```vert``` - сверху вниз (стандартное значение)
* ```horz``` - слева направо
* ```back``` - справа налево
* ```down``` - снизу вверх

Цвет градиента можно менять аттрибутом `gradient_color`. Работает по тому же принципу, что и обычный аттрибут `color`.

![image](https://github.com/user-attachments/assets/041a244d-fee1-48bc-a1d5-d705fda5d248) 

Пример реализации:
```xml
	    <caption x="0" y="20" width="467" height="30">
		    <text font="graffiti32" align="c" color="red" gradient="1" gradient_mode="vert" gradient_color="blue">ui_mm_load_game</text>
	    </caption>
```
### CUIMotionIcon
> [!IMPORTANT]  
> **Статус**: Поддерживается <br>
> **Минимальная версия**: 1.3
* Добавлена поддержка ```motion_icon.xml``` из ТЧ.

Пример реализации:
```xml
<?xml version='1.0' encoding="UTF-8"?>
<window x="0" y="0" w="640" h="480" r="227" g="121" b="222" texture="1">

	<background  x="0" y="640" width="94" height="125" stretch="1">
		<texture>ui_hud_stamina_full</texture>
	</background>
	
	<state_normal x="20" y="22" width="50" height="75" stretch="1">
		<texture>ui_hud_soldier_normal</texture>
	</state_normal>

	<state_crouch x="20" y="22" width="50" height="75" stretch="1">
		<texture>ui_hud_soldier_crouch</texture>
	</state_crouch>

	<state_creep x="20" y="22" width="50" height="75" stretch="1">
		<texture>ui_hud_soldier_creep</texture>
	</state_creep>

	<state_climb x="20" y="22" width="50" height="75" stretch="1">
		<texture>ui_hud_soldier_climb</texture>
	</state_climb>

	<state_run x="20" y="22" width="50" height="75" stretch="1">
		<texture>ui_hud_soldier_run</texture>
	</state_run>

	<state_sprint x="20" y="22" width="50" height="75" stretch="1">
		<texture>ui_hud_soldier_sprint</texture>
	</state_sprint>

	<power_progress  x="24" y="94" width="43" height="7" horz="1" min="0" max="100" pos="50">
		<progress>
			<texture>ui_hud_shk_stamina</texture>
		</progress>
	</power_progress>
	
	<luminosity_progress x="79" y="34" width="8" height="71" horz="0" min="0" max="200" pos="100">
		<progress>
			<texture>ui_hud_shk_light</texture>
		</progress>
	</luminosity_progress>
	
	<noise_progress x="8" y="34" width="8" height="71" horz="0" min="0" max="400" pos="100">
		<progress>
			<texture>ui_hud_shk_noise</texture>
		</progress>
	</noise_progress>
</window>
```

### CUIZoneMap
> [!IMPORTANT]  
> **Статус**: Поддерживается <br>
> **Минимальная версия**: 1.3
* Добавлена поддержка квадратной миникарты, как в ЧН/ТЧ.

![image](https://github.com/user-attachments/assets/9473739e-71c0-4d11-8dd9-6a1322901095)

Пример реализации:
```xml
<window>
	<minimap>
		<level_frame x="17" y="14" width="137" height="166"/>
		
		<background x="3" y="3" width="164" height="191" stretch="1"> 
			<texture>ui_hud_map</texture>
			
			<dist_text x="116" y="4" width="38" height="14">
				<text align="r" font="arial_14" color="ui_3"/>
			</dist_text>
		</background>
		
		<compass x="117" y="18" width="31" height="31" heading="1" s_tretch="0">
			<texture a="170">ui_hud_compas</texture>
		</compass>
		
		<center width="3" height="4" alignment="c" stretch="1"> 
			<texture >ui_minimap_point</texture>
		</center>

		<static_counter x="133" y="167" width="29" height="29" light_anim="ui_pda_contacts" la_cyclic="0" la_texture="0" la_text="1" la_alpha="1" stretch="1">
			<texture>ui_hud_map_counter</texture>
			<text_static x="7" y="7" width="12" height="14">
				<text  align="c" font="graffiti19" color="ui_7"/>
			</text_static>
		</static_counter>
	</minimap>
</window>
```

### CHUDTarget

> [!IMPORTANT]  
> **Статус**: Поддерживается  <br>
> **Минимальная версия**: 1.3
* Теперь шрифт, цвета, текстуру и шейдер можно менять в файле ```hud_target.xml```

Пример реализации:

```xml
<?xml version="1.0" encoding="UTF-8"?>
<w>
	<shader>hud\cursor</shader>
	<texture>ui\cursor</texture>
	<enemy_color r="255" g="0" b="0" a="128"/>
	<friend_color r="0" g="255" b="0" a="128"/>
	<neutral_color r="255" g="255" b="128" a="128"/>
	<default_color r="255" g="255" b="255" a="128"/>
	<target_font font="letterica18"/>
</w>
```
