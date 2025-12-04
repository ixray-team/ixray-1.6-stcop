# Инвентарные слоты
> [!IMPORTANT]  
> **Статус**: Поддерживается <br>
> **Минимальная версия**: 1.3
Добавлен отдельный слот под пистолеты, идущий после слота для шлема.

> [!IMPORTANT]  
> **Статус**: Поддерживается <br>
> **Минимальная версия**: 1.0

Отныне слоты не требуют регистрации слотов. Для добавления слота достаточно зарегистрировать его в секции `[inventory]`, находящейся в `system.ltx` со **СЛЕДУЮЩИМ ПОРЯДКОВЫМ НОМЕРОМ**
* Пример
```ini
slot_persistent_14  = false ;helmet
slot_active_14      = false
```

Если вам нужно отобразить его в инвентаре, то в `actor_menu.xml` идём в блок `inventory_slot_wnd` и в конце дописываем: 
```xml
<slot> <!-- New Slot-->
	<slot_highlight x="472" y="17" width="76" height="98" stretch="1">
		<texture>ui_inGame2_helmet_highlighter</texture>
	</slot_highlight>
    	<slot_progress x="488" y="118" width="47" height="5" horz="1" min="0" max="1" pos="0">
    	    <progress stretch="1">
    	        <texture r="142" g="149" b="149">ui_inGame2_inventory_status_bar</texture>
    	    </progress>
	<min_color r="196" g="18" b="18"/>
	<middle_color r="255" g="255" b="118"/>
	<max_color r="107" g="207" b="119"/>
    	</slot_progress>
	<slot_dragdrop x="469" y="14" width="85" height="110" 
			cell_width="33" cell_height="41" rows_num="2" cols_num="2" custom_placement="0" a="0" virtual_cells="1" 
			vc_vert_align="c" vc_horiz_align="c" />
</slot>
```
Главное, соблюдать порядок: **(отсчёт с 0 в `system.ltx`)** 13 слот должен быть **(отсчёт с 1 в `inventory_slot_wnd`)** 14 в списке!
