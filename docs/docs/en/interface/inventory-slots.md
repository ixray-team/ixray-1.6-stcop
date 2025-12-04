# Inventory Slots
> [!IMPORTANT]  
> **Status**: Supported <br>
> **Minimum Version**: 1.3
Added a separate slot for pistols, following the helmet slot.

> [!IMPORTANT]  
> **Status**: Supported <br>
> **Minimum Version**: 1.0

Slots no longer require slot registration. To add a slot, just register it in the `[inventory]` section found in `system.ltx` with the **NEXT SEQUENCE NUMBER**
* Example
```ini
slot_persistent_14  = false ;helmet
slot_active_14      = false
```

If you need to display it in the inventory, go to the `inventory_slot_wnd` block in `actor_menu.xml` and add at the end: 
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
The main thing is to maintain the order: **(counting from 0 in `system.ltx`)** slot 13 should be **(counting from 1 in `inventory_slot_wnd`)** 14 in the list!
