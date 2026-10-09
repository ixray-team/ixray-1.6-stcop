# Inventory slots

::: important Support
**Status:** Supported · **Minimum version:** 1.0
:::

Since version **1.3**, a separate pistol slot is available after the helmet slot.

## Adding a slot

Add the `slot_persistent_N` and `slot_active_N` parameters to the `[inventory]` section of `system.ltx`. Replace `N` with the **next sequential slot number**.

To display the slot in the inventory, append a `<slot>` element to the `inventory_slot_wnd` block in `actor_menu.xml`.

::: code-group

```ini [system.ltx]
slot_persistent_14  = false ;helmet
slot_active_14      = false
```

```xml [actor_menu.xml]
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
  <!-- [!code ++:3] -->
  <slot_dragdrop x="469" y="14" width="85" height="110"
    cell_width="33" cell_height="41" rows_num="2" cols_num="2" custom_placement="0" a="0" virtual_cells="1"
    vc_vert_align="c" vc_horiz_align="c" />
</slot>
```
:::

::: tip Slot numbering
Numbering starts at **0** in `system.ltx` and at **1** in the `inventory_slot_wnd` list. Slot `14` corresponds to the **15th** `<slot>` element.
:::
