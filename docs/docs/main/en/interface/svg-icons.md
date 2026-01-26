> [!IMPORTANT]
> **Status**: Supported  <br>
> **Minimum version**: IX-Ray Platform 1.4

# SVG Icons

**SVG (Scalable Vector Graphics)** is an image format based on **vector graphics**, not pixels.

## Main Advantages of SVG Icons

- 🔍 **Scalability**  
  No quality loss at any size (from small 16×16 to 4K).
- 🧩 **Text Format**  
  It is XML, easy to read, edit, and generate.
- 🌐 **Web and UI Friendly**  
  Suitable for buttons, menus, toolbars, HUDs, and other interface elements.

## Usage
### Inventory Items
Just specify the name of the `SVG` file in the `textures/ui/` folder:
```ini
inv_vector_icon = ui_inv_ak74.svg
```

### UI
The principle is similar to inventory items, but here you specify:

* The `svg` attribute for HUD indicators
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

* The `svg` tag for other types
```xml
</treasure_spot>
  <treasure_spot_mini width="17" height="17" alignment="c" stretch="1">
      <texture>ui_inGame2_PDA_icon_secret</texture>
      <svg color="green">ui_map_legend_icon_secret.svg</svg>
  </treasure_spot_mini>
```
