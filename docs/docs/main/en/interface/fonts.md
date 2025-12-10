# Fonts
## Overview
> [!IMPORTANT]  
> **Status**: Supported <br>
> **Minimum Version**: 1.0

 Added a system for supporting .ttf fonts. All settings are stored in the `fonts.ltx` config. 
- If a corresponding .ttf font is not found for your localization, then **the game will require the presence of default.ttf in the DIRECTORY `$game_fonts$/your_localization`**

## New Font
* Place your font in: `$game_fonts$`. (default `gamedata/fonts/{lang}`)
* * `{lang}` - localization folder (rus/en/chs/etc...)
* Register in `fonts.ltx` as follows:
```ini
[new_font] ; Name for use in XML 
shader = hud\font
name = Arial; Name of .ttf file without extension 
size = 12 ; Font size
; Uncomment if you have encoding problems 
;opentype = true
```
* Enjoy

## Sinographic Writing System Family
> [!IMPORTANT]  
> **Status**: Supported <br>
> **Minimum Version**: 1.2

Or in other words: **Hieroglyphics**

The problem with these fonts is the huge number of characters that don't fit in the texture when rendered. For such languages, you need to set the parameter:

* `gamedata\configs\engine_external.ltx`
```ini
[render]
...
FontAtlasSize = 4096 ; Value of the atlas texture size. Must be a power of two
```
