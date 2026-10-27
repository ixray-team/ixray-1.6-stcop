# Inventory item parameters

`src/xrGame/ui/UIItemParam.*` renders parameter rows for artefacts, outfit/backpack bonuses, and consumables. `UIArtefactParamItem` is an alias for this component; `UIBoosterInfoItem` adapts values already scaled by the consumable information panel. Both modern `caption`/`value` XML and legacy `static_<parameter>` artefact layouts are supported.

## Shared XML

Stock artefact and consumable panels currently keep standalone row definitions. Shared templates remain an optional engine capability: custom panels can include a user-provided `ui/item_params.xml` (`item_params_16.xml` for widescreen). The `param_templates:row` template owns geometry and text styling; semantic templates inherit it and supply icons. Rows using this optional format reference `template` and keep only value magnitude, units, sign/color flags, and deliberate local overrides. `ItemParamXml::Resolve` fills missing attributes and recursively merges missing child properties before UI initialization. Local properties take precedence. Missing templates and chains exceeding 16 nodes produce a log diagnostic and fail row initialization. Old XML without templates and legacy `static_` rows remain supported.

## Values and icons

- Signed modifiers show `+` or `-`. Built-in `af_slots`, `condition`, and `boost_time` are absolute values and omit `+` by default.
- The engine defaults to no automatic coloring when the XML attribute is absent. Stock `af_params` and `booster_params` XML files enable coloring for effect rows, excluding absolute values and text-only effects. To enable it in a custom row: set `value` attribute `colorize="1"` (or `colorize="1"` on a legacy `static_<parameter>` node). With it enabled, useful effects are blue; harmful effects are red. `value` attribute `sign_inverse="1"` reverses usefulness, for example for radiation accumulation. This changes the color, not the displayed sign.
- `value` attribute `show_sign="0"` disables the leading plus. `colorize="0"` (the engine fallback when the attribute is absent) preserves the original icon shader/colors and value colors: gray/red for modern artefact rows, green/red for legacy text, and gray for consumables.
- `magnitude` and `unit_str` keep their existing meanings. Artefact values are scaled in the row; consumable values are scaled by the panel before they reach the row.
- With automatic coloring enabled, icons use `hud\font`, which takes coverage from the texture alpha channel and RGB from the UI color. Colored and white DDS icons work when their alpha channel encodes the icon silhouette. Caption text keeps its XML color. The legacy SoC artefact icons have opaque alpha and store their shapes in RGB, so their stock rows use `colorize="0" text_colorize="1"` to preserve the original icons while coloring values.
- `texture_minus` remains an optional alternate icon for harmful effects. Automatic coloring works without it.
- Negative consumable effects use the same rows and colors as positive effects rather than being filtered out.

`value:text_colorize` defaults to `colorize` but can independently disable value coloring. Stock `additional_weight` and `boost_max_weight` rows set it to zero, preserving gray positive carrying-weight values while tinting the icon. Legacy rows use the same attribute on the row element.

## Grouping

Outfit and backpack bonuses continue immediately after equipment protection values without an extra `af_params:prop_line`. Standalone artefact parameters retain their separator. Upgrade properties keep their separate panel and separator; built-in container counts come from the item section and do not include upgrade deltas.

Stock modern parameter rows use 18-unit-high icons and a 19-unit row pitch, leaving a one-unit vertical gap. `outfit_info:row_spacing="1"` adds the same gap between protection rows. Upgrade properties use `height="19"` and `next_line_pos="19"`, with 18-unit icons and text aligned to the protection captions. These are UI coordinates and scale with the interface. Upgrade labels are measured with the active font before layout. Floating item tooltips expand to fit the longest label in each of two equal columns, preserving single-line text and the one-unit row gap. The title, description, cost position, and background follow the new width. The XML width is restored for the next item so expansion does not accumulate. Embedded fixed-size panels keep their available width and wrap labels within their columns.

## Verification

Build `xrGame` in Debug and RelWithDebInfo. In the inventory, inspect an outfit with built-in containers and an installed container upgrade, an artefact with useful and harmful effects, and consumables with positive and negative restore values. Check both aspect-ratio layouts and the legacy SoC artefact layout. The base count should read `2`, upgrade text should retain its delta, and icons with `colorize="1"` should switch blue/red with the effect while preserving their silhouette and transparency.

XML examples and configuration details: [Russian modder documentation](../docs/main/ru/interface/item-parameters.md#автоматическая-окраска-иконок-параметров), [English modder documentation](../docs/main/en/interface/item-parameters.md#automatic-parameter-icon-coloring).
