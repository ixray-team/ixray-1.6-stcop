# Render debug

A window and a console command for looking at what the renderer draws: debug views that replace the picture, and the frame's render targets.

Open it with **Debug → Shader Debug** in the top menu, or with the console command `ui_dbg_graphic 1`. The window needs a Debug or RelWithDebInfo build; the console command `r_debug_view` works in every build.

## Debug views

A debug view replaces the shading of the level with one value per pixel. The top of the window is a row of icon buttons, one per view; hold the cursor over a button to see its name. Press a button to switch its view on, press it again to return the normal picture. From the console: `r_debug_view <name>`, and `r_debug_view off`.

| Name | Shows |
| --- | --- |
| `off` | The frame as it is |
| `overdraw_raster` | How many layers of level geometry cover the pixel, counting everything the culling let through, hidden or not. Grass, leaves and other cut-out surfaces count as their whole cards, see-through parts included: that is the area the depth pass has to test. Use it to see where occlusion culling fails: a wall with bright colour on it has geometry drawn behind it |
| `overdraw_shaded` | How many surfaces at the pixel passed the depth test, in the order of the draws. Cut-out surfaces count only where their texture is solid, so grass and leaves keep their shape. The gap between this view and `overdraw_raster` on vegetation is the see-through card area |
| `clusters` | A colour per cluster of level geometry |
| `triangles` | A colour per triangle |
| `instances` | A colour per level object |
| `materials` | A colour per material |
| `lights` | The number of local lights that reach the pixel: green none, yellow 4, red 8 or more |
| `base` | Base colour with the detail texture, without light |
| `normal` | The surface normal in the world, with the normal map |
| `roughness` | Roughness |
| `hemi` | How much sky light the point gets |
| `metalness` | How much of the surface the lighting treats as metal: white is metal, black is not. In the default rendering path the level and the models show the stored value, which is rounded to twelfths with a fine per-pixel dither, so a half-metal surface looks grainy here; the weapon and hands are lit on their own with the exact value and show no grain; a surface that lets light through (skin, leaves) shows black. Blue: a surface shaded without PBR maps, which has no metalness: the old gloss maps, the grass, and a texture whose description (`.thm`) does not set the PBR material type, even when its maps are made as PBR ones |
| `lmap` | Baked light alone |
| `streaming` | The state of the base texture: green — the full texture is loaded and the pixel needs it; yellow — loaded, but the small copy would be enough; red — needed, but not loaded yet (the pixel shows the small copy); blue — the small copy is enough; cyan — the full texture is always loaded (grass and cut-out surfaces); grey — the texture is small enough to be loaded whole; purple — an object, a weapon or anything else outside the level's streamed textures, loaded whole and not limited by `r_texture_budget`. Terrain is coloured by the detail layer that weighs most on the pixel, not by its own texture |
| `lit` | The light that falls on the surface before its colour is applied: the sun, the sky and the lamps together, shown at half strength |
| `highlight` | The sun's highlight alone |
| `mirror` | What the surface mirrors of the sky and of the scene around it |
| `heightfield` | How much sun the level's heightfield lets through past the sun cascades (`r4_heightfield_shadows 1`, `r_shadow_quality high`): white is lit, black is shadowed. Blue where it gives no answer: off the heightfield, the switch off, a lower shadow quality, or a level without a heightfield |
| `velocity` | How far each pixel moved on the screen since the frame before: what TAA, motion blur and the upscalers read. The hue is the direction (right red, up yellow-green, left cyan, down violet), the brightness is the distance on a log scale: a quarter of a pixel is already visible, 16 pixels or more is full colour. Black is still. With a still camera only what moves by itself shows: the weapon and hands, characters, swaying grass and trees. The frame under it is drawn as usual. Needs a mode that uses motion vectors (`r_anti_aliasing taa`, motion blur, or an upscaler); without one the normal picture stays. At `r_motion_vectors_quality low` everything moves only as the camera does |

Overdraw colours: black is nothing, dark red is one layer, full red is 7 layers, yellow is 25, white is 100.

In the views with a colour per cluster, triangle or object, grey marks what is not drawn in clusters: the weapon and hands, characters, dynamic objects.

## Render targets

Under the view list the window lists the frame's render targets with their format and size. Tick the box left of a target to show it. Any number of targets can be shown at once.

Ambient occlusion: `$user$ssao_temp` is what the surfaces are darkened by; at `r_occlusion_quality medium` and up `$user$gtao_current` is the same frame's occlusion before it is blended with the frames before, so the two side by side show what the history smooths and where it lags.

For a shown target:

- **R**, **G**, **B** switch its colour channels on and off.
- **Brightness** darkens it.
- Hold the cursor over the picture to see the pixel coordinates and a magnified piece of the target under the cursor.

The last button of the icon row, **Tiles on screen**, moves all shown targets out of the window and lays them over the game's picture in a grid, each with its name in the corner: over the **Game** window's picture when it is open (`imgui_game_window 1`), also when that window is taken out into a desktop window of its own, else over the picture in the main window, without the black bars at its edges. The slider that appears next to it sets the number of columns.

The bottom of the window lists the GPU time of the frame's passes; **Stack Levels** sets how deep the list goes.

## Limitations

- Debug views work in the forward frame, the only one there is.
- The overdraw views count the level's clustered geometry and the grass. The weapon, characters and dynamic objects are drawn as one flat layer.
- `overdraw_shaded` redraws the depth of the level from nothing, so things drawn after the level (particles, the sky) can show through the weapon and dynamic objects while it is on.
- Particles, glows, wallmarks and water are hidden while a debug view is on: they blend over the frame and would cover the view. `velocity` keeps them.
- `velocity` with DLSS, FSR or XeSS goes through the upscaler, which smears it; it is exact with TAA or no anti-aliasing.
- The `$user$velocity` target itself is hard to read: one frame's motion is a few hundredths of its range, and motion to the left or down is negative and shows black. Use the `velocity` view instead.
- With MSAA on, the level is drawn in clusters into the multisampled targets: the overdraw views count every sample and show their average. Not checked in a running game.
- A debug view goes through the frame's exposure and tone curve at every shading quality, so colours are not exact.
- Targets are shown as they are stored: values above 1 are clipped, integer and depth targets may look black or flat, and there is no exact value readout yet.

## Check list after a build

1. Load a level in the simplified preset. `r_debug_view off`: the picture is unchanged.
2. `r_debug_view overdraw_raster` inside a building: the walls in front are dark red, and nothing bright shows through them. Bright patches on a wall mean hidden geometry was drawn.
3. `r_debug_view overdraw_shaded`: the picture is mostly dark red, and grass and leaves show as blades and leaves, not as flat cards.
4. `clusters`, `triangles`, `instances`, `materials`: stable colours that do not flicker when the camera stands still.
5. `streaming`: near surfaces are green; red patches disappear within a second or two after turning the camera.
6. In the window, show two targets, press **Tiles on screen**: both lie on the screen side by side with their names. With **View → Game window** on, move and shrink the **Game** window: the tiles stay inside its picture. Drag the **Game** window out of the main window onto another monitor: the tiles are drawn in that window, not in the main one.
7. On a level with a heightfield (zaton), `r_shadow_quality high`, `r4_heightfield_shadows 1`, `r_debug_view heightfield`: the ground shows what the heightfield shadows, near and far, and it moves as the sun moves; without the switch the view is blue.
8. `r_occlusion_quality medium`, show `$user$gtao_current` and `$user$ssao_temp`: with a still camera the second has none of the first one's speckle; turning the camera leaves no dark trails behind the edges of near objects. At `low` `$user$gtao_current` stays black (it is not used).
9. `r_anti_aliasing taa`, `r_motion_vectors_quality high`, `r_debug_view velocity`, on D3D11 and D3D12. Still camera: the level is black, and a reload colours the hands and weapon, a walking character colours itself, grass in wind glimmers. Turning right: the level turns cyan (the picture moves left), with the weapon nearly black. Strafing: near things brighter than far ones.
10. `r_debug_view metalness`, with `r_rendering_path` 0 and 3: bare metal (the weapon's steel, pipes, rails) is white, painted and rusty metal grey, skin black, the level and the grass of the original game blue; the two paths agree, apart from the dither grain in the first.
11. `r_debug_view off` again: the normal picture returns at once, with no hitch.
