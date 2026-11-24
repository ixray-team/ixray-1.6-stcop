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
| `lmap` | Baked light alone |
| `streaming` | The state of the base texture: green — the full texture is loaded and the pixel needs it; yellow — loaded, but the small copy would be enough; red — needed, but not loaded yet (the pixel shows the small copy); blue — the small copy is enough; cyan — the full texture is always loaded (grass and cut-out surfaces); grey — the texture is small enough to be loaded whole; purple — an object, a weapon or anything else outside the level's streamed textures, loaded whole and not limited by `r_texture_budget`. Terrain is coloured by the detail layer that weighs most on the pixel, not by its own texture |
| `lit` | The light that falls on the surface before its colour is applied: the sun, the sky and the lamps together, shown at half strength |
| `highlight` | The sun's highlight alone |
| `mirror` | What the surface mirrors of the sky and of the scene around it |
| `heightfield` | How much sun the level's heightfield lets through past the sun cascades (`r4_heightfield_shadows 1`, `r_shadow_quality high`): white is lit, black is shadowed. Blue where it gives no answer: off the heightfield, the switch off, a lower shadow quality, or a level without a heightfield |

Overdraw colours: black is nothing, dark red is one layer, full red is 7 layers, yellow is 25, white is 100.

In the views with a colour per cluster, triangle or object, grey marks what is not drawn in clusters: the weapon and hands, characters, dynamic objects.

## Render targets

Under the view list the window lists the frame's render targets with their format and size. Tick the box left of a target to show it. Any number of targets can be shown at once.

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
- Particles, glows, wallmarks and water are hidden while a debug view is on: they blend over the frame and would cover the view.
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
8. `r_debug_view off` again: the normal picture returns at once, with no hitch.
