# Detail Layers Editor

Open **Game → Editors → Detail Layers Editor** on a loaded level with R4 and `DEBUG_DRAW` (Debug or RelWithDebInfo). Alt toggles mouse capture.

## Controls

- **Scale** paints grass scale; **Mix** chooses cluster assets to replace grass.
- Left mouse paints on the surface under the cursor. Positive **Press intensity** builds up the mask, negative intensity reduces it; the default is zero. Right mouse wipes the mask under the brush.
- **Use pattern**, seed, frequency, threshold and sharpness control the noise pattern. Preview, minimap and cursor options show the affected area.
- **Generic** adjusts global detail generation, **Trample** adjusts vegetation bending, and **Wind** adjusts vegetation wind.
- **Save masks** writes the current painted masks. **Clear masks** clears them and requests a cache rebuild. **Rebuild cache** refreshes the detail cache; **Clear trail** removes the visual brush trail.

## Data and limitations

The editor overlays the loaded level's baked detail data. Painted masks are saved under `$level$/detail_layers/` as `user_mask_detail_fmb_noise.bin` and `user_mask_detail_perlin_scale.bin`, and loaded with the level. Untouched areas keep their baked or generated values. The mask save action saves painted masks; global detail, wind and trample settings are separate console settings.

Painting requires loaded detail data. Surface picking uses static level geometry and skips passable materials and actor obstacles; dynamic objects do not participate. Entering **Mix** enables cluster replacement and refreshes the cache. This tool edits detail vegetation, rather than the level mesh or terrain heightmap.

## Post-build checks

Use Debug first, then repeat in RelWithDebInfo:

1. Build R4 on macOS and Windows and load a level with detail vegetation.
2. Open the editor, release mouse capture with Alt, and check the cursor and preview over solid ground. Passable surfaces and actor obstacles should not intercept picking.
3. Paint and wipe a small area in Scale and Mix; confirm that the visible change follows the selected surface.
4. Save masks, reload the level, and confirm that the painted area persists while untouched vegetation stays unchanged.
