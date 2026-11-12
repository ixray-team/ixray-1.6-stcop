# Static collision streaming

Adapted from `9faa8a23fcc3116d004c10799cfa317db1ffeb0a` (`Add collision streaming`)
in the older Embree branch. The converter and level-builder projects are intentionally
unchanged by this port.

## Runtime and formats

- `XRay::CForm::CFormatStreamed` and `CFormatStreamedInstanced` write/read a metadata-only
  `level.cform`. It contains `hdrCFORM`, tile size (`float`), tile count (`u32`), signed
  X/Z tile coordinates, manual-sector count (`u32`), and nonzero sector IDs (`u32`).
- Geometry lives in `level_tile_<X>_<Z>.cform` and `level_sector_<ID>.cform`. Subfiles
  use this branch's existing Vanilla or Instanced format. Deploy the master and all
  its subfiles together. A missing referenced subfile is a load failure, not empty geometry.
- `CDB::TRI::StreamedSectorID == 0` selects automatic grid streaming. Nonzero values
  select whole manually loaded sectors. An instance has a separate render-sector ID
  and streaming-sector ID; the render-sector ID is preserved in its subfile.
- Large triangles and instances are assigned to every tile overlapped by their XZ
  bounding box. Aggregated collision queries remove duplicate boundary triangles.
- `IGame_Level::OnFrame()` updates camera-centered residency before `Objects.Update()`.
  Default load/unload radii are 250/300 m. The first update is synchronous and uses the
  view entity's position when available, before the camera's first object tick.
  Later updates prepare fully built tile snapshots in `Device.async_tasks`, with only
  one outstanding preparation per level. Camera position and radii are captured on the
  frame thread. A completed snapshot is published on a later frame before simulation;
  workers never mutate live residency. Unfinished work does not block the frame.
  Stop/unload drains scheduled work before releasing the level or collision format.
  A manual residency change invalidates older prepared snapshots; they are discarded
  and the next task prepares a fresh one.
- Manual sectors use `CObjectSpace::LoadStreamedSector()` / `UnloadStreamedSector()`;
  automatic updates never change them. No Lua binding or automatic underground-sector
  policy is provided.
- Model snapshots retain ownership during queries. ODE invalidates cached triangle
  pointers and negative contacts when residency changes. Sound and debug visualization
  query resident geometry; debug selection waits for its snapshot to be published.
- `GetStaticModel()` still exposes only the legacy, non-streamed model. New callers
  should use the `CDB::COLLIDER` overloads accepting `CObjectSpace&`, or retain a
  `GetActiveStreamedModels()` snapshot while traversing resident geometry.
- Residency changes must be performed at simulation/frame boundaries, not inside a
  collision callback or concurrently with arbitrary game logic retaining raw results.

Existing format IDs stay unchanged: Vanilla = 4, VanillaChunked = 5,
VanillaChunkedData = 6, Instanced = 7, InstancedChunked = 8,
InstancedChunkedData = 9. Streamed = 10 and StreamedInstanced = 11.
Vanilla/Instanced disk triangles remain 16 bytes; runtime triangles are 20 bytes.
This preserves this branch's existing cforms. It does **not** make experimental
files produced by the older branch binary-compatible: that commit renumbered format
IDs and used a different instanced serialization. Re-export them with this branch.

## Work intentionally left in the tools

`xrLevelFilesConverter` / `src/utils/xrCformConvert`:

1. Add Streamed to the target-format selector and expose a positive tile size in meters.
2. Add the conversion dispatch in `StartupConv.cpp`: extract the input static mesh,
   create `CFormatStreamed(TileSize)`, add the mesh, then write the output master/subfiles.
3. Define conversion behavior for instanced inputs and streamed input masters; their
   `GetStaticGeom()` deliberately does not flatten the whole level.

`xrLevelBuilder` and its compiler UI/settings:

1. Add Streamed and StreamedInstanced to the format selectors. Persist a tile-size
   setting (the original default was 100 m, UI minimum 10 m).
2. Extend the format dispatch in
   `src/utils/xrLevelBuilder/xrLC/CForm/xrBuildCForm.cpp` (`BuildCForm` / `BuildCTree`).
3. Read `b_mu_reference::CollisionStreamingSector` into the compiler MU reference and
   pass it as the last `IFormat::AddInstanceRef()` argument. Existing callers remain
   source-compatible because this argument defaults to zero. Do not confuse it with
   the existing render sector.
4. Carry manual-sector IDs through static-face collection, simplification and export
   into `CDB::TRI::StreamedSectorID`. The original commit did not complete this path.
5. Re-export `build.prj` with the updated editor before compiling: the in-memory TRI
   extension also changes raw triangle arrays in the editor/compiler intermediate data.

The level editor now saves `CSector::CollisionStreamingID` and exports MU reference
markers in the reserved portion of `b_mu_reference`; the compiler still needs the
steps above to consume them. The Box3D backend is not adapted by this port; use ODE
for streamed levels. Its independently baked static level mesh needs a separate
residency/rebuild integration.

## Verification

Build `TestCollisionStreaming` with `IXRAY_TESTS=ON`, then run:

```text
TestCollisionStreaming <dedicated-empty-directory>
```

The test leaves generated fixtures in the supplied directory. It checks legacy disk
layout, both streamed formats, signed/interior tiles, independent render/manual sectors,
instance prototype counts, metadata-only loading, boundary queries, unload revisions,
snapshot lifetime, background preparation versus publication, stale-update rejection,
cancelled pending updates, and empty-residency results. A real-level gameplay test is still
needed for initial spawn, teleports, off-camera simulation, sound, and renderer behavior.
