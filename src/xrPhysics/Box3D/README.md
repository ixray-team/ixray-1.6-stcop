# Box3D static collision streaming

`CB3World::SyncStaticGeometry()` synchronizes the native static world with the
resident `CDB::MODEL` snapshot. All implementation changes live in `xrPhysics`;
Box3D itself and the engine's CDB streamer are not modified.

## Scheduling and ownership

- The first nonempty snapshot is cooked and attached synchronously. Creating the
  physics world before the CDB streamer publishes its first tiles does not consume
  this initialization. Subsequent mesh preparation uses a world-owned
  `xr_task_group`, with at most one outstanding batch.
- The worker owns references to its source models and shares immutable flattened
  geometry. It performs instance transformation, boundary-triangle deduplication,
  material-slot assignment, native mesh/BVH construction, and source-face mapping.
  Material properties are copied on the physics thread, not read from `GMLib` by
  the worker. No worker creates or destroys bodies or shapes in a live world.
- Ready batches are published on the physics owner thread, outside native queries,
  simulation, and guarded contact/event processing. Unchanged chunks retain their
  native shapes and contacts. Replacements attach before obsolete shapes retire,
  including when another resident tile takes over duplicate boundary geometry.
- A revision change cancels outdated preparation between cooking operations. Stale
  results are discarded without attaching them. World destruction cancels and joins
  the task before releasing meshes, source references, or the native world.
- Normal frame/query paths never wait for background cooking. Until a batch is
  ready, queries and simulation use the previous complete native snapshot. Thus a
  newly published CDB tile/sector is not necessarily visible to Box3D immediately;
  removed native geometry may remain briefly during the handover. An empty target
  snapshot needs no cooking and can be retired immediately.
- `WaitForStaticGeometry()` is an explicit loading/test barrier; it must only be
  used on the physics owner thread outside simulation and callbacks. It is not
  called by normal frame/query paths. All other world operations remain serialized
  on that owner thread; asynchronous cooking does not make `CB3World` thread-safe.

The current Box3D `b3CreateMesh()` builds independent data without accessing a
world. Its default allocator is thread-safe and its allocation counter is atomic.
Do not change Box3D's global allocator, length scale, or callbacks concurrently
with cooking. Recheck these assumptions when updating the Box3D dependency.

## Known limitation: duplicated collision memory

**Static geometry and acceleration structures are stored both in `CDB::MODEL` and
in Box3D's native meshes/BVHs.** Instanced geometry is flattened into world-space
vertices for native cooking. The integration also retains source-face mappings
and flattened geometry; asynchronous replacement temporarily retains old and new
snapshots together. This increases resident and peak memory usage.

This is an acknowledged limitation, intentionally not addressed by asynchronous
streaming. Sharing CDB's mesh/BVH with Box3D is future work and would require a
different integration rather than passing a `CDB::MODEL` to `b3CreateMesh()`.

## Verification

Build `xrPhysics` and `TestBox3DCollision` with `IXRAY_PHYSICS_BACKEND=Box3D` and
`IXRAY_TESTS=ON`. Run `TestBox3DCollision <new-empty-fixture-directory>`.
The test covers initial synchronous cooking, asynchronous replacement, stable
shapes, stale batch rejection, boundary ownership transfer, materials and
instances, support removal, pending-task teardown/recreation, and native triangle
reordering. Real-level streaming and teleport behavior still need gameplay testing.
