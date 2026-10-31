# Detail v4 binary format and game runtime integration

Applies to ixray-team/ixray-1.6-stcop `default` at
`478899e687b1e5d7eab834120a252ca01b3dca36`.

> **Status on the `detail-v4` branch:** the portable v3/v4 codec and game
> renderer loader are implemented. A game smoke test using a Zaton v4 file
> with 1025 model subchunks and referenced ID 1024 succeeded. The Level Editor,
> `detail_object.part` writer, detail exporter and lighting compiler are **not**
> yet upgraded to generate/modify v4 in the normal asset pipeline. These are
> separate milestones. The game renderer retains v3 compatibility.

## Versioning and envelope

The `build.details` / `level.details` container retains its X-Ray chunked
layout:

| Chunk | Content |
|---|---|
| `0` | 24-byte `DetailHeader` payload |
| `1` | Existing numbered mesh subchunks; unchanged |
| `2` | Fixed-stride slot payload; no slot-count prefix |

`DetailHeader` is six little-endian 32-bit words:

| Byte offset | Type | Name |
|---|---|---|
| 0 | `u32` | version (`3` or `4`) |
| 4 | `u32` | object_count |
| 8 | `i32` | offs_x |
| 12 | `i32` | offs_z |
| 16 | `u32` | size_x |
| 20 | `u32` | size_z |

Chunk `2` MUST have exactly `size_x * size_z * stride` bytes, where stride is
16 for v3 and 20 for v4. Slot order is row-major: `z * size_x + x`.
Existing mesh subchunks and their shader/texture/vertex/index encoding are not
changed by the codec.

**Do not confuse this version with the Level Editor `detail_object.part` version.**
That scene file stores `DETMGR_VERSION` in chunk `0x1000`; its chunk `2`
starts with a `u32 slot_count`, followed by packed slots. Its embedded header
may contain a version that is not the serialization discriminator. Updating the
`.part` writer/reader requires a separate, backward-compatible patch.

## v3 slot compatibility

The legacy v3 format has a 16-byte slot. Bit positions below use `LSB0`:
bit 0 is the least significant bit of byte 0; for multi-byte fields, bits
increase in little-endian order.

| Bit range | Width | Value |
|---|---:|---|
| 0–11 | 12 | `y_base` |
| 12–19 | 8 | `y_height` |
| 20–25 | 6 | `id0` |
| 26–31 | 6 | `id1` |
| 32–37 | 6 | `id2` |
| 38–43 | 6 | `id3` |
| 44–47 | 4 | `c_dir` |
| 48–51 | 4 | `c_hemi` |
| 52–55 | 4 | `c_r` |
| 56–59 | 4 | `c_g` |
| 60–63 | 4 | `c_b` |
| 64–127 | 64 | four palettes (16 bits each) |

Each palette is four consecutive 4-bit alphas `a0,a1,a2,a3` in order.
Legacy disk empty ID = `63`; valid model IDs = `0..62`. The legacy `object_count`
can be above 63 if only the first 63 entries are referenced. The codec does
not reject such files solely because of the object count.

## v4 slot — 20 bytes, explicit little-endian LSB0

| Bit range | Width | Value |
|---|---:|---|
| 0–11 | 12 | `y_base` (same quantization as v3) |
| 12–19 | 8 | `y_height` (same quantization as v3) |
| 20–30 | 11 | `id0` |
| 31–41 | 11 | `id1` |
| 42–52 | 11 | `id2` |
| 53–63 | 11 | `id3` |
| 64–67 | 4 | `c_dir` |
| 68–71 | 4 | `c_hemi` |
| 72–75 | 4 | `c_r` |
| 76–79 | 4 | `c_g` |
| 80–83 | 4 | `c_b` |
| 84–95 | 12 | RESERVED, must be zero |
| 96–111 | 16 | palette0: `a0,a1,a2,a3` (4 bits each) |
| 112–127 | 16 | palette1 |
| 128–143 | 16 | palette2 |
| 144–159 | 16 | palette3 |

ID `2047` (`0x7ff`) means empty on disk; legal model IDs are `0..2046`.
`object_count` must be at most `2047`, and each non-empty ID must be less than
`object_count`. Hence 256, 512 and 1024 models are expressible without a new
revision. The internal normalized API represents empty with `uint16_t(0xffff)`
for **both** versions, preventing collisions with real ID 63 or 255.

All 12 reserved bits must be zero when writing v4 and are rejected on reading.
The fixed 20-byte stride is an on-disk contract, not `sizeof(Slot)` or a C++
bit-field ABI promise.

## Compatibility rules

* Old `.details` version 3 must remain byte-for-byte decodable and playable.
* New readers choose v3/v4 by `DetailHeader.version` for `.details` containers;
  they must not reinterpret a v3 pointer as a v4 struct.
* For `.part`, select slot version by the **outer** `DETMGR_VERSION`, retaining
  the legacy v3 chunk contents unchanged. This is a next-stage integration item.
* v3 -> v4 is lossless at slot-data level (the codec supports both structures).
* v4 -> v3 is possible only when all non-empty IDs fit in `0..62` (and all
  other fields remain representable); no silent truncation is permitted.
* The lighting compiler must write its modified slot data back in the matching
  version's encoding, including when using decoded temporary memory.
* The current 8-bit `EDetailManager::Export` remap (`255` invalid marker),
  the generator's `u8` ID locals, and per-color-group `u8` counts must be
  addressed in later integration patches.

## Implemented components and standalone tests

- `src/xrCore/DetailFormatCodec.{h,cpp}` — portable, validated v3/v4 codec.
- `src/xrCore/DetailRuntimeSlot.h` — 16-bit model IDs in the renderer.
- `src/Layers/xrRender/DetailManager{.h,.cpp,_CACHE.cpp,_Decompress.cpp}` —
  game runtime decoding and rendering with wide IDs (legacy editor layout retained).
- `tests/detail_format_v4/test_detail_format_codec.cpp` — portable unit tests,
  including a complete 1025-model v4 slot roundtrip with ID 1024.
- `tests/detail_format_v4/test_real_v3_file.cpp` — optional real v3 fixture,
  byte-exact roundtrip and runtime conversion.
- `tests/detail_format_v4/test_real_v4_file.cpp` — optional real v4 fixture,
  requires at least 1025 numbered model subchunks and a referenced ID >=1024;
  validates the real outer container and slot payload byte-for-byte.

The tests are an **independent CMake project**, not part of the engine's
`IXRAY_TESTS` option. Configure them separately; otherwise CTest in the main
engine build will not list them. On Windows, for example:

```powershell
cmake -S tests/detail_format_v4 -B build/detail-v4-tests -G "Visual Studio 17 2022" -A x64 `
  -DIXRAY_DETAIL_V3_FILE="D:/ixray/sdk/gamedata/levels/zaton/level.details" `
  -DIXRAY_DETAIL_V4_FILE="D:/ixray/IXRay-Source-r136/build/detail-v4-zaton-stress/level.details"
cmake --build build/detail-v4-tests --config Release --parallel 4
ctest --test-dir build/detail-v4-tests -C Release --output-on-failure
```

The C++ tests need only C++17; they do not link DirectX or the game. If the
optional fixture paths are omitted, only the codec unit test is registered.
The v4 real-file test verifies encoded references and mesh subchunk framing,
**not** the visual quality, actual mesh diversity, or GPU draw calls.

## Remaining integration before full authoring of v4 maps

1. Update `EDetailManager` `.part` load/save and `build.details` export/remap.
2. Update the detail generator's model IDs and independent color-index counts.
3. Update `xrLC_Light` to correctly read/write v4 while lighting.
4. Coordinate support in SDK tooling / converters while retaining existing
   v3 assets and unmodified v3 loading.
5. Benchmark memory, atlas, GPU rendering and draw-call costs on maps with
   large **distinct** model libraries. The existing 1025-model smoke-test
   duplicates one original mesh into additional numbered model entries.

The Detail v4 game runtime reads v3 and v4 already. This does **not** mean
that the editor/compiler asset-export pipeline writes v4 yet.
