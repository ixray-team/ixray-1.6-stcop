# Detail v4 binary format — initial codec milestone

Applies to ixray-team/ixray-1.6-stcop `default` at
`478899e687b1e5d7eab834120a252ca01b3dca36`.

> **Status: proposed v4 / implemented standalone codec.** No gameplay or Level
> Editor integration yet. v3 behaviour is preserved; the new codec is an
> opt-in dependency for subsequent integration commits.

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

## First milestone files and standalone tests

- `src/xrCore/DetailFormatCodec.h`
- `src/xrCore/DetailFormatCodec.cpp`
- `tests/detail_format_v4/CMakeLists.txt`
- `tests/detail_format_v4/test_detail_format_codec.cpp`

From the repository root:

```sh
cmake -S tests/detail_format_v4 -B build-detail-format-tests
cmake --build build-detail-format-tests
ctest --test-dir build-detail-format-tests --output-on-failure
```

This isolated test project needs only C++17 and a C++ compiler; it does not
link DirectX, the full IX-Ray engine, or platform-specific SDK dependencies.
It checks v3 golden bytes, v3->v4 round-tripping, boundary IDs, reserved bits,
malformed lengths, invalid references and count/size overflow.

## Still required before gameplay use

1. Add a normalized slot store to `CDetailManager`, with versioned loading.
2. Update `EDetailManager` `.part` load/save and `build.details` export/remap.
3. Update the generator's object IDs and independent color-index counts.
4. Update `xrLC_Light` to correctly read/write v4 while lighting.
5. Optionally extend PCore readers, and measure texture-atlas and draw-call
   costs on 64, 128, 256, 512, and 1024-model maps.

**This initial patch deliberately does not change the runtime behaviour or
write v4 game files from the editor.**
