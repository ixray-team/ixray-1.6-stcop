#include "../../src/xrCore/DetailFormatCodec.h"
#include "../../src/xrCore/DetailRuntimeSlot.h"

#include <algorithm>
#include <array>
#include <cstdint>
#include <cstdlib>
#include <cstring>
#include <iostream>
#include <limits>
#include <vector>

using namespace ixray::detail_format;

#define CHECK(cond)                                                                                  \
    do { if (!(cond)) { std::cerr << "FAILED: " << #cond << " at line " << __LINE__ << '\n';         \
                         std::exit(EXIT_FAILURE); } } while (false)

namespace
{
Slot example()
{
    Slot slot;
    slot.yBase = 1000;
    slot.yHeight = 75;
    slot.ids = {0, 62, EmptyId, 17};
    slot.cDir = 12;
    slot.cHemi = 4;
    slot.cR = 9;
    slot.cG = 2;
    slot.cB = 15;
    for (std::size_t i = 0; i < 4; ++i)
        for (std::size_t j = 0; j < 4; ++j)
            slot.palettes[i].alpha[j] = static_cast<std::uint8_t>(i * 4 + j);
    return slot;
}

void assertSame(const Slot& a, const Slot& b)
{
    CHECK(a.yBase == b.yBase);
    CHECK(a.yHeight == b.yHeight);
    CHECK(a.ids == b.ids);
    CHECK(a.cDir == b.cDir);
    CHECK(a.cHemi == b.cHemi);
    CHECK(a.cR == b.cR);
    CHECK(a.cG == b.cG);
    CHECK(a.cB == b.cB);
    for (std::size_t i = 0; i < 4; ++i)
        CHECK(a.palettes[i].alpha == b.palettes[i].alpha);
}

void testV3Golden()
{
    // Independently calculated MSVC/X-Ray v3 little-endian bytes: bit-fields
    // [yBase:12,yHeight:8,id0..3:6,cDir/cHemi/cR/cG/cB:4], palettes a0..3.
    constexpr std::array<std::uint8_t, SlotSizeV3> golden = {
        0xe8, 0xb3, 0x04, 0xf8, 0x7f, 0xc4, 0x94, 0xf2,
        0x10, 0x32, 0x54, 0x76, 0x98, 0xba, 0xdc, 0xfe
    };
    Slot decoded;
    CHECK(decodeSlot(Version::V3, golden.data(), golden.size(), decoded) == Status::Ok);
    assertSame(decoded, example());
    std::array<std::uint8_t, SlotSizeV3> encoded{};
    CHECK(encodeSlot(Version::V3, example(), encoded.data(), encoded.size()) == Status::Ok);
    CHECK(encoded == golden);
    CHECK(decoded.ids[2] == EmptyId);
}

void testV4Roundtrips()
{
    constexpr std::array<std::uint16_t, 15> idCases = {
        0, 62, 63, 64, 127, 128, 255, 256, 511, 512, 1023, 1024, 2045, 2046, EmptyId
    };
    for (std::uint16_t id : idCases)
    {
        Slot original = example();
        original.ids = {id, 63, 255, EmptyId};
        std::array<std::uint8_t, SlotSizeV4> raw;
        raw.fill(0xff);
        CHECK(encodeSlot(Version::V4, original, raw.data(), raw.size()) == Status::Ok);
        CHECK(raw[11] == 0 && (raw[10] & 0xf0) == 0); // 12 reserved bits.
        Slot result;
        CHECK(decodeSlot(Version::V4, raw.data(), raw.size(), result) == Status::Ok);
        assertSame(original, result);
    }
    Slot original;
    original.ids = {2046, 2046, 2046, 2046};
    std::array<std::uint8_t, SlotSizeV4> raw{};
    CHECK(encodeSlot(Version::V4, original, raw.data(), raw.size()) == Status::Ok);
    original.ids[0] = 2047; // Must not collide with empty marker.
    CHECK(encodeSlot(Version::V4, original, raw.data(), raw.size()) == Status::UnrepresentableId);
}

void testConversionAndBounds()
{
    Slot v3 = example();
    std::array<std::uint8_t, SlotSizeV3> oldBytes{};
    CHECK(encodeSlot(Version::V3, v3, oldBytes.data(), oldBytes.size()) == Status::Ok);
    Slot normalized;
    CHECK(decodeSlot(Version::V3, oldBytes.data(), oldBytes.size(), normalized) == Status::Ok);
    std::array<std::uint8_t, SlotSizeV4> newBytes{};
    CHECK(encodeSlot(Version::V4, normalized, newBytes.data(), newBytes.size()) == Status::Ok);
    Slot final;
    CHECK(decodeSlot(Version::V4, newBytes.data(), newBytes.size(), final) == Status::Ok);
    assertSame(v3, final);

    final.ids[0] = 63; // Valid in v4, reserved empty in v3.
    CHECK(encodeSlot(Version::V3, final, oldBytes.data(), oldBytes.size()) == Status::UnrepresentableId);
    final.ids[0] = EmptyId;
    final.cR = 16;
    CHECK(encodeSlot(Version::V4, final, newBytes.data(), newBytes.size()) == Status::OutOfRange);
    final.cR = 0;
    final.yBase = 4096;
    CHECK(encodeSlot(Version::V4, final, newBytes.data(), newBytes.size()) == Status::OutOfRange);
    final.yBase = 0;
    final.palettes[2].alpha[1] = 16;
    CHECK(encodeSlot(Version::V4, final, newBytes.data(), newBytes.size()) == Status::OutOfRange);

    CHECK(decodeSlot(Version::V4, oldBytes.data(), oldBytes.size(), final) == Status::BadSize);
    CHECK(decodeSlot(static_cast<Version>(99), oldBytes.data(), oldBytes.size(), final) == Status::UnsupportedVersion);
    CHECK(decodeSlot(Version::V4, nullptr, SlotSizeV4, final) == Status::NullData);
    std::array<std::uint8_t, SlotSizeV4> bad{};
    CHECK(encodeSlot(Version::V4, Slot{}, bad.data(), bad.size()) == Status::Ok);
    bad[11] = 0x1;
    CHECK(decodeSlot(Version::V4, bad.data(), bad.size(), final) == Status::ReservedBitsSet);
}

void testRuntimeConversion()
{
    for (Version version : {Version::V3, Version::V4})
    {
        Slot original = example();
        if (version == Version::V4)
            original.ids = {63, 255, 1024, EmptyId};
        const std::size_t stride = version == Version::V3 ? SlotSizeV3 : SlotSizeV4;
        std::array<std::uint8_t, SlotSizeV4> bytes{};
        CHECK(encodeSlot(version, original, bytes.data(), stride) == Status::Ok);
        Slot decoded;
        CHECK(decodeSlot(version, bytes.data(), stride, decoded) == Status::Ok);
        const RuntimeSlot runtime = toRuntimeSlot(decoded);
        CHECK(runtime.y_base == original.yBase);
        CHECK(runtime.y_height == original.yHeight);
        CHECK(runtime.r_id(0) == original.ids[0]);
        CHECK(runtime.r_id(1) == original.ids[1]);
        CHECK(runtime.r_id(2) == original.ids[2]);
        CHECK(runtime.r_id(3) == original.ids[3]);
        CHECK(runtime.r_id(4) == EmptyId);
        CHECK(runtime.r_ybase() == float(original.yBase) * .2f - 200.f);
        CHECK(runtime.r_yheight() == float(original.yHeight) * .1f);
        CHECK(runtime.c_hemi == original.cHemi);
        CHECK(runtime.c_dir == original.cDir);
        CHECK(runtime.c_r == original.cR);
        CHECK(runtime.c_g == original.cG);
        CHECK(runtime.c_b == original.cB);
        for (std::size_t i = 0; i < 4; ++i)
        {
            CHECK(runtime.palette[i].a0 == original.palettes[i].alpha[0]);
            CHECK(runtime.palette[i].a1 == original.palettes[i].alpha[1]);
            CHECK(runtime.palette[i].a2 == original.palettes[i].alpha[2]);
            CHECK(runtime.palette[i].a3 == original.palettes[i].alpha[3]);
        }
    }
}

void testHeader()
{
    Header h{4, 1024, -500, 701, 1582, 1582};
    std::array<std::uint8_t, HeaderSize> bytes{};
    CHECK(encodeHeader(h, bytes.data(), bytes.size()) == Status::Ok);
    CHECK(bytes[0] == 4 && bytes[4] == 0 && bytes[5] == 4);
    CHECK(bytes[8] == 0x0c && bytes[9] == 0xfe && bytes[10] == 0xff && bytes[11] == 0xff);
    Header r;
    CHECK(decodeHeader(bytes.data(), bytes.size(), r) == Status::Ok);
    CHECK(r.version == h.version && r.objectCount == h.objectCount);
    CHECK(r.offsX == h.offsX && r.offsZ == h.offsZ);
    CHECK(r.sizeX == h.sizeX && r.sizeZ == h.sizeZ);
    CHECK(decodeHeader(nullptr, bytes.size(), r) == Status::NullData);
    CHECK(encodeHeader(h, bytes.data(), bytes.size() - 1) == Status::BadSize);
}

// Regression: IDs over 1023 are real model IDs, not truncated or empty.
// A v4 file with 1025 models must preserve ID 1024 across slot and runtime conversions.
void test1025Models()
{
    Header header{4, 1025, -123, 456, 4, 2};
    std::vector<Slot> original(8);
    for (std::size_t i = 0; i < original.size(); ++i)
        original[i].ids = {1024, static_cast<std::uint16_t>(i), EmptyId, 1023};

    std::vector<std::uint8_t> encoded;
    CHECK(encodeSlots(Version::V4, header, original, encoded) == Status::Ok);
    CHECK(encoded.size() == original.size() * SlotSizeV4);

    std::vector<Slot> decoded;
    CHECK(decodeSlots(Version::V4, header, encoded.data(), encoded.size(), decoded) == Status::Ok);
    CHECK(decoded.size() == original.size());
    for (std::size_t i = 0; i < original.size(); ++i)
    {
        assertSame(original[i], decoded[i]);
        const RuntimeSlot runtime = toRuntimeSlot(decoded[i]);
        CHECK(runtime.r_id(0) == 1024);
        CHECK(runtime.r_id(3) == 1023);
        CHECK(runtime.r_id(2) == EmptyId);
    }

    // No truncation to 8 bits, 10 bits, or the legacy v3 6-bit IDs.
    Header insufficient = header;
    insufficient.objectCount = 1024;
    CHECK(decodeSlots(Version::V4, insufficient, encoded.data(), encoded.size(), decoded)
          == Status::InvalidModelReference);
    CHECK(encodeSlots(Version::V4, insufficient, original, encoded)
          == Status::InvalidModelReference);
    CHECK(encodeSlot(Version::V3, original[0], encoded.data(), SlotSizeV3)
          == Status::UnrepresentableId);
}

void testChunks()
{
    Header header{4, 1024, 0, 0, 2, 2};
    std::vector<Slot> slots(4);
    slots[0].ids = {63, 255, 256, EmptyId};
    slots[1].ids = {511, 512, 1023, 1023};
    slots[2].ids = {0, EmptyId, EmptyId, EmptyId};
    std::vector<std::uint8_t> payload;
    CHECK(encodeSlots(Version::V4, header, slots, payload) == Status::Ok);
    CHECK(payload.size() == 4 * SlotSizeV4);
    std::vector<Slot> decoded;
    CHECK(decodeSlots(Version::V4, header, payload.data(), payload.size(), decoded) == Status::Ok);
    CHECK(decoded.size() == 4);
    for (std::size_t i = 0; i < slots.size(); ++i) assertSame(slots[i], decoded[i]);

    const std::vector<Slot> snapshot = decoded;
    header.objectCount = 1023; // ID 1023 now outside declared model count.
    CHECK(decodeSlots(Version::V4, header, payload.data(), payload.size(), decoded) == Status::InvalidModelReference);
    CHECK(decoded.size() == snapshot.size());
    assertSame(decoded[1], snapshot[1]); // No partial overwrite on failure.
    header.objectCount = 1024;
    CHECK(decodeSlots(Version::V4, header, payload.data(), payload.size() - 1, decoded) == Status::BadSize);
    header.objectCount = 2048;
    CHECK(decodeSlots(Version::V4, header, payload.data(), payload.size(), decoded) == Status::InvalidModelCount);

    Header oldHeader{3, 125, 0, 0, 1, 1}; // v3 may register >63 models but address only 0..62.
    std::vector<Slot> oldSlots(1, example());
    CHECK(encodeSlots(Version::V3, oldHeader, oldSlots, payload) == Status::Ok);
    CHECK(payload.size() == SlotSizeV3);
    CHECK(decodeSlots(Version::V3, oldHeader, payload.data(), payload.size(), decoded) == Status::Ok);
    assertSame(oldSlots[0], decoded[0]);

    Header overflow{4, 1024, 0, 0, std::numeric_limits<std::uint32_t>::max(),
                    std::numeric_limits<std::uint32_t>::max()};
    CHECK(encodeSlots(Version::V4, overflow, slots, payload) == Status::SizeOverflow);
    CHECK(std::strcmp(statusMessage(Status::InvalidModelReference), "detail ID exceeds available model count") == 0);
}
}

int main()
{
    testV3Golden();
    testRuntimeConversion();
    testV4Roundtrips();
    test1025Models();
    testConversionAndBounds();
    testHeader();
    testChunks();
    std::cout << "OK: X-Ray detail v3/v4 codec + validation tests\n";
}
