// MSVC's ixray xrCore target uses /Yustdafx.h; standalone tests intentionally
// bypass engine PCH and compile the codec as ordinary portable C++.
#if defined(_MSC_VER) && !defined(IXRAY_DETAIL_CODEC_STANDALONE)
#    include "stdafx.h"
#endif

#include "DetailFormatCodec.h"

#include <cstring>
#include <limits>
#include <utility>

namespace ixray::detail_format
{
namespace
{
constexpr std::uint32_t BaseBits = 12;
constexpr std::uint32_t HeightBits = 8;
constexpr std::uint32_t ColorBits = 4;
constexpr std::uint32_t SlotsPerCell = 4;
constexpr std::uint32_t PaletteCorners = 4;

std::uint32_t readLe32(const std::uint8_t* p) noexcept
{
    return std::uint32_t(p[0]) | (std::uint32_t(p[1]) << 8) |
           (std::uint32_t(p[2]) << 16) | (std::uint32_t(p[3]) << 24);
}

void writeLe32(std::uint8_t* p, std::uint32_t n) noexcept
{
    for (std::uint32_t i = 0; i < 4; ++i)
        p[i] = static_cast<std::uint8_t>(n >> (i * 8));
}

// Bit index 0 is the least-significant bit of the first byte.
std::uint32_t readBits(const std::uint8_t* bytes, std::uint32_t first, std::uint32_t count) noexcept
{
    std::uint32_t value = 0;
    for (std::uint32_t bit = 0; bit < count; ++bit)
        value |= std::uint32_t((bytes[(first + bit) / 8] >> ((first + bit) % 8)) & 1u) << bit;
    return value;
}

void writeBits(std::uint8_t* bytes, std::uint32_t first, std::uint32_t count, std::uint32_t value) noexcept
{
    for (std::uint32_t bit = 0; bit < count; ++bit)
    {
        const std::uint32_t pos = first + bit;
        if (((value >> bit) & 1u) != 0u)
            bytes[pos / 8] |= std::uint8_t(1u << (pos % 8));
    }
}

std::size_t slotSize(Version version) noexcept
{
    switch (version)
    {
    case Version::V3: return SlotSizeV3;
    case Version::V4: return SlotSizeV4;
    }
    return 0;
}

Status checkSlot(const Slot& slot, Version version) noexcept
{
    const std::uint32_t maxId = version == Version::V3 ? DiskEmptyV3 : DiskEmptyV4;
    if (slot.yBase > 4095)
        return Status::OutOfRange;
    if (slot.cDir > 15 || slot.cHemi > 15 || slot.cR > 15 || slot.cG > 15 || slot.cB > 15)
        return Status::OutOfRange;
    for (std::uint16_t id : slot.ids)
        if (id != EmptyId && id >= maxId)
            return Status::UnrepresentableId;
    for (const Palette& palette : slot.palettes)
        for (std::uint8_t a : palette.alpha)
            if (a > 15)
                return Status::OutOfRange;
    return Status::Ok;
}

Status checkModelReferences(const Slot& slot, std::uint32_t objectCount) noexcept
{
    for (const std::uint16_t id : slot.ids)
        if (id != EmptyId && std::uint32_t(id) >= objectCount)
            return Status::InvalidModelReference;
    return Status::Ok;
}

Status calculateSize(Version version, const Header& header, std::size_t& count,
                     std::size_t& size) noexcept
{
    const std::size_t stride = slotSize(version);
    if (!stride)
        return Status::UnsupportedVersion;
    if (version == Version::V4 && header.objectCount > MaxModelsV4)
        return Status::InvalidModelCount;
    if (header.sizeX != 0 && std::size_t(header.sizeZ) > std::numeric_limits<std::size_t>::max() / header.sizeX)
        return Status::SizeOverflow;
    count = std::size_t(header.sizeX) * std::size_t(header.sizeZ);
    if (count > std::numeric_limits<std::size_t>::max() / stride)
        return Status::SizeOverflow;
    size = count * stride;
    return Status::Ok;
}

} // namespace

Status decodeHeader(const std::uint8_t* bytes, std::size_t size, Header& out) noexcept
{
    if (size != HeaderSize) return Status::BadSize;
    if (!bytes) return Status::NullData;
    Header result;
    result.version = readLe32(bytes);
    result.objectCount = readLe32(bytes + 4);
    const std::uint32_t ox = readLe32(bytes + 8), oz = readLe32(bytes + 12);
    std::memcpy(&result.offsX, &ox, sizeof(ox));
    std::memcpy(&result.offsZ, &oz, sizeof(oz));
    result.sizeX = readLe32(bytes + 16);
    result.sizeZ = readLe32(bytes + 20);
    out = result;
    return Status::Ok;
}

Status encodeHeader(const Header& header, std::uint8_t* bytes, std::size_t size) noexcept
{
    if (size != HeaderSize) return Status::BadSize;
    if (!bytes) return Status::NullData;
    std::uint32_t ox = 0, oz = 0;
    std::memcpy(&ox, &header.offsX, sizeof(ox));
    std::memcpy(&oz, &header.offsZ, sizeof(oz));
    writeLe32(bytes, header.version);
    writeLe32(bytes + 4, header.objectCount);
    writeLe32(bytes + 8, ox);
    writeLe32(bytes + 12, oz);
    writeLe32(bytes + 16, header.sizeX);
    writeLe32(bytes + 20, header.sizeZ);
    return Status::Ok;
}

Status decodeSlot(Version version, const std::uint8_t* bytes, std::size_t size, Slot& out) noexcept
{
    const std::size_t stride = slotSize(version);
    if (!stride) return Status::UnsupportedVersion;
    if (size != stride) return Status::BadSize;
    if (!bytes) return Status::NullData;

    const std::uint32_t idBits = version == Version::V3 ? 6 : 11;
    const std::uint16_t diskEmpty = version == Version::V3 ? DiskEmptyV3 : DiskEmptyV4;
    const std::uint32_t colorStart = BaseBits + HeightBits + SlotsPerCell * idBits;
    const std::uint32_t paletteStart = version == Version::V3 ? 64 : 96;

    if (version == Version::V4 && readBits(bytes, 84, 12) != 0)
        return Status::ReservedBitsSet;

    Slot result;
    result.yBase = static_cast<std::uint16_t>(readBits(bytes, 0, BaseBits));
    result.yHeight = static_cast<std::uint8_t>(readBits(bytes, BaseBits, HeightBits));
    for (std::uint32_t i = 0; i < SlotsPerCell; ++i)
    {
        const auto id = static_cast<std::uint16_t>(readBits(bytes, BaseBits + HeightBits + i * idBits, idBits));
        result.ids[i] = id == diskEmpty ? EmptyId : id;
    }
    result.cDir = static_cast<std::uint8_t>(readBits(bytes, colorStart, ColorBits));
    result.cHemi = static_cast<std::uint8_t>(readBits(bytes, colorStart + 4, ColorBits));
    result.cR = static_cast<std::uint8_t>(readBits(bytes, colorStart + 8, ColorBits));
    result.cG = static_cast<std::uint8_t>(readBits(bytes, colorStart + 12, ColorBits));
    result.cB = static_cast<std::uint8_t>(readBits(bytes, colorStart + 16, ColorBits));
    for (std::uint32_t i = 0; i < SlotsPerCell; ++i)
        for (std::uint32_t corner = 0; corner < PaletteCorners; ++corner)
            result.palettes[i].alpha[corner] = static_cast<std::uint8_t>(
                readBits(bytes, paletteStart + i * 16 + corner * 4, 4));
    out = result;
    return Status::Ok;
}

Status encodeSlot(Version version, const Slot& slot, std::uint8_t* bytes, std::size_t size) noexcept
{
    const std::size_t stride = slotSize(version);
    if (!stride) return Status::UnsupportedVersion;
    if (size != stride) return Status::BadSize;
    if (!bytes) return Status::NullData;
    const Status valid = checkSlot(slot, version);
    if (valid != Status::Ok) return valid;

    const std::uint32_t idBits = version == Version::V3 ? 6 : 11;
    const std::uint16_t diskEmpty = version == Version::V3 ? DiskEmptyV3 : DiskEmptyV4;
    const std::uint32_t colorStart = BaseBits + HeightBits + SlotsPerCell * idBits;
    const std::uint32_t paletteStart = version == Version::V3 ? 64 : 96;
    std::memset(bytes, 0, size); // Reserved bits are always zero.
    writeBits(bytes, 0, BaseBits, slot.yBase);
    writeBits(bytes, BaseBits, HeightBits, slot.yHeight);
    for (std::uint32_t i = 0; i < SlotsPerCell; ++i)
        writeBits(bytes, BaseBits + HeightBits + i * idBits, idBits,
                  slot.ids[i] == EmptyId ? diskEmpty : slot.ids[i]);
    writeBits(bytes, colorStart, ColorBits, slot.cDir);
    writeBits(bytes, colorStart + 4, ColorBits, slot.cHemi);
    writeBits(bytes, colorStart + 8, ColorBits, slot.cR);
    writeBits(bytes, colorStart + 12, ColorBits, slot.cG);
    writeBits(bytes, colorStart + 16, ColorBits, slot.cB);
    for (std::uint32_t i = 0; i < SlotsPerCell; ++i)
        for (std::uint32_t corner = 0; corner < PaletteCorners; ++corner)
            writeBits(bytes, paletteStart + i * 16 + corner * 4, 4, slot.palettes[i].alpha[corner]);
    return Status::Ok;
}

Status decodeSlots(Version version, const Header& header, const std::uint8_t* bytes,
                   std::size_t size, std::vector<Slot>& out)
{
    std::size_t count = 0, expected = 0;
    const Status shape = calculateSize(version, header, count, expected);
    if (shape != Status::Ok) return shape;
    if (size != expected) return Status::BadSize;
    if (expected != 0 && !bytes) return Status::NullData;
    std::vector<Slot> decoded(count);
    const std::size_t stride = slotSize(version);
    for (std::size_t i = 0; i < count; ++i)
    {
        const Status result = decodeSlot(version, bytes + i * stride, stride, decoded[i]);
        if (result != Status::Ok) return result;
        const Status ref = checkModelReferences(decoded[i], header.objectCount);
        if (ref != Status::Ok) return ref;
    }
    out.swap(decoded);
    return Status::Ok;
}

Status encodeSlots(Version version, const Header& header, const std::vector<Slot>& slots,
                   std::vector<std::uint8_t>& out)
{
    std::size_t count = 0, expected = 0;
    const Status shape = calculateSize(version, header, count, expected);
    if (shape != Status::Ok) return shape;
    if (count != slots.size()) return Status::BadSize;
    std::vector<std::uint8_t> encoded(expected);
    const std::size_t stride = slotSize(version);
    for (std::size_t i = 0; i < count; ++i)
    {
        const Status ref = checkModelReferences(slots[i], header.objectCount);
        if (ref != Status::Ok) return ref;
        const Status result = encodeSlot(version, slots[i], encoded.data() + i * stride, stride);
        if (result != Status::Ok) return result;
    }
    out.swap(encoded);
    return Status::Ok;
}

const char* statusMessage(Status status) noexcept
{
    switch (status)
    {
    case Status::Ok: return "ok";
    case Status::NullData: return "null data";
    case Status::BadSize: return "incorrect buffer or slot count";
    case Status::UnsupportedVersion: return "unsupported detail slot version";
    case Status::OutOfRange: return "detail value out of range";
    case Status::UnrepresentableId: return "detail ID cannot be stored in this version";
    case Status::ReservedBitsSet: return "reserved detail bits are not zero";
    case Status::InvalidModelCount: return "too many models for detail v4";
    case Status::InvalidModelReference: return "detail ID exceeds available model count";
    case Status::SizeOverflow: return "detail grid size overflow";
    }
    return "unknown error";
}

} // namespace ixray::detail_format
