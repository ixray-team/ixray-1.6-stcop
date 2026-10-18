#pragma once

// Portable, explicitly little-endian codec for X-Ray detail slot payloads.
// Independent of xrCore types, stdafx, DirectX and the engine's compiler ABI.
#include <array>
#include <cstddef>
#include <cstdint>
#include <vector>

// xrCore is a DLL in the Windows SDK. Expose these functions to LevelEditor and
// the lighting compiler without pulling engine headers into the codec API.
#if defined(_WIN32) && !defined(IXRAY_DETAIL_CODEC_STANDALONE)
#    if defined(XRCORE_EXPORTS)
#        define IXR_DETAIL_CODEC_API __declspec(dllexport)
#    else
#        define IXR_DETAIL_CODEC_API __declspec(dllimport)
#    endif
#else
#    define IXR_DETAIL_CODEC_API
#endif

namespace ixray::detail_format
{

enum class Version : std::uint32_t
{
    V3 = 3,
    V4 = 4
};

enum class Status
{
    Ok,
    NullData,
    BadSize,
    UnsupportedVersion,
    OutOfRange,
    UnrepresentableId,
    ReservedBitsSet,
    InvalidModelCount,
    InvalidModelReference,
    SizeOverflow
};

constexpr std::size_t HeaderSize = 24;
constexpr std::size_t SlotSizeV3 = 16;
constexpr std::size_t SlotSizeV4 = 20;
constexpr std::uint16_t EmptyId = 0xffff; // Internal only; never written directly.
constexpr std::uint16_t DiskEmptyV3 = 0x003f;
constexpr std::uint16_t DiskEmptyV4 = 0x07ff;
constexpr std::uint32_t MaxModelsV4 = 2047; // Model IDs 0..2046; 2047 is empty.

struct Header
{
    std::uint32_t version = 0;
    std::uint32_t objectCount = 0;
    std::int32_t offsX = 0;
    std::int32_t offsZ = 0;
    std::uint32_t sizeX = 0;
    std::uint32_t sizeZ = 0;
};

struct Palette
{
    std::array<std::uint8_t, 4> alpha{};
};

struct Slot
{
    std::uint16_t yBase = 0;             // Stored as an unsigned 12-bit integer.
    std::uint8_t yHeight = 0;            // Stored as an unsigned 8-bit integer.
    std::array<std::uint16_t, 4> ids{EmptyId, EmptyId, EmptyId, EmptyId};
    std::uint8_t cDir = 0;
    std::uint8_t cHemi = 0;
    std::uint8_t cR = 0;
    std::uint8_t cG = 0;
    std::uint8_t cB = 0;
    std::array<Palette, 4> palettes{};
};

// Header is the 24-byte payload of chunk #0 of build.details/level.details.
// detail_object.part has its OWN DETMGR_VERSION in chunk #0x1000, and must
// not use header.version to detect its source-slot representation.
IXR_DETAIL_CODEC_API Status decodeHeader(const std::uint8_t* bytes, std::size_t size, Header& out) noexcept;
IXR_DETAIL_CODEC_API Status encodeHeader(const Header& header, std::uint8_t* bytes, std::size_t size) noexcept;

IXR_DETAIL_CODEC_API Status decodeSlot(Version version, const std::uint8_t* bytes, std::size_t size, Slot& out) noexcept;
IXR_DETAIL_CODEC_API Status encodeSlot(Version version, const Slot& slot, std::uint8_t* bytes, std::size_t size) noexcept;

// Validate and encode/decode the payload of slot chunk #2 (without chunk framing).
// The full chunk must contain exactly sizeX * sizeZ slots; output is only
// replaced on successful decoding/encoding.
IXR_DETAIL_CODEC_API Status decodeSlots(Version version, const Header& header, const std::uint8_t* bytes,
                   std::size_t size, std::vector<Slot>& out);
IXR_DETAIL_CODEC_API Status encodeSlots(Version version, const Header& header, const std::vector<Slot>& slots,
                   std::vector<std::uint8_t>& out);

IXR_DETAIL_CODEC_API const char* statusMessage(Status status) noexcept;

} // namespace ixray::detail_format

#undef IXR_DETAIL_CODEC_API
