#pragma once

// In-memory representation used only by the game detail renderer.
// It must never be serialized with sizeof()/memcpy(): the disk formats are
// 16-byte v3 or 20-byte v4, encoded by DetailFormatCodec.
#include "DetailFormatCodec.h"

#include <cstddef>
#include <cstdint>

namespace ixray::detail_format
{

struct RuntimePalette
{
    std::uint8_t a0 = 0;
    std::uint8_t a1 = 0;
    std::uint8_t a2 = 0;
    std::uint8_t a3 = 0;
};

struct RuntimeSlot
{
    static constexpr std::uint16_t ID_Empty = EmptyId;

    std::uint16_t y_base = 0;
    std::uint8_t y_height = 0;
    std::uint16_t id0 = ID_Empty;
    std::uint16_t id1 = ID_Empty;
    std::uint16_t id2 = ID_Empty;
    std::uint16_t id3 = ID_Empty;
    std::uint8_t c_dir = 0;
    std::uint8_t c_hemi = 0;
    std::uint8_t c_r = 0;
    std::uint8_t c_g = 0;
    std::uint8_t c_b = 0;
    RuntimePalette palette[4]{};

    float r_ybase() const noexcept { return float(y_base) * .2f - 200.f; }
    float r_yheight() const noexcept { return float(y_height) * .1f; }
    float r_qclr(std::uint32_t value, std::uint32_t range) const noexcept
    {
        return float(value) / float(range);
    }
    std::uint16_t r_id(std::uint32_t idx) const noexcept
    {
        switch (idx)
        {
        case 0: return id0;
        case 1: return id1;
        case 2: return id2;
        case 3: return id3;
        default: return ID_Empty;
        }
    }
    void w_id(std::uint32_t idx, std::uint16_t value) noexcept
    {
        switch (idx)
        {
        case 0: id0 = value; break;
        case 1: id1 = value; break;
        case 2: id2 = value; break;
        case 3: id3 = value; break;
        default: break;
        }
    }
};

// Both v3 and v4 are decoded to Slot first; conversion is lossless and
// decouples the renderer from the legacy six-bit editor-only bitfields.
inline RuntimeSlot toRuntimeSlot(const Slot& slot) noexcept
{
    RuntimeSlot out;
    out.y_base = slot.yBase;
    out.y_height = slot.yHeight;
    out.id0 = slot.ids[0];
    out.id1 = slot.ids[1];
    out.id2 = slot.ids[2];
    out.id3 = slot.ids[3];
    out.c_dir = slot.cDir;
    out.c_hemi = slot.cHemi;
    out.c_r = slot.cR;
    out.c_g = slot.cG;
    out.c_b = slot.cB;
    for (std::size_t i = 0; i < 4; ++i)
    {
        out.palette[i].a0 = slot.palettes[i].alpha[0];
        out.palette[i].a1 = slot.palettes[i].alpha[1];
        out.palette[i].a2 = slot.palettes[i].alpha[2];
        out.palette[i].a3 = slot.palettes[i].alpha[3];
    }
    return out;
}

} // namespace ixray::detail_format
