// Optional integration test for a real X-Ray level.details v3 file.
// The fixture remains outside Git. Run with its full path as argv[1].
#include "../../src/xrCore/DetailFormatCodec.h"
#include "../../src/xrCore/DetailRuntimeSlot.h"

#include <algorithm>
#include <array>
#include <cstddef>
#include <cstdint>
#include <fstream>
#include <iostream>
#include <iterator>
#include <string>
#include <vector>

using namespace ixray::detail_format;

namespace
{
struct Chunk
{
    std::size_t begin = 0;
    std::size_t length = 0;
    bool found = false;
};

std::uint32_t le32(const std::uint8_t* p)
{
    return std::uint32_t(p[0]) | (std::uint32_t(p[1]) << 8) |
           (std::uint32_t(p[2]) << 16) | (std::uint32_t(p[3]) << 24);
}

std::uint64_t fingerprint(const std::vector<std::uint8_t>& bytes)
{
    std::uint64_t hash = UINT64_C(0xcbf29ce484222325);
    for (std::uint8_t b : bytes)
    {
        hash ^= b;
        hash *= UINT64_C(0x100000001b3);
    }
    return hash;
}

bool error(const std::string& message)
{
    std::cerr << "FAIL: " << message << '\n';
    return false;
}

bool parseContainer(const std::vector<std::uint8_t>& bytes, std::array<Chunk, 3>& chunks)
{
    std::size_t pos = 0;
    while (pos < bytes.size())
    {
        if (bytes.size() - pos < 8)
            return error("truncated outer chunk header");
        const std::uint32_t id = le32(bytes.data() + pos);
        const std::uint32_t length = le32(bytes.data() + pos + 4);
        pos += 8;
        if (std::size_t(length) > bytes.size() - pos)
            return error("outer chunk exceeds file size");
        if (id <= 2)
        {
            if (chunks[id].found)
                return error("duplicate required chunk");
            chunks[id] = {pos, length, true};
        }
        pos += length;
    }
    for (std::size_t i = 0; i < chunks.size(); ++i)
        if (!chunks[i].found)
            return error("missing required chunk " + std::to_string(i));
    return true;
}

bool checkModels(const std::vector<std::uint8_t>& bytes, const Chunk& models, std::uint32_t count)
{
    std::size_t pos = models.begin;
    const std::size_t end = models.begin + models.length;
    for (std::uint32_t id = 0; id < count; ++id)
    {
        if (end - pos < 8)
            return error("missing/truncated model chunk header: " + std::to_string(id));
        const std::uint32_t modelId = le32(bytes.data() + pos);
        const std::uint32_t length = le32(bytes.data() + pos + 4);
        pos += 8;
        if (modelId != id)
            return error("unexpected model chunk id");
        if (std::size_t(length) > end - pos)
            return error("model chunk exceeds objects payload");
        pos += length;
    }
    if (pos != end)
        return error("extra model chunk data");
    return true;
}

bool sameSlot(const Slot& slot, const RuntimeSlot& runtime)
{
    if (slot.yBase != runtime.y_base || slot.yHeight != runtime.y_height ||
        slot.cDir != runtime.c_dir || slot.cHemi != runtime.c_hemi ||
        slot.cR != runtime.c_r || slot.cG != runtime.c_g || slot.cB != runtime.c_b)
        return false;
    for (std::size_t i = 0; i < 4; ++i)
    {
        if (slot.ids[i] != runtime.r_id(static_cast<std::uint32_t>(i)))
            return false;
        if (slot.palettes[i].alpha[0] != runtime.palette[i].a0 ||
            slot.palettes[i].alpha[1] != runtime.palette[i].a1 ||
            slot.palettes[i].alpha[2] != runtime.palette[i].a2 ||
            slot.palettes[i].alpha[3] != runtime.palette[i].a3)
            return false;
    }
    return true;
}

bool checkFile(const std::string& filename)
{
    std::ifstream in(filename, std::ios::binary);
    if (!in)
        return error("cannot open fixture: " + filename);
    const std::vector<std::uint8_t> bytes{std::istreambuf_iterator<char>(in),
                                           std::istreambuf_iterator<char>()};
    if (in.bad() || bytes.empty())
        return error("fixture read failed or empty");

    std::array<Chunk, 3> chunks{};
    if (!parseContainer(bytes, chunks))
        return false;
    Header header;
    const Status hs = decodeHeader(bytes.data() + chunks[0].begin, chunks[0].length, header);
    if (hs != Status::Ok)
        return error(std::string("decodeHeader: ") + statusMessage(hs));
    if (header.version != 3)
        return error("this test expects a real v3 level.details file");
    if (!checkModels(bytes, chunks[1], header.objectCount))
        return false;

    const std::uint8_t* payload = bytes.data() + chunks[2].begin;
    const std::size_t size = chunks[2].length;
    std::vector<Slot> slots;
    const Status ds = decodeSlots(Version::V3, header, payload, size, slots);
    if (ds != Status::Ok)
        return error(std::string("decodeSlots v3: ") + statusMessage(ds));

    std::array<std::size_t, 5> slotsByIds{};
    std::size_t occupiedPositions = 0;
    for (const Slot& slot : slots)
    {
        std::size_t occupied = 0;
        for (std::uint16_t id : slot.ids)
            if (id != EmptyId)
                ++occupied;
        ++slotsByIds[occupied];
        occupiedPositions += occupied;
        if (!sameSlot(slot, toRuntimeSlot(slot)))
            return error("toRuntimeSlot lost a value");
    }

    // This known original fixture checks independent reference statistics as well
    // as pure codec roundtrip. Other real v3 files use the generic checks below.
    const bool knownFixture = fingerprint(bytes) == UINT64_C(0x575d1ef27f27ff46);
    if (knownFixture)
    {
        const std::array<std::size_t, 5> expected = {96424, 34610, 42303, 101260, 167628};
        if (header.objectCount != 16 || header.sizeX != 665 || header.sizeZ != 665 ||
            slotsByIds != expected || occupiedPositions != 1093508)
            return error("known fixture's independently measured stats changed");
    }

    std::vector<std::uint8_t> rebuiltV3;
    const Status es = encodeSlots(Version::V3, header, slots, rebuiltV3);
    if (es != Status::Ok)
        return error(std::string("encodeSlots v3: ") + statusMessage(es));
    if (rebuiltV3.size() != size ||
        !std::equal(rebuiltV3.begin(), rebuiltV3.end(), payload))
        return error("v3 slot payload differs after decode/encode");

    Header v4Header = header;
    v4Header.version = 4;
    std::vector<std::uint8_t> encodedV4;
    const Status v4Write = encodeSlots(Version::V4, v4Header, slots, encodedV4);
    if (v4Write != Status::Ok)
        return error(std::string("encodeSlots v4: ") + statusMessage(v4Write));
    std::vector<Slot> roundtripSlots;
    const Status v4Read = decodeSlots(Version::V4, v4Header,
                                      encodedV4.data(), encodedV4.size(), roundtripSlots);
    if (v4Read != Status::Ok)
        return error(std::string("decodeSlots v4: ") + statusMessage(v4Read));
    std::vector<std::uint8_t> backToV3;
    const Status back = encodeSlots(Version::V3, header, roundtripSlots, backToV3);
    if (back != Status::Ok || backToV3 != rebuiltV3)
        return error("v3 -> v4 -> v3 changed slot bytes");

    std::cout << "PASS: real level.details v3\n"
              << "models=" << header.objectCount << " grid=" << header.sizeX << 'x' << header.sizeZ
              << " slots=" << slots.size() << " occupied=" << occupiedPositions << '\n'
              << "slots with 0/1/2/3/4 models=";
    for (std::size_t i = 0; i < slotsByIds.size(); ++i)
        std::cout << (i ? "/" : "") << slotsByIds[i];
    std::cout << "\nbyte-exact v3 roundtrip=PASS; v3 -> v4 -> v3=PASS; runtime conversion=PASS"
              << "\nknown original fixture=" << (knownFixture ? "YES" : "NO") << '\n';
    return true;
}
} // namespace

int main(int argc, char* argv[])
{
    if (argc != 2)
    {
        std::cerr << "Usage: detail_format_real_v3_tests <full_path_to_level.details>\n";
        return 2;
    }
    return checkFile(argv[1]) ? 0 : 1;
}
