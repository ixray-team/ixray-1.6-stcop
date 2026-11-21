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

std::uint32_t read32(const std::uint8_t* p)
{
    return std::uint32_t(p[0]) | (std::uint32_t(p[1]) << 8) |
           (std::uint32_t(p[2]) << 16) | (std::uint32_t(p[3]) << 24);
}

bool fail(const std::string& reason)
{
    std::cerr << "FAIL: " << reason << '\n';
    return false;
}

bool requiredChunks(const std::vector<std::uint8_t>& bytes, std::array<Chunk, 3>& chunks)
{
    std::size_t cursor = 0;
    while (cursor < bytes.size())
    {
        if (bytes.size() - cursor < 8)
            return fail("truncated outer chunk header");
        const std::uint32_t rawId = read32(bytes.data() + cursor);
        const std::size_t length = read32(bytes.data() + cursor + 4);
        cursor += 8;
        if (length > bytes.size() - cursor)
            return fail("outer chunk exceeds file length");
        if ((rawId & 0x7fffffffU) <= 2U)
        {
            const std::uint32_t id = rawId & 0x7fffffffU;
            if (rawId != id)
                return fail("compressed required chunk is not supported by this fixture test");
            if (chunks[id].found)
                return fail("duplicate required chunk");
            chunks[id] = {cursor, length, true};
        }
        cursor += length;
    }
    for (std::size_t i = 0; i < chunks.size(); ++i)
        if (!chunks[i].found)
            return fail("missing required chunk " + std::to_string(i));
    return true;
}

bool numberedModelChunks(const std::vector<std::uint8_t>& bytes, const Chunk& models,
                         std::uint32_t modelCount)
{
    std::size_t cursor = models.begin;
    const std::size_t end = cursor + models.length;
    for (std::uint32_t id = 0; id < modelCount; ++id)
    {
        if (end - cursor < 8)
            return fail("missing model subchunk " + std::to_string(id));
        const std::uint32_t modelId = read32(bytes.data() + cursor);
        const std::size_t length = read32(bytes.data() + cursor + 4);
        cursor += 8;
        if (modelId != id)
            return fail("non-sequential or compressed model subchunk");
        if (length > end - cursor)
            return fail("model subchunk exceeds model section");
        cursor += length;
    }
    return cursor == end || fail("extra data in model section");
}

bool check(const std::string& path)
{
    std::ifstream in(path, std::ios::binary);
    if (!in)
        return fail("cannot open v4 fixture: " + path);
    const std::vector<std::uint8_t> bytes{std::istreambuf_iterator<char>(in),
                                           std::istreambuf_iterator<char>()};
    if (bytes.empty() || in.bad())
        return fail("empty or unreadable v4 fixture");

    std::array<Chunk, 3> chunks{};
    if (!requiredChunks(bytes, chunks))
        return false;

    Header header;
    const Status headerResult = decodeHeader(bytes.data() + chunks[0].begin,
                                             chunks[0].length, header);
    if (headerResult != Status::Ok)
        return fail(std::string("header: ") + statusMessage(headerResult));
    if (header.version != 4 || header.objectCount < 1025 || header.objectCount > MaxModelsV4)
        return fail("expected Detail v4 and at least 1025 (at most 2047) models");
    if (!numberedModelChunks(bytes, chunks[1], header.objectCount))
        return false;

    std::vector<Slot> decoded;
    const std::uint8_t* payload = bytes.data() + chunks[2].begin;
    const Status slotResult = decodeSlots(Version::V4, header, payload,
                                           chunks[2].length, decoded);
    if (slotResult != Status::Ok)
        return fail(std::string("slots: ") + statusMessage(slotResult));

    std::size_t highReferences = 0;
    std::size_t occupiedReferences = 0;
    std::uint16_t maxReferencedId = 0;
    for (const Slot& slot : decoded)
    {
        const RuntimeSlot runtime = toRuntimeSlot(slot);
        for (std::uint32_t i = 0; i < 4; ++i)
        {
            const std::uint16_t id = slot.ids[i];
            if (runtime.r_id(i) != id)
                return fail("runtime conversion changed model ID");
            if (id == EmptyId)
                continue;
            ++occupiedReferences;
            if (id >= 1024)
                ++highReferences;
            maxReferencedId = std::max(maxReferencedId, id);
        }
    }
    if (highReferences == 0 || maxReferencedId < 1024)
        return fail("fixture contains no actually referenced high model IDs");

    std::vector<std::uint8_t> rebuilt;
    const Status reencode = encodeSlots(Version::V4, header, decoded, rebuilt);
    if (reencode != Status::Ok)
        return fail(std::string("re-encode: ") + statusMessage(reencode));
    if (rebuilt.size() != chunks[2].length ||
        !std::equal(rebuilt.begin(), rebuilt.end(), payload))
        return fail("v4 encode/decode is not byte-exact");

    std::cout << "PASS: real Detail v4 high-ID fixture\n"
              << "models=" << header.objectCount
              << " grid=" << header.sizeX << 'x' << header.sizeZ
              << " slots=" << decoded.size() << '\n'
              << "max referenced ID=" << maxReferencedId
              << " references ID>=1024=" << highReferences
              << " occupied references=" << occupiedReferences << '\n'
              << "model subchunks=PASS; runtime high IDs=PASS; "
                 "byte-exact v4 slot roundtrip=PASS\n";
    return true;
}
} // namespace

int main(int argc, char* argv[])
{
    if (argc != 2)
    {
        std::cerr << "Usage: detail_format_real_v4_tests <high_ID_level.details>\n";
        return 2;
    }
    return check(argv[1]) ? 0 : 1;
}
