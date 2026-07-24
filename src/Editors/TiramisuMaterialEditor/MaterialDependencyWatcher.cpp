#include "MaterialDependencyWatcher.h"

#include <array>
#include <fstream>

namespace Tiramisu::Editor
{
namespace
{
constexpr u64 FnvOffset = 1469598103934665603ull;
constexpr u64 FnvPrime = 1099511628211ull;
}

std::filesystem::path TiramisuMaterialDependencyWatcher::NormalizePath(
    const std::filesystem::path& Path)
{
    std::error_code Error;
    std::filesystem::path Result = std::filesystem::weakly_canonical(Path, Error);
    if (Error)
    {
        Error.clear();
        Result = std::filesystem::absolute(Path, Error);
    }
    if (Error)
        Result = Path;
    return Result.lexically_normal();
}

TiramisuMaterialDependencyWatcher::FSnapshot TiramisuMaterialDependencyWatcher::Capture(
    const std::filesystem::path& Path)
{
    FSnapshot Result;
    std::error_code Error;
    Result.Exists = std::filesystem::is_regular_file(Path, Error) && !Error;
    if (!Result.Exists)
        return Result;

    Result.Size = std::filesystem::file_size(Path, Error);
    if (Error)
        Result.Size = 0;
    Error.clear();
    Result.WriteTime = std::filesystem::last_write_time(Path, Error);
    if (Error)
        Result.WriteTime = {};

    std::ifstream Input(Path, std::ios::binary);
    if (!Input)
    {
        Result.Exists = false;
        Result.Size = 0;
        Result.WriteTime = {};
        return Result;
    }
    u64 Hash = FnvOffset;
    xr_array<char, 16 * 1024> Buffer{};
    while (Input)
    {
        Input.read(Buffer.data(), static_cast<std::streamsize>(Buffer.size()));
        const std::streamsize Count = Input.gcount();
        for (std::streamsize Index = 0; Index < Count; ++Index)
        {
            Hash ^= static_cast<unsigned char>(Buffer[static_cast<size_t>(Index)]);
            Hash *= FnvPrime;
        }
    }
    Result.ContentHash = Hash;
    return Result;
}

void TiramisuMaterialDependencyWatcher::Reset(
    const xr_span<const std::filesystem::path> Dependencies)
{
    Snapshots.clear();
    for (const std::filesystem::path& Dependency : Dependencies)
    {
        if (Dependency.empty())
            continue;
        const std::filesystem::path Path = NormalizePath(Dependency);
        Snapshots.insert_or_assign(Path, Capture(Path));
    }
}

xr_vector<FMaterialDependencyChange> TiramisuMaterialDependencyWatcher::Poll()
{
    xr_vector<FMaterialDependencyChange> Changes;
    for (auto& [Path, Previous] : Snapshots)
    {
        FSnapshot Current = Capture(Path);
        if (Current == Previous)
            continue;

        EMaterialDependencyChange Change = EMaterialDependencyChange::Modified;
        if (!Previous.Exists && Current.Exists)
            Change = EMaterialDependencyChange::Created;
        else if (Previous.Exists && !Current.Exists)
            Change = EMaterialDependencyChange::Removed;
        Changes.push_back({Path, Change});
        Previous = Current;
    }
    return Changes;
}
} // namespace Tiramisu::Editor
