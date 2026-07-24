#include "TiramisuRenderAdapterSelection.h"

#include <tuple>

namespace
{
u8 ApiBit(const ETiramisuGraphicsApi Api) noexcept
{
    return static_cast<u8>(Api);
}

auto AdapterScore(const FTiramisuAdapterCandidate& Candidate) noexcept
{
    return std::tuple{
        static_cast<u8>(Candidate.Kind),
        Candidate.DedicatedVideoMemory,
        Candidate.SharedSystemMemory,
        Candidate.ComputeQueueCount != 0,
        Candidate.CopyQueueCount != 0};
}
} // namespace

xr_optional<size_t> SelectBestTiramisuAdapter(
    const xr_span<const FTiramisuAdapterCandidate> Candidates,
    const ETiramisuGraphicsApi RequiredApi) noexcept
{
    xr_optional<size_t> Best;
    for (size_t Index = 0; Index < Candidates.size(); ++Index)
    {
        const FTiramisuAdapterCandidate& Candidate = Candidates[Index];
        if ((Candidate.SupportedApis & ApiBit(RequiredApi)) == 0 ||
            Candidate.GraphicsQueueCount == 0)
        {
            continue;
        }

        if (!Best || AdapterScore(Candidate) > AdapterScore(Candidates[*Best]))
            Best = Index;
    }
    return Best;
}
