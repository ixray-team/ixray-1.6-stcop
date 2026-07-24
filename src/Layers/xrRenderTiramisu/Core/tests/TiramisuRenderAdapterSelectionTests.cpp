#include "../TiramisuRenderAdapterSelection.h"

#include <array>
#include <iostream>

namespace
{
constexpr u8 ApiMask(const ETiramisuGraphicsApi Api)
{
	return static_cast<u8>(Api);
}

int Fail(const char* Message)
{
	std::cerr << Message << '\n';
	return 1;
}
} // namespace

int main()
{
	if (SelectBestTiramisuAdapter({}, ETiramisuGraphicsApi::Vulkan))
	{
		return Fail("An empty adapter list must not produce a selection");
	}

	const xr_array Incompatible = {
		FTiramisuAdapterCandidate{ApiMask(ETiramisuGraphicsApi::D3D12), ETiramisuAdapterKind::Discrete, 1, 8'000, 0},
		FTiramisuAdapterCandidate{ApiMask(ETiramisuGraphicsApi::Vulkan), ETiramisuAdapterKind::Discrete, 0, 16'000, 0}
	};
	if (SelectBestTiramisuAdapter(Incompatible, ETiramisuGraphicsApi::Vulkan))
	{
		return Fail("Adapters without the API or a graphics queue must be rejected");
	}

	const xr_array Candidates = {
		FTiramisuAdapterCandidate{ApiMask(ETiramisuGraphicsApi::Vulkan) | ApiMask(ETiramisuGraphicsApi::D3D12), ETiramisuAdapterKind::Integrated, 1, 32'000, 64'000},
		FTiramisuAdapterCandidate{ApiMask(ETiramisuGraphicsApi::Vulkan), ETiramisuAdapterKind::Discrete, 1, 8'000, 16'000},
		FTiramisuAdapterCandidate{ApiMask(ETiramisuGraphicsApi::D3D12), ETiramisuAdapterKind::Discrete, 1, 12'000, 8'000},
		FTiramisuAdapterCandidate{ApiMask(ETiramisuGraphicsApi::Vulkan), ETiramisuAdapterKind::Discrete, 1, 16'000, 8'000}
	};

	const xr_optional<size_t> Vulkan =
		SelectBestTiramisuAdapter(Candidates, ETiramisuGraphicsApi::Vulkan);
	if (!Vulkan || *Vulkan != 3)
	{
		return Fail("Vulkan selection must prefer a discrete adapter and then dedicated memory");
	}

	const xr_optional<size_t> D3D12 =
		SelectBestTiramisuAdapter(Candidates, ETiramisuGraphicsApi::D3D12);
	if (!D3D12 || *D3D12 != 2)
	{
		return Fail("D3D12 selection must ignore a Vulkan-only discrete adapter");
	}

	const xr_array StableTie = {
		FTiramisuAdapterCandidate{ApiMask(ETiramisuGraphicsApi::Vulkan), ETiramisuAdapterKind::Discrete, 1, 10'000, 5'000},
		FTiramisuAdapterCandidate{ApiMask(ETiramisuGraphicsApi::Vulkan), ETiramisuAdapterKind::Discrete, 1, 10'000, 5'000}
	};
	const xr_optional<size_t> Tie =
		SelectBestTiramisuAdapter(StableTie, ETiramisuGraphicsApi::Vulkan);
	if (!Tie || *Tie != 0)
	{
		return Fail("Equal adapters must keep enumeration order for deterministic selection");
	}

	const xr_array QueueTie = {
		FTiramisuAdapterCandidate{ApiMask(ETiramisuGraphicsApi::Vulkan), ETiramisuAdapterKind::Discrete, 1, 10'000, 5'000, 0, 0},
		FTiramisuAdapterCandidate{ApiMask(ETiramisuGraphicsApi::Vulkan), ETiramisuAdapterKind::Discrete, 1, 10'000, 5'000, 1, 1}
	};
	const xr_optional<size_t> QueuePreferred =
		SelectBestTiramisuAdapter(QueueTie, ETiramisuGraphicsApi::Vulkan);
	if (!QueuePreferred || *QueuePreferred != 1)
	{
		return Fail("An otherwise equal adapter with async compute/copy queues must be preferred");
	}

	return 0;
}
