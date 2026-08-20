#pragma once

#include <NRI.h>

// DescriptorHeapIndexing требует обновлять bindless slots после записи
// command buffer. Vulkan разрешает это только при согласованных flags
// descriptor range, set layout и pool.
class TiramisuDescriptorHeapLayout final
{
public:
	static constexpr nri::DescriptorRangeBits ResourceRangeFlags =
		nri::DescriptorRangeBits::ARRAY |
		nri::DescriptorRangeBits::PARTIALLY_BOUND |
		nri::DescriptorRangeBits::ALLOW_UPDATE_AFTER_SET;
	static constexpr nri::DescriptorSetBits ResourceSetFlags =
		nri::DescriptorSetBits::ALLOW_UPDATE_AFTER_SET;
	static constexpr nri::DescriptorPoolBits PoolFlags =
		nri::DescriptorPoolBits::ALLOW_UPDATE_AFTER_SET;
};
