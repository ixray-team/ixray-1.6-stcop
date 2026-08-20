#include "Resources/TiramisuDescriptorHeapLayout.h"

#include <iostream>

namespace
{
int Fail(const char* Message)
{
	std::cerr << Message << '\n';
	return 1;
}
} // namespace

int main()
{
	const nri::DescriptorRangeBits RangeFlags =
		TiramisuDescriptorHeapLayout::ResourceRangeFlags;
	if (!(RangeFlags & nri::DescriptorRangeBits::ARRAY) ||
		!(RangeFlags & nri::DescriptorRangeBits::PARTIALLY_BOUND) ||
		!(RangeFlags &
			nri::DescriptorRangeBits::ALLOW_UPDATE_AFTER_SET))
	{
		return Fail("Bindless resource range lost required indexing flags");
	}
	if (!(TiramisuDescriptorHeapLayout::ResourceSetFlags &
		nri::DescriptorSetBits::ALLOW_UPDATE_AFTER_SET))
	{
		return Fail("Bindless descriptor set must permit update after bind");
	}
	if (!(TiramisuDescriptorHeapLayout::PoolFlags &
		nri::DescriptorPoolBits::ALLOW_UPDATE_AFTER_SET))
	{
		return Fail("Bindless descriptor pool must permit update after bind");
	}
	return 0;
}
