#include "stdafx.h"
#include "xrParams.h"

#include <magic_enum/magic_enum.hpp>

template <>
struct magic_enum::customize::enum_range<ECoreParams> 
{
	static constexpr bool is_flags = true;
};

void LoadParams()
{
	xr_string CommandLine = Core.Params;
	auto CommandList = CommandLine.Split(' ');

	for (xr_string Command : CommandList)
	{
		if (!Command.starts_with("-"))
			continue;

		Command = Command.substr(1);

		auto EnumData = magic_enum::enum_cast<ECoreParams>(Command);

		if (EnumData.has_value())
		{
			Core.ParamsData.set((u64)EnumData.value(), true);
		}
	}
}