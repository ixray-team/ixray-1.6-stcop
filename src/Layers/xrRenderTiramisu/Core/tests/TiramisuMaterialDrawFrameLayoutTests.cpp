#include "Resources/Materials/TiramisuMaterialDrawFrameLayout.h"

#include <iostream>

namespace
{
int Fail(const char* Message)
{
	std::cerr << Message << '\n';
	return 1;
}
} // namespace

int main(int ArgumentCount, char** Arguments)
{
	if (ArgumentCount < 2 || xr_string_view(Arguments[1]) != "-rdbg")
	{
		return Fail("Тест должен запускаться с точным аргументом -rdbg");
	}

	using FLayout = TiramisuMaterialDrawFrameLayout;
	if (FLayout::GetAbsoluteDrawIndex(0, 0) != 0 ||
		FLayout::GetAbsoluteDrawIndex(1, 0) !=
			FLayout::MaxDrawsPerFrame ||
		FLayout::GetAbsoluteDrawIndex(2, 17) !=
			2 * FLayout::MaxDrawsPerFrame + 17)
	{
		return Fail("Frame-local draw index преобразован неверно");
	}

	const u64 ExpectedSize =
		u64(FLayout::BufferedFrameCount) *
		FLayout::MaxDrawsPerFrame * MaterialDrawGpuDataSize;
	if (FLayout::BufferSize != ExpectedSize)
	{
		return Fail("Размер buffered draw table не совпадает с ABI");
	}
	return 0;
}
