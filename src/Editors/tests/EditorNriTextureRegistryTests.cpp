#include "../../Layers/xrRenderTiramisu/Editor/TiramisuEditorNriTextureRegistry.h"

#include <cstdlib>
#include <iostream>

namespace
{
int Fail(const char* Message)
{
	std::cerr << Message << '\n';
	return EXIT_FAILURE;
}
} // namespace

int main()
{
	TiramisuEditorNriTextureRegistry Registry;
	int FirstDescriptor = 0;
	int SecondDescriptor = 0;
	const auto FirstId = reinterpret_cast<std::uintptr_t>(&FirstDescriptor);
	const auto SecondId = reinterpret_cast<std::uintptr_t>(&SecondDescriptor);

	if (Registry.Contains(0) || Registry.Contains(FirstId))
	{
		return Fail("An empty NRI texture registry accepted an unowned descriptor");
	}

	Registry.Register(nullptr);
	Registry.Register(&FirstDescriptor);
	Registry.Register(&FirstDescriptor);
	if (!Registry.Contains(FirstId) || Registry.Contains(SecondId))
	{
		return Fail("The NRI texture registry did not isolate registered descriptors");
	}

	Registry.Unregister(&FirstDescriptor);
	if (Registry.Contains(FirstId))
	{
		return Fail("An unregistered NRI descriptor remained valid");
	}

	Registry.Register(&FirstDescriptor);
	Registry.Register(&SecondDescriptor);
	Registry.Clear();
	if (Registry.Contains(FirstId) || Registry.Contains(SecondId))
	{
		return Fail("Clearing the NRI texture registry left stale descriptors");
	}

	return EXIT_SUCCESS;
}
