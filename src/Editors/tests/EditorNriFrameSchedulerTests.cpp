#include "../../Layers/xrRenderTiramisu/Editor/TiramisuEditorNriFrameScheduler.h"

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
	if (MakeEditorNriFramePlan(0, 0, 4) ||
		MakeEditorNriFramePlan(0, 3, 0))
	{
		return Fail("Invalid NRI editor frame counts were accepted");
	}

	for (u64 Frame = 0; Frame < 12; ++Frame)
	{
		const auto Plan = MakeEditorNriFramePlan(Frame, 3, 4);
		if (!Plan)
			return Fail("A valid NRI editor frame plan was rejected");
		if (Plan->FrameContextIndex != Frame % 3 ||
			Plan->RecycledSemaphoreIndex != Frame % 4)
		{
			return Fail("NRI editor frame or semaphore rotation is incorrect");
		}
		const u64 ExpectedReuse = Frame >= 3 ? 1 + Frame - 3 : 0;
		if (Plan->ReuseFenceValue != ExpectedReuse)
			return Fail("NRI editor frame context can be reused before its fence");
		if (Plan->SignalFenceValue != Frame + 1)
			return Fail("NRI editor timeline fence signal is not monotonic");
	}

	return EXIT_SUCCESS;
}
