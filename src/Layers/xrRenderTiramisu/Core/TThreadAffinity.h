#pragma once

#include "TiramisuRenderTypes.h"

#include <cstddef>

namespace Tiramisu::Threading
{
enum class EThreadRole
{
	Game,
	Render
};

constexpr bool IsThreadRoleSatisfied(const EThreadRole Role, const bool RenderThreadRunning, const size_t CurrentThreadId, const size_t GameThreadId, const size_t RenderThreadId) noexcept
{
	if (Role == EThreadRole::Game)
	{
		return CurrentThreadId == GameThreadId;
	}

	if (Role == EThreadRole::Render)
	{
		return CurrentThreadId == (RenderThreadRunning ? RenderThreadId : GameThreadId);
	}

	return false;
}
} // namespace Tiramisu::Threading
