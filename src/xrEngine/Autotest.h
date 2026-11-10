#pragma once

namespace Autotest
{
	ENGINE_API bool	Active();
	ENGINE_API void	FrameBegin();
	ENGINE_API void	FrameEnd();
	ENGINE_API int	Verdict();
}
