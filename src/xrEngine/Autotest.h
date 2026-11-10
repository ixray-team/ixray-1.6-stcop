#pragma once

namespace Autotest
{
	ENGINE_API bool	Active();
	ENGINE_API void	FrameBegin();
	ENGINE_API void	FrameEnd();
	ENGINE_API int	Verdict();
	ENGINE_API void	RequestFrame(const char* Path, float Aspect);
	ENGINE_API s32	FrameStatus();
	ENGINE_API void	CaptureFrame();
	ENGINE_API bool	OpenVideo(const char* Path, double Rate);
	ENGINE_API u32	CloseVideo();
}
