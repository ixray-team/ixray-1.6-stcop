#pragma once
struct ImDrawData;

namespace RHIUtils::ImGui
{
	RHI_API void Init();
	RHI_API void NewFrame();
	RHI_API void DrawData();
	RHI_API void Destroy();
	RHI_API void Reset();
	RHI_API void* GetBlenderState();
}