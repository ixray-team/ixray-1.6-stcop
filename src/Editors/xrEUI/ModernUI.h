#pragma once

namespace XRay::ImGui
{
	XREUI_API bool InputVector3(const char* Label, float V[3], float Step);
	XREUI_API bool TumblerButton(const char* Label, bool& State, ImVec2 Size = {0, 0});
	XREUI_API bool ToggleFlagButton(const char* Label, uint32_t* Flags, uint32_t Mask, const ImVec2& Size);
}