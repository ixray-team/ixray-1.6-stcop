#include "stdafx.h"
#include "imgui_internal.h"

#ifdef IXR_WINDOWS
#define SystemOpenURL(url) std::system((xr_string("start ") + url).c_str())
#elif defined(IXR_APPLE_SERIES)
#define SystemOpenURL(url) std::system((xr_string("open ") + url).c_str())
#elif defined(IXR_LINUX)
#define SystemOpenURL(url) std::system((xr_string("xdg-open ") + url).c_str())
#else
#error "Unknown compiler"
#endif

namespace ImGui
{
	XREUI_API bool HyperLink(const char* label, const char* url, bool underlineWhenHoveredOnly)
	{
		const ImU32 linkColor = ImGui::ColorConvertFloat4ToU32({ 0.2, 0.3, 0.8, 1 });
		const ImU32 linkHoverColor = ImGui::ColorConvertFloat4ToU32({ 0.4, 0.6, 0.8, 1 });

		const ImGuiID id = ImGui::GetID(label);

		const auto window = ImGui::GetCurrentWindow();
		ImDrawList* const draw = ImGui::GetWindowDrawList();

		const ImVec2 pos(window->DC.CursorPos.x, window->DC.CursorPos.y + window->DC.CurrLineTextBaseOffset);
		const ImVec2 size = ImGui::CalcTextSize(label);
		ImRect bb(pos, { pos.x + size.x, pos.y + size.y });

		ImGui::ItemSize(bb, 0.0f);
		if (!ImGui::ItemAdd(bb, id))
			return false;

		bool isHovered = false;
		const bool isClicked = ImGui::ButtonBehavior(bb, id, &isHovered, nullptr);

		const ImU32 color = isHovered ? linkHoverColor : linkColor;

		draw->AddText(bb.Min, color, label);

		if (!underlineWhenHoveredOnly || isHovered)
			draw->AddLine({ bb.Min.x, bb.Max.y }, bb.Max, color);

		if (isClicked)
		{
			SystemOpenURL(url);
		}

		return isClicked;
	}
}