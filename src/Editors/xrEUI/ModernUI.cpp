#include "stdafx.h"
#include "ModernUI.h"

XREUI_API bool XRay::ImGui::InputVector3(const char* Label, float V[3], float Step)
{
	bool Changed = false;

	::ImGui::PushID(Label);
	::ImGui::BeginGroup();

	static const ImVec4 AxisColors[3] =
	{
		{0.30f, 0.50f, 0.90f, 1.0f},
		{0.30f, 0.90f, 0.30f, 1.0f},
		{0.90f, 0.30f, 0.30f, 1.0f}
	};

	static const char* Ids[3] = { "##x", "##y", "##z" };
	constexpr float StripeWidth = 4.0f;
	constexpr float Rounding = 4.0f;

	float TotalWidth = ::ImGui::GetContentRegionAvail().x;
	float InputWidth = (TotalWidth - 2.0f * 0.0f - 3 * StripeWidth) / 3.0f;

	for (int i = 0; i < 3; ++i)
	{
		if (i > 0)
		{
			::ImGui::SameLine(0.0f, 0.0f);
		}

		::ImGui::SetNextItemWidth(InputWidth);
		::ImGui::PushStyleVar(ImGuiStyleVar_FramePadding, ImVec2(4, 4));

		Changed |= ::ImGui::DragFloat(Ids[i], &V[i], Step, 0, 0, "%.3f");

		::ImGui::PopStyleVar();

		ImDrawList* DrawList = ::ImGui::GetWindowDrawList();
		ImVec2 Min = ::ImGui::GetItemRectMin();
		ImVec2 Max = ::ImGui::GetItemRectMax();

		DrawList->AddRectFilled
		(
			Min,
			ImVec2(Min.x + StripeWidth, Max.y),
			::ImGui::ColorConvertFloat4ToU32(AxisColors[i]),
			Rounding,
			ImDrawFlags_RoundCornersLeft
		);
	}

	::ImGui::EndGroup();
	::ImGui::PopID();

	return Changed;
}
