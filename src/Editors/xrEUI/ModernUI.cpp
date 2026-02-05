#include "stdafx.h"
#include "ModernUI.h"

XREUI_API bool XRay::ImGui::ToggleFlagButton(const char* Label, uint32_t* Flags, uint32_t Mask, const ImVec2& Size)
{
	bool Enabled = (*Flags & Mask) != 0;
	bool Changed = false;

	::ImGui::PushID(Label);

	const ImVec4 EnabledColor = ImVec4(0.20f, 0.60f, 1.00f, 1.00f);
	const ImVec4 DisabledColor = ImVec4(0.25f, 0.25f, 0.25f, 1.00f);
	const ImVec4 BorderColor = Enabled
		? ImVec4(0.20f, 0.60f, 1.00f, 1.00f)
		: ImVec4(0.40f, 0.40f, 0.40f, 1.00f);

	constexpr float StripeWidth = 3.0f;
	constexpr float Rounding = 5.0f;

	::ImGui::PushStyleVar(ImGuiStyleVar_FrameRounding, Rounding);
	::ImGui::PushStyleColor(ImGuiCol_Button, Enabled ? EnabledColor : DisabledColor);
	::ImGui::PushStyleColor(ImGuiCol_ButtonHovered, Enabled ? EnabledColor : DisabledColor);
	::ImGui::PushStyleColor(ImGuiCol_ButtonActive, Enabled ? EnabledColor : DisabledColor);

	const char* Text = Enabled ? "Enable" : "Disable";

	if (::ImGui::Button(Text, Size))
	{
		*Flags ^= Mask;
		Changed = true;
		Enabled = !Enabled;
	}

	::ImGui::PopStyleColor(3);
	::ImGui::PopStyleVar();

	// Рисуем левую оконтовку
	ImDrawList* DrawList = ::ImGui::GetWindowDrawList();
	ImVec2 Min = ::ImGui::GetItemRectMin();
	ImVec2 Max = ::ImGui::GetItemRectMax();

	Min.x += 1;
	DrawList->AddRectFilled(
		Min,
		ImVec2(Min.x + StripeWidth, Max.y),
		::ImGui::ColorConvertFloat4ToU32(BorderColor),
		Rounding,
		ImDrawFlags_RoundCornersLeft
	);

	::ImGui::PopID();
	return Changed;
}

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
	constexpr float StripeWidth = 2.0f;
	constexpr float Rounding = 4.0f;

	float TotalWidth = ::ImGui::GetContentRegionAvail().x;
	float InputWidth = (TotalWidth) / 3.0f;

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

XREUI_API bool XRay::ImGui::TumblerButton(const char* Label, bool& State, ImVec2 Size)
{
	bool OldState = State;
	if (OldState)
	{
		::ImGui::PushStyleColor(ImGuiCol_Button, ImVec4(0.20f, 0.60f, 1.00f, 1.00f));
		::ImGui::PushStyleColor(ImGuiCol_ButtonHovered, ImVec4(0.30f, 0.70f, 1.00f, 1.00f));
		::ImGui::PushStyleColor(ImGuiCol_ButtonActive, ImVec4(0.15f, 0.55f, 0.95f, 1.00f));
	}

	bool OutVal = ::ImGui::Button(Label, Size);
	if (OutVal)
	{
		State = !State;
	}

	if (OldState)
	{
		::ImGui::PopStyleColor(3);
	}

	return OutVal;
}
