#include "stdafx.h"
#include "IconsFontAwesome7.h"
#include "imgui_internal.h"
#include <SDL3/SDL.h>

static constexpr u32 ToastFadeMs = 250;
static constexpr u32 ToastMaxVisible = 5;

static ImVec4 ToastAccent(EToastType Type)
{
	switch (Type)
	{
	case EToastType::Success:
		return ImVec4(0.30f, 0.78f, 0.42f, 1.0f);
	case EToastType::Warning:
		return ImVec4(0.95f, 0.72f, 0.20f, 1.0f);
	case EToastType::Error:
		return ImVec4(0.90f, 0.30f, 0.28f, 1.0f);
	default:
		return ImVec4(0.30f, 0.60f, 0.95f, 1.0f);
	}
}

static const char* ToastIcon(EToastType Type)
{
	switch (Type)
	{
	case EToastType::Success:
		return ICON_FA_CIRCLE_CHECK;
	case EToastType::Warning:
		return ICON_FA_TRIANGLE_EXCLAMATION;
	case EToastType::Error:
		return ICON_FA_CIRCLE_XMARK;
	default:
		return ICON_FA_CIRCLE_INFO;
	}
}

static ImU32 ToastColor(ImVec4 Color, float Alpha)
{
	Color.w *= Alpha;
	return ImGui::ColorConvertFloat4ToU32(Color);
}

void UIToastQueue::Push(EToastType Type, const char* Title, const char* Text, u32 DurationMs)
{
	xrCriticalSectionGuard Guard(Lock);

	Toast& New = Toasts.emplace_back();
	New.Type = Type;
	New.Title = Title != nullptr ? Title : "";
	New.Text = Text != nullptr ? Text : "";
	New.DurationMs = std::max(DurationMs, ToastFadeMs * 2);
}

void UIToastQueue::Draw()
{
	xrCriticalSectionGuard Guard(Lock);
	const u64 Now = SDL_GetTicks();
	const u64 FrameMs = LastTicks != 0 ? Now - LastTicks : 0;
	LastTicks = Now;

	std::erase_if(Toasts, [Now](const Toast& T) { return T.StartTicks != 0 && Now >= T.StartTicks + T.DurationMs; });
	if (Toasts.empty())
	{
		return;
	}

	ImGuiViewport* Viewport = ImGui::GetMainViewport();
	ImFont* Font = ImGui::GetFont();
	const ImGuiStyle& Style = ImGui::GetStyle();

	const float FontSize = ImGui::GetFontSize();
	const float Padding = FontSize * 0.7f;
	const float Margin = FontSize;
	const float Width = FontSize * 24.0f;
	const float Rounding = FontSize * 0.35f;
	const float AccentWidth = FontSize * 0.25f;
	const float ProgressHeight = std::max(2.0f, FontSize * 0.12f);
	const float CloseSize = FontSize * 1.1f;
	// Room for the shadow inside the toast window
	const ImVec2 Shadow(2.0f, 3.0f);

	const float Right = Viewport->Pos.x + Viewport->Size.x - Margin;
	float Bottom = Viewport->Pos.y + Viewport->Size.y - Margin;

	const ImGuiWindowFlags Flags = ImGuiWindowFlags_NoDecoration | ImGuiWindowFlags_NoMove | ImGuiWindowFlags_NoSavedSettings | ImGuiWindowFlags_NoFocusOnAppearing | ImGuiWindowFlags_NoNav | ImGuiWindowFlags_NoDocking | ImGuiWindowFlags_NoBackground;

	// The newest toast is at the bottom, older ones move up
	u32 Shown = 0;
	for (auto It = Toasts.rbegin(); It != Toasts.rend() && Shown < ToastMaxVisible; ++It, ++Shown)
	{
		Toast& T = *It;
		if (T.StartTicks == 0)
		{
			T.StartTicks = Now;
		}

		const float Age = float(Now - T.StartTicks);
		const float Left = float(T.DurationMs) - Age;
		const float FadeIn = std::min(1.0f, Age / ToastFadeMs);
		const float Alpha = std::clamp(std::min(FadeIn, Left / ToastFadeMs), 0.0f, 1.0f);
		const float Slide = (1.0f - FadeIn) * (1.0f - FadeIn) * FontSize * 3.0f;

		const char* Icon = ToastIcon(T.Type);
		const ImVec2 IconSize = ImGui::CalcTextSize(Icon);
		const float TextX = AccentWidth + Padding + IconSize.x + Padding;
		const float WrapWidth = Width - TextX - CloseSize - Padding;
		const ImVec2 TitleSize = ImGui::CalcTextSize(T.Title.c_str(), nullptr, false, WrapWidth);
		const ImVec2 TextSize = T.Text.empty() ? ImVec2(0.0f, 0.0f) : ImGui::CalcTextSize(T.Text.c_str(), nullptr, false, WrapWidth);
		const float TextGap = T.Text.empty() ? 0.0f : FontSize * 0.3f;
		const float Height = Padding * 2.0f + std::max(IconSize.y, TitleSize.y + TextGap + TextSize.y) + ProgressHeight;

		const ImVec2 Min(Right - Width + Slide, Bottom - Height);
		const ImVec2 Max(Right + Slide, Bottom);
		const ImVec4 Accent = ToastAccent(T.Type);
		Bottom = Min.y - Padding * 0.6f;

		// Slots are reused, so ImGui keeps at most ToastMaxVisible toast windows
		string32 WindowID;
		xr_sprintf(WindowID, "##Toast%u", Shown);

		ImGui::SetNextWindowPos(Min, ImGuiCond_Always);
		ImGui::SetNextWindowSize(ImVec2(Width + Shadow.x, Height + Shadow.y), ImGuiCond_Always);
		ImGui::SetNextWindowViewport(Viewport->ID);
		ImGui::PushStyleVar(ImGuiStyleVar_WindowPadding, ImVec2(0.0f, 0.0f));
		ImGui::PushStyleVar(ImGuiStyleVar_WindowBorderSize, 0.0f);
		ImGui::PushStyleVar(ImGuiStyleVar_WindowMinSize, ImVec2(1.0f, 1.0f));
		const bool Visible = ImGui::Begin(WindowID, nullptr, Flags);
		ImGui::PopStyleVar(3);
		if (!Visible)
		{
			ImGui::End();
			continue;
		}

		ImGui::BringWindowToDisplayFront(ImGui::GetCurrentWindow());
		ImDrawList* DrawList = ImGui::GetWindowDrawList();

		DrawList->AddRectFilled(Min + Shadow, Max + Shadow, ToastColor(ImVec4(0.0f, 0.0f, 0.0f, 0.35f), Alpha), Rounding);
		DrawList->AddRectFilled(Min, Max, ToastColor(Style.Colors[ImGuiCol_PopupBg], Alpha * 0.97f), Rounding);
		DrawList->AddRectFilled(Min, ImVec2(Min.x + AccentWidth, Max.y), ToastColor(Accent, Alpha), Rounding, ImDrawFlags_RoundCornersLeft);
		DrawList->AddRect(Min, Max, ToastColor(Style.Colors[ImGuiCol_Border], Alpha), Rounding);

		const ImVec2 IconPos(Min.x + AccentWidth + Padding, Min.y + Padding);
		DrawList->AddText(IconPos, ToastColor(Accent, Alpha), Icon);

		const ImVec2 TitlePos(Min.x + TextX, Min.y + Padding);
		DrawList->AddText(Font, FontSize, TitlePos, ToastColor(Style.Colors[ImGuiCol_Text], Alpha), T.Title.c_str(), nullptr, WrapWidth);
		if (!T.Text.empty())
		{
			const ImVec2 TextPos(TitlePos.x, TitlePos.y + TitleSize.y + TextGap);
			DrawList->AddText(Font, FontSize, TextPos, ToastColor(Style.Colors[ImGuiCol_TextDisabled], Alpha), T.Text.c_str(), nullptr, WrapWidth);
		}

		// Close button in the top right corner
		const ImVec2 CloseMin(Max.x - Padding * 0.5f - CloseSize, Min.y + Padding * 0.5f);
		const ImVec2 CloseMax(CloseMin.x + CloseSize, CloseMin.y + CloseSize);
		ImGui::SetCursorScreenPos(CloseMin);
		const bool CloseClicked = ImGui::InvisibleButton("##Close", ImVec2(CloseSize, CloseSize));
		const bool CloseHovered = ImGui::IsItemHovered();
		if (CloseHovered)
		{
			ImGui::SetMouseCursor(ImGuiMouseCursor_Hand);
			DrawList->AddRectFilled(CloseMin, CloseMax, ToastColor(Style.Colors[ImGuiCol_ButtonHovered], Alpha), Rounding);
		}

		const float Cross = CloseSize * 0.28f;
		const ImVec2 CloseCenter((CloseMin.x + CloseMax.x) * 0.5f, (CloseMin.y + CloseMax.y) * 0.5f);
		const ImU32 CrossColor = ToastColor(Style.Colors[CloseHovered ? ImGuiCol_Text : ImGuiCol_TextDisabled], Alpha);
		DrawList->AddLine(ImVec2(CloseCenter.x - Cross, CloseCenter.y - Cross), ImVec2(CloseCenter.x + Cross, CloseCenter.y + Cross), CrossColor, 1.5f);
		DrawList->AddLine(ImVec2(CloseCenter.x + Cross, CloseCenter.y - Cross), ImVec2(CloseCenter.x - Cross, CloseCenter.y + Cross), CrossColor, 1.5f);

		// Time left
		const float Progress = std::clamp(Left / float(T.DurationMs), 0.0f, 1.0f);
		const float BarLeft = Min.x + AccentWidth;
		const float BarRight = Max.x - Rounding;
		DrawList->AddRectFilled(ImVec2(BarLeft, Max.y - ProgressHeight - 1.0f), ImVec2(BarLeft + (BarRight - BarLeft) * Progress, Max.y - 1.0f), ToastColor(Accent, Alpha * 0.6f));

		const bool Hovered = ImGui::IsWindowHovered(ImGuiHoveredFlags_AllowWhenBlockedByActiveItem);
		ImGui::End();

		if (CloseClicked && !T.Closing)
		{
			// Fade out from the current alpha
			T.Closing = true;
			T.DurationMs = u32(Age + std::min(Left, Alpha * ToastFadeMs));
		}
		else if (Hovered && !T.Closing && FadeIn >= 1.0f && Left > ToastFadeMs)
		{
			// Keep the toast while the pointer is on it
			T.StartTicks += FrameMs;
		}
	}
}
