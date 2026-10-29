#include "stdafx.h"
#include "UIEditLightAnim.h"
#include "../xrEngine/LightAnimLibrary.h"
#include "imgui_user.h"
#include "IconsFontAwesome7.h"

#include "ui_main.h"

UIEditLightAnim* UIEditLightAnim::Form = nullptr;

// Light animation colours are 0xAARRGGBB
static ImU32 ToImColor(u32 Color, bool UseAlpha)
{
	return IM_COL32(color_get_R(Color), color_get_G(Color), color_get_B(Color), UseAlpha ? color_get_A(Color) : 255);
}

static void DrawChecker(ImDrawList* DrawList, const ImVec2& Min, const ImVec2& Max, float Cell)
{
	DrawList->AddRectFilled(Min, Max, IM_COL32(90, 90, 90, 255));
	DrawList->PushClipRect(Min, Max, true);
	for (float Y = Min.y; Y < Max.y; Y += Cell)
	{
		const bool OddRow = (int((Y - Min.y) / Cell) & 1) != 0;
		for (float X = Min.x + (OddRow ? Cell : 0.f); X < Max.x; X += Cell * 2.f)
		{
			DrawList->AddRectFilled({ X, Y }, { X + Cell, Y + Cell }, IM_COL32(150, 150, 150, 255));
		}
	}
	DrawList->PopClipRect();
}

static bool IconButton(const char* Label, const char* Tooltip, bool Active = false, bool Enabled = true)
{
	ImGui::BeginDisabled(!Enabled);
	if (Active)
	{
		ImGui::PushStyleColor(ImGuiCol_Button, ImGui::GetStyleColorVec4(ImGuiCol_ButtonActive));
	}
	const bool Pressed = ImGui::Button(Label);
	if (Active)
	{
		ImGui::PopStyleColor();
	}
	ImGui::EndDisabled();
	if (ImGui::IsItemHovered(ImGuiHoveredFlags_AllowWhenDisabled))
	{
		ImGui::SetTooltip("%s", Tooltip);
	}
	return Pressed;
}

UIEditLightAnim::UIEditLightAnim()
{
	TabIndex = 2;
	m_Modife = false;
	m_Items = new UIItemListForm();
	m_Items->m_Flags.set(UIItemListForm::fMenuEdit, true);
	m_Props = new UIPropertiesForm();
	m_Props->SetModifiedEvent(TOnModifiedEvent(this, &UIEditLightAnim::OnModified));
	m_CurrentItem = nullptr;
	m_PointerValue = 0;
	m_RenderAlpha = false;

	m_Items->SetOnItemCreaetEvent(xr_make_delegate(this, &UIEditLightAnim::OnCreateItem));
	m_Items->SetOnItemRemoveEvent({this, &UIEditLightAnim::OnRemoveItem});
	m_Items->SetOnItemCloneEvent(xr_make_delegate(this, &UIEditLightAnim::OnCloneItem));
	m_Items->SetOnItemRenameEvent(xr_make_delegate(this, &UIEditLightAnim::OnRenameItem));
	m_Items->SetOnItemFocusedEvent(xr_make_delegate(this, &UIEditLightAnim::OnItemFocused));

	InitializeItems();
}

UIEditLightAnim::~UIEditLightAnim()
{
	if (m_Modife)
	{
		if (ELog.DlgMsg(mtConfirmation, mbYes | mbNo, "Save current change?") == mrYes)
		{
			LALib.Save();
		}
		else
		{
			LALib.Reload();
		}
	}
	xr_delete(m_Props);
	xr_delete(m_Items);
}

//---------------------------------------------------------------------------
// Playback

int UIEditLightAnim::GetPreviewFrame() const
{
	if (m_CurrentItem == nullptr)
	{
		return 0;
	}

	if (Playing)
	{
		return std::clamp(iFloor(PlayTime * m_CurrentItem->fFPS), 0, m_CurrentItem->iFrameCount - 1);
	}
	return m_PointerValue;
}

void UIEditLightAnim::SetFrame(int Frame)
{
	if (m_CurrentItem == nullptr)
	{
		m_PointerValue = 0;
		return;
	}

	Frame = std::clamp(Frame, 0, m_CurrentItem->iFrameCount - 1);
	if (Frame != m_PointerValue)
	{
		m_PointerValue = Frame;
		UpdateProperties();
	}
}

void UIEditLightAnim::StopPlayback()
{
	Playing = false;
}

void UIEditLightAnim::TogglePlayback()
{
	if (m_CurrentItem == nullptr || m_CurrentItem->fFPS <= 0.f)
	{
		return;
	}

	if (Playing)
	{
		// Pause keeps the reached frame selected
		const int Frame = GetPreviewFrame();
		Playing = false;
		SetFrame(Frame);
		return;
	}

	if (m_PointerValue >= m_CurrentItem->iFrameCount - 1)
	{
		m_PointerValue = 0;
	}
	PlayTime = float(m_PointerValue) / m_CurrentItem->fFPS;
	Playing = true;
}

//---------------------------------------------------------------------------
// Keys

void UIEditLightAnim::DeleteCurrentKey()
{
	// The first key holds the start colour of the animation
	if (m_CurrentItem == nullptr || m_PointerValue == 0 || !m_CurrentItem->IsKey(m_PointerValue))
	{
		return;
	}

	m_CurrentItem->DeleteKey(m_PointerValue);
	UpdateProperties();
	OnModified();
}

void UIEditLightAnim::MoveCurrentKey(int Direction)
{
	if (m_CurrentItem == nullptr || m_PointerValue == 0 || !m_CurrentItem->IsKey(m_PointerValue))
	{
		return;
	}

	for (int Frame = m_PointerValue + Direction; Frame > 0 && Frame < m_CurrentItem->iFrameCount; Frame += Direction)
	{
		if (!m_CurrentItem->IsKey(Frame))
		{
			m_CurrentItem->MoveKey(m_PointerValue, Frame);
			m_PointerValue = Frame;
			UpdateProperties();
			OnModified();
			return;
		}
	}
}

void UIEditLightAnim::CopyColor()
{
	CopiedColor = m_CurrentItem->InterpolateRGB(m_PointerValue);
	HasCopiedColor = true;
}

void UIEditLightAnim::PasteColor()
{
	if (!HasCopiedColor)
	{
		return;
	}

	if (u32* Key = m_CurrentItem->GetKey(m_PointerValue))
	{
		*Key = CopiedColor;
	}
	else
	{
		m_CurrentItem->InsertKey(m_PointerValue, CopiedColor);
	}
	UpdateProperties();
	OnModified();
}

//---------------------------------------------------------------------------
// UI

void UIEditLightAnim::HandleShortcuts()
{
	if (m_CurrentItem == nullptr || !IsFocused || ImGui::IsAnyItemActive() || ImGui::GetIO().WantTextInput)
	{
		return;
	}

	const bool Ctrl = ImGui::GetIO().KeyCtrl;
	if (ImGui::IsKeyPressed(ImGuiKey_Space, false))
	{
		TogglePlayback();
	}
	if (ImGui::IsKeyPressed(ImGuiKey_LeftArrow))
	{
		StopPlayback();
		SetFrame(Ctrl ? m_CurrentItem->PrevKeyFrame(m_PointerValue) : m_PointerValue - 1);
	}
	if (ImGui::IsKeyPressed(ImGuiKey_RightArrow))
	{
		StopPlayback();
		SetFrame(Ctrl ? m_CurrentItem->NextKeyFrame(m_PointerValue) : m_PointerValue + 1);
	}
	if (ImGui::IsKeyPressed(ImGuiKey_Home, false))
	{
		StopPlayback();
		SetFrame(0);
	}
	if (ImGui::IsKeyPressed(ImGuiKey_End, false))
	{
		StopPlayback();
		SetFrame(m_CurrentItem->iFrameCount - 1);
	}
	if (ImGui::IsKeyPressed(ImGuiKey_Insert, false))
	{
		OnCreateKeyClick();
	}
	if (ImGui::IsKeyPressed(ImGuiKey_Delete, false))
	{
		DeleteCurrentKey();
	}
	if (Ctrl && ImGui::IsKeyPressed(ImGuiKey_C, false))
	{
		CopyColor();
	}
	if (Ctrl && ImGui::IsKeyPressed(ImGuiKey_V, false))
	{
		PasteColor();
	}
}

void UIEditLightAnim::DrawItemList()
{
	const char* SaveLabel = m_Modife ? ICON_FA_FLOPPY_DISK " Save *" : ICON_FA_FLOPPY_DISK " Save";
	if (IconButton(SaveLabel, m_Modife ? "Save the light animation library (modified)" : "Save the light animation library", m_Modife))
	{
		m_Modife = false;
		LALib.Save();
	}
	ImGui::SameLine();
	if (IconButton(ICON_FA_ARROWS_ROTATE " Reload", "Reload the library and drop unsaved changes"))
	{
		m_Modife = false;
		LALib.Reload();
		OnItemFocused(nullptr);
		InitializeItems();
	}

	ImGui::SetNextItemWidth(-1);
	ImGui::InputTextWithHint("##LASearch", ICON_FA_MAGNIFYING_GLASS " Search...", SearchFilter, sizeof(SearchFilter));
	m_Items->SetFilter(SearchFilter);

	if (ImGui::BeginChild("##LAItems", ImVec2(0, 0), ImGuiChildFlags_Borders))
	{
		m_Items->Draw();
		IsContextMenu = m_Items->m_UseMenuEdit;
	}
	ImGui::EndChild();
}

void UIEditLightAnim::DrawTransport()
{
	const int LastFrame = m_CurrentItem->iFrameCount - 1;

	if (IconButton(ICON_FA_BACKWARD_STEP, "First frame (Home)"))
	{
		StopPlayback();
		SetFrame(0);
	}
	ImGui::SameLine();
	if (IconButton(ICON_FA_BACKWARD, "Previous key (Ctrl+Left)"))
	{
		StopPlayback();
		SetFrame(m_CurrentItem->PrevKeyFrame(m_PointerValue));
	}
	ImGui::SameLine();
	if (IconButton(Playing ? ICON_FA_PAUSE : ICON_FA_PLAY, Playing ? "Pause (Space)" : "Play preview (Space)", Playing))
	{
		TogglePlayback();
	}
	ImGui::SameLine();
	if (IconButton(ICON_FA_STOP, "Stop and rewind"))
	{
		StopPlayback();
		PlayTime = 0.f;
		SetFrame(0);
	}
	ImGui::SameLine();
	if (IconButton(ICON_FA_FORWARD, "Next key (Ctrl+Right)"))
	{
		StopPlayback();
		SetFrame(m_CurrentItem->NextKeyFrame(m_PointerValue));
	}
	ImGui::SameLine();
	if (IconButton(ICON_FA_FORWARD_STEP, "Last frame (End)"))
	{
		StopPlayback();
		SetFrame(LastFrame);
	}
	ImGui::SameLine();
	if (IconButton(ICON_FA_REPEAT, "Loop preview", Loop))
	{
		Loop = !Loop;
	}

	ImGui::SameLine(0, 16.f);
	int Frame = GetPreviewFrame();
	ImGui::SetNextItemWidth(ImGui::GetFontSize() * 7.f);
	if (ImGui::InputInt("##Frame", &Frame, 1, 10))
	{
		StopPlayback();
		SetFrame(Frame);
	}
	ImGui::SameLine();
	const float FPS = m_CurrentItem->fFPS > 0.f ? m_CurrentItem->fFPS : 1.f;
	ImGui::TextDisabled("/ %d   %.2fs / %.2fs", LastFrame, float(GetPreviewFrame()) / FPS, float(m_CurrentItem->iFrameCount) / FPS);

	ImGui::SameLine(0, 16.f);
	const bool IsKey = m_CurrentItem->IsKey(m_PointerValue);
	if (IconButton(ICON_FA_PLUS " Key", "Add a key at the current frame (Insert, double click)", false, !IsKey))
	{
		OnCreateKeyClick();
	}
	ImGui::SameLine();
	if (IconButton(ICON_FA_MINUS " Key", "Delete the current key (Delete)", false, IsKey && m_PointerValue != 0))
	{
		DeleteCurrentKey();
	}
	ImGui::SameLine();
	if (IconButton(ICON_FA_ARROW_LEFT, "Move the current key left", false, IsKey && m_PointerValue != 0))
	{
		MoveCurrentKey(-1);
	}
	ImGui::SameLine();
	if (IconButton(ICON_FA_ARROW_RIGHT, "Move the current key right", false, IsKey && m_PointerValue != 0))
	{
		MoveCurrentKey(1);
	}

	ImGui::SameLine(0, 16.f);
	if (IconButton(ICON_FA_CHESS_BOARD, "Show alpha", m_RenderAlpha))
	{
		m_RenderAlpha = !m_RenderAlpha;
	}
	ImGui::SameLine();
	ImGui::SetNextItemWidth(ImGui::GetFontSize() * 6.f);
	ImGui::SliderFloat("##Zoom", &TimelineZoom, 1.f, 20.f, ICON_FA_MAGNIFYING_GLASS_PLUS " %.1fx", ImGuiSliderFlags_Logarithmic);
}

void UIEditLightAnim::DrawTimeline()
{
	const float RulerHeight = ImGui::GetFontSize() + 4.f;
	const float SecondsHeight = ImGui::GetFontSize() + 4.f;
	const float StripHeight = 34.f;
	const float AlphaHeight = 10.f;
	const float KeysHeight = 16.f;
	const float AreaHeight = RulerHeight + SecondsHeight + StripHeight + AlphaHeight + KeysHeight;
	const float TotalHeight = AreaHeight + ImGui::GetStyle().ScrollbarSize + ImGui::GetStyle().WindowPadding.y * 2.f;

	if (!ImGui::BeginChild("##LATimeline", ImVec2(0, TotalHeight), ImGuiChildFlags_Borders, ImGuiWindowFlags_HorizontalScrollbar))
	{
		ImGui::EndChild();
		return;
	}

	const int Count = std::max(m_CurrentItem->iFrameCount, 1);
	const float ViewWidth = ImGui::GetContentRegionAvail().x;
	const float Width = std::max(ViewWidth * TimelineZoom, 1.f);
	const float FrameWidth = Width / float(Count);

	const ImVec2 Origin = ImGui::GetCursorScreenPos();
	ImGui::InvisibleButton("##TimelineArea", ImVec2(Width, AreaHeight), ImGuiButtonFlags_MouseButtonLeft | ImGuiButtonFlags_MouseButtonRight);
	const bool Hovered = ImGui::IsItemHovered();
	const bool Active = ImGui::IsItemActive();

	ImDrawList* DrawList = ImGui::GetWindowDrawList();
	const float SecondsTop = Origin.y + RulerHeight;
	const float StripTop = SecondsTop + SecondsHeight;
	const float AlphaTop = StripTop + StripHeight;
	const float KeysTop = AlphaTop + AlphaHeight;
	const float Bottom = KeysTop + KeysHeight;

	auto FrameX = [&](int Frame) { return Origin.x + float(Frame) * FrameWidth; };
	auto FrameAt = [&](float X) { return std::clamp(iFloor((X - Origin.x) / FrameWidth), 0, Count - 1); };

	// Only the visible frames are drawn
	const int FirstVisible = FrameAt(DrawList->GetClipRectMin().x);
	const int LastVisible = FrameAt(DrawList->GetClipRectMax().x);

	// Ruler: tick spacing follows the frame width
	DrawList->AddRectFilled(Origin, { Origin.x + Width, SecondsTop }, ImGui::GetColorU32(ImGuiCol_FrameBg));
	static const int Steps[] = { 1, 2, 5, 10, 20, 50, 100, 200, 500, 1000, 2000, 5000, 10000, 20000, 50000 };
	int TickStep = Steps[std::size(Steps) - 1];
	for (int Step : Steps)
	{
		if (Step * FrameWidth >= 6.f)
		{
			TickStep = Step;
			break;
		}
	}
	int LabelStep = Steps[std::size(Steps) - 1];
	const float LabelWidth = ImGui::CalcTextSize("00000").x + 8.f;
	for (int Step : Steps)
	{
		if (Step >= TickStep && Step * FrameWidth >= LabelWidth)
		{
			LabelStep = Step;
			break;
		}
	}

	const ImU32 TickColor = ImGui::GetColorU32(ImGuiCol_TextDisabled);
	for (int Frame = FirstVisible - FirstVisible % TickStep; Frame <= LastVisible; Frame += TickStep)
	{
		const float X = FrameX(Frame) + FrameWidth * 0.5f;
		const bool Major = Frame % LabelStep == 0;
		DrawList->AddLine({ X, SecondsTop - (Major ? 7.f : 3.f) }, { X, SecondsTop }, TickColor);
		if (Major)
		{
			char Text[16];
			xr_sprintf(Text, "%d", Frame);
			DrawList->AddText({ X + 2.f, Origin.y + 1.f }, TickColor, Text);
		}
	}

	// Seconds ruler: time T starts at the left edge of frame T * FPS
	DrawList->AddRectFilled({ Origin.x, SecondsTop }, { Origin.x + Width, StripTop }, ImGui::GetColorU32(ImGuiCol_TableHeaderBg));
	if (m_CurrentItem->fFPS > 0.f)
	{
		const float PixelsPerSecond = FrameWidth * m_CurrentItem->fFPS;
		static const float TimeSteps[] = { 0.01f, 0.02f, 0.05f, 0.1f, 0.2f, 0.5f, 1.f, 2.f, 5.f, 10.f, 30.f, 60.f, 120.f, 300.f, 600.f };
		float TimeTick = TimeSteps[std::size(TimeSteps) - 1];
		for (float Step : TimeSteps)
		{
			if (Step * PixelsPerSecond >= 8.f)
			{
				TimeTick = Step;
				break;
			}
		}
		float TimeLabel = TimeSteps[std::size(TimeSteps) - 1];
		const float TimeLabelWidth = ImGui::CalcTextSize("000.00s").x + 8.f;
		for (float Step : TimeSteps)
		{
			if (Step >= TimeTick && Step * PixelsPerSecond >= TimeLabelWidth)
			{
				TimeLabel = Step;
				break;
			}
		}

		const float Length = float(Count) / m_CurrentItem->fFPS;
		const float VisibleStart = std::max((DrawList->GetClipRectMin().x - Origin.x) / PixelsPerSecond, 0.f);
		const float VisibleEnd = std::min((DrawList->GetClipRectMax().x - Origin.x) / PixelsPerSecond, Length);
		const int FirstTick = iFloor(VisibleStart / TimeTick);
		const int LastTick = iCeil(VisibleEnd / TimeTick);
		const int LabelEvery = std::max(iFloor(TimeLabel / TimeTick + 0.5f), 1);
		const ImU32 SecondColor = ImGui::GetColorU32(ImGuiCol_Text);

		for (int Tick = FirstTick; Tick <= LastTick; ++Tick)
		{
			const float Time = float(Tick) * TimeTick;
			const float X = Origin.x + Time * PixelsPerSecond;
			const bool Major = Tick % LabelEvery == 0;
			DrawList->AddLine({ X, StripTop - (Major ? 7.f : 3.f) }, { X, StripTop }, Major ? SecondColor : TickColor);
			if (Major)
			{
				char Text[16];
				xr_sprintf(Text, TimeLabel < 0.1f ? "%.2fs" : (TimeLabel < 1.f ? "%.1fs" : "%.0fs"), Time);
				DrawList->AddText({ X + 2.f, SecondsTop + 1.f }, SecondColor, Text);
			}
		}
	}

	// Colour and alpha strips
	if (m_RenderAlpha)
	{
		DrawChecker(DrawList, { Origin.x, StripTop }, { Origin.x + Width, AlphaTop }, 6.f);
	}
	for (int Frame = FirstVisible; Frame <= LastVisible; ++Frame)
	{
		const u32 Color = m_CurrentItem->InterpolateRGB(Frame);
		const float X0 = FrameX(Frame);
		const float X1 = FrameX(Frame + 1);
		DrawList->AddRectFilled({ X0, StripTop }, { X1, AlphaTop }, ToImColor(Color, m_RenderAlpha));

		const u32 Alpha = color_get_A(Color);
		DrawList->AddRectFilled({ X0, AlphaTop }, { X1, KeysTop }, IM_COL32(Alpha, Alpha, Alpha, 255));
	}
	DrawList->AddRectFilled({ Origin.x, KeysTop }, { Origin.x + Width, Bottom }, ImGui::GetColorU32(ImGuiCol_FrameBg));

	// Keys
	const ImVec2 Mouse = ImGui::GetIO().MousePos;
	const float KeySize = 5.f;
	int HoveredKey = -1;
	for (const auto& [KeyFrame, KeyColor] : m_CurrentItem->Keys)
	{
		if (KeyFrame < FirstVisible - 1 || KeyFrame > LastVisible + 1)
		{
			continue;
		}

		const ImVec2 Center = { FrameX(KeyFrame) + FrameWidth * 0.5f, KeysTop + KeysHeight * 0.5f };
		const bool IsHovered = Hovered && std::abs(Mouse.x - Center.x) <= KeySize + 1.f && Mouse.y >= KeysTop;
		if (IsHovered)
		{
			HoveredKey = KeyFrame;
		}

		const bool Selected = KeyFrame == m_PointerValue;
		const ImU32 Fill = Selected ? IM_COL32(255, 210, 60, 255) : (IsHovered ? IM_COL32(255, 255, 255, 255) : IM_COL32(190, 190, 190, 255));
		const ImVec2 P0 = { Center.x, Center.y - KeySize };
		const ImVec2 P1 = { Center.x + KeySize, Center.y };
		const ImVec2 P2 = { Center.x, Center.y + KeySize };
		const ImVec2 P3 = { Center.x - KeySize, Center.y };
		DrawList->AddQuadFilled(P0, P1, P2, P3, Fill);
		DrawList->AddQuad(P0, P1, P2, P3, IM_COL32(0, 0, 0, 255));
		// Key mark on the colour strip
		DrawList->AddLine({ Center.x, StripTop }, { Center.x, StripTop + 5.f }, IM_COL32(0, 0, 0, 180), 2.f);
	}

	// Playhead
	{
		const float X = FrameX(GetPreviewFrame()) + FrameWidth * 0.5f;
		const ImU32 HeadColor = Playing ? IM_COL32(80, 220, 120, 255) : IM_COL32(255, 210, 60, 255);
		DrawList->AddLine({ X, Origin.y }, { X, Bottom }, HeadColor, 2.f);
		DrawList->AddTriangleFilled({ X - 5.f, Origin.y }, { X + 5.f, Origin.y }, { X, Origin.y + 6.f }, HeadColor);
	}

	// Picking a frame stops the preview and shows that frame
	if (ImGui::IsItemClicked(ImGuiMouseButton_Left))
	{
		StopPlayback();
		if (HoveredKey >= 0)
		{
			DraggedKey = HoveredKey;
			SetFrame(HoveredKey);
		}
		else
		{
			Scrubbing = true;
			SetFrame(FrameAt(Mouse.x));
		}
	}

	if (Active && ImGui::IsMouseDown(ImGuiMouseButton_Left))
	{
		const int Target = FrameAt(Mouse.x);
		// The first key stays at frame 0
		if (DraggedKey > 0 && Target > 0 && Target != DraggedKey && !m_CurrentItem->IsKey(Target))
		{
			m_CurrentItem->MoveKey(DraggedKey, Target);
			DraggedKey = Target;
			m_PointerValue = Target;
			UpdateProperties();
			OnModified();
		}
		else if (Scrubbing)
		{
			SetFrame(Target);
		}
	}

	if (!ImGui::IsMouseDown(ImGuiMouseButton_Left))
	{
		DraggedKey = -1;
		Scrubbing = false;
	}

	if (Hovered && ImGui::IsMouseDoubleClicked(ImGuiMouseButton_Left))
	{
		SetFrame(FrameAt(Mouse.x));
		OnCreateKeyClick();
	}

	if (Hovered && ImGui::GetIO().KeyCtrl && ImGui::GetIO().MouseWheel != 0.f)
	{
		TimelineZoom = std::clamp(TimelineZoom * (ImGui::GetIO().MouseWheel > 0.f ? 1.25f : 0.8f), 1.f, 20.f);
	}

	if (Hovered && !Active)
	{
		const int Frame = HoveredKey >= 0 ? HoveredKey : FrameAt(Mouse.x);
		const u32 Color = m_CurrentItem->InterpolateRGB(Frame);
		ImGui::SetTooltip("%s %d\nR %u  G %u  B %u  A %u", HoveredKey >= 0 ? "Key" : "Frame", Frame,
			color_get_R(Color), color_get_G(Color), color_get_B(Color), color_get_A(Color));
	}

	if (ImGui::IsItemClicked(ImGuiMouseButton_Right))
	{
		StopPlayback();
		SetFrame(HoveredKey >= 0 ? HoveredKey : FrameAt(Mouse.x));
		ImGui::OpenPopup("##TimelineMenu");
	}

	if (ImGui::BeginPopup("##TimelineMenu"))
	{
		const bool IsKey = m_CurrentItem->IsKey(m_PointerValue);
		if (ImGui::MenuItem(ICON_FA_PLUS " Add Key", "Insert", false, !IsKey))
		{
			OnCreateKeyClick();
		}
		if (ImGui::MenuItem(ICON_FA_MINUS " Delete Key", "Delete", false, IsKey && m_PointerValue != 0))
		{
			DeleteCurrentKey();
		}
		ImGui::Separator();
		if (ImGui::MenuItem(ICON_FA_COPY " Copy Color", "Ctrl+C"))
		{
			CopyColor();
		}
		if (ImGui::MenuItem(ICON_FA_PASTE " Paste Color", "Ctrl+V", false, HasCopiedColor))
		{
			PasteColor();
		}
		ImGui::EndPopup();
	}

	// Keep the playhead in view while playing on a zoomed timeline
	if (Playing && TimelineZoom > 1.f)
	{
		const float X = float(GetPreviewFrame()) * FrameWidth;
		if (X < ImGui::GetScrollX() || X > ImGui::GetScrollX() + ViewWidth)
		{
			ImGui::SetScrollX(X - ViewWidth * 0.1f);
		}
	}

	ImGui::EndChild();
}

void UIEditLightAnim::DrawPreview()
{
	const int Frame = GetPreviewFrame();
	const u32 Color = m_CurrentItem->InterpolateRGB(Frame);

	const float InfoHeight = ImGui::GetTextLineHeightWithSpacing() * 3.5f;
	const ImVec2 Avail = ImGui::GetContentRegionAvail();
	const float Side = std::max(std::min(Avail.x, Avail.y - InfoHeight), 32.f);

	const ImVec2 Min = ImGui::GetCursorScreenPos();
	const ImVec2 Max = { Min.x + Avail.x, Min.y + Side };
	ImDrawList* DrawList = ImGui::GetWindowDrawList();
	if (m_RenderAlpha)
	{
		DrawChecker(DrawList, Min, Max, 12.f);
	}
	DrawList->AddRectFilled(Min, Max, ToImColor(Color, m_RenderAlpha), 4.f);
	DrawList->AddRect(Min, Max, ImGui::GetColorU32(ImGuiCol_Border), 4.f);
	ImGui::Dummy({ Avail.x, Side });

	const u32 R = color_get_R(Color);
	const u32 G = color_get_G(Color);
	const u32 B = color_get_B(Color);
	const u32 A = color_get_A(Color);
	ImGui::Text(ICON_FA_LIGHTBULB " Frame %d%s", Frame, m_CurrentItem->IsKey(Frame) ? "  " ICON_FA_DIAMOND " key" : "");
	ImGui::Text("R %u  G %u  B %u  A %u", R, G, B, A);
	ImGui::Text("#%02X%02X%02X  Intensity %.2f", R, G, B, (0.2126f * R + 0.7152f * G + 0.0722f * B) / 255.f);
}

void UIEditLightAnim::Draw()
{
	IsDocked = ImGui::IsWindowDocked();
	IsFocused = ImGui::IsWindowFocused(ImGuiFocusedFlags_RootAndChildWindows);

	HandleShortcuts();

	if (Playing && m_CurrentItem != nullptr && m_CurrentItem->fFPS > 0.f)
	{
		PlayTime += EDevice->fTimeDelta;
		const float Length = float(m_CurrentItem->iFrameCount) / m_CurrentItem->fFPS;
		if (PlayTime >= Length)
		{
			if (Loop && Length > 0.f)
			{
				PlayTime = std::fmod(PlayTime, Length);
			}
			else
			{
				Playing = false;
				SetFrame(m_CurrentItem->iFrameCount - 1);
			}
		}
	}

	if (!ImGui::BeginTable("##LALayout", 2, ImGuiTableFlags_Resizable | ImGuiTableFlags_BordersInnerV))
	{
		return;
	}
	ImGui::TableSetupColumn("Items", ImGuiTableColumnFlags_WidthFixed, 250.f);
	ImGui::TableSetupColumn("Editor", ImGuiTableColumnFlags_WidthStretch);
	ImGui::TableNextRow();

	ImGui::TableNextColumn();
	if (ImGui::BeginChild("##LALeft"))
	{
		DrawItemList();
	}
	ImGui::EndChild();

	ImGui::TableNextColumn();
	if (ImGui::BeginChild("##LARight"))
	{
		if (m_CurrentItem == nullptr)
		{
			ImGui::TextDisabled("Select a light animation in the list");
		}
		else
		{
			ImGui::Text(ICON_FA_LIGHTBULB " %s", m_CurrentItem->cName.c_str());
			DrawTransport();
			DrawTimeline();

			if (ImGui::BeginTable("##LABottom", 2, ImGuiTableFlags_Resizable | ImGuiTableFlags_BordersInnerV))
			{
				ImGui::TableSetupColumn("Properties", ImGuiTableColumnFlags_WidthStretch);
				ImGui::TableSetupColumn("Preview", ImGuiTableColumnFlags_WidthFixed, 280.f);
				ImGui::TableNextRow();

				ImGui::TableNextColumn();
				if (ImGui::BeginChild("##LAProps"))
				{
					m_Props->Draw();
				}
				ImGui::EndChild();

				ImGui::TableNextColumn();
				if (ImGui::BeginChild("##LAPreview"))
				{
					DrawPreview();
				}
				ImGui::EndChild();

				ImGui::EndTable();
			}
		}
	}
	ImGui::EndChild();

	ImGui::EndTable();
}

void UIEditLightAnim::Update()
{
	if (Form)
	{
		if (!Form->IsClosed())
		{
			if (EContext.UI->ActiveTabIndex != Form->TabIndex)
				return;

			// The top bar tab already names the workspace: no tab bar of its own in the dock node
			ImGuiWindowClass WindowClass;
			WindowClass.DockNodeFlagsOverrideSet = ImGuiDockNodeFlags_AutoHideTabBar;
			ImGui::SetNextWindowClass(&WindowClass);

			ImGui::PushStyleVar(ImGuiStyleVar_WindowMinSize, ImVec2(300, 300));
			if (ImGui::Begin("Light Animations Editor", &Form->bOpen))
			{
				Form->Draw();
			}
			ImGui::PopStyleVar(1);
			ImGui::End();
		}
		else
		{
			xr_delete(Form);
		}
	}
}

void UIEditLightAnim::Show()
{
	if (Form == nullptr)
	{
		Form = new UIEditLightAnim();
		Form->SetStateName("light_anim");
		Form->bOpen = true;
	}
}

bool UIEditLightAnim::IsOpen()
{
	return Form && Form->bOpen;
}

void UIEditLightAnim::UpdateProperties()
{
	PropItemVec items;
	if (m_CurrentItem) 
	{
	  // PHelper().CreateName(items, "Name", &m_CurrentItem->cName, m_CurrentOwner);
		PHelper().CreateFloat(items, "FPS", &m_CurrentItem->fFPS, 0.1f, 1000, 1.f, 1);
		S32Value* V;
		V = PHelper().CreateNumeric(items, "Frame Count", &m_CurrentItem->iFrameCount, 1, 100000, 1);
		V->OnAfterEditEvent.bind(this, &UIEditLightAnim::OnFrameCountAfterEdit);

		u32 frame = m_PointerValue;
		PHelper().CreateCaption(items, "Current\\Frame", shared_str().printf("%d", frame));
		u32* val = m_CurrentItem->GetKey(m_PointerValue);
		if (val) 
		{
			PHelper().CreateColor(items, "Current\\Color", val);
			PHelper().CreateNumeric(items, "Current\\Alpha", ((u8*)val) + 3, 0x00, 0xFF);
		}
	}
	m_Props->AssignItems(items);
}

void UIEditLightAnim::InitializeItems()
{
	ListItemsVec items;
	for (LAItemIt it = LALib.Items.begin(); it != LALib.Items.end(); it++)
		LHelper().CreateItem(items, *(*it)->cName, 0, 0, nullptr);
	m_Items->AssignItems(items);
}

void UIEditLightAnim::OnCreateKeyClick()
{
	if (!(m_CurrentItem->IsKey(m_PointerValue)))
	{
		u32 color = m_CurrentItem->InterpolateRGB(m_PointerValue);

		m_CurrentItem->InsertKey(m_PointerValue, color);
		UpdateProperties();
		OnModified();
	}
}

void UIEditLightAnim::OnModified()
{
	m_Modife = true;
}

void UIEditLightAnim::OnItemFocused(ListItem*item)
{
	m_CurrentItem = nullptr;
	StopPlayback();
	PlayTime = 0.f;
	m_PointerValue = 0;
	DraggedKey = -1;
	if (item)
	{
		m_CurrentItem = LALib.FindItem(item->Key());
		UpdateProperties();
	}
	else
	{
		m_Props->ClearProperties();
	}
}

bool UIEditLightAnim::OnFrameCountAfterEdit(PropValue* v, s32& val)
{
	if (val != m_CurrentItem->iFrameCount)
	{
		OnModified();
	}

	m_CurrentItem->Resize(val);

	if (m_PointerValue > m_CurrentItem->iFrameCount - 1)
	{
		m_PointerValue = m_CurrentItem->iFrameCount - 1;
	}

	if (m_PointerValue < 0)
	{
		m_PointerValue = 0;
	}

	return true;
}

void UIEditLightAnim::OnCloneItem(const char* parent_path, const char* new_full_name)
{
	LALib.AppendItem(new_full_name, LALib.FindItem(parent_path));;
	InitializeItems();
	m_Items->SelectItem(new_full_name);
	OnModified();
}

void UIEditLightAnim::OnCreateItem(const char* path)
{
	LALib.AppendItem(path, nullptr);;
	InitializeItems();
	m_Items->SelectItem(path);
	OnModified();
}

void UIEditLightAnim::OnRemoveItem(UIItemListForm::Node& node)
{
	EItemType type = TYPE_FOLDER;
	if (node.IsObject())
	{
		type = TYPE_OBJECT;
	}
	bool res = false;
	if (m_CurrentItem != nullptr && m_CurrentItem->cName == node.Name.c_str()) {
		OnItemFocused(nullptr);
	}
	LALib.RemoveObject(node.Name.c_str(), type, res);

	OnModified();
}

void UIEditLightAnim::OnRenameItem(UIItemListForm::Node& node, const char* old_full_name, const char* new_full_name, EItemType type)
{
	bool res = false;
	LALib.RenameObject(old_full_name, new_full_name,type);

	OnModified();
}
