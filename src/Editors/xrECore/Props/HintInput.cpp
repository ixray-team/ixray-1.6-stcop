#include "stdafx.h"
#include "HintInput.h"
#include "imgui_internal.h"


// One input has the focus: the state of its list
struct SHintState
{
	ImGuiID Id = 0;
	xr_string Before;
	int WordStart = 0;
	int Cursor = 0;
	xr_vector<xr_string> Items;
	int Current = 0;
	int Move = 0;
	bool Forced = false;
	// A hint clicked in the list, put into the text when the input is active again
	xr_string Pending;
	bool ListHeld = false;
};

static SHintState HintState;

static void ReplaceWord(ImGuiInputTextCallbackData* Data, const xr_string& Text)
{
	const int Start = std::clamp(HintState.WordStart, 0, Data->BufTextLen);
	const int End = std::clamp(HintState.Cursor, Start, Data->BufTextLen);
	Data->DeleteChars(Start, End - Start);
	Data->InsertChars(Start, Text.c_str());
	Data->CursorPos = Start + (int)Text.size();
	Data->SelectionStart = Data->SelectionEnd = Data->CursorPos;
	HintState.Items.clear();
	HintState.Forced = false;
}

static int HintCallback(ImGuiInputTextCallbackData* Data)
{
	if (Data->EventFlag == ImGuiInputTextFlags_CallbackCompletion)
	{
		if (!HintState.Items.empty())
		{
			ReplaceWord(Data, HintState.Items[HintState.Current]);
		}
		return 0;
	}
	if (Data->EventFlag == ImGuiInputTextFlags_CallbackHistory)
	{
		HintState.Move += Data->EventKey == ImGuiKey_UpArrow ? -1 : 1;
		return 0;
	}

	if (!HintState.Pending.empty())
	{
		ReplaceWord(Data, HintState.Pending);
		HintState.Pending.clear();
	}

	HintState.Cursor = Data->CursorPos;
	HintState.Before.assign(Data->Buf, Data->CursorPos);
	int Start = Data->CursorPos;
	while (Start > 0 && HintInput::IsHintChar(Data->Buf[Start - 1]))
	{
		--Start;
	}
	HintState.WordStart = Start;
	return 0;
}

namespace HintInput
{
	bool InputText(const char* Label, char* Buffer, size_t BufferSize, const TOnHintEvent& Hints, const ImVec2& Size, ImGuiInputTextFlags Flags)
	{
		const bool Multiline = Size.y > 0.0f;
		if (Hints.empty())
		{
			return Multiline ? ImGui::InputTextMultiline(Label, Buffer, BufferSize, Size, Flags) : ImGui::InputText(Label, Buffer, BufferSize, Flags);
		}

		const ImGuiID Id = ImGui::GetID(Label);
		const bool Owner = HintState.Id == Id;
		const bool Open = Owner && !HintState.Items.empty();

		// Up and Down move the cursor of a multiline input: its list is picked by Tab or the mouse
		Flags |= ImGuiInputTextFlags_CallbackAlways | ImGuiInputTextFlags_CallbackCompletion;
		Flags &= ~ImGuiInputTextFlags_AllowTabInput;
		if (!Multiline && Open)
		{
			Flags |= ImGuiInputTextFlags_CallbackHistory;
		}

		if (ImGui::IsWindowFocused(ImGuiFocusedFlags_RootAndChildWindows) && ImGui::GetActiveID() == Id && ImGui::IsKeyChordPressed(ImGuiMod_Ctrl | ImGuiKey_Space))
		{
			HintState.Forced = true;
		}

		bool Changed = Multiline ? ImGui::InputTextMultiline(Label, Buffer, BufferSize, Size, Flags, HintCallback) : ImGui::InputText(Label, Buffer, BufferSize, Flags, HintCallback);
		const bool Active = ImGui::IsItemActive();
		const ImRect Rect = { ImGui::GetItemRectMin(), ImGui::GetItemRectMax() };

		if (Active && HintState.Id != Id)
		{
			HintState = {};
			HintState.Id = Id;
		}
		if (HintState.Id != Id || (!Active && !HintState.ListHeld && HintState.Pending.empty()))
		{
			if (HintState.Id == Id)
			{
				HintState.Items.clear();
			}
			return Changed;
		}

		if (Active)
		{
			const xr_string Word = HintState.Before.substr(HintState.WordStart).c_str();
			xr_vector<xr_string> All;
			if (!Word.empty() || HintState.Forced)
			{
				Hints(HintState.Before, All);
			}

			HintState.Items.clear();
			for (xr_string& Item : All)
			{
				if (HintInput::StartsWithNoCase(Item, Word) && Item != Word)
				{
					HintState.Items.push_back(std::move(Item));
				}
			}
			std::sort(HintState.Items.begin(), HintState.Items.end());
			HintState.Items.erase(std::unique(HintState.Items.begin(), HintState.Items.end()), HintState.Items.end());
			if (HintState.Items.size() > HintInput::MaxHints)
			{
				HintState.Items.resize(HintInput::MaxHints);
			}
			HintState.Current = std::clamp(HintState.Current + HintState.Move, 0, std::max(0, (int)HintState.Items.size() - 1));
		}

		const int Move = HintState.Move;
		HintState.Move = 0;
		HintState.ListHeld = false;
		if (HintState.Items.empty())
		{
			return Changed;
		}

		const float Row = ImGui::GetTextLineHeightWithSpacing();
		const int Rows = std::min((int)HintState.Items.size(), HintInput::MaxVisible);
		const ImGuiStyle& Style = ImGui::GetStyle();
		float Width = 200.0f;
		for (const xr_string& Item : HintState.Items)
		{
			Width = std::max(Width, ImGui::CalcTextSize(Item.c_str()).x + Style.ScrollbarSize);
		}
		const ImVec2 ListSize = { std::min(Width + Style.WindowPadding.x * 2.0f, 600.0f), Rows * Row + Style.WindowPadding.y * 2.0f };

		// A window of its own would close the popup of a multiline editor: its list goes under it
		const bool InPopup = GImGui->BeginPopupStack.Size > 0;
		bool Drawn = false;
		if (InPopup)
		{
			Drawn = ImGui::BeginChild("##hints", ListSize, ImGuiChildFlags_Borders, ImGuiWindowFlags_NoNav);
		}
		else
		{
			ImGui::SetNextWindowPos({ Rect.Min.x, Rect.Max.y });
			ImGui::SetNextWindowSize(ListSize);
			const ImGuiWindowFlags WindowFlags = ImGuiWindowFlags_NoDecoration | ImGuiWindowFlags_NoSavedSettings | ImGuiWindowFlags_NoFocusOnAppearing | ImGuiWindowFlags_NoNav | ImGuiWindowFlags_Tooltip;
			Drawn = ImGui::Begin("##hint_list", nullptr, WindowFlags);
		}

		if (Drawn)
		{
			HintState.ListHeld = ImGui::IsWindowHovered() && (ImGui::IsMouseDown(ImGuiMouseButton_Left) || ImGui::IsMouseReleased(ImGuiMouseButton_Left));
			for (int Index = 0; Index < (int)HintState.Items.size(); ++Index)
			{
				const bool Selected = Index == HintState.Current;
				if (ImGui::Selectable(HintState.Items[Index].c_str(), Selected))
				{
					HintState.Current = Index;
					HintState.Pending = HintState.Items[Index];
					ImGui::ActivateItemByID(Id);
				}
				if (Selected && Move != 0)
				{
					ImGui::SetScrollHereY();
				}
			}
		}
		if (InPopup)
		{
			ImGui::EndChild();
		}
		else
		{
			ImGui::End();
		}
		return Changed;
	}
}
