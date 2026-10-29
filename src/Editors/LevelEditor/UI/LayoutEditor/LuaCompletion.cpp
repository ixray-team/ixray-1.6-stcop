#include "stdafx.h"
#include "LuaCompletion.h"
#include "IconsFontAwesome6.h"
#include "imgui_internal.h"
#include "../../../xrECore/Editor/EditorContext.h"

static constexpr u32 MaxItems = 64;
static constexpr int MaxBaseDepth = 32;

static bool IsWordChar(char Char)
{
	return isalnum((u8)Char) || Char == '_';
}

static bool StartsWithNoCase(const xr_string& Text, const xr_string& Prefix)
{
	if (Prefix.size() > Text.size())
	{
		return false;
	}

	for (size_t Index = 0; Index < Prefix.size(); ++Index)
	{
		if (tolower((u8)Text[Index]) != tolower((u8)Prefix[Index]))
		{
			return false;
		}
	}
	return true;
}

void CLuaCompletion::CollectMembers(const xr_string& ClassName, xr_vector<SItem>& Out, xr_hash_set<xr_string>& Seen, int Depth) const
{
	const auto& Classes = EContext.Visor->GetClasses();
	auto It = Classes.find(ClassName);
	if (It == Classes.end() || Depth > MaxBaseDepth)
	{
		return;
	}

	for (const auto& [Name, Function] : It->second.Methods)
	{
		if (Seen.insert(Name).second)
		{
			Out.push_back({ Name, ClassName + ":" + Name + "(" + Function.Overloads.front() + ")", EItemKind::Method });
		}
	}
	for (const xr_string& Name : It->second.Properties)
	{
		if (Seen.insert(Name).second)
		{
			Out.push_back({ Name, ClassName + "." + Name, EItemKind::Property });
		}
	}
	for (const xr_string& Name : It->second.Constants)
	{
		if (Seen.insert(Name).second)
		{
			Out.push_back({ Name, ClassName + "." + Name + " (constant)", EItemKind::Constant });
		}
	}

	for (const xr_string& Base : It->second.Bases)
	{
		CollectMembers(Base, Out, Seen, Depth + 1);
	}
}

void CLuaCompletion::BeforeRender(TextEditor& View)
{
	Move = 0;
	AcceptRequest = false;
	CloseRequest = false;
	KeysTaken = false;

	// A click focuses the list before it is drawn
	const ImGuiWindow* Focused = GImGui->NavWindow;
	const bool ListFocused = Open && Focused != nullptr && Focused->RootWindow != nullptr && strstr(Focused->RootWindow->Name, "##lua_completion") != nullptr;
	if (!ListFocused && !ImGui::IsWindowFocused(ImGuiFocusedFlags_RootAndChildWindows))
	{
		Open = false;
		return;
	}

	if (ListFocused)
	{
		return;
	}

	EditorWindow = GImGui->NavWindow;

	if (ImGui::IsKeyChordPressed(ImGuiMod_Ctrl | ImGuiKey_Space))
	{
		ForceOpen = true;
		KeysTaken = true;
	}

	if (!Open)
	{
		if (KeysTaken)
		{
			View.SetHandleKeyboardInputs(false);
		}
		return;
	}

	if (ImGui::IsKeyPressed(ImGuiKey_DownArrow))
	{
		Move = 1;
	}
	else if (ImGui::IsKeyPressed(ImGuiKey_UpArrow))
	{
		Move = -1;
	}
	else if (ImGui::IsKeyPressed(ImGuiKey_PageDown))
	{
		Move = 10;
	}
	else if (ImGui::IsKeyPressed(ImGuiKey_PageUp))
	{
		Move = -10;
	}
	else if (ImGui::IsKeyPressed(ImGuiKey_Enter) || ImGui::IsKeyPressed(ImGuiKey_KeypadEnter) || ImGui::IsKeyPressed(ImGuiKey_Tab))
	{
		AcceptRequest = true;
	}
	else if (ImGui::IsKeyPressed(ImGuiKey_Escape))
	{
		CloseRequest = true;
	}
	else
	{
		static constexpr ImGuiKey FilterKeys[] = { ImGuiKey_M, ImGuiKey_F, ImGuiKey_T, ImGuiKey_N };
		for (int Filter = 0; Filter < (int)EFilter::Count; ++Filter)
		{
			if (ImGui::IsKeyChordPressed(ImGuiMod_Alt | FilterKeys[Filter]))
			{
				Filters[Filter] = !Filters[Filter];
				KeysTaken = true;
			}
		}
	}

	KeysTaken |= Move != 0 || AcceptRequest || CloseRequest;
	if (KeysTaken)
	{
		View.SetHandleKeyboardInputs(false);
	}
}

void CLuaCompletion::Accept(TextEditor& View)
{
	if (Current < 0 || Current >= (int)Items.size())
	{
		return;
	}

	const SItem& Item = Items[Current];
	const TextEditor::Coordinates Cursor = View.GetCursorPosition();
	if (!Word.empty())
	{
		// The typed part is a word: no tabs, one column per byte
		View.SetSelection({ Cursor.mLine, std::max(0, Cursor.mColumn - (int)Word.size()) }, Cursor);
	}

	if (!IsCallable(Item.Kind))
	{
		View.ReplaceSelection(Item.Name.c_str());
		return;
	}

	// The cursor goes between the brackets, the signature hint shows up there
	View.ReplaceSelection((Item.Name + "()").c_str());
	const TextEditor::Coordinates End = View.GetCursorPosition();
	View.SetCursorPosition({ End.mLine, std::max(0, End.mColumn - 1) });
}

CLuaCompletion::EFilter CLuaCompletion::GetFilter(EItemKind Kind)
{
	switch (Kind)
	{
	case EItemKind::Function:	return EFilter::Functions;
	case EItemKind::Class:		return EFilter::Types;
	case EItemKind::Namespace:	return EFilter::Namespaces;
	default:					return EFilter::Members;
	}
}

bool CLuaCompletion::IsCallable(EItemKind Kind)
{
	return Kind == EItemKind::Method || Kind == EItemKind::Function || Kind == EItemKind::Class;
}

struct SKindStyle
{
	const char* Icon;
	ImU32 Color;
};

static SKindStyle GetKindStyle(int Kind)
{
	static const SKindStyle Styles[] =
	{
		{ ICON_FA_CUBE, IM_COL32(180, 130, 255, 255) },		// Method
		{ ICON_FA_WRENCH, IM_COL32(200, 200, 200, 255) },	// Property
		{ ICON_FA_LOCK, IM_COL32(120, 170, 255, 255) },		// Constant
		{ ICON_FA_CUBE, IM_COL32(180, 130, 255, 255) },		// Function
		{ ICON_FA_TAG, IM_COL32(100, 180, 255, 255) },		// Field
		{ ICON_FA_SHAPES, IM_COL32(255, 175, 80, 255) },	// Class
		{ ICON_FA_LAYER_GROUP, IM_COL32(200, 200, 200, 255) },	// Namespace
	};
	return Kind >= 0 && Kind < (int)std::size(Styles) ? Styles[Kind] : Styles[0];
}

int CLuaCompletion::DrawItem(const SItem& Item, bool Selected) const
{
	const SKindStyle Style = GetKindStyle((int)Item.Kind);
	const ImVec2 Start = ImGui::GetCursorScreenPos();
	const float IconWidth = ImGui::GetFrameHeight();

	const bool Clicked = ImGui::Selectable(("##" + Item.Name).c_str(), Selected, ImGuiSelectableFlags_AllowDoubleClick);
	ImDrawList* DrawList = ImGui::GetWindowDrawList();
	DrawList->AddText({ Start.x + 2.0f, Start.y }, Style.Color, Style.Icon);
	DrawList->AddText({ Start.x + IconWidth, Start.y }, ImGui::GetColorU32(ImGuiCol_Text), Item.Name.c_str());

	if (!Clicked)
	{
		return 0;
	}
	return ImGui::IsMouseDoubleClicked(ImGuiMouseButton_Left) ? 2 : 1;
}

static const char* FilterKeys[] = { "M", "F", "T", "N" };
static const char* FilterNames[] = { "Members (Alt+M)", "Functions (Alt+F)", "Types (Alt+T)", "Namespaces (Alt+N)" };
static const int FilterKinds[] = { 0, 3, 5, 6 };

float CLuaCompletion::GetFiltersWidth(const u32 (&Counts)[(int)EFilter::Count]) const
{
	const ImGuiStyle& Style = ImGui::GetStyle();
	float Width = 0.0f;
	for (int Filter = 0; Filter < (int)EFilter::Count; ++Filter)
	{
		if (Counts[Filter] != 0)
		{
			const xr_string Label = xr_string(GetKindStyle(FilterKinds[Filter]).Icon) + " " + FilterKeys[Filter];
			Width += ImGui::CalcTextSize(Label.c_str()).x + Style.FramePadding.x * 2.0f + Style.ItemSpacing.x;
		}
	}
	return Width;
}

bool CLuaCompletion::DrawFilters(const u32 (&Counts)[(int)EFilter::Count])
{
	bool Clicked = false;
	bool First = true;
	for (int Filter = 0; Filter < (int)EFilter::Count; ++Filter)
	{
		if (Counts[Filter] == 0)
		{
			continue;
		}

		if (!First)
		{
			ImGui::SameLine();
		}
		First = false;

		const SKindStyle Style = GetKindStyle(FilterKinds[Filter]);
		ImGui::PushStyleColor(ImGuiCol_Button, ImGui::GetStyleColorVec4(Filters[Filter] ? ImGuiCol_ButtonActive : ImGuiCol_FrameBg));
		ImGui::PushStyleColor(ImGuiCol_Text, Filters[Filter] ? ImGui::ColorConvertU32ToFloat4(Style.Color) : ImGui::GetStyleColorVec4(ImGuiCol_TextDisabled));
		const xr_string Label = xr_string(Style.Icon) + " " + FilterKeys[Filter] + "##filter" + FilterKeys[Filter];
		if (ImGui::Button(Label.c_str()))
		{
			Filters[Filter] = !Filters[Filter];
			Clicked = true;
		}
		ImGui::PopStyleColor(2);
		ImGui::SetItemTooltip("%s: %u", FilterNames[Filter], Counts[Filter]);
	}
	return Clicked;
}

void CLuaCompletion::AfterRender(TextEditor& View, const xr_string& Script, const FieldResolver& Fields)
{
	if (KeysTaken)
	{
		View.SetHandleKeyboardInputs(true);
	}

	const CXVisor& Visor = *EContext.Visor;
	if (!EContext.Visor->Load() || !Visor.IsApiLoaded())
	{
		Open = false;
		return;
	}
	const auto& Classes = Visor.GetClasses();
	const auto& Namespaces = Visor.GetNamespaces();
	const auto& NamespaceFields = Visor.GetNamespaceFields();

	if (CloseRequest)
	{
		Open = false;
		return;
	}

	if (AcceptRequest)
	{
		Accept(View);
		Open = false;
		return;
	}

	char Buffer[512];
	View.CopyTextBeforeCursor(Buffer, sizeof(Buffer));
	const xr_string Text = Buffer;

	size_t WordStart = Text.size();
	while (WordStart > 0 && IsWordChar(Text[WordStart - 1]))
	{
		--WordStart;
	}
	Word = Text.substr(WordStart).c_str();

	const char Separator = WordStart > 0 ? Text[WordStart - 1] : 0;
	const bool IsMember = Separator == '.' || Separator == ':';
	xr_string Expression;
	if (IsMember)
	{
		size_t ExpressionStart = WordStart - 1;
		while (ExpressionStart > 0 && (IsWordChar(Text[ExpressionStart - 1]) || Text[ExpressionStart - 1] == '.' || Text[ExpressionStart - 1] == ':'))
		{
			--ExpressionStart;
		}
		Expression = Text.substr(ExpressionStart, WordStart - 1 - ExpressionStart).c_str();
	}

	const bool InComment = Text.find("--") != xr_string::npos;
	const bool InString = std::count(Text.begin(), Text.end(), '"') % 2 == 1 || std::count(Text.begin(), Text.end(), '\'') % 2 == 1;
	const int CursorLine = View.GetCursorPosition().mLine;

	if (View.IsTextChanged() || ForceOpen)
	{
		const bool Typed = !Text.empty() && (IsWordChar(Text.back()) || Text.back() == '.' || Text.back() == ':');
		Open = !InComment && !InString && (ForceOpen || (Typed && (IsMember || Word.size() >= 2)));
		Current = 0;
		Line = CursorLine;
	}
	ForceOpen = false;

	if (Open && (CursorLine != Line || (!IsMember && Word.empty()) || InComment || InString))
	{
		Open = false;
	}

	if (Open)
	{
		xr_vector<SItem> All;
		xr_hash_set<xr_string> Seen;
		if (IsMember)
		{
			auto Namespace = Namespaces.find(Expression);
			if (Separator == '.' && Namespace != Namespaces.end())
			{
				for (const auto& [Name, Function] : Namespace->second)
				{
					All.push_back({ Name, Expression + "." + Name + "(" + Function.Overloads.front() + ")", EItemKind::Function });
				}

				auto NamespaceFieldList = NamespaceFields.find(Expression);
				if (NamespaceFieldList != NamespaceFields.end())
				{
					for (const auto& [Name, Type] : NamespaceFieldList->second)
					{
						if (!Namespace->second.contains(Name))
						{
							All.push_back({ Name, Expression + "." + Name + ": " + Type, EItemKind::Field });
						}
					}
				}
			}
			else
			{
				const xr_string Type = Visor.ResolveType(Expression, Script, Fields);
				if (!Type.empty())
				{
					CollectMembers(Type, All, Seen);
				}
				else
				{
					for (const auto& [ClassName, Class] : Classes)
					{
						CollectMembers(ClassName, All, Seen, MaxBaseDepth);
					}
				}
			}
		}
		else
		{
			xr_hash_map<xr_string, xr_string> ScriptFunctions;
			xr_hash_set<xr_string> ScriptVariables;
			CXVisor::CollectScriptNames(Script, ScriptFunctions, ScriptVariables);
			for (const auto& [Name, Params] : ScriptFunctions)
			{
				Seen.insert(Name);
				All.push_back({ Name, "function " + Name + "(" + Params + ")", EItemKind::Function });
			}
			for (const xr_string& Name : ScriptVariables)
			{
				Seen.insert(Name);
				All.push_back({ Name, "local " + Name, EItemKind::Field });
			}

			auto Globals = Namespaces.find("");
			if (Globals != Namespaces.end())
			{
				for (const auto& [Name, Function] : Globals->second)
				{
					if (!Seen.contains(Name))
					{
						All.push_back({ Name, Name + "(" + Function.Overloads.front() + ")", EItemKind::Function });
					}
				}
			}
			for (const auto& [Name, Functions] : Namespaces)
			{
				if (!Name.empty() && !Seen.contains(Name) && !(Globals != Namespaces.end() && Globals->second.contains(Name)))
				{
					All.push_back({ Name, "namespace " + Name + ", " + xr_string::ToString((u32)Functions.size()) + " functions", EItemKind::Namespace });
				}
			}
			for (const auto& [Name, Class] : Classes)
			{
				All.push_back({ Name, "class " + Name, EItemKind::Class });
			}

			auto GlobalFields = NamespaceFields.find("");
			if (GlobalFields != NamespaceFields.end())
			{
				for (const auto& [Name, Type] : GlobalFields->second)
				{
					if (!Seen.contains(Name) && !Namespaces.contains(Name) && !Classes.contains(Name) && !(Globals != Namespaces.end() && Globals->second.contains(Name)))
					{
						All.push_back({ Name, Name + ": " + Type, EItemKind::Field });
					}
				}
			}
		}

		Items.clear();
		std::fill(std::begin(FilterCounts), std::end(FilterCounts), 0u);
		for (SItem& Item : All)
		{
			if (!StartsWithNoCase(Item.Name, Word) || Item.Name == Word)
			{
				continue;
			}

			const int Filter = (int)GetFilter(Item.Kind);
			++FilterCounts[Filter];
			if (Filters[Filter])
			{
				Items.push_back(std::move(Item));
			}
		}
		std::sort(Items.begin(), Items.end(), [](const SItem& Left, const SItem& Right) { return Left.Name < Right.Name; });
		if (Items.size() > MaxItems)
		{
			Items.resize(MaxItems);
		}
		Open = std::any_of(std::begin(FilterCounts), std::end(FilterCounts), [](u32 Count) { return Count != 0; });
		Current = std::clamp(Current + Move, 0, std::max(0, (int)Items.size() - 1));
	}

	const ImVec2 CursorPos = View.GetCursorScreenPos();
	const ImGuiWindowFlags Flags = ImGuiWindowFlags_NoDecoration | ImGuiWindowFlags_AlwaysAutoResize | ImGuiWindowFlags_NoSavedSettings
		| ImGuiWindowFlags_NoFocusOnAppearing | ImGuiWindowFlags_NoNav | ImGuiWindowFlags_NoInputs | ImGuiWindowFlags_Tooltip;
	const ImGuiWindowFlags ListFlags = (Flags & ~(ImGuiWindowFlags_NoInputs | ImGuiWindowFlags_AlwaysAutoResize)) | ImGuiWindowFlags_NoScrollbar;

	if (Open)
	{
		static constexpr int MaxVisible = 14;
		const float IconWidth = ImGui::GetFrameHeight();
		float Width = 280.0f;
		for (const SItem& Item : Items)
		{
			Width = std::max(Width, IconWidth + ImGui::CalcTextSize(Item.Name.c_str()).x);
		}
		const ImGuiStyle& Style = ImGui::GetStyle();
		const int Rows = std::max(1, std::min((int)Items.size(), MaxVisible));
		const float ListHeight = Rows * ImGui::GetTextLineHeightWithSpacing();
		const float FooterHeight = Style.ItemSpacing.y * 2.0f + 1.0f + ImGui::GetFrameHeight();
		const float Height = ListHeight + FooterHeight + Style.ItemSpacing.y + Style.WindowPadding.y * 2.0f;
		Width = std::max(Width + Style.ScrollbarSize, GetFiltersWidth(FilterCounts));
		Width = std::min(Width + Style.WindowPadding.x * 2.0f, 700.0f);

		ImGui::SetNextWindowPos(CursorPos);
		ImGui::SetNextWindowSize({ Width, Height });
		bool Clicked = false;
		bool Insert = false;
		if (ImGui::Begin("##lua_completion", nullptr, ListFlags))
		{
			if (ImGui::BeginChild("##lua_completion_items", { 0.0f, ListHeight }, ImGuiChildFlags_None, ImGuiWindowFlags_NoNav))
			{
				for (int Index = 0; Index < (int)Items.size(); ++Index)
				{
					const bool Selected = Index == Current;
					const int Click = DrawItem(Items[Index], Selected);
					if (Click != 0)
					{
						Current = Index;
						Clicked = true;
						Insert = Click == 2;
					}
					if (Selected && Move != 0)
					{
						ImGui::SetScrollHereY();
					}
				}
				if (Items.empty())
				{
					ImGui::TextDisabled("Everything is filtered out");
				}
			}
			ImGui::EndChild();

			ImGui::Separator();
			Clicked |= DrawFilters(FilterCounts);
		}
		ImGui::End();

		if (Clicked && EditorWindow != nullptr)
		{
			ImGui::FocusWindow(EditorWindow);
		}
		if (Insert)
		{
			Accept(View);
			Open = false;
			return;
		}

		if (!Items.empty())
		{
			ImGui::SetNextWindowPos({ CursorPos.x, CursorPos.y - ImGui::GetTextLineHeightWithSpacing() * 2.0f });
			if (ImGui::Begin("##lua_completion_detail", nullptr, Flags))
			{
				ImGui::TextUnformatted(Items[Current].Detail.c_str());
			}
			ImGui::End();
		}
		return;
	}

	if (!InComment)
	{
		xr_string Name;
		if (const CXVisor::SLuaFunction* Function = Visor.FindCalledFunction(Text, Script, Fields, Name))
		{
			ImGui::SetNextWindowPos({ CursorPos.x, CursorPos.y - ImGui::GetTextLineHeightWithSpacing() * (Function->Overloads.size() + 1.5f) });
			if (ImGui::Begin("##lua_signature", nullptr, Flags))
			{
				for (const xr_string& Overload : Function->Overloads)
				{
					ImGui::Text("%s(%s)", Name.c_str(), Overload.c_str());
				}
			}
			ImGui::End();
		}
	}
}
