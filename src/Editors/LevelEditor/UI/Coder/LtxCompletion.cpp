#include "stdafx.h"
#include "LtxCompletion.h"
#include "CodeEditorUtils.h"
#include "../LayoutEditor/LuaCompletion.h"
#include "IconsFontAwesome6.h"
#include "imgui_internal.h"

static constexpr int MaxDepth = 32;
static constexpr u32 MaxItems = 64;

static bool IsKeyChar(char Char)
{
	return isalnum((u8)Char) || Char == '_' || Char == '$' || Char == '.';
}

static xr_string Trim(const xr_string& Text)
{
	size_t Begin = 0;
	size_t End = Text.size();
	while (Begin < End && isspace((u8)Text[Begin]))
	{
		++Begin;
	}
	while (End > Begin && isspace((u8)Text[End - 1]))
	{
		--End;
	}
	return Text.substr(Begin, End - Begin).c_str();
}

static xr_vector<xr_string> Split(const xr_string& Text, char Separator)
{
	xr_vector<xr_string> Parts;
	size_t Start = 0;
	for (size_t At = Text.find(Separator); ; At = Text.find(Separator, Start))
	{
		const xr_string Part = Trim(Text.substr(Start, At == xr_string::npos ? xr_string::npos : At - Start).c_str());
		if (!Part.empty())
		{
			Parts.push_back(Part);
		}
		if (At == xr_string::npos)
		{
			break;
		}
		Start = At + 1;
	}
	return Parts;
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

static bool IsBoolType(const xr_string& Type)
{
	return Type == "bool" || Type == "BOOL";
}

xr_string CLtxCompletion::GetSchemaPath()
{
	string_path Path;
	FS.update_path(Path, "$app_data_root$", "ltx_schema.txt");
	return Path;
}

bool CLtxCompletion::Load()
{
	const u32 Generation = CLuaCompletion::GetBakeGeneration();
	if (LoadedGeneration == Generation)
	{
		return IsLoaded();
	}
	LoadedGeneration = Generation;
	Schemas.clear();
	ClassIds.clear();

	if (!FS.path_exist("$app_data_root$") || !FS.exist(GetSchemaPath().c_str()))
	{
		Msg("! ltx schema: no %s, bake the API", GetSchemaPath().c_str());
		return false;
	}

	IReader* Reader = FS.r_open(GetSchemaPath().c_str());
	if (Reader == nullptr)
	{
		return false;
	}

	string4096 Buffer;
	while (!Reader->eof())
	{
		Reader->r_string(Buffer, sizeof(Buffer));
		const xr_string Line = Buffer;
		if (Line.empty() || Line[0] == '#')
		{
			continue;
		}

		xr_vector<xr_string> Parts;
		size_t Start = 0;
		for (size_t Tab = Line.find('\t'); ; Tab = Line.find('\t', Start))
		{
			Parts.push_back(Line.substr(Start, Tab == xr_string::npos ? xr_string::npos : Tab - Start).c_str());
			if (Tab == xr_string::npos)
			{
				break;
			}
			Start = Tab + 1;
		}

		if (Parts[0] == "S" && Parts.size() >= 4)
		{
			Schemas[Parts[1]].Bases = Split(Parts[3], ',');
		}
		else if (Parts[0] == "F" && Parts.size() >= 7)
		{
			Schemas[Parts[1]].Fields.push_back({ Parts[2], Parts[3], Parts[4] == "1", Parts[5], Parts[6] == "1" });
		}
		else if (Parts[0] == "O" && Parts.size() >= 3)
		{
			ClassIds[Parts[1]].push_back(Parts[2]);
		}
	}
	FS.r_close(Reader);
	Msg("* ltx schema: %u schemas, %u class ids from %s", (u32)Schemas.size(), (u32)ClassIds.size(), GetSchemaPath().c_str());
	return IsLoaded();
}

xr_string CLtxCompletion::GetStatus() const
{
	string256 Text;
	xr_sprintf(Text, "ltx schema: %u schemas, %u class ids, %u sections", (u32)Schemas.size(), (u32)ClassIds.size(), (u32)Sections.size());
	return Text;
}

void CLtxCompletion::BuildLanguage(TextEditor::LanguageDefinition& Out) const
{
	Out = CodeEditor::GetIniLanguage();

	// Hover: the type of a key and the schemas that read it
	xr_hash_map<xr_string, xr_string> Declarations;
	for (const auto& [Name, Schema] : Schemas)
	{
		xr_vector<SKey> Keys;
		ExpandSchema(Name, "", Keys);
		for (const SKey& Key : Keys)
		{
			xr_string& Declaration = Declarations[Key.Key];
			const xr_string Line = Key.Type + " " + Key.Key + (Key.Required ? ", required" : "") + (Key.If.empty() ? "" : ", if " + Key.If) + " (" + Key.Schema + ")";
			if (Declaration.find(Line) == xr_string::npos)
			{
				Declaration += (Declaration.empty() ? "" : "\n") + Line;
			}
		}
	}

	for (const auto& [Key, Declaration] : Declarations)
	{
		TextEditor::Identifier Id;
		Id.mDeclaration = Declaration.c_str();
		Out.mIdentifiers.insert({ Key.c_str(), Id });
	}
}

void CLtxCompletion::ParseText(const xr_string& Text, SectionMap& Out, xr_vector<xr_string>* Lines)
{
	xr_string Current;
	int LineIndex = 0;
	size_t Start = 0;
	while (Start <= Text.size())
	{
		size_t End = Text.find('\n', Start);
		if (End == xr_string::npos)
		{
			End = Text.size();
		}
		xr_string Line = Text.substr(Start, End - Start).c_str();
		Start = End + 1;

		const size_t Comment = Line.find(';');
		if (Comment != xr_string::npos)
		{
			Line.resize(Comment);
		}
		Line = Trim(Line);

		// DLTX: ![section] and @[section] override, !key deletes
		bool Override = false;
		if (!Line.empty() && (Line[0] == '!' || Line[0] == '@') && Line.size() > 1 && Line[1] == '[')
		{
			Line.erase(0, 1);
			Override = true;
		}

		if (!Line.empty() && Line[0] == '[')
		{
			const size_t Close = Line.find(']');
			if (Close != xr_string::npos)
			{
				Current = Trim(Line.substr(1, Close - 1).c_str());
				SSection& Section = Out[Current];
				Section.Line = LineIndex;
				Section.Override = Override;
				const size_t Colon = Line.find(':', Close);
				if (Colon != xr_string::npos)
				{
					for (const xr_string& Parent : Split(Line.substr(Colon + 1).c_str(), ','))
					{
						Section.Parents.push_back(Parent);
					}
				}
			}
		}
		else if (!Line.empty() && Line[0] != '#' && !Current.empty())
		{
			const size_t Assign = Line.find('=');
			xr_string Key = Trim(Assign != xr_string::npos ? Line.substr(0, Assign).c_str() : Line);
			if (!Key.empty() && Key[0] == '!')
			{
				Key.erase(0, 1);
			}
			if (!Key.empty())
			{
				Out[Current].Keys[Key] = Assign != xr_string::npos ? Trim(Line.substr(Assign + 1).c_str()) : xr_string();
			}
		}

		if (Lines != nullptr)
		{
			Lines->push_back(Current);
		}
		++LineIndex;
		if (End == Text.size())
		{
			break;
		}
	}
}

void CLtxCompletion::BuildIndex()
{
	Indexed = true;
	Sections.clear();

	string_path Root;
	FS.update_path(Root, _game_config_, "");

	std::error_code Code;
	for (const auto& Entry : std::filesystem::recursive_directory_iterator(Root, Code))
	{
		if (!Entry.is_regular_file() || Entry.path().extension() != ".ltx")
		{
			continue;
		}

		IReader* Reader = FS.r_open(Entry.path().string().c_str());
		if (Reader == nullptr)
		{
			continue;
		}

		xr_string Text((const char*)Reader->pointer(), Reader->length());
		FS.r_close(Reader);
		std::erase(Text, '\r');

		SectionMap File;
		ParseText(Text, File);
		for (auto& [Name, Section] : File)
		{
			// DLTX overrides merge into the section
			SSection& Merged = Sections[Name];
			if (!Section.Override && Merged.Path.empty())
			{
				Merged.Path = Entry.path().string().c_str();
				Merged.Line = Section.Line;
			}
			if (Merged.Parents.empty())
			{
				Merged.Parents = Section.Parents;
			}
			for (auto& [Key, Value] : Section.Keys)
			{
				Merged.Keys[Key] = Value;
			}
		}
	}
	Msg("* ltx index: %u sections from %s", (u32)Sections.size(), Root);
}

bool CLtxCompletion::FindDefinition(const xr_string& Name, xr_string& Path, int& Line)
{
	if (!Indexed)
	{
		BuildIndex();
	}

	auto Own = Local.find(Name);
	if (Own != Local.end() && !Own->second.Override)
	{
		Path = LocalPath;
		Line = Own->second.Line;
		return true;
	}

	auto It = Sections.find(Name);
	if (It == Sections.end() || It->second.Path.empty())
	{
		return false;
	}
	Path = It->second.Path;
	Line = It->second.Line;
	return true;
}

const CLtxCompletion::SSection* CLtxCompletion::FindSection(const xr_string& Name) const
{
	auto It = Local.find(Name);
	if (It != Local.end())
	{
		return &It->second;
	}
	It = Sections.find(Name);
	return It != Sections.end() ? &It->second : nullptr;
}

xr_string CLtxCompletion::GetClassId(const xr_string& Name, int Depth) const
{
	const SSection* Section = FindSection(Name);
	if (Section == nullptr || Depth > MaxDepth)
	{
		return "";
	}

	auto Class = Section->Keys.find("class");
	if (Class != Section->Keys.end())
	{
		return Class->second;
	}

	// The last parent wins, like the values of the keys
	for (auto Parent = Section->Parents.rbegin(); Parent != Section->Parents.rend(); ++Parent)
	{
		const xr_string Id = GetClassId(*Parent, Depth + 1);
		if (!Id.empty())
		{
			return Id;
		}
	}
	return "";
}

bool CLtxCompletion::HasKey(const xr_string& Name, const xr_string& Key, int Depth) const
{
	const SSection* Section = FindSection(Name);
	if (Section == nullptr || Depth > MaxDepth)
	{
		return false;
	}
	if (Section->Keys.contains(Key))
	{
		return true;
	}
	for (const xr_string& Parent : Section->Parents)
	{
		if (HasKey(Parent, Key, Depth + 1))
		{
			return true;
		}
	}
	return false;
}

void CLtxCompletion::ExpandSchema(const xr_string& Name, const xr_string& Prefix, xr_vector<SKey>& Out, int Depth) const
{
	auto It = Schemas.find(Name);
	if (It == Schemas.end() || Depth > MaxDepth)
	{
		return;
	}

	for (const xr_string& Base : It->second.Bases)
	{
		ExpandSchema(Base, Prefix, Out, Depth + 1);
	}

	for (const SField& Field : It->second.Fields)
	{
		if (Field.Nested)
		{
			// The nested struct reads its keys with the prefix: hud_offset_pos
			const size_t Scope = Field.Type.rfind("::");
			ExpandSchema(Scope != xr_string::npos ? xr_string(Field.Type.substr(Scope + 2).c_str()) : Field.Type, Prefix + Field.Key, Out, Depth + 1);
			continue;
		}

		const xr_string Key = Prefix + Field.Key;
		if (std::none_of(Out.begin(), Out.end(), [&Key](const SKey& Other) { return Other.Key == Key; }))
		{
			Out.push_back({ Key, Field.Type, Field.Required, Field.If.empty() ? xr_string() : Prefix + Field.If, Name });
		}
	}
}

xr_vector<CLtxCompletion::SKey> CLtxCompletion::GetKeys(const xr_string& Section) const
{
	xr_vector<SKey> Keys;
	auto Class = ClassIds.find(GetClassId(Section));
	if (Class != ClassIds.end())
	{
		for (const xr_string& Schema : Class->second)
		{
			ExpandSchema(Schema, "", Keys);
		}
	}
	return Keys;
}

void CLtxCompletion::UpdateMarkers(TextEditor& View, const xr_string& Path, const xr_string& Text)
{
	if (!Load())
	{
		return;
	}
	if (!Indexed)
	{
		BuildIndex();
	}

	Local.clear();
	LocalLines.clear();
	LocalPath = Path;
	ParseText(Text, Local, &LocalLines);

	View.ClearErrorMarkers();
	for (const auto& [Name, Section] : Local)
	{
		xr_string Missing;
		for (const SKey& Key : GetKeys(Name))
		{
			// The If keys depend on the values, they are not checked
			if (Key.Required && Key.If.empty() && !HasKey(Name, Key.Key))
			{
				Missing += (Missing.empty() ? "" : ", ") + Key.Key;
			}
		}
		if (!Missing.empty())
		{
			View.AddErrorMarker(Section.Line + 1, ("Missing required keys: " + Missing).c_str());
		}
	}
}

void CLtxCompletion::BeforeRender(TextEditor& View)
{
	Move = 0;
	AcceptRequest = false;
	CloseRequest = false;
	KeysTaken = false;

	// A click focuses the list before it is drawn
	const ImGuiWindow* Focused = GImGui->NavWindow;
	const bool ListFocused = Open && Focused != nullptr && Focused->RootWindow != nullptr && strstr(Focused->RootWindow->Name, "##ltx_completion") != nullptr;
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

	if (Open && !Items.empty())
	{
		if (ImGui::IsKeyPressed(ImGuiKey_DownArrow))
		{
			Move = 1;
		}
		else if (ImGui::IsKeyPressed(ImGuiKey_UpArrow))
		{
			Move = -1;
		}
		else if (ImGui::IsKeyPressed(ImGuiKey_Enter) || ImGui::IsKeyPressed(ImGuiKey_KeypadEnter) || ImGui::IsKeyPressed(ImGuiKey_Tab))
		{
			AcceptRequest = true;
		}
		else if (ImGui::IsKeyPressed(ImGuiKey_Escape))
		{
			CloseRequest = true;
		}
		KeysTaken |= Move != 0 || AcceptRequest || CloseRequest;
	}

	if (KeysTaken)
	{
		View.SetHandleKeyboardInputs(false);
	}
}

void CLtxCompletion::Accept(TextEditor& View)
{
	if (Current < 0 || Current >= (int)Items.size())
	{
		return;
	}

	const TextEditor::Coordinates Cursor = View.GetCursorPosition();
	if (!Word.empty())
	{
		View.SetSelection({ Cursor.mLine, std::max(0, Cursor.mColumn - (int)Word.size()) }, Cursor);
		View.Delete();
	}
	View.InsertText((Items[Current].Name + (AppendAssign ? " = " : "")).c_str());
}

void CLtxCompletion::AfterRender(TextEditor& View, const xr_string& Path, const xr_string& Text)
{
	if (KeysTaken)
	{
		View.SetHandleKeyboardInputs(true);
	}

	if (!Load())
	{
		Open = false;
		return;
	}

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

	if (View.IsTextChanged() || LocalPath != Path)
	{
		UpdateMarkers(View, Path, Text);
	}

	char Buffer[512];
	View.CopyTextBeforeCursor(Buffer, sizeof(Buffer));
	const xr_string Prefix = Buffer;
	const int CursorLine = View.GetCursorPosition().mLine;

	size_t WordStart = Prefix.size();
	while (WordStart > 0 && IsKeyChar(Prefix[WordStart - 1]))
	{
		--WordStart;
	}
	Word = Prefix.substr(WordStart).c_str();

	const bool InComment = Prefix.find(';') != xr_string::npos;
	const size_t Assign = Prefix.find('=');
	const bool IsValue = Assign != xr_string::npos;
	const bool InHeader = Trim(Prefix).starts_with("[");
	// A key starts the line: only spaces before it
	const bool IsKey = !IsValue && Trim(Prefix.substr(0, WordStart).c_str()).empty();

	if (View.IsTextChanged() || ForceOpen)
	{
		const bool Typed = !Prefix.empty() && (IsKeyChar(Prefix.back()) || (IsValue && (Prefix.back() == '=' || Prefix.back() == ' ')));
		Open = !InComment && !InHeader && (IsKey || IsValue) && (ForceOpen || Typed);
		Current = 0;
		Line = CursorLine;
	}
	ForceOpen = false;

	if (Open && (CursorLine != Line || InComment || InHeader || !(IsKey || IsValue)))
	{
		Open = false;
	}

	if (Open)
	{
		const xr_string Section = CursorLine >= 0 && CursorLine < (int)LocalLines.size() ? LocalLines[CursorLine] : xr_string();
		xr_vector<SItem> All;
		AppendAssign = !IsValue;

		if (IsValue)
		{
			const xr_string Key = Trim(Prefix.substr(0, Assign).c_str());
			if (Key == "class")
			{
				for (const auto& [Id, Names] : ClassIds)
				{
					xr_string Detail;
					for (const xr_string& Name : Names)
					{
						Detail += (Detail.empty() ? "" : ", ") + Name;
					}
					All.push_back({ Id, Detail, false });
				}
			}
			else
			{
				for (const SKey& Field : GetKeys(Section))
				{
					if (Field.Key == Key && IsBoolType(Field.Type))
					{
						All.push_back({ "true", "bool", false });
						All.push_back({ "false", "bool", false });
						break;
					}
				}
			}
		}
		else if (!Section.empty())
		{
			auto Own = Local.find(Section);
			for (const SKey& Field : GetKeys(Section))
			{
				// The keys of the section itself are written already, the inherited ones can be overridden
				if (Own != Local.end() && Own->second.Keys.contains(Field.Key))
				{
					continue;
				}
				All.push_back({ Field.Key, Field.Type + (Field.Required ? ", required" : "") + (Field.If.empty() ? "" : ", if " + Field.If) + " (" + Field.Schema + ")", true });
			}
		}

		Items.clear();
		for (SItem& Item : All)
		{
			if (StartsWithNoCase(Item.Name, Word) && Item.Name != Word)
			{
				Items.push_back(std::move(Item));
			}
		}
		std::sort(Items.begin(), Items.end(), [](const SItem& Left, const SItem& Right) { return Left.Name < Right.Name; });
		if (Items.size() > MaxItems)
		{
			Items.resize(MaxItems);
		}
		Open = !Items.empty();
		Current = std::clamp(Current + Move, 0, std::max(0, (int)Items.size() - 1));
	}

	if (!Open)
	{
		return;
	}

	const ImVec2 CursorPos = View.GetCursorScreenPos();
	const ImGuiWindowFlags Flags = ImGuiWindowFlags_NoDecoration | ImGuiWindowFlags_NoSavedSettings | ImGuiWindowFlags_NoFocusOnAppearing
		| ImGuiWindowFlags_NoNav | ImGuiWindowFlags_NoInputs | ImGuiWindowFlags_Tooltip;

	static constexpr int MaxVisible = 14;
	const float IconWidth = ImGui::GetFrameHeight();
	float Width = 280.0f;
	for (const SItem& Item : Items)
	{
		Width = std::max(Width, IconWidth + ImGui::CalcTextSize(Item.Name.c_str()).x);
	}
	const ImGuiStyle& Style = ImGui::GetStyle();
	const int Rows = std::min((int)Items.size(), MaxVisible);
	ImGui::SetNextWindowPos(CursorPos);
	ImGui::SetNextWindowSize({ std::min(Width + Style.WindowPadding.x * 2.0f + Style.ScrollbarSize, 700.0f), Rows * ImGui::GetTextLineHeightWithSpacing() + Style.WindowPadding.y * 2.0f });
	bool Clicked = false;
	bool Insert = false;
	if (ImGui::Begin("##ltx_completion", nullptr, Flags & ~ImGuiWindowFlags_NoInputs))
	{
		for (int Index = 0; Index < (int)Items.size(); ++Index)
		{
			const SItem& Item = Items[Index];
			const bool Selected = Index == Current;
			const ImVec2 Start = ImGui::GetCursorScreenPos();
			if (ImGui::Selectable(("##" + Item.Name).c_str(), Selected, ImGuiSelectableFlags_AllowDoubleClick))
			{
				Current = Index;
				Clicked = true;
				Insert = ImGui::IsMouseDoubleClicked(ImGuiMouseButton_Left);
			}
			ImDrawList* DrawList = ImGui::GetWindowDrawList();
			DrawList->AddText({ Start.x + 2.0f, Start.y }, Item.IsKey ? IM_COL32(100, 180, 255, 255) : IM_COL32(255, 175, 80, 255), Item.IsKey ? ICON_FA_TAG : ICON_FA_HASHTAG);
			DrawList->AddText({ Start.x + IconWidth, Start.y }, ImGui::GetColorU32(ImGuiCol_Text), Item.Name.c_str());
			if (Selected && Move != 0)
			{
				ImGui::SetScrollHereY();
			}
		}
	}
	ImGui::End();

	// The text view keeps the focus: the list stays open and the typing goes on
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

	if (!Items[Current].Detail.empty())
	{
		ImGui::SetNextWindowPos({ CursorPos.x, CursorPos.y - ImGui::GetTextLineHeightWithSpacing() * 2.0f });
		if (ImGui::Begin("##ltx_completion_detail", nullptr, Flags | ImGuiWindowFlags_AlwaysAutoResize))
		{
			ImGui::TextUnformatted(Items[Current].Detail.c_str());
		}
		ImGui::End();
	}
}
