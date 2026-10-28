#include "stdafx.h"
#include "UILogicEditorForm.h"
#include "../../../xrEUI/IconsFontAwesome6.h"
#include "imgui_internal.h"
#include "../../Editor/Utils/ContentView.h"
#include "../../UI/Coder/UICoderForm.h"

#include <regex>

namespace ed = ax::NodeEditor;

UILogicEditorForm* UILogicEditorForm::Form = nullptr;

static const char* GraphWindow = "Logic Graph###LogicGraph";
static const char* DetailsWindow = "Details###LogicDetails";
static const char* OutlinerWindow = "Outliner###LogicOutliner";
static const char* MessagesWindow = "Message Log###LogicMessages";
static const char* DebuggerWindow = "Logic Debugger###LogicDebugger";
static const char* ContentWindow = "Content Browser##LG";
static const char* Utf8BomText = "\xEF\xBB\xBF";

static constexpr float PinSize = 14.0f;
static constexpr size_t UndoLimit = 128;

static const ImColor ColorInfoHas = ImColor(110, 200, 90);
static const ImColor ColorInfoHasNot = ImColor(230, 90, 80);
static const ImColor ColorFunction = ImColor(90, 170, 240);
static const ImColor ColorFunctionNot = ImColor(240, 150, 60);
static const ImColor ColorProbability = ImColor(230, 210, 80);
static const ImColor ColorEffect = ImColor(190, 120, 240);
static const ImColor ColorMissing = ImColor(240, 70, 70);
static const ImColor ColorDim = ImColor(150, 150, 150);
static const ImColor ColorFunctionHeader = ImColor(150, 95, 230);

// Mods keep their files in UTF-8 without a BOM: CP1251 text with letters is almost never valid UTF-8
static bool IsUtf8(const xr_string& Raw)
{
	bool HasMultiByte = false;
	for (size_t Index = 0; Index < Raw.size();)
	{
		const u8 Lead = (u8)Raw[Index];
		const size_t Length = Lead < 0x80 ? 1 : (Lead >> 5) == 0x6 ? 2 : (Lead >> 4) == 0xE ? 3 : (Lead >> 3) == 0x1E ? 4 : 0;
		if (Length == 0 || Index + Length > Raw.size())
		{
			return false;
		}
		for (size_t Next = 1; Next < Length; ++Next)
		{
			if (((u8)Raw[Index + Next] >> 6) != 0x2)
			{
				return false;
			}
		}
		HasMultiByte |= Length > 1;
		Index += Length;
	}
	return HasMultiByte;
}

static bool InputString(const char* Label, xr_string& Value, ImGuiInputTextFlags Flags = 0)
{
	char Buffer[2048] = {};
	const size_t Size = std::min(Value.size(), sizeof(Buffer) - 1);
	memcpy(Buffer, Value.data(), Size);
	if (ImGui::InputText(Label, Buffer, sizeof(Buffer), Flags))
	{
		Value = Buffer;
		return true;
	}
	return false;
}

static bool PassFilter(const char* Filter, const xr_string& Name)
{
	if (Filter[0] == 0)
	{
		return true;
	}
	xr_string Lower = Name;
	xr_string Key = Filter;
	xr_strlwr(Lower);
	xr_strlwr(Key);
	return Lower.find(Key) != xr_string::npos;
}

// Execution pin of UE: a pentagon pointing right
static void DrawExecPin(ImDrawList* DrawList, ImVec2 Min, float Size, ImColor Color, bool Filled)
{
	const float Inset = Size * 0.15f;
	const ImVec2 A = { Min.x + Inset, Min.y + Inset };
	const ImVec2 B = { Min.x + Size * 0.55f, Min.y + Inset };
	const ImVec2 C = { Min.x + Size - Inset, Min.y + Size * 0.5f };
	const ImVec2 D = { Min.x + Size * 0.55f, Min.y + Size - Inset };
	const ImVec2 E = { Min.x + Inset, Min.y + Size - Inset };
	const ImVec2 Points[] = { A, B, C, D, E };
	if (Filled)
	{
		DrawList->AddConvexPolyFilled(Points, 5, Color);
	}
	else
	{
		DrawList->AddPolyline(Points, 5, Color, ImDrawFlags_Closed, 1.5f);
	}
}

static ImColor TokenColor(const LogicCondlist::SToken& Token, bool Effect)
{
	if (Effect)
	{
		return Token.Kind == '-' ? ColorInfoHasNot : Token.Kind == '+' ? ColorInfoHas : ColorEffect;
	}
	switch (Token.Kind)
	{
	case '+':	return ColorInfoHas;
	case '-':	return ColorInfoHasNot;
	case '!':	return ColorFunctionNot;
	case '~':	return ColorProbability;
	default:	return ColorFunction;
	}
}

// Rounded chips, wrapped in the width
static void DrawTokenChips(const xr_vector<LogicCondlist::SToken>& Tokens, bool Effect, float Width)
{
	ImDrawList* DrawList = ImGui::GetWindowDrawList();
	const float StartX = ImGui::GetCursorScreenPos().x;
	float LineX = 0.0f;
	bool First = true;
	for (const LogicCondlist::SToken& Token : Tokens)
	{
		const xr_string Text = Token.Kind == '~' ? Token.Build() + "%" : Token.Build();
		const ImVec2 TextSize = ImGui::CalcTextSize(Text.c_str());
		const ImVec2 ChipSize = { TextSize.x + 10.0f, TextSize.y + 2.0f };
		if (!First && LineX + ChipSize.x <= Width)
		{
			ImGui::SameLine(0.0f, 4.0f);
		}
		else
		{
			LineX = 0.0f;
			if (!First)
			{
				ImGui::SetCursorScreenPos({ StartX, ImGui::GetCursorScreenPos().y });
			}
		}
		First = false;
		LineX += ChipSize.x + 4.0f;

		ImGui::Dummy(ChipSize);
		const ImVec2 Min = ImGui::GetItemRectMin();
		const ImVec2 Max = ImGui::GetItemRectMax();
		ImColor Color = TokenColor(Token, Effect);
		ImColor Fill = Color;
		Fill.Value.w = 0.18f;
		DrawList->AddRectFilled(Min, Max, Fill, 4.0f);
		DrawList->AddRect(Min, Max, Color, 4.0f);
		DrawList->AddText({ Min.x + 5.0f, Min.y + 1.0f }, Color, Text.c_str());
	}
}

ed::PinId UILogicEditorForm::InputPin(int Section)
{
	return ed::PinId(((u64)Section << 32) | 1);
}

ed::PinId UILogicEditorForm::OutputPin(int Section, int Line, int Branch)
{
	return ed::PinId(((u64)Section << 32) | ((u64)(Line + 1) << 16) | ((u64)(Branch + 1) << 2) | 2);
}

ed::PinId UILogicEditorForm::AddPin(int Section)
{
	return ed::PinId(((u64)Section << 32) | 3);
}

UILogicEditorForm::SPin UILogicEditorForm::DecodePin(ed::PinId Pin)
{
	const u64 Value = (u64)Pin.Get();
	SPin Result;
	Result.Section = (int)(Value >> 32);
	switch (Value & 3)
	{
	case 1:
		Result.IsInput = true;
		break;
	case 2:
		Result.Line = (int)((Value >> 16) & 0xFFFF) - 1;
		Result.Branch = (int)((Value >> 2) & 0x3FFF) - 1;
		break;
	default:
		Result.IsAdd = true;
		break;
	}
	return Result;
}

UILogicEditorForm::ESchemeKind UILogicEditorForm::GetKind(const SLogicSection& Section) const
{
	const xr_string Scheme = Section.Scheme();
	if (Scheme == "logic")
	{
		return ESchemeKind::Logic;
	}
	for (const auto& [Name, Kind] : Schemes)
	{
		if (Name == Scheme)
		{
			return Kind;
		}
	}
	if (Scheme.starts_with("sr_"))
	{
		return ESchemeKind::Restrictor;
	}
	if (Scheme.starts_with("ph_"))
	{
		return ESchemeKind::Item;
	}
	if (Scheme.starts_with("mob_"))
	{
		return ESchemeKind::Monster;
	}
	if (Scheme.starts_with("heli_"))
	{
		return ESchemeKind::Heli;
	}
	return ESchemeKind::Data;
}

ImColor UILogicEditorForm::KindColor(ESchemeKind Kind)
{
	switch (Kind)
	{
	case ESchemeKind::Logic:		return ImColor(170, 35, 35);
	case ESchemeKind::Stalker:		return ImColor(35, 90, 165);
	case ESchemeKind::Monster:		return ImColor(175, 100, 25);
	case ESchemeKind::Item:			return ImColor(55, 130, 60);
	case ESchemeKind::Heli:			return ImColor(115, 55, 155);
	case ESchemeKind::Restrictor:	return ImColor(25, 130, 130);
	default:						return ImColor(80, 80, 80);
	}
}

const char* UILogicEditorForm::KindIcon(ESchemeKind Kind)
{
	switch (Kind)
	{
	case ESchemeKind::Logic:		return ICON_FA_BOLT;
	case ESchemeKind::Stalker:		return ICON_FA_PERSON;
	case ESchemeKind::Monster:		return ICON_FA_PAW;
	case ESchemeKind::Item:			return ICON_FA_CUBE;
	case ESchemeKind::Heli:			return ICON_FA_HELICOPTER;
	case ESchemeKind::Restrictor:	return ICON_FA_VECTOR_SQUARE;
	default:						return ICON_FA_LIST;
	}
}

static const char* KindName(int Kind)
{
	static const char* Names[] = { "Logic", "Stalker", "Monster", "Physic object", "Helicopter", "Restrictor", "Data" };
	return Names[Kind];
}

UILogicEditorForm::SDocument::~SDocument()
{
	if (Editor != nullptr)
	{
		ed::DestroyEditor(Editor);
	}
	if (FunctionEditor != nullptr)
	{
		ed::DestroyEditor(FunctionEditor);
	}
}

static void BuildWorkspaceLayout()
{
	static bool Checked = false;
	if (Checked)
	{
		return;
	}

	ImGuiWindow* Viewport = ImGui::FindWindowByName("Render##0");
	ImGuiWindow* Graph = ImGui::FindWindowByName(GraphWindow);
	ImGuiWindow* Browser = ImGui::FindWindowByName(ContentWindow);
	if (Viewport == nullptr || Viewport->DockNode == nullptr || Graph == nullptr || Browser == nullptr)
	{
		return;
	}
	Checked = true;

	if (Graph->DockId != 0 && Browser->DockId != 0)
	{
		return;
	}

	ImGuiID Center = Viewport->DockId;
	ImGuiID Left = ImGui::DockBuilderSplitNode(Center, ImGuiDir_Left, 0.2f, nullptr, &Center);
	const ImGuiID Right = ImGui::DockBuilderSplitNode(Center, ImGuiDir_Right, 0.27f, nullptr, &Center);
	const ImGuiID Bottom = ImGui::DockBuilderSplitNode(Center, ImGuiDir_Down, 0.2f, nullptr, &Center);
	const ImGuiID LeftBottom = ImGui::DockBuilderSplitNode(Left, ImGuiDir_Down, 0.45f, nullptr, &Left);
	ImGui::DockBuilderDockWindow(ContentWindow, Left);
	ImGui::DockBuilderDockWindow(OutlinerWindow, LeftBottom);
	ImGui::DockBuilderDockWindow(DetailsWindow, Right);
	ImGui::DockBuilderDockWindow(MessagesWindow, Bottom);
	ImGui::DockBuilderDockWindow(DebuggerWindow, Bottom);
	ImGui::DockBuilderDockWindow(GraphWindow, Center);
	ImGui::DockBuilderFinish(ImGui::DockNodeGetRootNode(Viewport->DockNode)->ID);
}

UILogicEditorForm::UILogicEditorForm()
{
}

UILogicEditorForm::~UILogicEditorForm()
{
}

void UILogicEditorForm::Create(int Tab)
{
	R_ASSERT(Form == nullptr);
	Form = new UILogicEditorForm();
	Form->TabIndex = Tab;
	EContext.UI->Push(Form, false);
	Device.seqFrame.Add(Form);
	Form->SetStateName("LogicEditorForm");

	CContentView* Browser = new CContentView(ContentWindow);
	Browser->Init();
	Browser->TabIndex = Tab;
	Browser->SetStateName("LogicContentBrowser");
	EContext.UI->Push(Browser, false);
}

void UILogicEditorForm::Serialize(nlohmann::json& Data, bool IsSaving)
{
	IEditorWnd::Serialize(Data, IsSaving);
	if (IsSaving)
	{
		nlohmann::json Files = nlohmann::json::array();
		for (const xr_unique_ptr<SDocument>& Document : Documents)
		{
			Files.push_back(Document->Path.c_str());
		}
		Data["files"] = Files;
		Data["active"] = Active;
		return;
	}

	auto Iter = Data.find("files");
	if (Iter == Data.end() || !Iter->is_array())
	{
		return;
	}

	// Reopening the files must not switch the editor to this tab
	const int RequestedTab = EContext.UI->RequestedTabIndex;
	const int ActiveTab = EContext.UI->ActiveTabIndex;
	for (const nlohmann::json& File : *Iter)
	{
		if (File.is_string())
		{
			const xr_string Path = File.get_ref<const nlohmann::json::string_t&>().c_str();
			if (std::filesystem::exists(xr_path(Path)))
			{
				OpenFile(Path);
			}
		}
	}
	EContext.UI->RequestedTabIndex = RequestedTab;
	EContext.UI->ActiveTabIndex = ActiveTab;

	size_t Selected = 0;
	if (ReadState(Data, "active", Selected) && Selected < Documents.size())
	{
		SelectRequest = (int)Selected;
	}
}

void UILogicEditorForm::Destroy()
{
	if (Form != nullptr)
	{
		Device.seqFrame.Remove(Form);
	}
	xr_delete(Form);
}

bool UILogicEditorForm::IsActive()
{
	return Form != nullptr && EContext.UI->ActiveTabIndex == Form->TabIndex;
}

bool UILogicEditorForm::IsModified()
{
	if (Form == nullptr)
	{
		return false;
	}
	return std::any_of(Form->Documents.begin(), Form->Documents.end(), [](const xr_unique_ptr<SDocument>& Document) { return Document->Modified; });
}

bool UILogicEditorForm::CanOpen(const xr_string& FullPath)
{
	xr_string Path = FullPath;
	xr_strlwr(Path);
	std::replace(Path.begin(), Path.end(), '/', '\\');
	return Path.ends_with(".ltx") && Path.find("configs\\scripts\\") != xr_string::npos;
}

bool UILogicEditorForm::OpenFile(const xr_string& FullPath)
{
	if (Form == nullptr)
	{
		return false;
	}

	EContext.UI->RequestedTabIndex = Form->TabIndex;
	EContext.UI->ActiveTabIndex = Form->TabIndex;

	xr_string Key = FullPath;
	xr_strlwr(Key);
	for (size_t Index = 0; Index < Form->Documents.size(); ++Index)
	{
		xr_string Path = Form->Documents[Index]->Path;
		xr_strlwr(Path);
		if (Path == Key)
		{
			Form->SelectRequest = (int)Index;
			return true;
		}
	}

	Form->LoadCatalogs();

	xr_unique_ptr<SDocument> Document = xr_make_unique<SDocument>();
	Document->Path = FullPath;
	Document->Name = xr_path(FullPath).xfilename();
	if (!Form->Load(*Document))
	{
		ELog.DlgMsg(mtError, "Logic: can't read %s", FullPath.c_str());
		return false;
	}

	ed::Config Config;
	Config.SettingsFile = nullptr;
	Document->Editor = ed::CreateEditor(&Config);
	Document->FunctionEditor = ed::CreateEditor(&Config);

	Form->Documents.push_back(std::move(Document));
	Form->SelectRequest = (int)Form->Documents.size() - 1;
	return true;
}

bool UILogicEditorForm::Save()
{
	SDocument* Document = Form != nullptr ? Form->Current() : nullptr;
	return Document != nullptr && Form->SaveDocument(*Document);
}

bool UILogicEditorForm::Undo(bool Redo)
{
	SDocument* Document = Form != nullptr ? Form->Current() : nullptr;
	return Document != nullptr && Form->UndoDocument(*Document, Redo);
}

UILogicEditorForm::SDocument* UILogicEditorForm::Current()
{
	return Active < Documents.size() ? Documents[Active].get() : nullptr;
}

bool UILogicEditorForm::Load(SDocument& Document)
{
	IReader* Reader = FS.r_open(Document.Path.c_str());
	if (Reader == nullptr)
	{
		return false;
	}

	xr_string Raw((const char*)Reader->pointer(), Reader->length());
	FS.r_close(Reader);

	Document.UseCRLF = Raw.find("\r\n") != xr_string::npos || Raw.find('\n') == xr_string::npos;
	Document.Utf8Bom = Raw.starts_with(Utf8BomText);
	Document.Utf8 = Document.Utf8Bom || IsUtf8(Raw);

	xr_string Text = Document.Utf8Bom ? xr_string(Raw.substr(3).c_str()) : Document.Utf8 ? Raw : Platform::ANSI_TO_UTF8(Raw);
	std::erase(Text, '\r');

	Document.Logic.Load(Text);
	Document.Modified = false;
	Document.LayoutDelay = 2;
	Document.UndoStack.clear();
	Document.RedoStack.clear();
	Validate(Document);
	return true;
}

bool UILogicEditorForm::SaveDocument(SDocument& Document)
{
	const xr_string Text = Document.Logic.Save();
	xr_string Data = !Document.Utf8 ? xr_string(Platform::UTF8_to_CP1251(Text)) : Document.Utf8Bom ? xr_string(xr_string(Utf8BomText) + Text) : Text;
	if (Document.UseCRLF)
	{
		xr_string WithCRLF;
		WithCRLF.reserve(Data.size() + Data.size() / 16);
		for (char Char : Data)
		{
			if (Char == '\n')
			{
				WithCRLF += '\r';
			}
			WithCRLF += Char;
		}
		Data = WithCRLF;
	}

	IWriter* Writer = FS.w_open(Document.Path.c_str());
	if (Writer == nullptr)
	{
		ELog.DlgMsg(mtError, "Logic: can't write %s", Document.Path.c_str());
		return false;
	}
	Writer->w(Data.data(), Data.size());
	FS.w_close(Writer);

	Document.Modified = false;
	StatusText = "Saved " + Document.Path;
	ELog.Msg(mtInformation, "Logic: saved %s", Document.Path.c_str());
	return true;
}

bool UILogicEditorForm::CloseDocument(size_t Index)
{
	SDocument& Document = *Documents[Index];
	if (Document.Modified)
	{
		const int Result = ELog.DlgMsg(mtConfirmation, TMsgDlgButtons() | mbYes | mbNo | mbCancel, "%s has unsaved changes. Save them?", Document.Name.c_str());
		if (Result == mrCancel || (Result == mrYes && !SaveDocument(Document)))
		{
			return false;
		}
	}

	Documents.erase(Documents.begin() + Index);
	if (Active >= Documents.size())
	{
		Active = Documents.empty() ? 0 : Documents.size() - 1;
	}
	return true;
}

void UILogicEditorForm::Reload(SDocument& Document, const xr_string& Text)
{
	// The nodes keep their places by the section name
	xr_map<xr_string, ImVec2> Positions;
	xr_string SelectedName;
	for (const xr_unique_ptr<SLogicSection>& Section : Document.Logic.Sections)
	{
		Positions[Section->Name] = Section->Position;
		if (Section->Id == Document.Selected)
		{
			SelectedName = Section->Name;
		}
	}

	Document.Logic.Load(Text);
	Document.Selected = 0;
	for (xr_unique_ptr<SLogicSection>& Section : Document.Logic.Sections)
	{
		auto Iter = Positions.find(Section->Name);
		if (Iter != Positions.end())
		{
			Section->Position = Iter->second;
			Section->HasPosition = true;
		}
		if (Section->Name == SelectedName)
		{
			Document.Selected = Section->Id;
		}
	}
	Validate(Document);
}

void UILogicEditorForm::PushUndo(SDocument& Document)
{
	Document.UndoStack.push_back(Document.Logic.Save());
	if (Document.UndoStack.size() > UndoLimit)
	{
		Document.UndoStack.erase(Document.UndoStack.begin());
	}
	Document.RedoStack.clear();
	Document.Modified = true;
}

bool UILogicEditorForm::UndoDocument(SDocument& Document, bool Redo)
{
	xr_vector<xr_string>& From = Redo ? Document.RedoStack : Document.UndoStack;
	xr_vector<xr_string>& To = Redo ? Document.UndoStack : Document.RedoStack;
	if (From.empty())
	{
		return false;
	}

	To.push_back(Document.Logic.Save());
	const xr_string Text = From.back();
	From.pop_back();
	Reload(Document, Text);
	Document.Modified = true;
	return true;
}

void UILogicEditorForm::LoadCatalogs()
{
	if (CatalogsLoaded)
	{
		return;
	}
	CatalogsLoaded = true;

	auto ReadScript = [](const char* FileName, auto&& OnLine)
	{
		if (!FS.exist("$game_scripts$", FileName))
		{
			return;
		}
		IReader* Reader = FS.r_open("$game_scripts$", FileName);
		if (Reader == nullptr)
		{
			return;
		}
		xr_string Text((const char*)Reader->pointer(), Reader->length());
		FS.r_close(Reader);

		size_t Begin = 0;
		while (Begin < Text.size())
		{
			size_t End = Text.find('\n', Begin);
			if (End == xr_string::npos)
			{
				End = Text.size();
			}
			OnLine(Text.substr(Begin, End - Begin));
			Begin = End + 1;
		}
	};

	auto ReadFunctions = [&ReadScript](const char* FileName, xr_vector<xr_string>& Into)
	{
		ReadScript(FileName, [&Into](const xr_string& Line)
		{
			if (!Line.starts_with("function "))
			{
				return;
			}
			size_t Index = 9;
			xr_string Name;
			while (Index < Line.size() && (isalnum((u8)Line[Index]) || Line[Index] == '_'))
			{
				Name += Line[Index++];
			}
			// "function check_npc_name (actor, npc, p)"
			while (Index < Line.size() && (Line[Index] == ' ' || Line[Index] == '\t'))
			{
				++Index;
			}
			if (!Name.empty() && Index < Line.size() && Line[Index] == '(')
			{
				Into.push_back(Name);
			}
		});
		std::sort(Into.begin(), Into.end());
		Into.erase(std::unique(Into.begin(), Into.end()), Into.end());
	};

	ReadFunctions("xr_conditions.script", Conditions);
	ReadFunctions("xr_effects.script", Effects);

	// The switches and the overrides of xr_logic, the common stalker settings
	ReadScriptKeys("xr_logic.script", true, CommonKeys);
	ReadScriptKeys("stalker_generic.script", true, CommonKeys);
	static const char* SwitchKeys[] =
	{
		"on_info", "on_timer", "on_game_timer", "on_signal", "on_actor_dist_le", "on_actor_dist_le_nvis", "on_actor_dist_ge", "on_actor_dist_ge_nvis",
		"on_actor_inside", "on_actor_outside", "on_actor_in_zone", "on_actor_not_in_zone", "on_npc_in_zone", "on_npc_not_in_zone",
		"on_npc_inside", "on_npc_outside", "on_self_in_zone", "on_self_not_in_zone",
	};
	for (const char* Key : SwitchKeys)
	{
		if (std::none_of(CommonKeys.begin(), CommonKeys.end(), [Key](const SKeyInfo& Info) { return Info.Name == Key; }))
		{
			CommonKeys.push_back({ Key, "condlist", true });
		}
	}

	// [logic]: the active scheme, the job of the smart terrain
	static const std::pair<const char*, const char*> Logic[] =
	{
		{ "active", "condlist" }, { "on_death", "section" }, { "on_hit", "section" }, { "on_combat", "condlist" }, { "on_offline", "condlist" },
		{ "combat_ignore", "section" }, { "meet", "section" }, { "wounded", "section" }, { "danger", "section" }, { "actor_dialogs", "section" },
		{ "trade", "string" }, { "level_spot", "string" }, { "post_combat_time", "number" }, { "prior", "number" }, { "suitable", "condlist" },
		{ "can_select_weapon", "condlist" }, { "dont_spawn_character_supplies", "bool" }, { "no_smart", "bool" },
	};
	for (const auto& [Key, Type] : Logic)
	{
		LogicKeys.push_back({ Key, Type, false });
	}

	// load_scheme("xr_walker", "walker", stype_stalker)
	ReadScript("modules.script", [this](const xr_string& Line)
	{
		if (!Line.starts_with("load_scheme("))
		{
			return;
		}
		const size_t First = Line.find('"', Line.find(',') );
		const size_t Second = First == xr_string::npos ? xr_string::npos : Line.find('"', First + 1);
		if (Second == xr_string::npos)
		{
			return;
		}
		const xr_string Name = Line.substr(First + 1, Second - First - 1);
		const size_t FileBegin = Line.find('"');
		const size_t FileEnd = Line.find('"', FileBegin + 1);
		if (FileBegin < First && FileEnd < First)
		{
			SchemeFiles[Name] = Line.substr(FileBegin + 1, FileEnd - FileBegin - 1) + ".script";
		}
		ESchemeKind Kind = ESchemeKind::Data;
		if (Line.find("stype_stalker") != xr_string::npos)
		{
			Kind = ESchemeKind::Stalker;
		}
		else if (Line.find("stype_mobile") != xr_string::npos)
		{
			Kind = ESchemeKind::Monster;
		}
		else if (Line.find("stype_item") != xr_string::npos)
		{
			Kind = ESchemeKind::Item;
		}
		else if (Line.find("stype_heli") != xr_string::npos)
		{
			Kind = ESchemeKind::Heli;
		}
		else if (Line.find("stype_restrictor") != xr_string::npos)
		{
			Kind = ESchemeKind::Restrictor;
		}
		Schemes.emplace_back(Name, Kind);
	});

	if (Schemes.empty())
	{
		const std::pair<const char*, ESchemeKind> Defaults[] =
		{
			{ "walker", ESchemeKind::Stalker }, { "remark", ESchemeKind::Stalker }, { "animpoint", ESchemeKind::Stalker },
			{ "camper", ESchemeKind::Stalker }, { "patrol", ESchemeKind::Stalker }, { "meet", ESchemeKind::Stalker },
			{ "mob_walker", ESchemeKind::Monster }, { "mob_home", ESchemeKind::Monster },
			{ "ph_door", ESchemeKind::Item }, { "ph_idle", ESchemeKind::Item },
			{ "sr_idle", ESchemeKind::Restrictor }, { "sr_timer", ESchemeKind::Restrictor },
			{ "heli_move", ESchemeKind::Heli },
		};
		for (const auto& [Name, Kind] : Defaults)
		{
			Schemes.emplace_back(Name, Kind);
		}
	}
}

void UILogicEditorForm::ReadScriptKeys(const char* FileName, bool Common, xr_vector<SKeyInfo>& Into)
{
	if (!FS.exist("$game_scripts$", FileName))
	{
		return;
	}
	IReader* Reader = FS.r_open("$game_scripts$", FileName);
	if (Reader == nullptr)
	{
		return;
	}
	const std::string Text((const char*)Reader->pointer(), Reader->length());
	FS.r_close(Reader);

	// cfg_get_bool(ini, section, "use_camp", ...)
	static const std::regex Pattern(R"re(cfg_get_(\w+)\s*\(\s*[^,()]+,\s*[^,()]+,\s*"(\w+)")re");
	for (std::sregex_iterator Iter(Text.begin(), Text.end(), Pattern), End; Iter != End; ++Iter)
	{
		const xr_string Function = (*Iter)[1].str().c_str();
		const xr_string Name = (*Iter)[2].str().c_str();
		if (std::any_of(Into.begin(), Into.end(), [&Name](const SKeyInfo& Info) { return Info.Name == Name; }))
		{
			continue;
		}

		xr_string Type = "string";
		if (Function == "bool" || Function == "number")
		{
			Type = Function;
		}
		else if (Function.find("condlist") != xr_string::npos || Function == "switch_conditions")
		{
			Type = "condlist";
		}
		Into.push_back({ Name, Type, Common });
	}
}

const xr_vector<UILogicEditorForm::SKeyInfo>& UILogicEditorForm::GetKeys(const SLogicSection& Section)
{
	static const xr_vector<SKeyInfo> Empty;
	const xr_string Scheme = Section.Scheme();
	if (GetKind(Section) == ESchemeKind::Logic)
	{
		return LogicKeys;
	}

	auto Cached = SchemeKeys.find(Scheme);
	if (Cached != SchemeKeys.end())
	{
		return Cached->second;
	}

	auto File = SchemeFiles.find(Scheme);
	if (File == SchemeFiles.end())
	{
		return Empty;
	}

	xr_vector<SKeyInfo> Keys;
	ReadScriptKeys(File->second.c_str(), false, Keys);
	for (const SKeyInfo& Info : CommonKeys)
	{
		if (std::none_of(Keys.begin(), Keys.end(), [&Info](const SKeyInfo& Key) { return Key.Name == Info.Name; }))
		{
			Keys.push_back(Info);
		}
	}
	return SchemeKeys.emplace(Scheme, std::move(Keys)).first->second;
}

void UILogicEditorForm::Validate(SDocument& Document)
{
	Messages.clear();
	CLogicDocument& Logic = Document.Logic;

	xr_set<xr_string> Referenced;
	bool HasLogic = false;
	for (const xr_unique_ptr<SLogicSection>& Section : Logic.Sections)
	{
		// "[ph_door@close]:ph_door@def": a parent is used by its children
		size_t Begin = 0;
		while (Begin < Section->Parents.size())
		{
			size_t End = Section->Parents.find(',', Begin);
			if (End == xr_string::npos)
			{
				End = Section->Parents.size();
			}
			xr_string Parent = Section->Parents.substr(Begin, End - Begin);
			std::erase_if(Parent, [](char Char) { return isspace((u8)Char) != 0; });
			if (!Parent.empty())
			{
				Referenced.insert(Parent);
			}
			Begin = End + 1;
		}

		const ESchemeKind Kind = GetKind(*Section);
		if (Kind == ESchemeKind::Logic)
		{
			HasLogic = true;
			if (Section->FindKey("active") == nullptr)
			{
				Messages.push_back({ false, Section->Id, "[" + Section->Name + "] has no 'active' scheme" });
			}
		}

		// The [logic] list is not complete: only the scheme sections are checked
		if (Kind != ESchemeKind::Logic && Kind != ESchemeKind::Data)
		{
			const xr_vector<SKeyInfo>& Known = GetKeys(*Section);
			for (const SLogicLine& Line : Section->Lines)
			{
				if (!Line.IsKey || Known.empty())
				{
					continue;
				}
				// on_info2, on_timer3
				xr_string Name = Line.Key.Name;
				while (!Name.empty() && isdigit((u8)Name.back()))
				{
					Name.pop_back();
				}
				if (std::none_of(Known.begin(), Known.end(), [&Name](const SKeyInfo& Info) { return Info.Name == Name; }))
				{
					Messages.push_back({ false, Section->Id, "[" + Section->Name + "] '" + Line.Key.Name + "' is not read by the " + Section->Scheme() + " scheme" });
				}
			}
		}

		for (const SLogicLine& Line : Section->Lines)
		{
			if (!Line.IsKey || !Line.Key.IsCondlist)
			{
				continue;
			}
			for (const LogicCondlist::SBranch& Branch : Line.Key.Branches)
			{
				if (Branch.HasTarget())
				{
					Referenced.insert(Branch.Target);
					if (Logic.Find(Branch.Target) == nullptr && Kind != ESchemeKind::Data)
					{
						Messages.push_back({ true, Section->Id, "[" + Section->Name + "] " + Line.Key.Name + ": section '" + Branch.Target + "' is not in the file" });
					}
				}

				auto CheckFunctions = [&](const xr_vector<LogicCondlist::SToken>& Tokens, const xr_vector<xr_string>& Known, const char* What)
				{
					if (Known.empty())
					{
						return;
					}
					for (const LogicCondlist::SToken& Token : Tokens)
					{
						if ((Token.Kind == '=' || Token.Kind == '!') && !std::binary_search(Known.begin(), Known.end(), Token.Name))
						{
							Messages.push_back({ true, Section->Id, "[" + Section->Name + "] " + Line.Key.Name + ": unknown " + What + " '" + Token.Name + "'" });
						}
					}
				};
				CheckFunctions(Branch.Conditions, Conditions, "condition");
				CheckFunctions(Branch.Effects, Effects, "effect");
			}
		}
	}

	if (HasLogic)
	{
		for (const xr_unique_ptr<SLogicSection>& Section : Logic.Sections)
		{
			const ESchemeKind Kind = GetKind(*Section);
			if (Kind != ESchemeKind::Logic && Kind != ESchemeKind::Data && !Referenced.contains(Section->Name))
			{
				Messages.push_back({ false, Section->Id, "[" + Section->Name + "] is never switched to" });
			}
		}
	}
}

void UILogicEditorForm::Layout(SDocument& Document)
{
	CLogicDocument& Logic = Document.Logic;
	const size_t Count = Logic.Sections.size();
	xr_vector<int> Depth(Count, -1);
	xr_vector<bool> HasIncoming(Count, false);
	xr_vector<xr_vector<int>> Targets(Count);
	// The functions are not on the event graph
	xr_vector<bool> Skip(Count, false);

	xr_vector<SInherited> Inherited;
	for (size_t Index = 0; Index < Count; ++Index)
	{
		Skip[Index] = IsFunction(Logic, *Logic.Sections[Index]);

		xr_vector<const SLogicLine*> Lines;
		for (const SLogicLine& Line : Logic.Sections[Index]->Lines)
		{
			Lines.push_back(&Line);
		}
		CollectInherited(Logic, *Logic.Sections[Index], Inherited);
		for (const SInherited& Item : Inherited)
		{
			Lines.push_back(Item.Line);
		}

		for (const SLogicLine* LinePtr : Lines)
		{
			const SLogicLine& Line = *LinePtr;
			if (!Line.IsKey || !Line.Key.IsCondlist)
			{
				continue;
			}
			for (const LogicCondlist::SBranch& Branch : Line.Key.Branches)
			{
				const int Target = Branch.HasTarget() ? Logic.IndexOf(Branch.Target) : -1;
				if (Target >= 0 && Target != (int)Index)
				{
					Targets[Index].push_back(Target);
					HasIncoming[Target] = true;
				}
			}
		}
	}

	// The logic sections first, then the sections nothing switches to
	xr_vector<int> Roots;
	for (size_t Index = 0; Index < Count; ++Index)
	{
		if (!Skip[Index] && GetKind(*Logic.Sections[Index]) == ESchemeKind::Logic)
		{
			Roots.push_back((int)Index);
		}
	}
	for (size_t Index = 0; Index < Count; ++Index)
	{
		if (!Skip[Index] && !HasIncoming[Index] && std::find(Roots.begin(), Roots.end(), (int)Index) == Roots.end())
		{
			Roots.push_back((int)Index);
		}
	}

	auto Walk = [&](int Root)
	{
		if (Depth[Root] >= 0 || Skip[Root])
		{
			return;
		}
		xr_vector<int> Queue = { Root };
		Depth[Root] = 0;
		for (size_t Head = 0; Head < Queue.size(); ++Head)
		{
			const int Node = Queue[Head];
			for (int Target : Targets[Node])
			{
				if (Depth[Target] < 0 && !Skip[Target])
				{
					Depth[Target] = Depth[Node] + 1;
					Queue.push_back(Target);
				}
			}
		}
	};
	for (int Root : Roots)
	{
		Walk(Root);
	}
	// Cycles without an entry
	for (size_t Index = 0; Index < Count; ++Index)
	{
		Walk((int)Index);
	}

	int MaxDepth = 0;
	for (int Value : Depth)
	{
		MaxDepth = std::max(MaxDepth, Value);
	}

	constexpr float GapX = 110.0f;
	constexpr float GapY = 36.0f;
	float ColumnX = 0.0f;
	for (int Column = 0; Column <= MaxDepth; ++Column)
	{
		float ColumnWidth = 0.0f;
		float Y = 0.0f;
		for (size_t Index = 0; Index < Count; ++Index)
		{
			if (Depth[Index] != Column)
			{
				continue;
			}
			SLogicSection& Section = *Logic.Sections[Index];
			const ImVec2 Size = ed::GetNodeSize(Section.Id);
			ed::SetNodePosition(Section.Id, { ColumnX, Y });
			Section.Position = { ColumnX, Y };
			Y += std::max(Size.y, 60.0f) + GapY;
			ColumnWidth = std::max(ColumnWidth, Size.x);
		}
		ColumnX += std::max(ColumnWidth, 240.0f) + GapX;
	}
	Document.NeedFit = true;
}

void UILogicEditorForm::DrawTopBar(float ButtonSize)
{
	if (Form != nullptr)
	{
		Form->DrawToolbar(Form->Current(), ButtonSize);
	}
}

void UILogicEditorForm::DrawToolbar(SDocument* Document, float ButtonSize)
{
	auto Hint = [](const char* Text)
	{
		if (ImGui::IsItemHovered(ImGuiHoveredFlags_AllowWhenDisabled))
		{
			ImGui::SetMouseCursor(ImGuiMouseCursor_Hand);
			ImGui::SetTooltip("%s", Text);
		}
	};

	ImGui::BeginDisabled(Document == nullptr);
	if (XRay::ImGui::ToolbarButton("##logic_save", ICON_FA_FLOPPY_DISK " Save", nullptr, { 0.0f, ButtonSize }, ImDrawFlags_RoundCornersLeft) && Document != nullptr)
	{
		SaveDocument(*Document);
	}
	Hint("Save the logic file (Ctrl+S)");

	ImGui::SameLine();
	if (XRay::ImGui::ToolbarButton("##logic_reload", ICON_FA_ROTATE_RIGHT " Reload", nullptr, { 0.0f, ButtonSize }, ImDrawFlags_RoundCornersNone) && Document != nullptr)
	{
		if (!Document->Modified || ELog.DlgMsg(mtConfirmation, TMsgDlgButtons() | mbYes | mbNo, "Discard the changes of %s?", Document->Name.c_str()) == mrYes)
		{
			Load(*Document);
		}
	}
	Hint("Read the file from the disk again");

	ImGui::SameLine();
	if (XRay::ImGui::ToolbarButton("##logic_layout", ICON_FA_SITEMAP " Auto Layout", nullptr, { 0.0f, ButtonSize }, ImDrawFlags_RoundCornersNone) && Document != nullptr)
	{
		Document->LayoutDelay = 1;
	}
	Hint("Arrange the sections by their switches");

	ImGui::SameLine();
	if (XRay::ImGui::ToolbarButton("##logic_fit", ICON_FA_EXPAND " Fit", nullptr, { 0.0f, ButtonSize }, ImDrawFlags_RoundCornersNone) && Document != nullptr)
	{
		Document->NeedFit = true;
	}
	Hint("Show the whole graph (F)");

	ImGui::SameLine();
	if (XRay::ImGui::ToolbarButton("##logic_text", ICON_FA_CODE " Open as Text", nullptr, { 0.0f, ButtonSize }, ImDrawFlags_RoundCornersRight) && Document != nullptr)
	{
		if (Document->Modified)
		{
			SaveDocument(*Document);
		}
		UICoderForm::OpenFile(Document->Path);
	}
	Hint("Edit the file in the Coder");
	ImGui::EndDisabled();

	ImGui::SameLine(0.0f, 12.0f);
	ImGui::Checkbox("Properties", &ShowProperties);
	Hint("Show the plain keys of the sections on the nodes");

	if (!StatusText.empty())
	{
		ImGui::SameLine(0.0f, 12.0f);
		ImGui::AlignTextToFramePadding();
		ImGui::TextDisabled("%s", StatusText.c_str());
	}
}

bool UILogicEditorForm::BeginDocked(const char* Name)
{
	const bool Visible = ImGui::Begin(Name);
	ImGuiWindow* Window = ImGui::GetCurrentWindow();
	ImGuiDockNode* Node = Window->DockNode;
	if (Node == nullptr || Node->TabBar == nullptr)
	{
		return Visible;
	}

	if (RestoreFrames > 0)
	{
		auto Iter = DockSelection.find(Node->ID);
		if (Iter != DockSelection.end() && Iter->second == Window->TabId && Node->TabBar->SelectedTabId != Window->TabId)
		{
			if (ImGuiTabItem* Tab = ImGui::TabBarFindTabByID(Node->TabBar, Window->TabId))
			{
				ImGui::TabBarQueueFocus(Node->TabBar, Tab);
			}
		}
	}
	else if (Window->DockTabIsVisible)
	{
		DockSelection[Node->ID] = Window->TabId;
	}
	return Visible;
}

void UILogicEditorForm::Draw()
{
	BuildWorkspaceLayout();

	// The tab was hidden: the selection of the docks is restored for a few frames
	if (ImGui::GetFrameCount() > LastDrawFrame + 1)
	{
		RestoreFrames = 3;
	}
	else if (RestoreFrames > 0)
	{
		--RestoreFrames;
	}
	LastDrawFrame = ImGui::GetFrameCount();

	SDocument* Document = nullptr;
	if (BeginDocked(GraphWindow))
	{
		if (ImGui::IsWindowFocused(ImGuiFocusedFlags_RootAndChildWindows) && ImGui::IsKeyChordPressed(ImGuiMod_Ctrl | ImGuiKey_S) && Current() != nullptr)
		{
			SaveDocument(*Current());
		}

		if (!Documents.empty() && ImGui::BeginTabBar("##logic_files", ImGuiTabBarFlags_Reorderable | ImGuiTabBarFlags_FittingPolicyScroll))
		{
			int CloseIndex = -1;
			for (size_t Index = 0; Index < Documents.size(); ++Index)
			{
				SDocument& Item = *Documents[Index];
				ImGuiTabItemFlags Flags = Item.Modified ? ImGuiTabItemFlags_UnsavedDocument : ImGuiTabItemFlags_None;
				if (SelectRequest == (int)Index)
				{
					Flags |= ImGuiTabItemFlags_SetSelected;
				}

				const xr_string Label = xr_string(ICON_FA_DIAGRAM_PROJECT " ") + Item.Name + "###" + Item.Path;
				bool Open = true;
				if (ImGui::BeginTabItem(Label.c_str(), &Open, Flags))
				{
					if (Active != Index)
					{
						Active = Index;
						Validate(Item);
					}
					ImGui::EndTabItem();
				}
				ImGui::SetItemTooltip("%s", Item.Path.c_str());

				if (!Open)
				{
					CloseIndex = (int)Index;
				}
			}
			SelectRequest = -1;
			ImGui::EndTabBar();

			if (CloseIndex >= 0)
			{
				CloseDocument((size_t)CloseIndex);
			}
		}

		Document = Current();
		if (Document != nullptr)
		{
			DrawGraph(*Document);
		}
		else
		{
			const ImVec2 Avail = ImGui::GetContentRegionAvail();
			const char* Text = ICON_FA_DIAGRAM_PROJECT "  Drop a logic .ltx from configs\\scripts here or double click it in the Content Browser";
			const ImVec2 TextSize = ImGui::CalcTextSize(Text);
			ImGui::SetCursorPos({ ImGui::GetCursorPosX() + std::max(0.0f, (Avail.x - TextSize.x) * 0.5f), ImGui::GetCursorPosY() + Avail.y * 0.45f });
			ImGui::TextDisabled("%s", Text);
		}

		// Files from the Content Browser
		ImGuiWindow* Window = ImGui::GetCurrentWindow();
		// IsDragging() is false on the frame of the release: the payload is checked as is
		if (ImGui::GetDragDropPayload() != nullptr && ImGui::BeginDragDropTargetCustom(Window->InnerRect, ImGui::GetID("##logic_drop")))
		{
			const char* Dropped = EditorDragDrop::Accept(EDragDropType::Logic);
			if (Dropped == nullptr)
			{
				// Several selected files are dragged as plain files
				Dropped = EditorDragDrop::Accept(EDragDropType::File);
			}
			if (Dropped != nullptr && CanOpen(Dropped))
			{
				OpenFile(Dropped);
			}
			ImGui::EndDragDropTarget();
		}
	}
	ImGui::End();

	Document = Current();
	DrawOutliner(Document);
	DrawDetails(Document);
	DrawMessages(Document);
	DrawDebugger(Document);

	if (Debug.Paused && ImGui::IsKeyPressed(ImGuiKey_F5, false))
	{
		SetPaused(false);
	}
}

void UILogicEditorForm::SplitParents(const xr_string& Parents, xr_vector<xr_string>& Names)
{
	Names.clear();
	size_t Begin = 0;
	while (Begin < Parents.size())
	{
		size_t End = Parents.find(',', Begin);
		if (End == xr_string::npos)
		{
			End = Parents.size();
		}
		xr_string Name = Parents.substr(Begin, End - Begin);
		std::erase_if(Name, [](char Char) { return isspace((u8)Char) != 0; });
		if (!Name.empty())
		{
			Names.push_back(Name);
		}
		Begin = End + 1;
	}
}

void UILogicEditorForm::CollectInherited(CLogicDocument& Logic, const SLogicSection& Section, xr_vector<SInherited>& Into)
{
	Into.clear();
	xr_set<xr_string> Known;
	for (const SLogicLine& Line : Section.Lines)
	{
		if (Line.IsKey)
		{
			Known.insert(Line.Key.Name);
		}
	}

	// Breadth first: a nearer parent wins, a cycle is visited once
	xr_vector<const SLogicSection*> Queue = { &Section };
	xr_set<const SLogicSection*> Visited = { &Section };
	xr_vector<xr_string> Names;
	for (size_t Head = 0; Head < Queue.size(); ++Head)
	{
		SplitParents(Queue[Head]->Parents, Names);
		for (const xr_string& Name : Names)
		{
			const SLogicSection* Parent = Logic.Find(Name);
			if (Parent == nullptr || Visited.contains(Parent))
			{
				continue;
			}
			Visited.insert(Parent);
			Queue.push_back(Parent);
			for (const SLogicLine& Line : Parent->Lines)
			{
				if (Line.IsKey && !Known.contains(Line.Key.Name))
				{
					Known.insert(Line.Key.Name);
					Into.push_back({ Parent, &Line });
				}
			}
		}
	}
}

bool UILogicEditorForm::IsFunction(CLogicDocument& Logic, const SLogicSection& Section)
{
	bool HasChildren = false;
	xr_vector<xr_string> Names;
	for (const xr_unique_ptr<SLogicSection>& Item : Logic.Sections)
	{
		SplitParents(Item->Parents, Names);
		HasChildren |= std::find(Names.begin(), Names.end(), Section.Name) != Names.end();

		for (const SLogicLine& Line : Item->Lines)
		{
			if (!Line.IsKey || !Line.Key.IsCondlist)
			{
				continue;
			}
			for (const LogicCondlist::SBranch& Branch : Line.Key.Branches)
			{
				if (Branch.Target == Section.Name)
				{
					return false;
				}
			}
		}
	}
	return HasChildren;
}

bool UILogicEditorForm::IsInView(SDocument& Document, const SLogicSection& Section)
{
	if (Document.Function.empty())
	{
		return !IsFunction(Document.Logic, Section);
	}
	if (Section.Name == Document.Function)
	{
		return true;
	}

	// The sections the function switches to
	const SLogicSection* Function = Document.Logic.Find(Document.Function);
	if (Function == nullptr)
	{
		return false;
	}
	for (const SLogicLine& Line : Function->Lines)
	{
		if (!Line.IsKey || !Line.Key.IsCondlist)
		{
			continue;
		}
		for (const LogicCondlist::SBranch& Branch : Line.Key.Branches)
		{
			if (Branch.Target == Section.Name)
			{
				return true;
			}
		}
	}
	return false;
}

void UILogicEditorForm::OpenFunction(SDocument& Document, const xr_string& Name)
{
	if (Document.Function == Name)
	{
		return;
	}
	Document.Function = Name;
	Document.FunctionLayoutDelay = Name.empty() ? 0 : 2;
	Document.Selected = 0;
	if (Name.empty())
	{
		Document.NeedFit = true;
	}
}

void UILogicEditorForm::LayoutFunction(SDocument& Document)
{
	SLogicSection* Function = Document.Logic.Find(Document.Function);
	if (Function == nullptr)
	{
		return;
	}

	ed::SetNodePosition(Function->Id, { 0.0f, 0.0f });
	const float X = std::max(ed::GetNodeSize(Function->Id).x, 240.0f) + 110.0f;
	float Y = 0.0f;
	for (xr_unique_ptr<SLogicSection>& Section : Document.Logic.Sections)
	{
		if (Section.get() != Function && IsInView(Document, *Section))
		{
			ed::SetNodePosition(Section->Id, { X, Y });
			Y += std::max(ed::GetNodeSize(Section->Id).y, 60.0f) + 36.0f;
		}
	}
	Document.NeedFit = true;
}

void UILogicEditorForm::DrawGraphViews(SDocument& Document)
{
	// UE breadcrumbs: the event graph, the opened function
	if (Document.Function.empty() && std::none_of(Document.Logic.Sections.begin(), Document.Logic.Sections.end(), [&](const xr_unique_ptr<SLogicSection>& Section) { return IsFunction(Document.Logic, *Section); }))
	{
		return;
	}

	ImGui::PushStyleVar(ImGuiStyleVar_FramePadding, { 8.0f, 3.0f });
	const bool EventGraph = Document.Function.empty();
	ImGui::PushStyleColor(ImGuiCol_Button, EventGraph ? ImGui::GetStyleColorVec4(ImGuiCol_ButtonActive) : ImVec4(0, 0, 0, 0));
	if (ImGui::Button(ICON_FA_DIAGRAM_PROJECT " Event Graph"))
	{
		OpenFunction(Document, "");
	}
	ImGui::PopStyleColor();

	if (!EventGraph)
	{
		ImGui::SameLine();
		ImGui::TextDisabled(ICON_FA_CHEVRON_RIGHT);
		ImGui::SameLine();
		ImGui::PushStyleColor(ImGuiCol_Button, ImGui::GetStyleColorVec4(ImGuiCol_ButtonActive));
		ImGui::PushStyleColor(ImGuiCol_Text, ColorFunctionHeader.Value);
		ImGui::Button((xr_string(ICON_FA_FILE_CODE " ") + Document.Function).c_str());
		ImGui::PopStyleColor(2);
		ImGui::SameLine();
		ImGui::AlignTextToFramePadding();
		ImGui::TextDisabled("Function: its keys are inherited by the children");
	}
	ImGui::PopStyleVar();
}

void UILogicEditorForm::DrawGraph(SDocument& Document)
{
	if (!Document.Function.empty() && Document.Logic.Find(Document.Function) == nullptr)
	{
		OpenFunction(Document, "");
	}
	DrawGraphViews(Document);

	const bool FunctionView = !Document.Function.empty();
	ed::SetCurrentEditor(FunctionView ? Document.FunctionEditor : Document.Editor);

	ed::Style& Style = ed::GetStyle();
	Style.NodeRounding = 7.0f;
	Style.NodePadding = ImVec4(10, 6, 10, 8);
	Style.NodeBorderWidth = 1.0f;
	Style.HoveredNodeBorderWidth = 2.0f;
	Style.SelectedNodeBorderWidth = 2.5f;
	Style.LinkStrength = 120.0f;
	Style.PinRounding = 0.0f;
	Style.FlowDuration = 1.2f;
	Style.Colors[ed::StyleColor_Bg] = FunctionView ? ImColor(30, 28, 36) : ImColor(32, 32, 32);
	Style.Colors[ed::StyleColor_Grid] = ImColor(70, 70, 70, 90);
	Style.Colors[ed::StyleColor_NodeBg] = ImColor(18, 18, 18, 240);
	Style.Colors[ed::StyleColor_NodeBorder] = ImColor(0, 0, 0, 220);
	Style.Colors[ed::StyleColor_HovNodeBorder] = ImColor(200, 200, 200, 160);
	Style.Colors[ed::StyleColor_SelNodeBorder] = ImColor(255, 170, 40);
	Style.Colors[ed::StyleColor_SelLinkBorder] = ImColor(255, 170, 40);
	Style.Colors[ed::StyleColor_HovLinkBorder] = ImColor(200, 200, 200, 160);
	Style.Colors[ed::StyleColor_Flow] = ImColor(255, 170, 40);
	Style.Colors[ed::StyleColor_FlowMarker] = ImColor(255, 210, 120);
	Style.Colors[ed::StyleColor_PinRect] = ImColor(0, 0, 0, 0);
	Style.Colors[ed::StyleColor_PinRectBorder] = ImColor(0, 0, 0, 0);

	ed::Begin(FunctionView ? "##logic_function" : "##logic_graph");

	if (!FunctionView && Document.LayoutDelay > 0 && --Document.LayoutDelay == 0)
	{
		Layout(Document);
	}
	if (FunctionView && Document.FunctionLayoutDelay > 0 && --Document.FunctionLayoutDelay == 0)
	{
		LayoutFunction(Document);
	}

	for (xr_unique_ptr<SLogicSection>& Section : Document.Logic.Sections)
	{
		if (!IsInView(Document, *Section))
		{
			continue;
		}
		// The positions of the event graph are kept by the sections
		if (!FunctionView && Section->HasPosition)
		{
			ed::SetNodePosition(Section->Id, Section->Position);
			Section->HasPosition = false;
		}
		DrawNode(Document, *Section);
		if (!FunctionView)
		{
			Section->Position = ed::GetNodePosition(Section->Id);
		}
	}

	DrawLinks(Document);
	HandleCreate(Document);
	HandleDelete(Document);

	if (Document.NeedFit)
	{
		ed::NavigateToContent(0.0f);
		Document.NeedFit = false;
	}

	if (Document.FocusRequest != 0)
	{
		SLogicSection* Focus = nullptr;
		for (xr_unique_ptr<SLogicSection>& Section : Document.Logic.Sections)
		{
			if (Section->Id == Document.FocusRequest)
			{
				Focus = Section.get();
			}
		}

		if (Focus == nullptr)
		{
			Document.FocusRequest = 0;
		}
		else if (!IsInView(Document, *Focus))
		{
			// The node is on the other graph: it is focused there in the next frame
			OpenFunction(Document, IsFunction(Document.Logic, *Focus) ? Focus->Name : "");
		}
		else
		{
			ed::SelectNode(Document.FocusRequest);
			ed::NavigateToSelection(false, 0.25f);
			Document.Selected = Document.FocusRequest;
			Document.FocusRequest = 0;
			Document.FlowFrames = 1;
		}
	}

	// The selection comes from the canvas
	// HasSelectionChanged() is reset in ed::End() right after the click is handled: compare by hand
	// F9: a breakpoint on the selected node
	if (Document.Selected != 0 && ImGui::IsWindowFocused(ImGuiFocusedFlags_RootAndChildWindows) && !ImGui::GetIO().WantTextInput && ImGui::IsKeyPressed(ImGuiKey_F9, false))
	{
		for (xr_unique_ptr<SLogicSection>& Section : Document.Logic.Sections)
		{
			if (Section->Id == Document.Selected)
			{
				ToggleBreakpoint(Document, Section->Name);
			}
		}
	}

	if (Document.FocusRequest == 0)
	{
		ed::NodeId Selected[1] = {};
		const int Id = ed::GetSelectedNodes(Selected, 1) > 0 ? (int)Selected[0].Get() : 0;
		if (Id != Document.Selected && (Id != 0 || ed::GetSelectedObjectCount() == 0))
		{
			Document.Selected = Id;
			Document.FlowFrames = 1;
		}
	}

	if (ImGui::IsWindowFocused(ImGuiFocusedFlags_RootAndChildWindows) && !ImGui::GetIO().WantTextInput && ImGui::IsKeyPressed(ImGuiKey_F))
	{
		if (ed::GetSelectedObjectCount() > 0)
		{
			ed::NavigateToSelection(false, 0.25f);
		}
		else
		{
			ed::NavigateToContent(0.25f);
		}
	}

	// UE: a double click on a child opens its function
	const ed::NodeId Clicked = ed::GetDoubleClickedNode();
	if (Clicked && !FunctionView)
	{
		for (xr_unique_ptr<SLogicSection>& Section : Document.Logic.Sections)
		{
			if (Section->Id != (int)Clicked.Get())
			{
				continue;
			}
			xr_vector<xr_string> Names;
			SplitParents(Section->Parents, Names);
			SLogicSection* Parent = Names.empty() ? nullptr : Document.Logic.Find(Names[0]);
			if (Parent != nullptr && IsFunction(Document.Logic, *Parent))
			{
				Document.FocusRequest = Parent->Id;
			}
		}
	}

	DrawContextMenus(Document);

	ed::End();
	ed::SetCurrentEditor(nullptr);
}

void UILogicEditorForm::DrawNode(SDocument& Document, SLogicSection& Section)
{
	const bool Function = IsFunction(Document.Logic, Section);
	const ESchemeKind Kind = GetKind(Section);
	const ImColor HeaderColor = Function ? ColorFunctionHeader : KindColor(Kind);
	// Play In Editor: the section the debugged object runs
	const bool DebugActive = IsDebugged(Document) && Section.Name == Debug.Active;
	const bool Breakpoint = Document.Breakpoints.contains(Section.Name);
	const ImVec4 Pin = ImColor(235, 235, 235);
	const float Spacing = ImGui::GetStyle().ItemSpacing.x;

	// The inherited keys are shown on the children in the event graph
	xr_vector<SInherited> Inherited;
	if (Document.Function.empty())
	{
		CollectInherited(Document.Logic, Section, Inherited);
	}

	// The width fits the longest row
	float Width = ImGui::CalcTextSize(Section.Name.c_str()).x + PinSize + 60.0f;
	auto FitLine = [&](const SLogicLine& Line)
	{
		if (!Line.IsKey)
		{
			return;
		}
		if (Line.Key.IsCondlist)
		{
			for (const LogicCondlist::SBranch& Branch : Line.Key.Branches)
			{
				Width = std::max(Width, ImGui::CalcTextSize(Line.Key.Name.c_str()).x + ImGui::CalcTextSize(Line.Key.Prefix.c_str()).x + ImGui::CalcTextSize(Branch.Target.c_str()).x + PinSize + 70.0f);

				// A chip is not wrapped: the longest one fits the row with the target
				for (const xr_vector<LogicCondlist::SToken>* Tokens : { &Branch.Conditions, &Branch.Effects })
				{
					for (const LogicCondlist::SToken& Token : *Tokens)
					{
						Width = std::max(Width, ImGui::CalcTextSize(Token.Build().c_str()).x + ImGui::CalcTextSize(Branch.Target.c_str()).x + PinSize + 60.0f);
					}
				}
			}
		}
		else if (ShowProperties)
		{
			Width = std::max(Width, std::min(ImGui::CalcTextSize((Line.Key.Name + "  " + Line.Key.Value).c_str()).x + 10.0f, 420.0f));
		}
	};
	for (const SLogicLine& Line : Section.Lines)
	{
		FitLine(Line);
	}
	for (const SInherited& Item : Inherited)
	{
		FitLine(*Item.Line);
		Width = std::max(Width, ImGui::CalcTextSize(Item.Parent->Name.c_str()).x + 60.0f);
	}
	Width = std::clamp(Width, 230.0f, 720.0f);

	ed::BeginNode(Section.Id);
	ImGui::PushID(Section.Id);

	const ImVec2 Origin = ImGui::GetCursorScreenPos();

	// Header: the input and the title. Nothing switches to a function
	if (Function)
	{
		ImGui::Dummy({ PinSize, PinSize });
	}
	else
	{
		ed::BeginPin(InputPin(Section.Id), ed::PinKind::Input);
		ed::PinPivotAlignment({ 0.0f, 0.5f });
		ed::PinPivotSize({ 0, 0 });
		ImGui::Dummy({ PinSize, PinSize });
		DrawExecPin(ImGui::GetWindowDrawList(), ImGui::GetItemRectMin(), PinSize, Pin, ed::PinHadAnyLinks(InputPin(Section.Id)));
		ed::EndPin();
	}

	if (Breakpoint)
	{
		ImGui::SameLine();
		ImGui::TextColored(ColorMissing, ICON_FA_CIRCLE);
	}
	ImGui::SameLine();
	ImGui::TextUnformatted(Function ? ICON_FA_FILE_CODE : KindIcon(Kind));
	ImGui::SameLine();
	ImGui::TextUnformatted(Section.Name.c_str());

	ImGui::SetCursorScreenPos({ Origin.x + PinSize + Spacing, ImGui::GetCursorScreenPos().y });
	{
		xr_string Caption = Function ? xr_string("Function of ") + KindName((int)Kind) : xr_string(KindName((int)Kind));
		if (!Section.Parents.empty())
		{
			Caption += xr_string("  ") + ICON_FA_FILE_CODE " " + Section.Parents;
		}
		ImGui::PushStyleColor(ImGuiCol_Text, ImVec4(1, 1, 1, 0.55f));
		ImGui::TextUnformatted(Caption.c_str());
		ImGui::PopStyleColor();
	}
	const float HeaderBottom = ImGui::GetItemRectMax().y + 4.0f;
	ImGui::SetCursorScreenPos({ Origin.x, HeaderBottom + 6.0f });

	auto RightPin = [&](const char* Label, ImColor LabelColor, ImColor PinColor, ed::PinId Id, float RowY, bool Filled)
	{
		const float LabelWidth = ImGui::CalcTextSize(Label).x;
		ImGui::SetCursorScreenPos({ Origin.x + Width - PinSize - Spacing - LabelWidth, RowY });
		ImGui::TextColored(LabelColor, "%s", Label);
		ImGui::SameLine();
		ed::BeginPin(Id, ed::PinKind::Output);
		ed::PinPivotAlignment({ 1.0f, 0.5f });
		ed::PinPivotSize({ 0, 0 });
		ImGui::Dummy({ PinSize, PinSize });
		DrawExecPin(ImGui::GetWindowDrawList(), ImGui::GetItemRectMin(), PinSize, PinColor, Filled);
		ed::EndPin();
		return ImGui::GetItemRectMax().y;
	};

	auto DrawCondlist = [&](const SLogicKey& Key, int PinLine, bool IsInherited)
	{
		ImGui::PushStyleVar(ImGuiStyleVar_Alpha, ImGui::GetStyle().Alpha * (IsInherited ? 0.6f : 1.0f));
		const size_t BranchCount = std::max<size_t>(Key.Branches.size(), 1);
		for (size_t BranchIndex = 0; BranchIndex < BranchCount; ++BranchIndex)
		{
			const LogicCondlist::SBranch* Branch = BranchIndex < Key.Branches.size() ? &Key.Branches[BranchIndex] : nullptr;
			const float RowY = ImGui::GetCursorScreenPos().y;

			ImGui::BeginGroup();
			if (BranchIndex == 0)
			{
				ImGui::TextUnformatted(Key.Name.c_str());
				if (!Key.Prefix.empty())
				{
					ImGui::SameLine();
					ImGui::TextColored(ColorProbability, "[%s]", Key.Prefix.c_str());
				}
			}
			else
			{
				ImGui::TextColored(ColorDim, "  else");
			}
			const float ChipsWidth = Width - PinSize - 40.0f - (Branch != nullptr ? ImGui::CalcTextSize(Branch->Target.c_str()).x : 0.0f);
			if (Branch != nullptr && !Branch->Conditions.empty())
			{
				DrawTokenChips(Branch->Conditions, false, std::max(ChipsWidth, 120.0f));
			}
			if (Branch != nullptr && !Branch->Effects.empty())
			{
				DrawTokenChips(Branch->Effects, true, std::max(ChipsWidth, 120.0f));
			}
			ImGui::EndGroup();
			float Bottom = ImGui::GetItemRectMax().y;

			const ed::PinId Id = OutputPin(Section.Id, PinLine, (int)BranchIndex);
			const ImColor PinColor = IsInherited ? ColorFunctionHeader : ImColor(Pin);

			// The conditions checked in the game: the first passed branch is taken
			xr_string Status;
			ImColor StatusColor = ColorDim;
			if (DebugActive)
			{
				auto Result = Debug.Results.find((u64)Id.Get());
				if (Result != Debug.Results.end())
				{
					switch (Result->second)
					{
					case ResultAlways:								Status = ICON_FA_CHECK " ";	StatusColor = ImColor(235, 235, 235);	break;
					case (s8)CLogicDebugger::EResult::Passed:		Status = ICON_FA_CHECK " ";	StatusColor = ColorInfoHas;			break;
					case (s8)CLogicDebugger::EResult::Failed:		Status = ICON_FA_XMARK " ";	StatusColor = ColorInfoHasNot;		break;
					default:										Status = "? ";				StatusColor = ColorProbability;		break;
					}
				}
			}

			if (Branch == nullptr || Branch->Target.empty())
			{
				Bottom = std::max(Bottom, RightPin(Status.c_str(), StatusColor, ColorDim, Id, RowY, false));
			}
			else if (!Branch->HasTarget())
			{
				// nil, true, false
				Bottom = std::max(Bottom, RightPin((Status + Branch->Target).c_str(), Status.empty() ? ColorDim : StatusColor, ColorDim, Id, RowY, false));
			}
			else
			{
				const bool Exists = Document.Logic.Find(Branch->Target) != nullptr;
				const xr_string Label = Status + (Exists ? Branch->Target : xr_string(xr_string(ICON_FA_TRIANGLE_EXCLAMATION " ") + Branch->Target));
				Bottom = std::max(Bottom, RightPin(Label.c_str(), !Exists ? ColorMissing : Status.empty() ? ColorDim : StatusColor, Exists ? PinColor : ColorMissing, Id, RowY, Exists));
			}
			ImGui::SetCursorScreenPos({ Origin.x, Bottom + 4.0f });
		}
		ImGui::PopStyleVar();
	};

	auto DrawProperty = [&](const SLogicKey& Key, bool IsInherited)
	{
		ImGui::TextColored(IsInherited ? ColorFunctionHeader : ColorDim, "%s", Key.Name.c_str());
		ImGui::SameLine();
		xr_string Value = Key.Value;
		const float Available = Width - ImGui::CalcTextSize(Key.Name.c_str()).x - Spacing;
		if (ImGui::CalcTextSize(Value.c_str()).x > Available)
		{
			while (Value.size() > 1 && ImGui::CalcTextSize((Value + "..").c_str()).x > Available)
			{
				Value.pop_back();
			}
			Value += "..";
		}
		if (IsInherited)
		{
			ImGui::TextDisabled("%s", Value.c_str());
		}
		else
		{
			ImGui::TextUnformatted(Value.c_str());
		}
	};

	auto Separator = [&]()
	{
		const float Y = ImGui::GetCursorScreenPos().y + 2.0f;
		ImGui::GetWindowDrawList()->AddLine({ Origin.x, Y }, { Origin.x + Width, Y }, ImColor(255, 255, 255, 30));
		ImGui::SetCursorScreenPos({ Origin.x, Y + 4.0f });
	};

	bool HasProperties = false;
	for (size_t LineIndex = 0; LineIndex < Section.Lines.size(); ++LineIndex)
	{
		const SLogicLine& Line = Section.Lines[LineIndex];
		if (Line.IsKey && Line.Key.IsCondlist)
		{
			DrawCondlist(Line.Key, (int)LineIndex, false);
		}
		HasProperties |= Line.IsKey && !Line.Key.IsCondlist;
	}

	if (ShowProperties && HasProperties)
	{
		Separator();
		for (const SLogicLine& Line : Section.Lines)
		{
			if (Line.IsKey && !Line.Key.IsCondlist)
			{
				DrawProperty(Line.Key, false);
			}
		}
	}

	// The keys of the functions: the name of the function, then its rows
	const SLogicSection* Group = nullptr;
	for (size_t Index = 0; Index < Inherited.size(); ++Index)
	{
		const SInherited& Item = Inherited[Index];
		if (!Item.Line->Key.IsCondlist && !ShowProperties)
		{
			continue;
		}
		if (Item.Parent != Group)
		{
			Group = Item.Parent;
			Separator();
			ImGui::TextColored(ColorFunctionHeader, ICON_FA_FILE_CODE " %s", Group->Name.c_str());
		}
		if (Item.Line->Key.IsCondlist)
		{
			DrawCondlist(Item.Line->Key, InheritedLine + (int)Index, true);
		}
		else
		{
			DrawProperty(Item.Line->Key, true);
		}
	}

	// A new switch: "on_info", or "active" of the logic
	{
		const float RowY = ImGui::GetCursorScreenPos().y + 2.0f;
		const float Bottom = RightPin(ICON_FA_PLUS " switch", ImColor(255, 255, 255, 90), ColorDim, AddPin(Section.Id), RowY, false);
		ImGui::SetCursorScreenPos({ Origin.x, Bottom });
		ImGui::Dummy({ Width, 0.0f });
	}

	ImGui::PopID();
	ed::EndNode();

	// UE header: the color of the kind fading to the right
	const ImVec2 Position = ed::GetNodePosition(Section.Id);
	const ImVec2 Size = ed::GetNodeSize(Section.Id);
	if (Size.x > 1.0f)
	{
		ImDrawList* DrawList = ed::GetNodeBackgroundDrawList(Section.Id);
		const float Rounding = ed::GetStyle().NodeRounding;
		const float HeaderHeight = HeaderBottom - Origin.y + ed::GetStyle().NodePadding.y;
		const ImVec2 Min = Position;
		const ImVec2 Max = { Position.x + Size.x, Position.y + HeaderHeight };
		ImColor Dark = HeaderColor;
		Dark.Value.x *= 0.25f;
		Dark.Value.y *= 0.25f;
		Dark.Value.z *= 0.25f;
		DrawList->AddRectFilled(Min, Max, Dark, Rounding, ImDrawFlags_RoundCornersTop);
		DrawList->AddRectFilled(Min, { Min.x + Rounding * 2.0f, Max.y }, HeaderColor, Rounding, ImDrawFlags_RoundCornersTopLeft);
		DrawList->AddRectFilledMultiColor({ Min.x + Rounding, Min.y }, { Max.x - Rounding, Max.y }, HeaderColor, Dark, Dark, HeaderColor);
		DrawList->AddLine({ Min.x, Max.y }, { Max.x, Max.y }, ImColor(0, 0, 0, 200), 1.0f);
		// A function keeps the colour of its scheme as a stripe
		if (Function)
		{
			DrawList->AddRectFilled({ Min.x, Max.y }, { Max.x, Max.y + 2.0f }, KindColor(Kind));
		}

		// UE debugging: the running node glows, a breakpoint is a red frame
		const ImVec2 NodeMax = { Position.x + Size.x, Position.y + Size.y };
		if (DebugActive)
		{
			const float Pulse = 0.55f + 0.45f * sinf((float)ImGui::GetTime() * 5.0f);
			DrawList->AddRect({ Min.x - 3.0f, Min.y - 3.0f }, { NodeMax.x + 3.0f, NodeMax.y + 3.0f }, ImColor(90, 230, 90, (int)(255 * Pulse)), Rounding + 3.0f, 0, 3.0f);
			const char* Badge = Debug.Paused ? ICON_FA_PAUSE " PAUSED" : ICON_FA_PLAY " ACTIVE";
			const ImVec2 BadgeSize = ImGui::CalcTextSize(Badge);
			DrawList->AddText({ Max.x - BadgeSize.x - 8.0f, Min.y + 4.0f }, ImColor(120, 255, 120), Badge);
		}
		else if (Breakpoint)
		{
			DrawList->AddRect({ Min.x - 2.0f, Min.y - 2.0f }, { NodeMax.x + 2.0f, NodeMax.y + 2.0f }, ImColor(230, 60, 60, 200), Rounding + 2.0f, 0, 1.5f);
		}
	}
}

void UILogicEditorForm::DrawLinks(SDocument& Document)
{
	const bool Flow = Document.FlowFrames > 0;
	Document.FlowFrames = 0;

	auto LinkKey = [&](const SLogicSection& Section, const SLogicKey& Key, int PinLine, bool IsInherited)
	{
		for (size_t BranchIndex = 0; BranchIndex < Key.Branches.size(); ++BranchIndex)
		{
			const LogicCondlist::SBranch& Branch = Key.Branches[BranchIndex];
			SLogicSection* Target = Branch.HasTarget() ? Document.Logic.Find(Branch.Target) : nullptr;
			if (Target == nullptr || !IsInView(Document, *Target))
			{
				continue;
			}

			const ed::PinId From = OutputPin(Section.Id, PinLine, (int)BranchIndex);
			const ed::LinkId Id = ed::LinkId(From.Get());
			ImColor Color = Branch.Conditions.empty() ? ImColor(235, 235, 235) : ImColor(150, 200, 255);
			if (IsInherited)
			{
				Color = ColorFunctionHeader;
				Color.Value.w = 0.6f;
			}
			// The switches the debugged object would take now
			bool Passing = false;
			if (IsDebugged(Document) && Section.Name == Debug.Active)
			{
				auto Result = Debug.Results.find((u64)From.Get());
				Passing = Result != Debug.Results.end() && (Result->second == ResultAlways || Result->second == (s8)CLogicDebugger::EResult::Passed);
			}
			if (Passing)
			{
				Color = ColorInfoHas;
			}

			ed::Link(Id, From, InputPin(Target->Id), Color, IsInherited ? 1.5f : 2.5f);
			if ((Flow && (Section.Id == Document.Selected || Target->Id == Document.Selected)) || (Passing && Debug.Flow))
			{
				ed::Flow(Id);
			}
		}
	};

	xr_vector<SInherited> Inherited;
	for (xr_unique_ptr<SLogicSection>& Section : Document.Logic.Sections)
	{
		if (!IsInView(Document, *Section))
		{
			continue;
		}
		for (size_t LineIndex = 0; LineIndex < Section->Lines.size(); ++LineIndex)
		{
			const SLogicLine& Line = Section->Lines[LineIndex];
			if (Line.IsKey && Line.Key.IsCondlist)
			{
				LinkKey(*Section, Line.Key, (int)LineIndex, false);
			}
		}

		if (Document.Function.empty())
		{
			CollectInherited(Document.Logic, *Section, Inherited);
			for (size_t Index = 0; Index < Inherited.size(); ++Index)
			{
				if (Inherited[Index].Line->Key.IsCondlist)
				{
					LinkKey(*Section, Inherited[Index].Line->Key, InheritedLine + (int)Index, true);
				}
			}
		}
	}

	// One flow per pulse of the debugger
	Debug.Flow = false;
}

void UILogicEditorForm::Link(SDocument& Document, const SPin& From, const xr_string& Target)
{
	SLogicSection* Section = nullptr;
	for (xr_unique_ptr<SLogicSection>& Item : Document.Logic.Sections)
	{
		if (Item->Id == From.Section)
		{
			Section = Item.get();
		}
	}
	if (Section == nullptr)
	{
		return;
	}

	PushUndo(Document);
	if (From.Line >= InheritedLine)
	{
		// UE override: the inherited key is copied to the child and switched there
		xr_vector<SInherited> Inherited;
		CollectInherited(Document.Logic, *Section, Inherited);
		const size_t Index = (size_t)(From.Line - InheritedLine);
		if (Index < Inherited.size())
		{
			const SLogicKey& Source = Inherited[Index].Line->Key;
			SLogicKey& Key = Section->AddKey(Source.Name.c_str(), Source.Value.c_str());
			if (From.Branch >= 0 && From.Branch < (int)Key.Branches.size())
			{
				Key.Branches[From.Branch].Target = Target;
			}
			else
			{
				LogicCondlist::SBranch Branch;
				Branch.Target = Target;
				Key.Branches.push_back(Branch);
			}
			Key.Rebuild();
			StatusText = "[" + Section->Name + "] overrides " + Key.Name + " of [" + Inherited[Index].Parent->Name + "]";
		}
	}
	else if (From.IsAdd)
	{
		if (GetKind(*Section) == ESchemeKind::Logic && Section->FindKey("active") == nullptr)
		{
			Section->AddKey("active", Target.c_str());
		}
		else
		{
			xr_string KeyName = "on_info";
			for (int Index = 2; Section->FindKey(KeyName.c_str()) != nullptr; ++Index)
			{
				KeyName = xr_string("on_info") + std::to_string(Index).c_str();
			}
			Section->AddKey(KeyName.c_str(), Target.c_str());
		}
	}
	else if (From.Line >= 0 && From.Line < (int)Section->Lines.size())
	{
		SLogicKey& Key = Section->Lines[From.Line].Key;
		if (From.Branch >= 0 && From.Branch < (int)Key.Branches.size())
		{
			Key.Branches[From.Branch].Target = Target;
		}
		else
		{
			LogicCondlist::SBranch Branch;
			Branch.Target = Target;
			Key.Branches.push_back(Branch);
		}
		Key.Rebuild();
	}
	Validate(Document);
}

void UILogicEditorForm::HandleCreate(SDocument& Document)
{
	if (ed::BeginCreate(ImColor(255, 255, 255), 2.5f))
	{
		ed::PinId Start = 0;
		ed::PinId End = 0;
		if (ed::QueryNewLink(&Start, &End))
		{
			SPin A = DecodePin(Start);
			SPin B = DecodePin(End);
			if (A.IsInput)
			{
				std::swap(A, B);
			}

			if (A.IsInput || !B.IsInput)
			{
				ed::RejectNewItem(ColorMissing, 2.0f);
			}
			else
			{
				SLogicSection* Target = nullptr;
				for (xr_unique_ptr<SLogicSection>& Item : Document.Logic.Sections)
				{
					if (Item->Id == B.Section)
					{
						Target = Item.get();
					}
				}
				if (Target != nullptr && ed::AcceptNewItem(ImColor(130, 255, 130), 3.0f))
				{
					Link(Document, A, Target->Name);
				}
			}
		}

		ed::PinId Pin = 0;
		if (ed::QueryNewNode(&Pin))
		{
			const SPin From = DecodePin(Pin);
			if (!From.IsInput && ed::AcceptNewItem())
			{
				PendingLink = From;
				HasPendingLink = true;
				CreatePosition = ed::ScreenToCanvas(ImGui::GetMousePos());
				ed::Suspend();
				ImGui::OpenPopup("##logic_create");
				ed::Resume();
			}
		}
	}
	ed::EndCreate();
}

void UILogicEditorForm::HandleDelete(SDocument& Document)
{
	if (ed::BeginDelete())
	{
		bool Changed = false;
		ed::LinkId LinkId = 0;
		while (ed::QueryDeletedLink(&LinkId))
		{
			// An inherited switch belongs to the function
			if (DecodePin(ed::PinId(LinkId.Get())).Line >= InheritedLine)
			{
				ed::RejectDeletedItem();
				StatusText = "The switch is inherited: change it in the function";
				continue;
			}
			if (!ed::AcceptDeletedItem())
			{
				continue;
			}
			if (!Changed)
			{
				PushUndo(Document);
				Changed = true;
			}

			const SPin From = DecodePin(ed::PinId(LinkId.Get()));
			for (xr_unique_ptr<SLogicSection>& Section : Document.Logic.Sections)
			{
				if (Section->Id != From.Section || From.Line < 0 || From.Line >= (int)Section->Lines.size())
				{
					continue;
				}
				SLogicKey& Key = Section->Lines[From.Line].Key;
				if (From.Branch < 0 || From.Branch >= (int)Key.Branches.size())
				{
					continue;
				}
				LogicCondlist::SBranch& Branch = Key.Branches[From.Branch];
				if (Branch.Conditions.empty() && Branch.Effects.empty() && Key.Branches.size() > 1)
				{
					Key.Branches.erase(Key.Branches.begin() + From.Branch);
				}
				else
				{
					Branch.Target.clear();
				}
				Key.Rebuild();
			}
		}

		ed::NodeId NodeId = 0;
		xr_vector<xr_string> Removed;
		while (ed::QueryDeletedNode(&NodeId))
		{
			if (!ed::AcceptDeletedItem())
			{
				continue;
			}
			for (xr_unique_ptr<SLogicSection>& Section : Document.Logic.Sections)
			{
				if (Section->Id == (int)NodeId.Get())
				{
					Removed.push_back(Section->Name);
				}
			}
		}
		if (!Removed.empty() && !Changed)
		{
			PushUndo(Document);
			Changed = true;
		}
		for (const xr_string& Name : Removed)
		{
			Document.Logic.RemoveSection(Name);
		}

		if (Changed)
		{
			Validate(Document);
		}
	}
	ed::EndDelete();
}

SLogicSection& UILogicEditorForm::CreateSection(SDocument& Document, const xr_string& Scheme, ImVec2 Position)
{
	PushUndo(Document);
	const xr_string Name = Scheme == "logic" ? Document.Logic.UniqueName("logic") : Document.Logic.UniqueName(Scheme + "@new");
	SLogicSection& Section = Document.Logic.AddSection(Name);
	if (Scheme == "logic")
	{
		Section.AddKey("active", "");
	}
	Section.Position = Position;
	Section.HasPosition = true;
	Document.FocusRequest = Section.Id;
	Validate(Document);
	return Section;
}

void UILogicEditorForm::DrawCreateMenu(SDocument& Document)
{
	ImGui::TextDisabled(HasPendingLink ? "Switch to a new section" : "Add a section");
	if (ImGui::IsWindowAppearing())
	{
		PickerFilter[0] = 0;
		ImGui::SetKeyboardFocusHere();
	}
	ImGui::SetNextItemWidth(220.0f);
	ImGui::InputTextWithHint("##filter", ICON_FA_MAGNIFYING_GLASS " Search", PickerFilter, sizeof(PickerFilter));
	ImGui::Separator();

	xr_string Chosen;
	if (PassFilter(PickerFilter, "logic") && ImGui::MenuItem(ICON_FA_BOLT " logic"))
	{
		Chosen = "logic";
	}
	for (int Kind = (int)ESchemeKind::Stalker; Kind <= (int)ESchemeKind::Restrictor; ++Kind)
	{
		bool Any = false;
		for (const auto& [Name, SchemeKind] : Schemes)
		{
			Any |= (int)SchemeKind == Kind && PassFilter(PickerFilter, Name);
		}
		if (!Any)
		{
			continue;
		}

		ImGui::PushStyleColor(ImGuiCol_Text, KindColor((ESchemeKind)Kind).Value);
		const xr_string Title = xr_string(KindIcon((ESchemeKind)Kind)) + " " + KindName(Kind);
		const bool Open = PickerFilter[0] != 0 || ImGui::BeginMenu(Title.c_str());
		ImGui::PopStyleColor();
		if (!Open)
		{
			continue;
		}
		for (const auto& [Name, SchemeKind] : Schemes)
		{
			if ((int)SchemeKind == Kind && PassFilter(PickerFilter, Name) && ImGui::MenuItem(Name.c_str()))
			{
				Chosen = Name;
			}
		}
		if (PickerFilter[0] == 0)
		{
			ImGui::EndMenu();
		}
	}

	if (!Chosen.empty())
	{
		SLogicSection& Section = CreateSection(Document, Chosen, CreatePosition);
		if (HasPendingLink)
		{
			// CreateSection saved the undo state already
			const xr_string Name = Section.Name;
			Link(Document, PendingLink, Name);
			Document.UndoStack.pop_back();
		}
		HasPendingLink = false;
		ImGui::CloseCurrentPopup();
	}
}

void UILogicEditorForm::DrawContextMenus(SDocument& Document)
{
	ed::Suspend();

	ed::NodeId NodeId = 0;
	if (ed::ShowNodeContextMenu(&NodeId))
	{
		ContextSection = (int)NodeId.Get();
		ImGui::OpenPopup("##logic_node");
	}
	else if (ed::ShowBackgroundContextMenu())
	{
		CreatePosition = ed::ScreenToCanvas(ImGui::GetMousePos());
		HasPendingLink = false;
		ImGui::OpenPopup("##logic_create");
	}

	if (ImGui::BeginPopup("##logic_create"))
	{
		DrawCreateMenu(Document);
		ImGui::EndPopup();
	}
	else
	{
		HasPendingLink = false;
	}

	if (ImGui::BeginPopup("##logic_node"))
	{
		SLogicSection* Section = nullptr;
		for (xr_unique_ptr<SLogicSection>& Item : Document.Logic.Sections)
		{
			if (Item->Id == ContextSection)
			{
				Section = Item.get();
			}
		}

		if (Section != nullptr)
		{
			ImGui::TextDisabled("%s", Section->Name.c_str());
			ImGui::Separator();

			SLogicSection* Logic = nullptr;
			for (xr_unique_ptr<SLogicSection>& Item : Document.Logic.Sections)
			{
				if (Logic == nullptr && GetKind(*Item) == ESchemeKind::Logic)
				{
					Logic = Item.get();
				}
			}
			if (ImGui::MenuItem(ICON_FA_PLAY " Set as active", nullptr, false, Logic != nullptr && Logic != Section))
			{
				PushUndo(Document);
				if (SLogicKey* Active = Logic->FindKey("active"))
				{
					Active->IsCondlist = true;
					Active->Value = Section->Name;
					Active->Parse();
				}
				else
				{
					Logic->AddKey("active", Section->Name.c_str());
				}
				Validate(Document);
			}
			if (ImGui::MenuItem(ICON_FA_CLONE " Duplicate"))
			{
				PushUndo(Document);
				const xr_string Name = Document.Logic.UniqueName(Section->Name + "_copy");
				const xr_vector<SLogicLine> Lines = Section->Lines;
				const ImVec2 Position = Section->Position;
				SLogicSection& Copy = Document.Logic.AddSection(Name);
				Copy.Lines = Lines;
				Copy.Position = { Position.x + 40.0f, Position.y + 40.0f };
				Copy.HasPosition = true;
				Document.FocusRequest = Copy.Id;
				Validate(Document);
			}
			if (ImGui::MenuItem(ICON_FA_CIRCLE " Toggle breakpoint", "F9", Document.Breakpoints.contains(Section->Name)))
			{
				ToggleBreakpoint(Document, Section->Name);
			}
			ImGui::Separator();
			if (ImGui::MenuItem(ICON_FA_TRASH " Delete", "Del"))
			{
				PushUndo(Document);
				Document.Logic.RemoveSection(Section->Name);
				Validate(Document);
			}
		}
		ImGui::EndPopup();
	}

	ed::Resume();
}

void UILogicEditorForm::DrawOutliner(SDocument* Document)
{
	if (BeginDocked(OutlinerWindow))
	{
		if (Document == nullptr)
		{
			ImGui::TextDisabled("No logic file");
		}
		else
		{
			ImGui::SetNextItemWidth(ImGui::GetContentRegionAvail().x);
			ImGui::InputTextWithHint("##outliner_filter", ICON_FA_MAGNIFYING_GLASS " Search sections", OutlinerFilter, sizeof(OutlinerFilter));
			if (ImGui::BeginChild("##outliner_list"))
			{
				// UE "My Blueprint": the event graph, then the functions
				for (int Pass = 0; Pass < 2; ++Pass)
				{
					const bool Functions = Pass == 1;
					const bool Any = std::any_of(Document->Logic.Sections.begin(), Document->Logic.Sections.end(), [&](const xr_unique_ptr<SLogicSection>& Section) { return IsFunction(Document->Logic, *Section) == Functions; });
					if (!Any)
					{
						continue;
					}
					if (!ImGui::CollapsingHeader(Functions ? ICON_FA_FILE_CODE " Functions" : ICON_FA_DIAGRAM_PROJECT " Event Graph", ImGuiTreeNodeFlags_DefaultOpen))
					{
						continue;
					}
					for (xr_unique_ptr<SLogicSection>& Section : Document->Logic.Sections)
					{
						if (IsFunction(Document->Logic, *Section) != Functions || !PassFilter(OutlinerFilter, Section->Name))
						{
							continue;
						}
						const ESchemeKind Kind = GetKind(*Section);
						ImGui::PushID(Section->Id);
						ImGui::PushStyleColor(ImGuiCol_Text, Functions ? ColorFunctionHeader.Value : KindColor(Kind).Value);
						ImGui::TextUnformatted(Functions ? ICON_FA_FILE_CODE : KindIcon(Kind));
						ImGui::PopStyleColor();
						ImGui::SameLine();
						if (ImGui::Selectable(Section->Name.c_str(), Document->Selected == Section->Id))
						{
							Document->FocusRequest = Section->Id;
						}
						if (Functions)
						{
							ImGui::SetItemTooltip("Opens the function: its keys are inherited by the sections it is a parent of");
						}
						ImGui::PopID();
					}
				}
			}
			ImGui::EndChild();
		}
	}
	ImGui::End();
}

bool UILogicEditorForm::DrawFunctionPicker(const char* Id, bool IsEffect, LogicCondlist::SToken& Token)
{
	bool Picked = false;
	if (ImGui::BeginPopup(Id))
	{
		if (ImGui::IsWindowAppearing())
		{
			PickerFilter[0] = 0;
			ImGui::SetKeyboardFocusHere();
		}
		ImGui::SetNextItemWidth(260.0f);
		ImGui::InputTextWithHint("##filter", IsEffect ? "Effect or info portion" : "Condition or info portion", PickerFilter, sizeof(PickerFilter));

		// The text as an info portion
		if (PickerFilter[0] != 0)
		{
			if (ImGui::MenuItem((xr_string("+") + PickerFilter).c_str()))
			{
				Token = { '+', PickerFilter, "" };
				Picked = true;
			}
			if (ImGui::MenuItem((xr_string("-") + PickerFilter).c_str()))
			{
				Token = { '-', PickerFilter, "" };
				Picked = true;
			}
		}
		ImGui::Separator();

		if (ImGui::BeginChild("##functions", { 260.0f, 280.0f }))
		{
			const xr_vector<xr_string>& Names = IsEffect ? Effects : Conditions;
			for (const xr_string& Name : Names)
			{
				if (!PassFilter(PickerFilter, Name))
				{
					continue;
				}
				ImGui::PushStyleColor(ImGuiCol_Text, (IsEffect ? ColorEffect : ColorFunction).Value);
				if (ImGui::Selectable(("=" + Name).c_str()))
				{
					Token = { '=', Name, "" };
					Picked = true;
				}
				ImGui::PopStyleColor();
				if (!IsEffect)
				{
					ImGui::SameLine(220.0f);
					ImGui::PushID(Name.c_str());
					ImGui::PushStyleColor(ImGuiCol_Text, ColorFunctionNot.Value);
					if (ImGui::SmallButton("!"))
					{
						Token = { '!', Name, "" };
						Picked = true;
					}
					ImGui::PopStyleColor();
					ImGui::SetItemTooltip("Negated: !%s", Name.c_str());
					ImGui::PopID();
				}
			}
		}
		ImGui::EndChild();

		if (Picked)
		{
			ImGui::CloseCurrentPopup();
		}
		ImGui::EndPopup();
	}
	return Picked;
}

bool UILogicEditorForm::DrawTokens(SDocument& Document, const char* Id, xr_vector<LogicCondlist::SToken>& Tokens, bool IsEffect)
{
	ImGui::PushID(Id);
	bool Changed = false;

	xr_string Text = LogicCondlist::BuildTokens(Tokens);
	ImGui::SetNextItemWidth(-ImGui::GetFrameHeight() - ImGui::GetStyle().ItemSpacing.x);
	ImGui::PushStyleColor(ImGuiCol_Text, (IsEffect ? ColorEffect : ColorFunction).Value);
	const bool Edited = InputString(IsEffect ? "##effects" : "##conditions", Text);
	ImGui::PopStyleColor();
	if (ImGui::IsItemActivated())
	{
		PushUndo(Document);
	}
	ImGui::SetItemTooltip(IsEffect ? "Effects: %%=function(a:b) +give_info -disable_info%%" : "Conditions: {+has_info -has_no_info =function(a:b) !negated ~50}");
	if (Edited)
	{
		LogicCondlist::ParseTokens(Text, Tokens);
		Changed = true;
	}

	ImGui::SameLine();
	if (ImGui::Button(ICON_FA_PLUS, { ImGui::GetFrameHeight(), 0.0f }))
	{
		ImGui::OpenPopup("##picker");
	}
	ImGui::SetItemTooltip(IsEffect ? "Add an effect" : "Add a condition");

	LogicCondlist::SToken Token;
	if (DrawFunctionPicker("##picker", IsEffect, Token))
	{
		PushUndo(Document);
		Tokens.push_back(Token);
		Changed = true;
	}

	ImGui::PopID();
	return Changed;
}

void UILogicEditorForm::DrawKeySuggestions(SDocument& Document, SLogicSection& Section, bool& Changed)
{
	const xr_vector<SKeyInfo>& Keys = GetKeys(Section);

	ImGui::SetNextItemWidth(ImGui::GetContentRegionAvail().x - ImGui::CalcTextSize(ICON_FA_PLUS " Add key").x - ImGui::GetStyle().FramePadding.x * 2.0f - ImGui::GetStyle().ItemSpacing.x);
	const char* Hint = Keys.empty() ? "Key name" : "Key name: the keys of the scheme are listed below";
	const bool Enter = ImGui::InputTextWithHint("##new_key", Hint, NewKey, sizeof(NewKey), ImGuiInputTextFlags_EnterReturnsTrue | ImGuiInputTextFlags_CharsNoBlank);
	const bool InputActive = ImGui::IsItemActive();
	ImGui::SameLine();
	const bool Add = ImGui::Button(ICON_FA_PLUS " Add key");

	auto AddKey = [&](const xr_string& Name, const xr_string& Type)
	{
		PushUndo(Document);
		Section.AddKey(Name.c_str(), Type == "bool" ? "true" : "");
		NewKey[0] = 0;
		ShowKeyList = false;
		Changed = true;
	};

	if ((Add || Enter) && NewKey[0] != 0)
	{
		auto Known = std::find_if(Keys.begin(), Keys.end(), [this](const SKeyInfo& Info) { return Info.Name == NewKey; });
		if (!Keys.empty() && Known == Keys.end())
		{
			StatusText = xr_string("'") + NewKey + "' is not read by the " + Section.Scheme() + " scheme";
		}
		AddKey(NewKey, Known != Keys.end() ? Known->Type : "");
		return;
	}

	if (InputActive)
	{
		ShowKeyList = true;
	}
	if (!ShowKeyList || Keys.empty())
	{
		return;
	}

	// The keys of the scheme the section has not set yet
	const float Height = std::min(10.0f, (float)Keys.size()) * ImGui::GetTextLineHeightWithSpacing() + ImGui::GetStyle().WindowPadding.y * 2.0f;
	bool ListHovered = false;
	if (ImGui::BeginChild("##key_list", { 0.0f, Height }, ImGuiChildFlags_Borders))
	{
		ListHovered = ImGui::IsWindowHovered(ImGuiHoveredFlags_ChildWindows);
		for (int Pass = 0; Pass < 2; ++Pass)
		{
			// The keys of the scheme first, then the common ones
			for (const SKeyInfo& Info : Keys)
			{
				if (Info.Common != (Pass == 1) || !PassFilter(NewKey, Info.Name) || Section.FindKey(Info.Name.c_str()) != nullptr)
				{
					continue;
				}
				ImGui::PushID(Info.Name.c_str());
				if (ImGui::Selectable(Info.Name.c_str()))
				{
					AddKey(Info.Name, Info.Type);
				}
				ImGui::SameLine(ImGui::GetContentRegionAvail().x - 70.0f + ImGui::GetCursorPosX());
				const ImColor TypeColor = Info.Type == "condlist" ? ColorFunction : Info.Type == "bool" ? ColorFunctionNot : Info.Type == "number" ? ColorProbability : ColorDim;
				ImGui::TextColored(TypeColor, "%s", Info.Type.c_str());
				if (Info.Common)
				{
					ImGui::SetItemTooltip("Read for every scheme by xr_logic");
				}
				ImGui::PopID();
			}
			if (Pass == 0)
			{
				ImGui::Separator();
			}
		}
	}
	ImGui::EndChild();

	if (!InputActive && !ListHovered)
	{
		ShowKeyList = false;
	}
}

void UILogicEditorForm::DrawDetails(SDocument* Document)
{
	if (BeginDocked(DetailsWindow))
	{
		SLogicSection* Section = nullptr;
		if (Document != nullptr)
		{
			for (xr_unique_ptr<SLogicSection>& Item : Document->Logic.Sections)
			{
				if (Item->Id == Document->Selected)
				{
					Section = Item.get();
				}
			}
		}

		if (Section == nullptr)
		{
			ImGui::TextDisabled("Select a section on the graph");
		}
		else
		{
			bool Changed = false;
			const ESchemeKind Kind = GetKind(*Section);
			const float LabelWidth = 90.0f;

			ImGui::PushStyleColor(ImGuiCol_Text, KindColor(Kind).Value);
			ImGui::TextUnformatted(KindIcon(Kind));
			ImGui::PopStyleColor();
			ImGui::SameLine();
			ImGui::TextUnformatted(KindName((int)Kind));
			ImGui::Separator();

			ImGui::AlignTextToFramePadding();
			ImGui::TextUnformatted("Section");
			ImGui::SameLine(LabelWidth);
			ImGui::SetNextItemWidth(ImGui::GetContentRegionAvail().x);
			xr_string Name = Section->Name;
			if (InputString("##name", Name, ImGuiInputTextFlags_EnterReturnsTrue | ImGuiInputTextFlags_CharsNoBlank) && !Name.empty() && Name != Section->Name)
			{
				if (Document->Logic.Find(Name) != nullptr)
				{
					StatusText = "Section [" + Name + "] already exists";
				}
				else
				{
					PushUndo(*Document);
					Document->Logic.RenameSection(Section->Name, Name);
					Changed = true;
				}
			}
			ImGui::SetItemTooltip("Press Enter to rename. The switches to the section are renamed as well");

			ImGui::AlignTextToFramePadding();
			ImGui::TextUnformatted("Parents");
			ImGui::SameLine(LabelWidth);
			ImGui::SetNextItemWidth(ImGui::GetContentRegionAvail().x);
			if (InputString("##parents", Section->Parents))
			{
				Section->LoadedName.clear();
				Document->Modified = true;
			}
			if (ImGui::IsItemActivated())
			{
				PushUndo(*Document);
			}

			ImGui::Spacing();
			ImGui::SeparatorText("Keys");

			int RemoveLine = -1;
			for (size_t LineIndex = 0; LineIndex < Section->Lines.size(); ++LineIndex)
			{
				SLogicLine& Line = Section->Lines[LineIndex];
				if (!Line.IsKey)
				{
					continue;
				}
				SLogicKey& Key = Line.Key;
				ImGui::PushID((int)LineIndex);

				const xr_string Header = (Key.IsCondlist ? xr_string(ICON_FA_DIAGRAM_PROJECT "  ") : xr_string("     ")) + Key.Name + "###key";
				ImGui::SetNextItemAllowOverlap();
				const bool Open = ImGui::CollapsingHeader(Header.c_str(), Key.IsCondlist ? ImGuiTreeNodeFlags_DefaultOpen : 0);
				ImGui::SameLine(ImGui::GetContentRegionAvail().x - ImGui::GetFrameHeight() + ImGui::GetCursorPosX() - ImGui::GetStyle().ItemSpacing.x);
				if (ImGui::SmallButton(ICON_FA_XMARK))
				{
					RemoveLine = (int)LineIndex;
				}
				ImGui::SetItemTooltip("Remove the key");

				if (Open)
				{
					ImGui::Indent(8.0f);

					ImGui::AlignTextToFramePadding();
					ImGui::TextUnformatted("Key");
					ImGui::SameLine(LabelWidth);
					ImGui::SetNextItemWidth(ImGui::GetContentRegionAvail().x);
					if (InputString("##key", Key.Name, ImGuiInputTextFlags_CharsNoBlank))
					{
						Changed = true;
					}
					if (ImGui::IsItemActivated())
					{
						PushUndo(*Document);
					}

					ImGui::AlignTextToFramePadding();
					ImGui::TextUnformatted("Value");
					ImGui::SameLine(LabelWidth);
					ImGui::SetNextItemWidth(ImGui::GetContentRegionAvail().x);
					if (InputString("##value", Key.Value))
					{
						Key.IsCondlist = Key.IsCondlist || CLogicDocument::IsCondlistKey(Key.Name, Key.Value);
						if (Key.IsCondlist)
						{
							Key.Parse();
						}
						Changed = true;
					}
					if (ImGui::IsItemActivated())
					{
						PushUndo(*Document);
					}

					if (Key.IsCondlist)
					{
						ImGui::AlignTextToFramePadding();
						ImGui::TextUnformatted("Parameter");
						ImGui::SameLine(LabelWidth);
						ImGui::SetNextItemWidth(ImGui::GetContentRegionAvail().x);
						if (InputString("##prefix", Key.Prefix))
						{
							Key.Rebuild();
							Changed = true;
						}
						if (ImGui::IsItemActivated())
						{
							PushUndo(*Document);
						}
						ImGui::SetItemTooltip("Time of on_timer, signal of on_signal, distance of on_actor_dist_le: the text before '|'");

						int RemoveBranch = -1;
						for (size_t BranchIndex = 0; BranchIndex < Key.Branches.size(); ++BranchIndex)
						{
							LogicCondlist::SBranch& Branch = Key.Branches[BranchIndex];
							ImGui::PushID((int)BranchIndex);

							const ImVec2 BoxMin = ImGui::GetCursorScreenPos();
							ImGui::BeginGroup();
							ImGui::TextColored(ColorDim, BranchIndex == 0 ? "If" : "Else if");
							ImGui::SameLine(ImGui::GetContentRegionAvail().x - ImGui::GetFrameHeight() + ImGui::GetCursorPosX() - ImGui::GetStyle().ItemSpacing.x);
							if (ImGui::SmallButton(ICON_FA_TRASH))
							{
								RemoveBranch = (int)BranchIndex;
							}
							ImGui::SetItemTooltip("Remove the branch");

							ImGui::AlignTextToFramePadding();
							ImGui::TextUnformatted("Conditions");
							ImGui::SameLine(LabelWidth);
							if (DrawTokens(*Document, "conditions", Branch.Conditions, false))
							{
								Key.Rebuild();
								Changed = true;
							}

							ImGui::AlignTextToFramePadding();
							ImGui::TextUnformatted("Switch to");
							ImGui::SameLine(LabelWidth);
							ImGui::SetNextItemWidth(-ImGui::GetFrameHeight() - ImGui::GetStyle().ItemSpacing.x);
							if (InputString("##target", Branch.Target, ImGuiInputTextFlags_CharsNoBlank))
							{
								Key.Rebuild();
								Changed = true;
							}
							if (ImGui::IsItemActivated())
							{
								PushUndo(*Document);
							}
							ImGui::SameLine();
							if (ImGui::BeginCombo("##targets", nullptr, ImGuiComboFlags_NoPreview | ImGuiComboFlags_PopupAlignLeft))
							{
								auto Choose = [&](const xr_string& Target)
								{
									PushUndo(*Document);
									Branch.Target = Target;
									Key.Rebuild();
									Changed = true;
								};
								if (ImGui::Selectable("(none)"))
								{
									Choose("");
								}
								if (ImGui::Selectable("nil"))
								{
									Choose("nil");
								}
								ImGui::Separator();
								for (xr_unique_ptr<SLogicSection>& Item : Document->Logic.Sections)
								{
									if (ImGui::Selectable(Item->Name.c_str(), Item->Name == Branch.Target))
									{
										Choose(Item->Name);
									}
								}
								ImGui::EndCombo();
							}

							ImGui::AlignTextToFramePadding();
							ImGui::TextUnformatted("Effects");
							ImGui::SameLine(LabelWidth);
							if (DrawTokens(*Document, "effects", Branch.Effects, true))
							{
								Key.Rebuild();
								Changed = true;
							}
							ImGui::EndGroup();

							const ImVec2 BoxMax = { BoxMin.x + ImGui::GetContentRegionAvail().x, ImGui::GetItemRectMax().y };
							ImGui::GetWindowDrawList()->AddRect({ BoxMin.x - 4.0f, BoxMin.y - 2.0f }, { BoxMax.x, BoxMax.y + 3.0f }, ImColor(255, 255, 255, 25), 4.0f);
							ImGui::Spacing();
							ImGui::PopID();
						}

						if (RemoveBranch >= 0)
						{
							PushUndo(*Document);
							Key.Branches.erase(Key.Branches.begin() + RemoveBranch);
							Key.Rebuild();
							Changed = true;
						}

						if (ImGui::Button(ICON_FA_PLUS " Branch"))
						{
							PushUndo(*Document);
							Key.Branches.emplace_back();
							Changed = true;
						}
						ImGui::SetItemTooltip("Add a condition branch: the first branch whose conditions pass is taken");
					}

					ImGui::Unindent(8.0f);
				}
				ImGui::PopID();
			}

			if (RemoveLine >= 0)
			{
				PushUndo(*Document);
				Section->RemoveKey((size_t)RemoveLine);
				Changed = true;
			}

			ImGui::Spacing();
			DrawKeySuggestions(*Document, *Section, Changed);

			if (Changed)
			{
				Document->Modified = true;
				Validate(*Document);
			}
		}
	}
	ImGui::End();
}

static xr_string NormalizePath(const xr_string& Path)
{
	xr_string Result = Path;
	xr_strlwr(Result);
	std::replace(Result.begin(), Result.end(), '/', '\\');
	return Result;
}

bool UILogicEditorForm::IsDebugged(const SDocument& Document) const
{
	return Debug.ObjectId >= 0 && NormalizePath(Document.Path) == NormalizePath(Debug.Path);
}

void UILogicEditorForm::StartDebug(const CLogicDebugger::SObject& Object)
{
	StopDebug();

	// "scripts\\x.ltx" of st.ini_filename is under configs
	string_path FullPath = {};
	FS.update_path(FullPath, _game_config_, Object.IniFile.c_str());
	if (!std::filesystem::exists(xr_path(FullPath)))
	{
		StatusText = "The logic of " + Object.Name + " is not a file: " + Object.IniFile;
		return;
	}

	Debug.ObjectId = Object.Id;
	Debug.ObjectName = Object.Name;
	Debug.IniFile = Object.IniFile;
	Debug.LogicSection = Object.LogicSection;
	Debug.Path = FullPath;
	Debug.Active.clear();
	Debug.NextPoll = 0.0;
	OpenFile(FullPath);
	StatusText = "Debugging " + Object.Name;
}

void UILogicEditorForm::StopDebug()
{
	if (Debug.Paused)
	{
		SetPaused(false);
	}
	Debug.ObjectId = -1;
	Debug.ObjectName.clear();
	Debug.Active.clear();
	Debug.Results.clear();
	Debug.History.clear();
	Debug.PendingSwitch.clear();
}

void UILogicEditorForm::SetPaused(bool Paused)
{
	if (Debug.Paused == Paused)
	{
		return;
	}
	Debug.Paused = Paused;
	if (CLogicDebugger::IsAvailable())
	{
		Device.Pause(Paused, true, true, "Logic breakpoint");
		Debug.Paused = g_pauseMngr.Paused();
		if (Debug.Paused != Paused)
		{
			StatusText = "The game can't be paused now";
		}
	}
}

void UILogicEditorForm::ToggleBreakpoint(SDocument& Document, const xr_string& Section)
{
	if (Document.Breakpoints.contains(Section))
	{
		Document.Breakpoints.erase(Section);
	}
	else
	{
		Document.Breakpoints.insert(Section);
	}
}

void UILogicEditorForm::EvaluateActive()
{
	Debug.Results.clear();

	SDocument* Document = nullptr;
	for (xr_unique_ptr<SDocument>& Item : Documents)
	{
		if (IsDebugged(*Item))
		{
			Document = Item.get();
		}
	}
	SLogicSection* Section = Document != nullptr ? Document->Logic.Find(Debug.Active) : nullptr;
	if (Section == nullptr)
	{
		return;
	}

	auto CheckKey = [&](const SLogicKey& Key, int PinLine)
	{
		for (size_t BranchIndex = 0; BranchIndex < Key.Branches.size(); ++BranchIndex)
		{
			const LogicCondlist::SBranch& Branch = Key.Branches[BranchIndex];
			const u64 Pin = (u64)OutputPin(Section->Id, PinLine, (int)BranchIndex).Get();
			if (Branch.Conditions.empty())
			{
				Debug.Results[Pin] = ResultAlways;
				continue;
			}
			const CLogicDebugger::EResult Result = Debugger.Check(Debug.ObjectId, LogicCondlist::BuildTokens(Branch.Conditions), Debug.UseCpp);
			Debug.Results[Pin] = (s8)Result;
			if (Result == CLogicDebugger::EResult::Error)
			{
				StatusText = "Logic debugger: " + Debugger.GetLastError();
			}
		}
	};

	for (size_t LineIndex = 0; LineIndex < Section->Lines.size(); ++LineIndex)
	{
		const SLogicLine& Line = Section->Lines[LineIndex];
		if (Line.IsKey && Line.Key.IsCondlist)
		{
			CheckKey(Line.Key, (int)LineIndex);
		}
	}

	xr_vector<SInherited> Inherited;
	CollectInherited(Document->Logic, *Section, Inherited);
	for (size_t Index = 0; Index < Inherited.size(); ++Index)
	{
		if (Inherited[Index].Line->Key.IsCondlist)
		{
			CheckKey(Inherited[Index].Line->Key, InheritedLine + (int)Index);
		}
	}
}

void UILogicEditorForm::OnFrame()
{
	if (!CLogicDebugger::IsAvailable())
	{
		// Play In Editor is stopped: the timers of the editor must not stay paused
		if (Debug.Paused && g_pauseMngr.Paused())
		{
			g_pauseMngr.Pause(false);
		}
		Debug.Paused = false;
		if (Debug.ObjectId >= 0)
		{
			StopDebug();
		}
		Debug.Objects.clear();
		return;
	}

	// The pause key of the game pauses it as well
	Debug.Paused = g_pauseMngr.Paused();

	// The UI time runs while the game is paused
	const double Now = ImGui::GetTime();
	if (Now >= Debug.NextObjects)
	{
		Debugger.GetObjects(Debug.Objects);
		Debug.NextObjects = Now + 1.0;
	}

	if (Debug.ObjectId < 0)
	{
		return;
	}

	if (!Debug.PendingSwitch.empty())
	{
		if (!Debugger.SwitchTo(Debug.ObjectId, Debug.PendingSwitch))
		{
			StatusText = "Can't switch " + Debug.ObjectName + " to [" + Debug.PendingSwitch + "]";
		}
		Debug.PendingSwitch.clear();
		Debug.NextPoll = 0.0;
	}

	if (Now < Debug.NextPoll)
	{
		return;
	}
	Debug.NextPoll = Now + 0.2;

	xr_string Active;
	if (!Debugger.GetActiveSection(Debug.ObjectId, Active))
	{
		StatusText = Debug.ObjectName + " is gone";
		StopDebug();
		return;
	}

	if (Active != Debug.Active)
	{
		Debug.History.push_back({ Device.fTimeGlobal, Debug.Active, Active });
		if (Debug.History.size() > 200)
		{
			Debug.History.erase(Debug.History.begin());
		}
		Debug.Active = Active;

		for (xr_unique_ptr<SDocument>& Item : Documents)
		{
			if (!IsDebugged(*Item))
			{
				continue;
			}
			SLogicSection* Section = Item->Logic.Find(Active);
			if (Section != nullptr)
			{
				Item->FocusRequest = Section->Id;
			}
			// UE: the game stops on the node
			if (Item->Breakpoints.contains(Active))
			{
				SetPaused(true);
				StatusText = "Breakpoint: " + Debug.ObjectName + " switched to [" + Active + "]";
				EContext.UI->RequestedTabIndex = TabIndex;
			}
		}
	}

	EvaluateActive();
	Debug.Flow = (Debug.Polls++ % 5) == 0;
}

void UILogicEditorForm::DrawDebugger(SDocument* Document)
{
	if (!BeginDocked(DebuggerWindow))
	{
		ImGui::End();
		return;
	}

	if (!CLogicDebugger::IsAvailable())
	{
		ImGui::TextDisabled(ICON_FA_BUG "  Start Play In Editor to debug the logic");
		ImGui::End();
		return;
	}

	// The object: those of the opened file first
	ImGui::AlignTextToFramePadding();
	ImGui::TextUnformatted("Debug object");
	ImGui::SameLine(110.0f);
	ImGui::SetNextItemWidth(ImGui::GetContentRegionAvail().x - 90.0f);
	const xr_string Preview = Debug.ObjectId >= 0 ? Debug.ObjectName : xr_string("No debug object selected");
	if (ImGui::BeginCombo("##debug_object", Preview.c_str(), ImGuiComboFlags_HeightLarge))
	{
		if (ImGui::IsWindowAppearing())
		{
			DebugFilter[0] = 0;
			ImGui::SetKeyboardFocusHere();
		}
		ImGui::SetNextItemWidth(-1.0f);
		ImGui::InputTextWithHint("##debug_filter", ICON_FA_MAGNIFYING_GLASS " Object or file", DebugFilter, sizeof(DebugFilter));

		for (int Pass = 0; Pass < 2; ++Pass)
		{
			const bool Other = Pass == 1;
			bool Header = false;
			for (const CLogicDebugger::SObject& Object : Debug.Objects)
			{
				xr_string File = NormalizePath(Object.IniFile);
				const bool OfDocument = Document != nullptr && NormalizePath(Document->Path).ends_with(File);
				if (OfDocument == Other || (!PassFilter(DebugFilter, Object.Name) && !PassFilter(DebugFilter, Object.IniFile)))
				{
					continue;
				}
				if (!Header)
				{
					ImGui::SeparatorText(Other ? "Other objects" : "Objects of this file");
					Header = true;
				}
				ImGui::PushID(Object.Id);
				if (ImGui::Selectable(Object.Name.c_str(), Object.Id == Debug.ObjectId))
				{
					StartDebug(Object);
				}
				ImGui::SameLine(220.0f);
				ImGui::TextDisabled("%s  [%s]", Object.IniFile.c_str(), Object.ActiveSection.c_str());
				ImGui::PopID();
			}
		}
		ImGui::EndCombo();
	}
	ImGui::SameLine();
	ImGui::BeginDisabled(Debug.ObjectId < 0);
	if (ImGui::Button(ICON_FA_XMARK " Stop"))
	{
		StopDebug();
	}
	ImGui::EndDisabled();

	ImGui::AlignTextToFramePadding();
	ImGui::TextUnformatted("Conditions");
	ImGui::SameLine(110.0f);
	int Backend = Debug.UseCpp ? 0 : 1;
	ImGui::RadioButton("C++ parser", &Backend, 0);
	ImGui::SetItemTooltip("CScriptXRParser through its Lua binding. A condlist with the symbols out of its mask goes to xr_logic");
	ImGui::SameLine();
	ImGui::RadioButton("xr_logic.script", &Backend, 1);
	Debug.UseCpp = Backend == 0;

	if (Debug.ObjectId < 0)
	{
		ImGui::Spacing();
		ImGui::TextDisabled("Select the object to see its active section and the conditions of its switches on the graph");
		ImGui::End();
		return;
	}

	ImGui::Separator();
	ImGui::TextColored(ColorDim, "Logic");
	ImGui::SameLine(110.0f);
	ImGui::Text("%s  [%s]", Debug.IniFile.c_str(), Debug.LogicSection.c_str());

	ImGui::TextColored(ColorDim, "Active");
	ImGui::SameLine(110.0f);
	ImGui::PushStyleColor(ImGuiCol_Text, ColorInfoHas.Value);
	if (ImGui::Selectable((ICON_FA_PLAY " " + (Debug.Active.empty() ? xr_string("(none)") : Debug.Active)).c_str()) && Document != nullptr && IsDebugged(*Document))
	{
		if (SLogicSection* Section = Document->Logic.Find(Debug.Active))
		{
			Document->FocusRequest = Section->Id;
		}
	}
	ImGui::PopStyleColor();

	// Pause, continue, switch
	if (Debug.Paused)
	{
		if (ImGui::Button(ICON_FA_FORWARD " Continue (F5)"))
		{
			SetPaused(false);
		}
	}
	else if (ImGui::Button(ICON_FA_PAUSE " Pause"))
	{
		SetPaused(true);
	}
	ImGui::SameLine();
	SLogicSection* Selected = nullptr;
	if (Document != nullptr && IsDebugged(*Document))
	{
		for (xr_unique_ptr<SLogicSection>& Section : Document->Logic.Sections)
		{
			if (Section->Id == Document->Selected)
			{
				Selected = Section.get();
			}
		}
	}
	ImGui::BeginDisabled(Selected == nullptr || Selected->Name == Debug.Active);
	if (ImGui::Button(ICON_FA_CROSSHAIRS " Switch to selected") && Selected != nullptr)
	{
		Debug.PendingSwitch = Selected->Name;
	}
	ImGui::EndDisabled();
	ImGui::SetItemTooltip("xr_logic.switch_to_section of the object to the section selected on the graph");

	if (Document != nullptr && IsDebugged(*Document) && !Document->Breakpoints.empty())
	{
		ImGui::SeparatorText(ICON_FA_CIRCLE " Breakpoints");
		xr_string Remove;
		for (const xr_string& Name : Document->Breakpoints)
		{
			ImGui::PushID(Name.c_str());
			if (ImGui::SmallButton(ICON_FA_XMARK))
			{
				Remove = Name;
			}
			ImGui::SameLine();
			ImGui::TextColored(ColorMissing, ICON_FA_CIRCLE);
			ImGui::SameLine();
			if (ImGui::Selectable(Name.c_str()))
			{
				if (SLogicSection* Section = Document->Logic.Find(Name))
				{
					Document->FocusRequest = Section->Id;
				}
			}
			ImGui::PopID();
		}
		if (!Remove.empty())
		{
			Document->Breakpoints.erase(Remove);
		}
	}

	ImGui::SeparatorText(ICON_FA_CLOCK_ROTATE_LEFT " History");
	if (ImGui::BeginChild("##debug_history"))
	{
		for (auto Iter = Debug.History.rbegin(); Iter != Debug.History.rend(); ++Iter)
		{
			ImGui::PushID((int)(Iter - Debug.History.rbegin()));
			ImGui::TextColored(ColorDim, "%8.2f", Iter->Time);
			ImGui::SameLine();
			ImGui::TextDisabled("%s", Iter->From.empty() ? "(none)" : Iter->From.c_str());
			ImGui::SameLine();
			ImGui::TextUnformatted(ICON_FA_CHEVRON_RIGHT);
			ImGui::SameLine();
			if (ImGui::Selectable(Iter->To.empty() ? "(none)" : Iter->To.c_str()) && Document != nullptr && IsDebugged(*Document))
			{
				if (SLogicSection* Section = Document->Logic.Find(Iter->To))
				{
					Document->FocusRequest = Section->Id;
				}
			}
			ImGui::PopID();
		}
	}
	ImGui::EndChild();
	ImGui::End();
}

void UILogicEditorForm::DrawMessages(SDocument* Document)
{
	if (BeginDocked(MessagesWindow))
	{
		if (Document == nullptr || Messages.empty())
		{
			ImGui::TextColored(ColorInfoHas, ICON_FA_CIRCLE_INFO " No problems");
		}
		else
		{
			for (size_t Index = 0; Index < Messages.size(); ++Index)
			{
				const SMessage& Message = Messages[Index];
				ImGui::PushID((int)Index);
				ImGui::PushStyleColor(ImGuiCol_Text, Message.IsError ? ColorMissing.Value : ColorProbability.Value);
				const xr_string Label = xr_string(Message.IsError ? ICON_FA_CIRCLE_EXCLAMATION " " : ICON_FA_TRIANGLE_EXCLAMATION " ") + Message.Text;
				if (ImGui::Selectable(Label.c_str()))
				{
					Document->FocusRequest = Message.Section;
				}
				ImGui::PopStyleColor();
				ImGui::PopID();
			}
		}
	}
	ImGui::End();
}
