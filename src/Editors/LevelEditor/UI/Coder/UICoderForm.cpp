#include "stdafx.h"
#include "UICoderForm.h"
#include "IconsFontAwesome6.h"
#include "imgui_internal.h"
#include "../../Editor/Utils/ContentView.h"

UICoderForm* UICoderForm::Form = nullptr;

static const char* CoderWindow = "Coder###CoderMain";
static const char* ContentWindow = "Content Browser##CD";
static const char* Utf8BomText = "\xEF\xBB\xBF";

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

static bool IsLuaNameChar(char Char)
{
	return isalnum((u8)Char) || Char == '_';
}

static bool IsLtxNameChar(char Char)
{
	return !isspace((u8)Char) && Char != '[' && Char != ']' && Char != ':' && Char != ',' && Char != '=' && Char != ';' && Char != '"';
}

static xr_string TrimLine(const xr_string& Text)
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

static bool SkipWord(const xr_string& Text, size_t& At, const char* Word)
{
	const size_t Length = xr_strlen(Word);
	if (Text.compare(At, Length, Word) != 0 || (At + Length < Text.size() && IsLuaNameChar(Text[At + Length])))
	{
		return false;
	}
	At += Length;
	while (At < Text.size() && isspace((u8)Text[At]))
	{
		++At;
	}
	return true;
}

static int FindLuaFunction(const xr_string& Text, const xr_string& Name, bool AnyOwner)
{
	int LineIndex = 0;
	for (size_t Start = 0; Start <= Text.size(); ++LineIndex)
	{
		size_t End = Text.find('\n', Start);
		if (End == xr_string::npos)
		{
			End = Text.size();
		}
		const xr_string Line = TrimLine(Text.substr(Start, End - Start).c_str());
		Start = End + 1;

		size_t At = 0;
		SkipWord(Line, At, "local");
		if (SkipWord(Line, At, "function"))
		{
			size_t NameEnd = At;
			while (NameEnd < Line.size() && (IsLuaNameChar(Line[NameEnd]) || Line[NameEnd] == '.' || Line[NameEnd] == ':'))
			{
				++NameEnd;
			}
			const xr_string Declared = Line.substr(At, NameEnd - At).c_str();
			const size_t Owner = Declared.find_last_of(".:");
			if (Declared == Name || (AnyOwner && Owner != xr_string::npos && Declared.substr(Owner + 1) == Name))
			{
				return LineIndex;
			}
		}
		else if (Line.compare(At, Name.size(), Name) == 0)
		{
			size_t Assign = At + Name.size();
			while (Assign < Line.size() && isspace((u8)Line[Assign]))
			{
				++Assign;
			}
			if (Assign < Line.size() && Line[Assign] == '=' && (Assign + 1 >= Line.size() || Line[Assign + 1] != '='))
			{
				++Assign;
				while (Assign < Line.size() && isspace((u8)Line[Assign]))
				{
					++Assign;
				}
				if (SkipWord(Line, Assign, "function"))
				{
					return LineIndex;
				}
			}
		}
	}
	return -1;
}

static bool ReadScript(const xr_string& Path, xr_string& Text)
{
	IReader* Reader = FS.r_open(Path.c_str());
	if (Reader == nullptr)
	{
		return false;
	}
	Text.assign((const char*)Reader->pointer(), Reader->length());
	FS.r_close(Reader);
	return true;
}

static void BuildWorkspaceLayout()
{
	static bool Checked = false;
	if (Checked)
	{
		return;
	}

	ImGuiWindow* Viewport = ImGui::FindWindowByName("Render##0");
	ImGuiWindow* Coder = ImGui::FindWindowByName(CoderWindow);
	ImGuiWindow* Browser = ImGui::FindWindowByName(ContentWindow);
	if (Viewport == nullptr || Viewport->DockNode == nullptr || Coder == nullptr || Browser == nullptr)
	{
		return;
	}
	Checked = true;

	if (Coder->DockId != 0 && Browser->DockId != 0)
	{
		return;
	}

	ImGuiID Center = Viewport->DockId;
	const ImGuiID Left = ImGui::DockBuilderSplitNode(Center, ImGuiDir_Left, 0.25f, nullptr, &Center);
	ImGui::DockBuilderDockWindow(ContentWindow, Left);
	ImGui::DockBuilderDockWindow(CoderWindow, Center);
	ImGui::DockBuilderFinish(ImGui::DockNodeGetRootNode(Viewport->DockNode)->ID);
}

UICoderForm::UICoderForm()
{
}

UICoderForm::~UICoderForm()
{
}

void UICoderForm::Create(int Tab)
{
	R_ASSERT(Form == nullptr);
	Form = new UICoderForm();
	Form->TabIndex = Tab;
	EContext.UI->Push(Form, false);
	Device.seqFrame.Add(Form);
	Form->SetStateName("CoderForm");

	CContentView* Browser = new CContentView(ContentWindow);
	Browser->Init();
	Browser->TabIndex = Tab;
	Browser->SetStateName("CoderContentBrowser");
	EContext.UI->Push(Browser, false);
	Form->Browser = Browser;
}

void UICoderForm::Serialize(nlohmann::json& Data, bool IsSaving)
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

void UICoderForm::Destroy()
{
	if (Form != nullptr)
	{
		Device.seqFrame.Remove(Form);
	}
	xr_delete(Form);
}

bool UICoderForm::IsActive()
{
	return Form != nullptr && EContext.UI->ActiveTabIndex == Form->TabIndex;
}

bool UICoderForm::IsModified()
{
	if (Form == nullptr)
	{
		return false;
	}
	return std::any_of(Form->Documents.begin(), Form->Documents.end(), [](const xr_unique_ptr<SDocument>& Document) { return Document->Modified; });
}

UICoderForm::EFileKind UICoderForm::GetKind(const xr_string& FullPath)
{
	xr_string Extension = xr_path(FullPath).extension().string().c_str();
	xr_strlwr(Extension);
	if (Extension == ".script" || Extension == ".lua")
	{
		return EFileKind::Lua;
	}
	if (Extension == ".xml")
	{
		return EFileKind::Xml;
	}
	if (Extension == ".ltx" || Extension == ".ini")
	{
		return EFileKind::Ini;
	}
	return EFileKind::Text;
}

bool UICoderForm::CanOpen(const xr_string& FullPath)
{
	return GetKind(FullPath) != EFileKind::Text;
}

bool UICoderForm::OpenFile(const xr_string& FullPath)
{
	if (Form == nullptr)
	{
		return false;
	}

	// The top bar selects the tab in its next frame
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

	xr_unique_ptr<SDocument> Document = xr_make_unique<SDocument>();
	Document->Path = FullPath;
	Document->Name = xr_path(FullPath).xfilename();
	Document->Kind = GetKind(FullPath);
	Document->View = xr_make_unique<TextEditor>();
	Document->View->SetShowWhitespaces(false);
	Document->View->SetPalette(CodeEditor::GetPalette());
	switch (Document->Kind)
	{
	case EFileKind::Lua:	Form->UpdateLanguage();	Document->View->SetLanguageDefinition(Form->LuaLanguage);	break;
	case EFileKind::Xml:	Document->View->SetLanguageDefinition(CodeEditor::GetXmlLanguage());			break;
	case EFileKind::Ini:	Form->UpdateLanguage();	Document->View->SetLanguageDefinition(Form->IniLanguage);	break;
	default:																							break;
	}

	if (!Form->Load(*Document))
	{
		ELog.DlgMsg(mtError, "Coder: can't read %s", FullPath.c_str());
		return false;
	}

	Form->Documents.push_back(std::move(Document));
	Form->SelectRequest = (int)Form->Documents.size() - 1;
	return true;
}

bool UICoderForm::Save()
{
	if (Form == nullptr || Form->Active >= Form->Documents.size())
	{
		return false;
	}
	return Form->SaveDocument(*Form->Documents[Form->Active]);
}

bool UICoderForm::Reload()
{
	if (Form == nullptr || Form->Active >= Form->Documents.size())
	{
		return false;
	}
	SDocument* Document = Form->Documents[Form->Active].get();
	if (!Document->Modified || ELog.DlgMsg(mtConfirmation, TMsgDlgButtons() | mbYes | mbNo, "Discard the changes of %s?", Document->Name.c_str()) == mrYes)
	{
		return Form->Load(*Document);
	}
	return false;
}

bool UICoderForm::Load(SDocument& Document)
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

	Document.Text = Text;
	Document.View->SetText(Text.c_str());
	Document.View->ClearErrorMarkers();
	Document.Modified = false;
	return true;
}

bool UICoderForm::SaveDocument(SDocument& Document)
{
	Document.Text = CodeEditor::GetText(*Document.View);

	// A broken script is saved anyway, the error is marked on its line
	Document.View->ClearErrorMarkers();
	if (Document.Kind == EFileKind::Lua)
	{
		xr_string Error;
		int Line = 0;
		if (!CXVisor::Compile(Document.Name.c_str(), Document.Text, Error, Line))
		{
			if (Line > 0)
			{
				Document.View->AddErrorMarker(Line, Error.c_str());
				Document.View->SetCursorPosition({ Line - 1, 0 });
			}
			ELog.Msg(mtError, "Coder: %s", Error.c_str());
			StatusText = "Script error: " + Error;
		}
		else
		{
			StatusText.clear();
		}
	}

	xr_string Data = !Document.Utf8 ? Platform::UTF8_to_CP1251(Document.Text) : Document.Utf8Bom ? xr_string(Utf8BomText) + Document.Text : Document.Text;
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
		ELog.DlgMsg(mtError, "Coder: can't write %s", Document.Path.c_str());
		return false;
	}
	Writer->w(Data.data(), Data.size());
	FS.w_close(Writer);

	if (Document.Kind == EFileKind::Xml)
	{
		// The xml files are cached by the engine under their configs path
		string_path Configs;
		FS.update_path(Configs, _game_config_, "");
		xr_string Root = Configs;
		xr_string Path = Document.Path;
		xr_strlwr(Root);
		xr_strlwr(Path);
		if (Path.starts_with(Root))
		{
			CXml::RemoveFromCache(_game_config_, Document.Path.substr(Root.size()).c_str());
		}
	}

	Document.Modified = false;
	// The hints of the Logic and Dialog editors read the functions of the scripts
	if (Document.Kind == EFileKind::Lua)
	{
		EContext.Visor->InvalidateScripts();
	}
	if (StatusText.empty())
	{
		StatusText = "Saved " + Document.Path;
	}
	ELog.Msg(mtInformation, "Coder: saved %s", Document.Path.c_str());
	return true;
}

bool UICoderForm::CloseDocument(size_t Index)
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

void UICoderForm::OnFrame()
{
	if (BakeRequest)
	{
		BakeRequest = false;
		Bake();
	}
}

void UICoderForm::BakeIfMissing()
{
	if (Form != nullptr && !Scene->IsPlayInEditor() && !std::filesystem::exists(xr_path(CXVisor::GetApiPath())))
	{
		Form->Bake();
	}
}

void UICoderForm::Bake()
{
	if (Scene->IsPlayInEditor())
	{
		StatusText = "Stop Play In Editor to bake the Lua API";
		return;
	}
	xr_string Error;
	if (EContext.Visor->Bake(Error))
	{
		StatusText = "Lua API baked: " + xr_string::ToString(EContext.Visor->GetClassCount()) + " classes";
		ELog.Msg(mtInformation, "Coder: %s", StatusText.c_str());
	}
	else
	{
		StatusText = "Lua API: " + Error;
		ELog.Msg(mtError, "Coder: %s", Error.c_str());
	}
}

void UICoderForm::RevealInBrowser()
{
	if (Form == nullptr || Form->Browser == nullptr || Form->Active >= Form->Documents.size())
	{
		return;
	}
	Form->Browser->Reveal(Form->Documents[Form->Active]->Path);
	ImGui::SetWindowFocus(ContentWindow);
}

bool UICoderForm::Undo(bool Redo)
{
	if (Form == nullptr || Form->Active >= Form->Documents.size())
	{
		return false;
	}

	TextEditor& View = *Form->Documents[Form->Active]->View;
	if (Redo ? !View.CanRedo() : !View.CanUndo())
	{
		return false;
	}

	Redo ? View.Redo() : View.Undo();
	return true;
}

void UICoderForm::RequestBake()
{
	if (Form != nullptr)
	{
		Form->BakeRequest = true;
		Form->StatusText = "Baking the Lua API...";
	}
}

void UICoderForm::DrawTopBar(float ButtonSize)
{
	if (Form != nullptr)
	{
		Form->DrawStatus(ButtonSize);
	}
}

void UICoderForm::DrawStatus(float ButtonSize)
{
	const SDocument* Document = Active < Documents.size() ? Documents[Active].get() : nullptr;
	xr_string Fallback;
	if (Document != nullptr && Document->Kind == EFileKind::Ini)
	{
		Fallback += EContext.Visor->GetLtxStatus();
	}
	const char* Text = !StatusText.empty() ? StatusText.c_str() : Fallback.c_str();
	ImGui::SetCursorPosY(ImGui::GetCursorPosY() + (ButtonSize - ImGui::GetTextLineHeight()) * 0.5f);
	ImGui::TextDisabled("%s", Text);
}

void UICoderForm::DrawDocument(SDocument& Document)
{
	TextEditor& View = *Document.View;

	const bool IsLua = Document.Kind == EFileKind::Lua;
	const bool IsIni = Document.Kind == EFileKind::Ini;
	if (IsLua)
	{
		Completion.BeforeRender(View);
	}
	else if (IsIni)
	{
		LtxCompletion.BeforeRender(View);
	}

	View.Render("##coder_text", ImGui::GetContentRegionAvail());
	const ImVec2 ViewMin = ImGui::GetItemRectMin();
	const ImVec2 ViewMax = ImGui::GetItemRectMax();
	if (View.IsTextChanged())
	{
		Document.Text = CodeEditor::GetText(View);
		Document.Modified = true;
	}

	const bool Ctrl = ImGui::GetIO().ConfigMacOSXBehaviors ? ImGui::GetIO().KeySuper : ImGui::GetIO().KeyCtrl;
	if (Ctrl && (IsLua || IsIni) && UpdateLink(Document))
	{
		const bool Clicked = ImGui::IsMouseClicked(ImGuiMouseButton_Left);
		if (Link.Found)
		{
			ImDrawList* DrawList = ImGui::GetWindowDrawList();
			DrawList->PushClipRect(ImGui::GetItemRectMin(), ImGui::GetItemRectMax(), true);
			DrawList->AddLine({ Link.From.x, Link.To.y - 1.0f }, { Link.To.x, Link.To.y - 1.0f }, ImGui::GetColorU32(ImGuiCol_Text));
			DrawList->PopClipRect();
			ImGui::SetMouseCursor(ImGuiMouseCursor_Hand);
			if (Clicked)
			{
				StatusText.clear();
				OpenAt(Link.Path, Link.Target);
			}
		}
		else if (Clicked)
		{
			StatusText = "No definition of " + Link.Name;
		}
	}

	if (IsLua)
	{
		Completion.AfterRender(View, Document.Text, CLuaCompletion::FieldResolver());
	}
	else if (IsIni)
	{
		LtxCompletion.AfterRender(View, Document.Path, Document.Text);
	}

	xr_vector<TextEditor*> Views;
	Views.reserve(Documents.size());
	for (const xr_unique_ptr<SDocument>& Other : Documents)
	{
		Views.push_back(Other->View.get());
	}

	if (Search.Draw(View, Views, ViewMin, ViewMax))
	{
		for (const xr_unique_ptr<SDocument>& Other : Documents)
		{
			xr_string Text = CodeEditor::GetText(*Other->View);
			if (Text != Other->Text)
			{
				Other->Text = std::move(Text);
				Other->Modified = true;
			}
		}
	}
}

bool UICoderForm::UpdateLink(SDocument& Document)
{
	TextEditor& View = *Document.View;
	TextEditor::Coordinates Hovered;
	if (!View.GetHoveredCoordinates(Hovered))
	{
		return false;
	}

	size_t LineStart = 0;
	for (int LineIndex = Hovered.mLine; LineIndex > 0 && LineStart != xr_string::npos; --LineIndex)
	{
		LineStart = Document.Text.find('\n', LineStart);
		LineStart = LineStart != xr_string::npos ? LineStart + 1 : LineStart;
	}
	if (LineStart == xr_string::npos)
	{
		return false;
	}
	const size_t LineEnd = Document.Text.find('\n', LineStart);
	const xr_string Line = Document.Text.substr(LineStart, LineEnd == xr_string::npos ? xr_string::npos : LineEnd - LineStart).c_str();
	const size_t Index = std::min((size_t)std::max(View.GetLineCharacterIndex(Hovered), 0), Line.size());
	const bool IsLua = Document.Kind == EFileKind::Lua;

	size_t Begin = Index;
	while (Begin > 0 && (IsLua ? IsLuaNameChar(Line[Begin - 1]) || Line[Begin - 1] == '.' || Line[Begin - 1] == ':' : IsLtxNameChar(Line[Begin - 1])))
	{
		--Begin;
	}
	size_t End = Index;
	while (End < Line.size() && (IsLua ? IsLuaNameChar(Line[End]) : IsLtxNameChar(Line[End])))
	{
		++End;
	}
	while (IsLua && Begin < End && (Line[Begin] == '.' || Line[Begin] == ':'))
	{
		++Begin;
	}
	if (Begin >= End)
	{
		return false;
	}

	const ImVec2 From = View.GetCoordinatesScreenPos({ Hovered.mLine, View.GetLineCharacterColumn(Hovered.mLine, (int)Begin) });
	const ImVec2 To = View.GetCoordinatesScreenPos({ Hovered.mLine, View.GetLineCharacterColumn(Hovered.mLine, (int)End) });
	const float MouseX = ImGui::GetMousePos().x;
	if (MouseX < From.x || MouseX >= To.x)
	{
		return false;
	}

	const xr_string Name = Line.substr(Begin, End - Begin).c_str();
	if (Link.Document != Document.Path || Link.Line != Hovered.mLine || Link.Begin != Begin || Link.Name != Name)
	{
		Link.Document = Document.Path;
		Link.Line = Hovered.mLine;
		Link.Begin = Begin;
		Link.Name = Name;
		Link.Found = IsLua ? FindLuaDefinition(Document, Name, Link.Path, Link.Target) : LtxCompletion.FindDefinition(Name, Link.Path, Link.Target);
	}
	Link.From = From;
	Link.To = { To.x, To.y + View.GetLineHeight() };
	return true;
}

bool UICoderForm::FindLuaDefinition(const SDocument& Document, const xr_string& Name, xr_string& Path, int& Line) const
{
	Line = FindLuaFunction(Document.Text, Name, false);
	if (Line >= 0)
	{
		Path = Document.Path;
		return true;
	}

	string_path Scripts;
	FS.update_path(Scripts, "$game_scripts$", "");

	const size_t Separator = Name.find_first_of(".:");
	const xr_string Head = Name.substr(0, Separator).c_str();
	xr_string Text;

	if (Name.find(':') == xr_string::npos)
	{
		const xr_string Module = xr_string(Scripts) + Head + ".script";
		if (ReadScript(Module, Text))
		{
			Path = Module;
			Line = Separator == xr_string::npos ? 0 : std::max(FindLuaFunction(Text, Name.substr(Separator + 1).c_str(), false), 0);
			return true;
		}
	}

	const xr_string Last = Name.substr(Name.find_last_of(".:") + 1).c_str();
	if (Separator != xr_string::npos)
	{
		Line = FindLuaFunction(Document.Text, Last, true);
		if (Line >= 0)
		{
			Path = Document.Path;
			return true;
		}
	}

	// ixray_system is loaded into the global namespace, not as modules: a plain name is looked up there first
	const xr_path SystemDir = xr_path(Scripts) / "ixray_system";
	std::error_code Code;
	if (Separator == xr_string::npos)
	{
		for (const auto& Entry : std::filesystem::recursive_directory_iterator(SystemDir, Code))
		{
			const xr_string File = Entry.path().string().c_str();
			if (!Entry.is_regular_file() || GetKind(File) != EFileKind::Lua || !ReadScript(File, Text))
			{
				continue;
			}
			Line = FindLuaFunction(Text, Name, false);
			if (Line >= 0)
			{
				Path = File;
				return true;
			}
		}
	}

	for (const auto& Entry : std::filesystem::recursive_directory_iterator(Scripts, Code))
	{
		if (!Entry.is_regular_file() || GetKind(Entry.path().string().c_str()) != EFileKind::Lua)
		{
			continue;
		}
		const xr_string File = Entry.path().string().c_str();
		if (!ReadScript(File, Text))
		{
			continue;
		}
		const bool IsSystem = File.find("ixray_system") != xr_string::npos;
		// module.function of a module in a subfolder
		if (!IsSystem && Separator != xr_string::npos && Name.find(':') == xr_string::npos && Entry.path().stem().string() == Head.c_str())
		{
			Path = File;
			Line = std::max(FindLuaFunction(Text, Name.substr(Separator + 1).c_str(), false), 0);
			return true;
		}
		Line = FindLuaFunction(Text, Separator == xr_string::npos ? Name : Last, Separator != xr_string::npos);
		if (Line >= 0)
		{
			Path = File;
			return true;
		}
	}
	return false;
}

void UICoderForm::OpenAt(const xr_string& Path, int Line)
{
	if (!OpenFile(Path) || Form == nullptr || Form->SelectRequest < 0)
	{
		return;
	}

	TextEditor& View = *Form->Documents[Form->SelectRequest]->View;
	const TextEditor::Coordinates Position(Line, 0);
	View.SetSelection(Position, Position);
	View.SetCursorPosition(Position);
}

void UICoderForm::UpdateLanguage()
{
	CXVisor& Visor = *EContext.Visor;
	Visor.Load();
	if (LanguageGeneration == Visor.GetGeneration())
	{
		return;
	}

	LanguageGeneration = Visor.GetGeneration();
	Visor.BuildLuaLanguage(LuaLanguage);
	Visor.BuildIniLanguage(IniLanguage);
	for (const xr_unique_ptr<SDocument>& Document : Documents)
	{
		if (Document->Kind == EFileKind::Lua)
		{
			Document->View->SetLanguageDefinition(LuaLanguage);
		}
		else if (Document->Kind == EFileKind::Ini)
		{
			Document->View->SetLanguageDefinition(IniLanguage);
		}
	}
}

void UICoderForm::Draw()
{
	BuildWorkspaceLayout();
	UpdateLanguage();

	// The file tabs are the tabs here: the window tab is hidden while it is alone in its dock node
	ImGuiWindowClass WindowClass;
	WindowClass.DockNodeFlagsOverrideSet = ImGuiDockNodeFlags_AutoHideTabBar;
	ImGui::SetNextWindowClass(&WindowClass);
	if (ImGui::Begin(CoderWindow))
	{
		// The global shortcuts are off while the code editor takes the text input
		if (ImGui::IsWindowFocused(ImGuiFocusedFlags_RootAndChildWindows) && ImGui::IsKeyChordPressed(ImGuiMod_Ctrl | ImGuiKey_S) && Active < Documents.size())
		{
			SaveDocument(*Documents[Active]);
		}

		if (Documents.empty())
		{
			ImGui::Spacing();
			ImGui::TextDisabled("Double click a .script, .xml or .ltx file in the Content Browser to open it here");
		}
		else if (ImGui::BeginTabBar("##coder_files", ImGuiTabBarFlags_Reorderable | ImGuiTabBarFlags_FittingPolicyScroll))
		{
			int CloseIndex = -1;
			for (size_t Index = 0; Index < Documents.size(); ++Index)
			{
				SDocument& Document = *Documents[Index];
				ImGuiTabItemFlags Flags = Document.Modified ? ImGuiTabItemFlags_UnsavedDocument : ImGuiTabItemFlags_None;
				if (SelectRequest == (int)Index)
				{
					Flags |= ImGuiTabItemFlags_SetSelected;
				}

				// The path is the id: files of the same name in different folders
				const xr_string Label = Document.Name + "###" + Document.Path;
				bool Open = true;
				if (ImGui::BeginTabItem(Label.c_str(), &Open, Flags))
				{
					Active = Index;
					ImGui::EndTabItem();
				}
				ImGui::SetItemTooltip("%s", Document.Path.c_str());

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

		if (Active < Documents.size())
		{
			DrawDocument(*Documents[Active]);
		}
	}
	ImGui::End();
}
