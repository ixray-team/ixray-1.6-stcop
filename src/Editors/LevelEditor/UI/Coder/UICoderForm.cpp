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

	CContentView* Browser = new CContentView(ContentWindow);
	Browser->Init();
	Browser->TabIndex = Tab;
	Browser->SetStateName("CoderContentBrowser");
	EContext.UI->Push(Browser, false);
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
		if (!CLuaCompletion::Compile(Document.Name.c_str(), Document.Text, Error, Line))
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
	if (!BakeRequest)
	{
		return;
	}

	BakeRequest = false;
	xr_string Error;
	if (Completion.Bake(Error))
	{
		StatusText = "Lua API baked: " + xr_string::ToString(Completion.GetClassCount()) + " classes";
		ELog.Msg(mtInformation, "Coder: %s", StatusText.c_str());
	}
	else
	{
		StatusText = "Lua API: " + Error;
		ELog.Msg(mtError, "Coder: %s", Error.c_str());
	}
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

void UICoderForm::DrawTopBar(float ButtonSize)
{
	if (Form != nullptr)
	{
		Form->DrawToolbar(Form->Active < Form->Documents.size() ? Form->Documents[Form->Active].get() : nullptr, ButtonSize);
		ImGui::SameLine(0.0f, 8.0f);
		Form->DrawStatus(ButtonSize);
	}
}

void UICoderForm::DrawToolbar(SDocument* Document, float ButtonSize)
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
	if (XRay::ImGui::ToolbarButton("##coder_save", ICON_FA_FLOPPY_DISK " Save", nullptr, { 0.0f, ButtonSize }, ImDrawFlags_RoundCornersLeft) && Document != nullptr)
	{
		SaveDocument(*Document);
	}
	Hint("Save the file (Ctrl+S)");

	ImGui::SameLine();
	if (XRay::ImGui::ToolbarButton("##coder_reload", ICON_FA_ROTATE_RIGHT " Reload", nullptr, { 0.0f, ButtonSize }, ImDrawFlags_RoundCornersRight) && Document != nullptr)
	{
		if (!Document->Modified || ELog.DlgMsg(mtConfirmation, TMsgDlgButtons() | mbYes | mbNo, "Discard the changes of %s?", Document->Name.c_str()) == mrYes)
		{
			Load(*Document);
		}
	}
	Hint("Read the file from the disk again");
	ImGui::EndDisabled();
}

void UICoderForm::DrawStatus(float ButtonSize)
{
	ImGui::BeginDisabled(BakeRequest);
	if (XRay::ImGui::ToolbarButton("##coder_bake", ICON_FA_DATABASE " Bake Lua API", nullptr, { 0.0f, ButtonSize }, ImDrawFlags_RoundCornersAll))
	{
		BakeRequest = true;
		StatusText = "Baking the Lua API...";
	}
	ImGui::EndDisabled();
	if (ImGui::IsItemHovered(ImGuiHoveredFlags_AllowWhenDisabled))
	{
		ImGui::SetTooltip("Writes $app_data_root$\\lua_api.txt and ltx_schema.txt for the code completion (Ctrl+Space)");
	}

	const SDocument* Document = Active < Documents.size() ? Documents[Active].get() : nullptr;
	xr_string Fallback = Document != nullptr ? Document->Path : xr_string();
	if (Document != nullptr && Document->Kind == EFileKind::Ini)
	{
		Fallback += "  |  " + LtxCompletion.GetStatus();
	}
	const char* Text = !StatusText.empty() ? StatusText.c_str() : Fallback.c_str();
	ImGui::SameLine(0.0f, 8.0f);
	ImGui::SetCursorPosY(ImGui::GetCursorPosY() + (ButtonSize - ImGui::GetTextLineHeight()) * 0.5f);
	ImGui::TextDisabled("%s", Text);
}

void UICoderForm::DrawDocument(SDocument& Document)
{
	TextEditor& View = *Document.View;
	Search.Draw(View, { &View });

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
	if (View.IsTextChanged())
	{
		Document.Text = CodeEditor::GetText(View);
		Document.Modified = true;
	}

	if (IsLua)
	{
		Completion.AfterRender(View, Document.Text, CLuaCompletion::FieldResolver());
	}
	else if (IsIni)
	{
		LtxCompletion.AfterRender(View, Document.Path, Document.Text);
	}
}

void UICoderForm::UpdateLanguage()
{
	Completion.Load();
	if (LanguageGeneration == Completion.GetGeneration())
	{
		return;
	}

	LanguageGeneration = Completion.GetGeneration();
	Completion.BuildLanguage(LuaLanguage);
	LtxCompletion.Load();
	LtxCompletion.BuildLanguage(IniLanguage);
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
