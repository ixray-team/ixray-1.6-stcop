#include "stdafx.h"
#include "UILayoutEditorForm.h"
#include "../../../../xrEngine/string_table.h"
#include "../../../../xrScripts/linker.h"
#include "../../../../xrScripts/script_engine.h"
#include "IconsFontAwesome6.h"
#include "imgui_internal.h"

#include <regex>

UILayoutEditorForm* UILayoutEditorForm::Form = nullptr;

static const char* TreeWindow = "Widgets###UILayoutTree";
static const char* CanvasWindow = "UI Layout###UILayoutCanvas";
static const char* CodeWindow = "Layout Code###UILayoutCode";
// Same window as the Scene View properties, so it keeps the shared dock slot
static const char* PropertiesWindow = "Properties";

static const char* WidgetPayload = "UI_LAYOUT_TYPE";
static const char* NodePayload = "UI_LAYOUT_NODE";

static constexpr const char* Fonts[] =
{
	"letterica16", "letterica18", "letterica25",
	"graffiti19", "graffiti22", "graffiti32", "graffiti50",
	"arial_14", "small", "medium", "di"
};

static xr_token FontTokens[] =
{
	{ "letterica16", 0 }, { "letterica18", 1 }, { "letterica25", 2 },
	{ "graffiti19", 3 }, { "graffiti22", 4 }, { "graffiti32", 5 }, { "graffiti50", 6 },
	{ "arial_14", 7 }, { "small", 8 }, { "medium", 9 }, { "di", 10 },
	{ nullptr, 0 }
};

static xr_token TypeTokens[] =
{
	{ "Window", (int)ELayoutWidget::Window },
	{ "Static", (int)ELayoutWidget::Static },
	{ "Text", (int)ELayoutWidget::TextWnd },
	{ "Frame", (int)ELayoutWidget::Frame },
	{ "Button", (int)ELayoutWidget::Button },
	{ "Check Box", (int)ELayoutWidget::Check },
	{ "Edit Box", (int)ELayoutWidget::EditBox },
	{ "Track Bar", (int)ELayoutWidget::TrackBar },
	{ "Progress Bar", (int)ELayoutWidget::ProgressBar },
	{ "Scroll View", (int)ELayoutWidget::ScrollView },
	{ "List Box", (int)ELayoutWidget::ListBox },
	{ "Combo Box", (int)ELayoutWidget::ComboBox },
	{ nullptr, 0 }
};

static xr_token AlignTokens[] =
{
	{ "Left", (int)ELayoutAlign::Left },
	{ "Center", (int)ELayoutAlign::Center },
	{ "Right", (int)ELayoutAlign::Right },
	{ nullptr, 0 }
};

static xr_token VertAlignTokens[] =
{
	{ "Top", (int)ELayoutVertAlign::Top },
	{ "Center", (int)ELayoutVertAlign::Center },
	{ "Bottom", (int)ELayoutVertAlign::Bottom },
	{ nullptr, 0 }
};

static constexpr size_t MaxUndo = 64;

// TextEditor has no XML highlighting: tags, attributes, values and comments
static constexpr float HandleRadius = 4.0f;

static const char* GetWidgetIcon(ELayoutWidget Type)
{
	switch (Type)
	{
	case ELayoutWidget::Window:			return ICON_FA_WINDOW_RESTORE;
	case ELayoutWidget::Static:			return ICON_FA_IMAGE;
	case ELayoutWidget::TextWnd:		return ICON_FA_FONT;
	case ELayoutWidget::Frame:			return ICON_FA_VECTOR_SQUARE;
	case ELayoutWidget::Button:			return ICON_FA_HAND_POINTER;
	case ELayoutWidget::Check:			return ICON_FA_SQUARE_CHECK;
	case ELayoutWidget::EditBox:		return ICON_FA_I_CURSOR;
	case ELayoutWidget::TrackBar:		return ICON_FA_SLIDERS;
	case ELayoutWidget::ProgressBar:	return ICON_FA_BARS_PROGRESS;
	case ELayoutWidget::ScrollView:		return ICON_FA_SCROLL;
	case ELayoutWidget::ListBox:		return ICON_FA_LIST;
	case ELayoutWidget::ComboBox:		return ICON_FA_CARET_DOWN;
	default:							return ICON_FA_SQUARE;
	}
}

static ImU32 GetWidgetColor(ELayoutWidget Type, u8 Alpha)
{
	switch (Type)
	{
	case ELayoutWidget::Window:			return IM_COL32(160, 160, 160, Alpha);
	case ELayoutWidget::Button:
	case ELayoutWidget::Check:			return IM_COL32(90, 170, 255, Alpha);
	case ELayoutWidget::EditBox:
	case ELayoutWidget::ComboBox:		return IM_COL32(120, 220, 140, Alpha);
	case ELayoutWidget::TrackBar:
	case ELayoutWidget::ProgressBar:	return IM_COL32(255, 190, 80, Alpha);
	case ELayoutWidget::ScrollView:
	case ELayoutWidget::ListBox:		return IM_COL32(200, 130, 255, Alpha);
	default:							return IM_COL32(230, 230, 230, Alpha);
	}
}

// letterica18 -> 18 pixels
static float GetFontHeight(const xr_string& Font)
{
	int Height = 0;
	for (char Char : Font)
	{
		if (isdigit((u8)Char))
		{
			Height = Height * 10 + (Char - '0');
		}
	}
	return Height >= 8 && Height <= 64 ? (float)Height : 14.0f;
}

static u32 ToImColor(u32 Color)
{
	return IM_COL32(color_get_R(Color), color_get_G(Color), color_get_B(Color), color_get_A(Color));
}

static bool ReadFileText(const char* Alias, const char* FileName, xr_string& Out)
{
	IReader* Reader = FS.exist(Alias, FileName) ? FS.r_open(Alias, FileName) : nullptr;
	if (Reader == nullptr)
	{
		return false;
	}

	Out.assign((const char*)Reader->pointer(), Reader->length());
	FS.r_close(Reader);
	return true;
}

UILayoutEditorForm::UILayoutEditorForm()
{
	XmlView = xr_make_unique<TextEditor>();
	XmlView->SetShowWhitespaces(false);
	XmlView->SetPalette(CodeEditor::GetPalette());
	XmlView->SetLanguageDefinition(CodeEditor::GetXmlLanguage());

	ScriptView = xr_make_unique<TextEditor>();
	ScriptView->SetShowWhitespaces(false);
	ScriptView->SetPalette(CodeEditor::GetPalette());
	ScriptView->SetLanguageDefinition(TextEditor::LanguageDefinition::Lua());

	Props = xr_make_unique<UIPropertiesForm>();
	Props->SetModifiedEvent(xr_make_delegate(this, &UILayoutEditorForm::OnPropsModified));

	NewLayout();
}

UILayoutEditorForm::~UILayoutEditorForm()
{
	TextureCache.clear();
}

void UILayoutEditorForm::Create(int Tab)
{
	R_ASSERT(Form == nullptr);
	Form = new UILayoutEditorForm();
	Form->TabIndex = Tab;
	EContext.UI->Push(Form, false);
	Device.seqFrame.Add(Form);
}

void UILayoutEditorForm::Destroy()
{
	if (Form != nullptr)
	{
		Device.seqFrame.Remove(Form);
	}
	xr_delete(Form);
}

bool UILayoutEditorForm::OpenWidget(const char* LayoutName)
{
	if (Form == nullptr)
	{
		return false;
	}

	// The top bar selects the tab in its next frame
	EContext.UI->RequestedTabIndex = Form->TabIndex;
	EContext.UI->ActiveTabIndex = Form->TabIndex;
	if (Form->Modified && Form->Doc.Name != LayoutName && ELog.DlgMsg(mtConfirmation, TMsgDlgButtons() | mbYes | mbNo, "The layout has unsaved changes. Discard them?") != mrYes)
	{
		return false;
	}
	return Form->OpenLayout(LayoutName);
}

void UILayoutEditorForm::ShowInGame(const char* LayoutName)
{
	if (Form != nullptr)
	{
		Form->PendingShow = LayoutName;
	}
}

void UILayoutEditorForm::OnFrame()
{
	if (PendingShow.empty())
	{
		return;
	}

	const xr_string LayoutName = PendingShow;
	PendingShow.clear();

	if (g_pGameLevel == nullptr || g_pScriptEngine == nullptr)
	{
		ELog.Msg(mtError, "UI Layout: start Play In Editor to show %s", LayoutName.c_str());
		return;
	}

	if (!FS.exist("$game_scripts$", (LayoutName + ".script").c_str()))
	{
		ELog.Msg(mtError, "UI Layout: scripts\\%s.script is not saved", LayoutName.c_str());
		return;
	}

	// The script may have been saved after the namespace was loaded
	g_pScriptEngine->process_file(LayoutName.c_str(), true);

	// Show() is the entry point, show() comes from the older layout scripts
	luabind::functor<void> Function;
	if (!g_pScriptEngine->functor((LayoutName + ".Show").c_str(), Function) && !g_pScriptEngine->functor((LayoutName + ".show").c_str(), Function))
	{
		ELog.Msg(mtError, "UI Layout: %s.script has no Show() function", LayoutName.c_str());
		return;
	}

	try
	{
		Function();
		ELog.Msg(mtInformation, "UI Layout: %s.Show()", LayoutName.c_str());
	}
	catch (...)
	{
		ELog.Msg(mtError, "UI Layout: %s.Show() failed", LayoutName.c_str());
	}
}

bool UILayoutEditorForm::IsActive()
{
	return Form != nullptr && EContext.UI->ActiveTabIndex == Form->TabIndex;
}

bool UILayoutEditorForm::IsModified()
{
	return Form != nullptr && Form->Modified;
}

bool UILayoutEditorForm::Save()
{
	return Form != nullptr && Form->SaveLayout();
}

bool UILayoutEditorForm::Open()
{
	if (Form == nullptr)
	{
		return false;
	}

	Form->NameDialogOpen = true;
	Form->NameDialogSave = false;
	Form->NameDialogImport = false;
	return true;
}

bool UILayoutEditorForm::Undo(bool Redo)
{
	if (Form == nullptr)
	{
		return false;
	}

	xr_vector<CUILayoutDocument>& From = Redo ? Form->RedoStack : Form->UndoStack;
	xr_vector<CUILayoutDocument>& To = Redo ? Form->UndoStack : Form->RedoStack;
	if (From.empty())
	{
		return false;
	}

	To.push_back(Form->Doc);
	Form->Doc = From.back();
	From.pop_back();

	if (Form->Doc.Find(Form->Selected) == nullptr)
	{
		Form->Selected = 0;
	}
	Form->Modified = true;
	Form->PropsDirty = true;
	return true;
}

bool UILayoutEditorForm::DeleteSelection()
{
	if (Form == nullptr || Form->Selected == 0)
	{
		return false;
	}

	Form->DeleteSelected();
	return true;
}

void UILayoutEditorForm::PushUndo()
{
	// Every change after it may move or replace the widgets the properties point to
	PropsDirty = true;
	UndoStack.push_back(Doc);
	if (UndoStack.size() > MaxUndo)
	{
		UndoStack.erase(UndoStack.begin());
	}
	RedoStack.clear();
}

void UILayoutEditorForm::MarkModified()
{
	Modified = true;
}

void UILayoutEditorForm::Select(u32 Id)
{
	Selected = Doc.Find(Id) != nullptr ? Id : 0;
}

void UILayoutEditorForm::AddWidget(ELayoutWidget Type, u32 Parent, const Fvector2* AbsPos)
{
	PushUndo();

	SLayoutWidget Widget = LayoutWidget::MakeDefault(Type);
	const Fvector2 ParentPos = Parent != 0 ? Doc.GetAbsPos(Parent) : Fvector2{ 0.0f, 0.0f };
	if (AbsPos != nullptr)
	{
		Widget.Pos = { Snap(AbsPos->x - ParentPos.x), Snap(AbsPos->y - ParentPos.y) };
	}
	else if (Parent != 0)
	{
		Widget.Pos = { 10.0f, 10.0f };
	}
	else
	{
		Widget.Pos = { Snap((CUILayoutDocument::ScreenWidth - Widget.Size.x) * 0.5f), Snap((CUILayoutDocument::ScreenHeight - Widget.Size.y) * 0.5f) };
	}

	Select(Doc.Add(Widget, Parent).Id);
	MarkModified();
}

void UILayoutEditorForm::DeleteSelected()
{
	if (Doc.Find(Selected) == nullptr)
	{
		return;
	}

	PushUndo();
	const u32 Parent = Doc.Find(Selected)->ParentId;
	Doc.Remove(Selected);
	Select(Parent);
	MarkModified();
}

void UILayoutEditorForm::NewLayout()
{
	Doc = CUILayoutDocument();
	UndoStack.clear();
	RedoStack.clear();
	Selected = 0;
	Modified = false;
	NeedFit = true;
	PropsDirty = true;

	// A fresh dialog starts with a background and the two usual buttons
	SLayoutWidget Background = LayoutWidget::MakeDefault(ELayoutWidget::Static);
	Background.Name = "background";
	Background.Pos = { 262.0f, 234.0f };
	Background.Size = { 500.0f, 300.0f };
	const u32 BackgroundId = Doc.Add(Background, 0).Id;

	SLayoutWidget Ok = LayoutWidget::MakeDefault(ELayoutWidget::Button);
	Ok.Name = "btn_ok";
	Ok.Text = "ui_inv_ok";
	Ok.Pos = { 112.0f, 252.0f };
	Doc.Add(Ok, BackgroundId);

	SLayoutWidget Cancel = LayoutWidget::MakeDefault(ELayoutWidget::Button);
	Cancel.Name = "btn_cancel";
	Cancel.Text = "Btn_Cancel";
	Cancel.Pos = { 261.0f, 252.0f };
	Doc.Add(Cancel, BackgroundId);

	ScriptSource = Doc.BuildScript();
	XmlEdited = false;
	StatusText = "New layout";
}

bool UILayoutEditorForm::OpenLayout(const char* LayoutName)
{
	CUILayoutDocument Loaded;
	xr_string Error;
	// The working copy loads as it was saved, the xml is parsed only without it
	const bool IsLoaded = CUILayoutDocument::WidgetExists(LayoutName) ? Loaded.LoadWidget(LayoutName, Error) : Loaded.Load(LayoutName, Error);
	if (!IsLoaded)
	{
		ELog.DlgMsg(mtError, "UI Layout: %s", Error.c_str());
		return false;
	}

	Doc = Loaded;
	if (!CUILayoutDocument::ReadScript(LayoutName, ScriptSource))
	{
		ScriptSource = Doc.BuildScript();
	}
	XmlEdited = false;
	UndoStack.clear();
	RedoStack.clear();
	Selected = 0;
	Modified = false;
	NeedFit = true;
	PropsDirty = true;
	StatusText = "Opened " + Doc.Name;
	return true;
}

bool UILayoutEditorForm::ImportLayout(const char* FileName)
{
	CUILayoutDocument Imported;
	xr_string Error;
	if (!Imported.Import(FileName, Error))
	{
		ELog.DlgMsg(mtError, "UI Layout: %s", Error.c_str());
		return false;
	}

	Doc = Imported;
	// The stock scripts are not generated ones: the import starts a script of its own
	ScriptSource = Doc.BuildScript();
	XmlEdited = false;
	UndoStack.clear();
	RedoStack.clear();
	Selected = 0;
	// Nothing is written yet: the layout is saved under its new name
	Modified = true;
	NeedFit = true;
	PropsDirty = true;
	StatusText = "Imported " + xr_string(FileName) + " as " + Doc.Name + ", " + xr_string::ToString((u32)Doc.Widgets.size()) + " widgets";
	return true;
}

bool UILayoutEditorForm::SaveLayout()
{
	if (!ApplyXmlEdit())
	{
		return false;
	}

	SyncScript();
	xr_string Error;
	if (!Doc.Save(ScriptSource, Error))
	{
		ELog.DlgMsg(mtError, "UI Layout: %s", Error.c_str());
		return false;
	}

	Modified = false;
	StatusText = "Saved " + Doc.Name + ".xml, " + Doc.Name + "_16.xml, " + Doc.Name + ".script";

	// The script is saved anyway: a half written handler must not lose the layout
	xr_string CompileError;
	int ErrorLine = 0;
	ScriptView->ClearErrorMarkers();
	if (!CXVisor::Compile((Doc.Name + ".script").c_str(), ScriptSource, CompileError, ErrorLine))
	{
		if (ErrorLine > 0)
		{
			ScriptView->AddErrorMarker(ErrorLine, CompileError.c_str());
			ScriptJumpLine = ErrorLine - 1;
			PreviewTab = 2;
			PreviewTabRequest = true;
		}
		StatusText += ". Script error: " + CompileError;
		ELog.Msg(mtError, "UI Layout: %s", CompileError.c_str());
	}

	// The game files are written, a failed working copy only costs the quick load
	if (Doc.SaveWidget(Error))
	{
		StatusText += ", widgets\\" + Doc.Name + CUILayoutDocument::WidgetExt;
	}
	else
	{
		ELog.Msg(mtError, "UI Layout: %s", Error.c_str());
	}
	ELog.Msg(mtInformation, "UI Layout: %s", StatusText.c_str());
	return true;
}

void UILayoutEditorForm::LoadTextureDescriptions()
{
	TexturesLoaded = true;
	TextureRegions.clear();
	TextureIds.clear();

	FS_FileSet Files;
	FS.file_list(Files, _game_config_, FS_ListFiles, "*.xml");

	for (const FS_File& File : Files)
	{
		if (!File.name.starts_with("ui\\textures_descr\\"))
		{
			continue;
		}

		xr_string Text;
		if (!ReadFileText(_game_config_, File.name.c_str(), Text))
		{
			continue;
		}

		tinyxml2::XMLDocument Xml;
		if (Xml.Parse(Text.data(), Text.size()) != tinyxml2::XML_SUCCESS || Xml.RootElement() == nullptr)
		{
			continue;
		}

		for (tinyxml2::XMLElement* FileNode = Xml.RootElement()->FirstChildElement("file"); FileNode != nullptr; FileNode = FileNode->NextSiblingElement("file"))
		{
			const char* TextureFile = FileNode->Attribute("name");
			if (TextureFile == nullptr)
			{
				continue;
			}

			for (tinyxml2::XMLElement* Node = FileNode->FirstChildElement("texture"); Node != nullptr; Node = Node->NextSiblingElement("texture"))
			{
				const char* Id = Node->Attribute("id");
				if (Id == nullptr)
				{
					continue;
				}

				xr_string Key = Id;
				xr_strlwr(Key);
				if (!TextureRegions.contains(Key))
				{
					TextureIds.push_back(Id);
				}

				STextureRegion& Region = TextureRegions[Key];
				Region.File = TextureFile;
				Region.Rect = { Node->FloatAttribute("x"), Node->FloatAttribute("y"), Node->FloatAttribute("width"), Node->FloatAttribute("height") };
			}
		}
	}

	std::sort(TextureIds.begin(), TextureIds.end(), [](const xr_string& Left, const xr_string& Right) { return _stricmp(Left.c_str(), Right.c_str()) < 0; });
}

const UILayoutEditorForm::STextureCache* UILayoutEditorForm::GetTexture(const xr_string& Id)
{
	if (Id.empty())
	{
		return nullptr;
	}

	xr_string Key = Id;
	xr_strlwr(Key);

	auto Cached = TextureCache.find(Key);
	if (Cached != TextureCache.end())
	{
		return Cached->second.Valid ? &Cached->second : nullptr;
	}

	if (!TexturesLoaded)
	{
		LoadTextureDescriptions();
	}

	STextureCache& Entry = TextureCache[Key];

	// Buttons and edit boxes are described per state or per part: <id>_e, <id>_back
	const STextureRegion* Region = nullptr;
	for (const char* Suffix : { "", "_e", "_back", "_b", "_n" })
	{
		auto It = TextureRegions.find(Key + Suffix);
		if (It != TextureRegions.end())
		{
			Region = &It->second;
			break;
		}
	}

	const xr_string File = Region != nullptr ? Region->File : Id;
	if (!FS.exist("$game_textures$", (File + ".dds").c_str()))
	{
		return nullptr;
	}

	u32 Memory = 0;
	IRHISurface* Surface = RImplementation.texture_load(File.c_str(), Memory);
	if (Surface == nullptr)
	{
		return nullptr;
	}

	Entry.Texture = new CTexture();
	Entry.Texture->surface_set(Surface);
	Surface->Release();

	const float Width = (float)Entry.Texture->get_Width();
	const float Height = (float)Entry.Texture->get_Height();
	if (Region != nullptr && Width > 0.0f && Height > 0.0f)
	{
		Entry.UV0 = { Region->Rect.x / Width, Region->Rect.y / Height };
		Entry.UV1 = { (Region->Rect.x + Region->Rect.z) / Width, (Region->Rect.y + Region->Rect.w) / Height };
	}

	Entry.Valid = Entry.Texture->get_SRView() != nullptr;
	return Entry.Valid ? &Entry : nullptr;
}

ImVec2 UILayoutEditorForm::ToScreen(const Fvector2& Point) const
{
	return { CanvasOrigin.x + Pan.x + Point.x * Zoom, CanvasOrigin.y + Pan.y + Point.y * Zoom };
}

Fvector2 UILayoutEditorForm::ToLayout(const ImVec2& Point) const
{
	return { (Point.x - CanvasOrigin.x - Pan.x) / Zoom, (Point.y - CanvasOrigin.y - Pan.y) / Zoom };
}

float UILayoutEditorForm::Snap(float Value) const
{
	if (!ShowGrid || GridStep <= 1)
	{
		return std::round(Value);
	}
	return std::round(Value / GridStep) * GridStep;
}

void UILayoutEditorForm::GetPreviewRect(const SLayoutWidget& Widget, Fvector2& AbsPos, Fvector2& Size) const
{
	AbsPos = { 0.0f, 0.0f };
	Size = Widget.Size;

	const SLayoutWidget* Current = &Widget;
	for (int Depth = 0; Current != nullptr && Depth < 256; ++Depth)
	{
		Fvector2 Pos = Current->Pos;
		if (WidePreview)
		{
			constexpr float Scale = CUILayoutDocument::WideScale;
			Pos.x = Current->ParentId == 0 ? Pos.x * Scale + CUILayoutDocument::ScreenWidth * (1.0f - Scale) * 0.5f : Pos.x * Scale;
			Pos.x = std::floor(Pos.x);
		}
		AbsPos.add(Pos);
		Current = Current->ParentId != 0 ? Doc.Find(Current->ParentId) : nullptr;
	}

	if (WidePreview)
	{
		Size.x = std::floor(Size.x * CUILayoutDocument::WideScale);
	}
}

u32 UILayoutEditorForm::PickWidget(const Fvector2& Point) const
{
	const xr_vector<u32> Ordered = Doc.GetOrdered();
	for (auto It = Ordered.rbegin(); It != Ordered.rend(); ++It)
	{
		const SLayoutWidget& Widget = *Doc.Find(*It);

		bool Hidden = false;
		for (const SLayoutWidget* Current = &Widget; Current != nullptr; Current = Current->ParentId != 0 ? Doc.Find(Current->ParentId) : nullptr)
		{
			Hidden |= Current->Hidden;
		}
		if (Hidden)
		{
			continue;
		}

		Fvector2 AbsPos, Size;
		GetPreviewRect(Widget, AbsPos, Size);
		if (Point.x >= AbsPos.x && Point.y >= AbsPos.y && Point.x <= AbsPos.x + Size.x && Point.y <= AbsPos.y + Size.y)
		{
			return Widget.Id;
		}
	}
	return 0;
}

void UILayoutEditorForm::FitCanvas(const ImVec2& CanvasSize)
{
	constexpr float Margin = 24.0f;
	Zoom = std::min((CanvasSize.x - Margin * 2.0f) / CUILayoutDocument::ScreenWidth, (CanvasSize.y - Margin * 2.0f) / CUILayoutDocument::ScreenHeight);
	Zoom = std::clamp(Zoom, 0.1f, 8.0f);
	Pan = { (CanvasSize.x - CUILayoutDocument::ScreenWidth * Zoom) * 0.5f, (CanvasSize.y - CUILayoutDocument::ScreenHeight * Zoom) * 0.5f };
}

static void BuildWorkspaceLayout()
{
	// Checked once per session, so the user can still undock the windows afterwards
	static bool Checked = false;
	if (Checked)
	{
		return;
	}

	ImGuiWindow* Viewport = ImGui::FindWindowByName("Render##0");
	if (Viewport == nullptr || Viewport->DockNode == nullptr)
	{
		return;
	}

	ImGuiWindow* Tree = ImGui::FindWindowByName(TreeWindow);
	ImGuiWindow* Canvas = ImGui::FindWindowByName(CanvasWindow);
	ImGuiWindow* Code = ImGui::FindWindowByName(CodeWindow);
	if (Tree == nullptr || Canvas == nullptr || Code == nullptr)
	{
		return;
	}
	Checked = true;

	// Already placed by the saved layout
	if (Tree->DockId != 0 && Canvas->DockId != 0 && Tree->DockId != Canvas->DockId)
	{
		return;
	}

	ImGuiID Center = Viewport->DockId;
	ImGuiID Left = ImGui::DockBuilderSplitNode(Center, ImGuiDir_Left, 0.2f, nullptr, &Center);

	ImGui::DockBuilderDockWindow(TreeWindow, Left);
	ImGui::DockBuilderDockWindow(CanvasWindow, Center);
	ImGui::DockBuilderDockWindow(CodeWindow, Center);
	ImGui::DockBuilderFinish(ImGui::DockNodeGetRootNode(Viewport->DockNode)->ID);
}

void UILayoutEditorForm::Draw()
{
	BuildWorkspaceLayout();
	SyncScript();

	if (ImGui::Begin(TreeWindow))
	{
		DrawPalette();
		ImGui::Separator();
		DrawHierarchy();
	}
	ImGui::End();

	ImGui::PushStyleVar(ImGuiStyleVar_WindowPadding, ImVec2(0, 0));
	const bool CanvasOpen = ImGui::Begin(CanvasWindow, nullptr, ImGuiWindowFlags_NoScrollbar | ImGuiWindowFlags_NoScrollWithMouse);
	ImGui::PopStyleVar();
	if (CanvasOpen)
	{
		DrawToolbar();
		DrawCanvas();
	}
	ImGui::End();

	if (ImGui::Begin(CodeWindow))
	{
		// The global shortcuts are off while the code editor takes the text input
		if (ImGui::IsWindowFocused(ImGuiFocusedFlags_RootAndChildWindows) && ImGui::IsKeyChordPressed(ImGuiMod_Ctrl | ImGuiKey_S, ImGuiInputFlags_None))
		{
			SaveLayout();
		}
		DrawCodePreview();
	}
	ImGui::End();

	if (ImGui::Begin(PropertiesWindow))
	{
		DrawProperties();
	}
	ImGui::End();

	DrawNameDialog();
	Localization.Draw();
}

void UILayoutEditorForm::OpenLocalization(const SLayoutWidget& Widget)
{
	// A text that is already an id is reused, otherwise the id is <layout>_<widget>
	const bool IsId = !Widget.Text.empty() && Doc.IsValidName(Widget.Text.c_str()) && g_pStringTable != nullptr && !g_pStringTable->HasString(Widget.Text.c_str());
	const xr_string StringId = IsId ? Widget.Text : xr_string(Doc.Name + "_" + Widget.Name);
	const xr_string FileName = "st_" + Doc.Name + ".xml";
	const u32 Id = Widget.Id;

	Localization.Open(StringId.c_str(), ("Widget " + Widget.Name).c_str(), FileName.c_str(), [this, Id](const char* NewId)
	{
		if (Doc.Find(Id) != nullptr)
		{
			PushUndo();
			Doc.Find(Id)->Text = NewId;
			MarkModified();
		}
	});
}

void UILayoutEditorForm::DrawToolbar()
{
	// Local coordinates start under the dock tab bar
	ImGui::SetCursorPos({ ImGui::GetCursorPosX() + 6.0f, ImGui::GetCursorPosY() + 6.0f });
	ImGui::BeginGroup();

	if (ImGui::Button(ICON_FA_FILE " New"))
	{
		if (!Modified || ELog.DlgMsg(mtConfirmation, TMsgDlgButtons() | mbYes | mbNo, "The layout has unsaved changes. Discard them?") == mrYes)
		{
			NewLayout();
		}
	}
	ImGui::SameLine();
	if (ImGui::Button(ICON_FA_FOLDER_OPEN " Open"))
	{
		Open();
	}
	ImGui::SameLine();
	if (ImGui::Button(ICON_FA_FILE_IMPORT " Import"))
	{
		NameDialogOpen = true;
		NameDialogSave = false;
		NameDialogImport = true;
	}
	ImGui::SetItemTooltip("Import a stock window from configs\\ui as a new layout");
	ImGui::SameLine();
	if (ImGui::Button(ICON_FA_FLOPPY_DISK " Save"))
	{
		SaveLayout();
	}
	ImGui::SameLine();
	if (ImGui::Button("Save As"))
	{
		NameDialogOpen = true;
		NameDialogSave = true;
		NameDialogImport = false;
	}

	ImGui::SameLine(0.0f, 16.0f);
	ImGui::BeginDisabled(UndoStack.empty());
	if (ImGui::Button(ICON_FA_ROTATE_LEFT))
	{
		Undo(false);
	}
	ImGui::EndDisabled();
	ImGui::SameLine();
	ImGui::BeginDisabled(RedoStack.empty());
	if (ImGui::Button(ICON_FA_ROTATE_RIGHT))
	{
		Undo(true);
	}
	ImGui::EndDisabled();

	// Frames make the unchecked boxes and the step field stand out from the toolbar
	constexpr float GroupSpacing = 16.0f;
	ImGui::PushStyleVar(ImGuiStyleVar_FrameBorderSize, 1.0f);
	ImGui::SameLine(0.0f, GroupSpacing);
	ImGui::Checkbox("16:9", &WidePreview);
	ImGui::SetItemTooltip("Preview the generated %s_16.xml. Editing is off in this mode", Doc.Name.c_str());
	ImGui::SameLine(0.0f, GroupSpacing);
	ImGui::Checkbox("Grid", &ShowGrid);
	ImGui::SameLine();
	ImGui::SetNextItemWidth(ImGui::CalcTextSize("000").x + ImGui::GetFrameHeight() * 2.0f + ImGui::GetStyle().FramePadding.x * 2.0f + ImGui::GetStyle().ItemInnerSpacing.x * 2.0f);
	ImGui::BeginDisabled(!ShowGrid);
	if (ImGui::InputInt("##grid_step", &GridStep, 1, 4))
	{
		GridStep = std::clamp(GridStep, 1, 64);
	}
	ImGui::EndDisabled();
	ImGui::SetItemTooltip("Grid step");
	ImGui::SameLine(0.0f, GroupSpacing);
	ImGui::Checkbox("Bounds", &ShowBounds);
	ImGui::PopStyleVar();
	ImGui::SameLine(0.0f, GroupSpacing);
	if (ImGui::Button(ICON_FA_EXPAND " Fit"))
	{
		NeedFit = true;
	}
	ImGui::SameLine();
	ImGui::TextDisabled("%s%s  %d%%  %s", Doc.Name.c_str(), Modified ? "*" : "", (int)(Zoom * 100.0f), StatusText.c_str());

	ImGui::EndGroup();
}

void UILayoutEditorForm::DrawPalette()
{
	const ImGuiStyle& Style = ImGui::GetStyle();
	const float Avail = ImGui::GetContentRegionAvail().x;

	// Two or more columns fill the panel; labels give way to icons when they don't fit
	constexpr float MinButtonWidth = 90.0f;
	const int Columns = std::clamp((int)((Avail + Style.ItemSpacing.x) / (MinButtonWidth + Style.ItemSpacing.x)), 2, 4);
	const float ButtonWidth = std::floor((Avail - Style.ItemSpacing.x * (Columns - 1)) / Columns);

	for (int Index = 0; Index < (int)ELayoutWidget::Count; ++Index)
	{
		const ELayoutWidget Type = (ELayoutWidget)Index;
		if (Index % Columns != 0)
		{
			ImGui::SameLine();
		}

		string128 Label;
		xr_sprintf(Label, "%s %s", GetWidgetIcon(Type), LayoutWidget::GetTypeName(Type));
		const bool IconOnly = ImGui::CalcTextSize(Label).x + Style.FramePadding.x * 2.0f > ButtonWidth;

		ImGui::PushID(Index);
		ImGui::PushStyleVar(ImGuiStyleVar_ButtonTextAlign, ImVec2(IconOnly ? 0.5f : 0.0f, 0.5f));
		if (ImGui::Button(IconOnly ? GetWidgetIcon(Type) : Label, { ButtonWidth, 0.0f }))
		{
			AddWidget(Type, Selected, nullptr);
		}
		ImGui::PopStyleVar();
		if (IconOnly)
		{
			ImGui::SetItemTooltip("%s", LayoutWidget::GetTypeName(Type));
		}

		if (ImGui::BeginDragDropSource())
		{
			ImGui::SetDragDropPayload(WidgetPayload, &Index, sizeof(Index));
			ImGui::Text("%s", Label);
			ImGui::EndDragDropSource();
		}
		ImGui::PopID();
	}
}

void UILayoutEditorForm::DrawHierarchy()
{
	ImGuiTreeNodeFlags Flags = ImGuiTreeNodeFlags_DefaultOpen | ImGuiTreeNodeFlags_OpenOnArrow | ImGuiTreeNodeFlags_SpanAvailWidth;
	if (Selected == 0)
	{
		Flags |= ImGuiTreeNodeFlags_Selected;
	}

	const bool Open = ImGui::TreeNodeEx("##dialog_root", Flags, ICON_FA_DISPLAY " %s (self)", Doc.GetClassName().c_str());
	if (ImGui::IsItemClicked(ImGuiMouseButton_Left) && !ImGui::IsItemToggledOpen())
	{
		Select(0);
	}

	if (ImGui::BeginDragDropTarget())
	{
		if (const ImGuiPayload* Payload = ImGui::AcceptDragDropPayload(NodePayload))
		{
			const u32 Id = *(const u32*)Payload->Data;
			PushUndo();
			if (Doc.Reparent(Id, 0))
			{
				MarkModified();
			}
			else
			{
				UndoStack.pop_back();
			}
		}
		ImGui::EndDragDropTarget();
	}

	if (Open)
	{
		// Copy: the tree may change the widget list
		xr_vector<u32> Roots;
		for (const SLayoutWidget& Widget : Doc.Widgets)
		{
			if (Widget.ParentId == 0)
			{
				Roots.push_back(Widget.Id);
			}
		}

		for (u32 Id : Roots)
		{
			DrawHierarchyNode(Id);
		}
		ImGui::TreePop();
	}

	if (ImGui::IsWindowFocused(ImGuiFocusedFlags_ChildWindows) && !ImGui::GetIO().WantTextInput && ImGui::IsKeyPressed(ImGuiKey_Delete, false))
	{
		DeleteSelected();
	}
}

void UILayoutEditorForm::DrawHierarchyNode(u32 Id)
{
	const SLayoutWidget* Widget = Doc.Find(Id);
	if (Widget == nullptr)
	{
		return;
	}

	xr_vector<u32> Children;
	for (const SLayoutWidget& Child : Doc.Widgets)
	{
		if (Child.ParentId == Id)
		{
			Children.push_back(Child.Id);
		}
	}

	ImGuiTreeNodeFlags Flags = ImGuiTreeNodeFlags_DefaultOpen | ImGuiTreeNodeFlags_OpenOnArrow | ImGuiTreeNodeFlags_SpanAvailWidth;
	if (Children.empty())
	{
		Flags |= ImGuiTreeNodeFlags_Leaf;
	}
	if (Selected == Id)
	{
		Flags |= ImGuiTreeNodeFlags_Selected;
	}

	ImGui::PushID((int)Id);
	if (Widget->Hidden)
	{
		ImGui::PushStyleColor(ImGuiCol_Text, ImGui::GetStyleColorVec4(ImGuiCol_TextDisabled));
	}
	const bool Open = ImGui::TreeNodeEx("##node", Flags, "%s %s", GetWidgetIcon(Widget->Type), Widget->Name.c_str());
	if (Widget->Hidden)
	{
		ImGui::PopStyleColor();
	}

	if (ImGui::IsItemClicked(ImGuiMouseButton_Left) && !ImGui::IsItemToggledOpen())
	{
		Select(Id);
	}
	if (ImGui::IsItemHovered() && ImGui::IsMouseDoubleClicked(ImGuiMouseButton_Left))
	{
		GoToHandler(Id);
	}

	if (ImGui::BeginDragDropSource())
	{
		ImGui::SetDragDropPayload(NodePayload, &Id, sizeof(Id));
		ImGui::Text("%s", Widget->Name.c_str());
		ImGui::EndDragDropSource();
	}

	if (ImGui::BeginDragDropTarget())
	{
		if (const ImGuiPayload* Payload = ImGui::AcceptDragDropPayload(NodePayload))
		{
			const u32 Dragged = *(const u32*)Payload->Data;
			PushUndo();
			if (Doc.Reparent(Dragged, Id))
			{
				MarkModified();
			}
			else
			{
				UndoStack.pop_back();
			}
		}
		if (const ImGuiPayload* Payload = ImGui::AcceptDragDropPayload(WidgetPayload))
		{
			AddWidget((ELayoutWidget)*(const int*)Payload->Data, Id, nullptr);
		}
		ImGui::EndDragDropTarget();
	}

	DrawWidgetContextMenu(Id);

	// Visibility and lock toggles on the right
	Widget = Doc.Find(Id);
	if (Widget != nullptr)
	{
		// Fixed width for both icons, so the row ends at the panel edge
		const float ButtonWidth = ImGui::CalcTextSize(ICON_FA_EYE_SLASH).x + ImGui::GetStyle().FramePadding.x * 2.0f;
		ImGui::SameLine(ImGui::GetWindowContentRegionMax().x - ButtonWidth * 2.0f - ImGui::GetStyle().ItemSpacing.x);
		// No vertical padding: the buttons keep the tree row height
		ImGui::PushStyleVar(ImGuiStyleVar_FramePadding, { ImGui::GetStyle().FramePadding.x, 0.0f });
		ImGui::PushStyleColor(ImGuiCol_Button, IM_COL32(0, 0, 0, 0));
		if (ImGui::Button(Widget->Hidden ? ICON_FA_EYE_SLASH : ICON_FA_EYE, { ButtonWidth, 0.0f }))
		{
			Doc.Find(Id)->Hidden = !Widget->Hidden;
		}
		ImGui::SameLine();
		if (ImGui::Button(Widget->Locked ? ICON_FA_LOCK : ICON_FA_LOCK_OPEN, { ButtonWidth, 0.0f }))
		{
			Doc.Find(Id)->Locked = !Widget->Locked;
		}
		ImGui::PopStyleColor();
		ImGui::PopStyleVar();
	}

	if (Open)
	{
		for (u32 Child : Children)
		{
			DrawHierarchyNode(Child);
		}
		ImGui::TreePop();
	}
	ImGui::PopID();
}

void UILayoutEditorForm::DrawWidgetContextMenu(u32 Id)
{
	if (!ImGui::BeginPopupContextItem("##widget_menu"))
	{
		return;
	}

	Select(Id);
	if (ImGui::MenuItem(ICON_FA_CLONE " Duplicate"))
	{
		PushUndo();
		Select(Doc.Duplicate(Id));
		MarkModified();
	}
	if (ImGui::MenuItem("Move Up"))
	{
		PushUndo();
		Doc.MoveSibling(Id, -1) ? MarkModified() : UndoStack.pop_back();
	}
	if (ImGui::MenuItem("Move Down"))
	{
		PushUndo();
		Doc.MoveSibling(Id, 1) ? MarkModified() : UndoStack.pop_back();
	}
	if (ImGui::BeginMenu("Add Child"))
	{
		for (int Index = 0; Index < (int)ELayoutWidget::Count; ++Index)
		{
			const ELayoutWidget Type = (ELayoutWidget)Index;
			string128 Label;
			xr_sprintf(Label, "%s %s", GetWidgetIcon(Type), LayoutWidget::GetTypeName(Type));
			if (ImGui::MenuItem(Label))
			{
				AddWidget(Type, Id, nullptr);
			}
		}
		ImGui::EndMenu();
	}
	ImGui::Separator();
	if (ImGui::MenuItem(ICON_FA_TRASH " Delete", "Del"))
	{
		DeleteSelected();
	}
	ImGui::EndPopup();
}

void UILayoutEditorForm::DrawWidget(ImDrawList* DrawList, const SLayoutWidget& Widget, const Fvector2& AbsPos, const Fvector2& Size)
{
	const ImVec2 Min = ToScreen(AbsPos);
	const ImVec2 Max = ToScreen({ AbsPos.x + Size.x, AbsPos.y + Size.y });
	const float Height = Max.y - Min.y;

	DrawList->PushClipRect(Min, Max, true);

	const STextureCache* Texture = LayoutWidget::HasTexture(Widget.Type) ? GetTexture(Widget.Texture) : nullptr;
	ImVec2 ContentMin = Min;

	switch (Widget.Type)
	{
	case ELayoutWidget::Check:
	{
		// The check box texture is the box, the text follows it
		const ImVec2 BoxMax = { Min.x + Height, Max.y };
		if (Texture != nullptr)
		{
			DrawList->AddImage(Texture->Texture->get_SRView()->GetRawSRV(), Min, BoxMax, Texture->UV0, Texture->UV1);
		}
		else
		{
			DrawList->AddRect({ Min.x + 2.0f, Min.y + 2.0f }, { BoxMax.x - 2.0f, BoxMax.y - 2.0f }, IM_COL32(200, 200, 200, 255));
		}
		ContentMin.x = BoxMax.x + 4.0f * Zoom;
		break;
	}
	case ELayoutWidget::ProgressBar:
	{
		DrawList->AddRectFilled(Min, Max, IM_COL32(20, 20, 20, 180));
		const float Range = Widget.Max - Widget.Min;
		const float Factor = Range != 0.0f ? std::clamp((Widget.Value - Widget.Min) / Range, 0.0f, 1.0f) : 0.0f;
		const ImVec2 FillMax = Widget.Horizontal ? ImVec2(Min.x + (Max.x - Min.x) * Factor, Max.y) : ImVec2(Max.x, Max.y);
		const ImVec2 FillMin = Widget.Horizontal ? Min : ImVec2(Min.x, Max.y - Height * Factor);
		if (Texture != nullptr)
		{
			DrawList->AddImage(Texture->Texture->get_SRView()->GetRawSRV(), FillMin, FillMax, Texture->UV0, Texture->UV1);
		}
		else
		{
			DrawList->AddRectFilled(FillMin, FillMax, IM_COL32(200, 60, 40, 220));
		}
		break;
	}
	case ELayoutWidget::TrackBar:
	{
		const float CenterY = (Min.y + Max.y) * 0.5f;
		DrawList->AddLine({ Min.x, CenterY }, { Max.x, CenterY }, IM_COL32(170, 170, 170, 255), std::max(1.0f, 2.0f * Zoom));
		const float Range = Widget.Max - Widget.Min;
		const float Factor = Range != 0.0f ? std::clamp((Widget.Value - Widget.Min) / Range, 0.0f, 1.0f) : 0.0f;
		const float KnobX = Min.x + (Max.x - Min.x) * Factor;
		DrawList->AddRectFilled({ KnobX - 3.0f * Zoom, Min.y }, { KnobX + 3.0f * Zoom, Max.y }, IM_COL32(220, 220, 220, 255));
		break;
	}
	case ELayoutWidget::EditBox:
	case ELayoutWidget::ComboBox:
		if (Texture != nullptr)
		{
			DrawList->AddImage(Texture->Texture->get_SRView()->GetRawSRV(), Min, Max, Texture->UV0, Texture->UV1);
		}
		else
		{
			DrawList->AddRectFilled(Min, Max, IM_COL32(15, 15, 15, 200));
			DrawList->AddRect(Min, Max, IM_COL32(110, 110, 110, 255));
		}
		if (Widget.Type == ELayoutWidget::ComboBox)
		{
			const float Arrow = Height * 0.3f;
			const ImVec2 Center = { Max.x - Height * 0.5f, (Min.y + Max.y) * 0.5f };
			DrawList->AddTriangleFilled({ Center.x - Arrow, Center.y - Arrow * 0.5f }, { Center.x + Arrow, Center.y - Arrow * 0.5f }, { Center.x, Center.y + Arrow * 0.5f }, IM_COL32(200, 200, 200, 255));
		}
		ContentMin.x += 4.0f * Zoom;
		break;
	case ELayoutWidget::ScrollView:
	case ELayoutWidget::ListBox:
		DrawList->AddRectFilled(Min, Max, IM_COL32(10, 10, 10, 120));
		DrawList->AddRectFilled({ Max.x - 6.0f * Zoom, Min.y }, Max, IM_COL32(90, 90, 90, 200));
		break;
	default:
		if (Texture != nullptr)
		{
			DrawList->AddImage(Texture->Texture->get_SRView()->GetRawSRV(), Min, Max, Texture->UV0, Texture->UV1);
		}
		else if (!Widget.Texture.empty() && LayoutWidget::HasTexture(Widget.Type))
		{
			// Unknown texture: a hatched placeholder with its id
			DrawList->AddRectFilled(Min, Max, IM_COL32(120, 40, 120, 90));
			DrawList->AddText({ Min.x + 2.0f, Min.y + 2.0f }, IM_COL32(255, 160, 255, 255), Widget.Texture.c_str());
		}
		else if (Widget.Type == ELayoutWidget::Button)
		{
			DrawList->AddRectFilled(Min, Max, IM_COL32(60, 60, 60, 220));
		}
		break;
	}

	// Text, translated like the game does
	if (LayoutWidget::HasText(Widget.Type) && Widget.Type != ELayoutWidget::ListBox && !Widget.Text.empty())
	{
		xr_string Text = Widget.Text;
		if (g_pStringTable != nullptr)
		{
			const xr_string GameText = Platform::UTF8_to_CP1251(Text);
			Text = Platform::ANSI_TO_UTF8(g_pStringTable->translate(GameText.c_str()).c_str());
		}

		const float FontSize = GetFontHeight(Widget.Font) * Zoom;
		ImFont* Font = ImGui::GetFont();
		const ImVec2 TextSize = Font->CalcTextSizeA(FontSize, FLT_MAX, 0.0f, Text.c_str());

		ImVec2 TextPos = ContentMin;
		const float AreaWidth = Max.x - ContentMin.x;
		if (Widget.Align == ELayoutAlign::Center)
		{
			TextPos.x += (AreaWidth - TextSize.x) * 0.5f;
		}
		else if (Widget.Align == ELayoutAlign::Right)
		{
			TextPos.x += AreaWidth - TextSize.x;
		}

		if (Widget.VertAlign == ELayoutVertAlign::Center)
		{
			TextPos.y += (Height - TextSize.y) * 0.5f;
		}
		else if (Widget.VertAlign == ELayoutVertAlign::Bottom)
		{
			TextPos.y += Height - TextSize.y;
		}

		DrawList->AddText(Font, FontSize, TextPos, ToImColor(Widget.TextColor), Text.c_str());
	}

	DrawList->PopClipRect();

	if (ShowBounds || Widget.Type == ELayoutWidget::Window)
	{
		DrawList->AddRect(Min, Max, GetWidgetColor(Widget.Type, ShowBounds ? 110 : 60));
	}
}

void UILayoutEditorForm::DrawCanvas()
{
	ImGui::SetCursorPosY(ImGui::GetCursorPosY() + 4.0f);
	const ImVec2 CanvasPos = ImGui::GetCursorScreenPos();
	const ImVec2 CanvasSize = ImGui::GetContentRegionAvail();
	if (CanvasSize.x < 32.0f || CanvasSize.y < 32.0f)
	{
		return;
	}

	CanvasOrigin = CanvasPos;
	if (NeedFit)
	{
		FitCanvas(CanvasSize);
		NeedFit = false;
	}

	ImGui::InvisibleButton("##canvas", CanvasSize, ImGuiButtonFlags_MouseButtonLeft | ImGuiButtonFlags_MouseButtonRight | ImGuiButtonFlags_MouseButtonMiddle);
	const bool Hovered = ImGui::IsItemHovered();
	HandleDrop(CanvasPos);

	ImDrawList* DrawList = ImGui::GetWindowDrawList();
	const ImVec2 CanvasMax = { CanvasPos.x + CanvasSize.x, CanvasPos.y + CanvasSize.y };
	DrawList->PushClipRect(CanvasPos, CanvasMax, true);
	DrawList->AddRectFilled(CanvasPos, CanvasMax, IM_COL32(28, 28, 30, 255));

	// The 1024x768 UI screen
	const ImVec2 ScreenMin = ToScreen({ 0.0f, 0.0f });
	const ImVec2 ScreenMax = ToScreen({ CUILayoutDocument::ScreenWidth, CUILayoutDocument::ScreenHeight });
	DrawList->AddRectFilled(ScreenMin, ScreenMax, IM_COL32(48, 52, 56, 255));

	if (ShowGrid && GridStep * Zoom >= 6.0f)
	{
		const ImU32 GridColor = IM_COL32(255, 255, 255, 14);
		for (float X = 0.0f; X <= CUILayoutDocument::ScreenWidth; X += GridStep)
		{
			DrawList->AddLine(ToScreen({ X, 0.0f }), ToScreen({ X, CUILayoutDocument::ScreenHeight }), GridColor);
		}
		for (float Y = 0.0f; Y <= CUILayoutDocument::ScreenHeight; Y += GridStep)
		{
			DrawList->AddLine(ToScreen({ 0.0f, Y }), ToScreen({ CUILayoutDocument::ScreenWidth, Y }), GridColor);
		}
	}

	// Screen center lines help to center dialogs
	const ImU32 CenterColor = IM_COL32(255, 200, 80, 40);
	DrawList->AddLine(ToScreen({ CUILayoutDocument::ScreenWidth * 0.5f, 0.0f }), ToScreen({ CUILayoutDocument::ScreenWidth * 0.5f, CUILayoutDocument::ScreenHeight }), CenterColor);
	DrawList->AddLine(ToScreen({ 0.0f, CUILayoutDocument::ScreenHeight * 0.5f }), ToScreen({ CUILayoutDocument::ScreenWidth, CUILayoutDocument::ScreenHeight * 0.5f }), CenterColor);

	for (u32 Id : Doc.GetOrdered())
	{
		const SLayoutWidget& Widget = *Doc.Find(Id);

		bool Hidden = false;
		for (const SLayoutWidget* Current = &Widget; Current != nullptr; Current = Current->ParentId != 0 ? Doc.Find(Current->ParentId) : nullptr)
		{
			Hidden |= Current->Hidden;
		}
		if (Hidden)
		{
			continue;
		}

		Fvector2 AbsPos, Size;
		GetPreviewRect(Widget, AbsPos, Size);
		DrawWidget(DrawList, Widget, AbsPos, Size);
	}

	DrawList->AddRect(ScreenMin, ScreenMax, IM_COL32(255, 255, 255, 80));

	// Selection with resize handles
	if (const SLayoutWidget* Widget = Doc.Find(Selected))
	{
		Fvector2 AbsPos, Size;
		GetPreviewRect(*Widget, AbsPos, Size);
		const ImVec2 Min = ToScreen(AbsPos);
		const ImVec2 Max = ToScreen({ AbsPos.x + Size.x, AbsPos.y + Size.y });
		const ImU32 Color = Widget->Locked ? IM_COL32(255, 120, 120, 255) : IM_COL32(255, 170, 40, 255);
		DrawList->AddRect(Min, Max, Color, 0.0f, 0, 2.0f);

		if (!Widget->Locked && !WidePreview)
		{
			const ImVec2 Mid = { (Min.x + Max.x) * 0.5f, (Min.y + Max.y) * 0.5f };
			for (const ImVec2& Handle : { Min, Max, ImVec2(Min.x, Max.y), ImVec2(Max.x, Min.y), ImVec2(Mid.x, Min.y), ImVec2(Mid.x, Max.y), ImVec2(Min.x, Mid.y), ImVec2(Max.x, Mid.y) })
			{
				DrawList->AddRectFilled({ Handle.x - HandleRadius, Handle.y - HandleRadius }, { Handle.x + HandleRadius, Handle.y + HandleRadius }, Color);
			}
		}

		string128 Info;
		xr_sprintf(Info, "%s  %g, %g  %g x %g", Widget->Name.c_str(), AbsPos.x, AbsPos.y, Size.x, Size.y);
		DrawList->AddText({ Min.x, Min.y - ImGui::GetTextLineHeight() - 2.0f }, Color, Info);
	}

	DrawList->PopClipRect();

	HandleCanvasInput(CanvasPos, CanvasSize, Hovered);
}

void UILayoutEditorForm::HandleDrop(const ImVec2& CanvasPos)
{
	if (!ImGui::BeginDragDropTarget())
	{
		return;
	}

	if (const ImGuiPayload* Payload = ImGui::AcceptDragDropPayload(WidgetPayload))
	{
		const Fvector2 Point = ToLayout(ImGui::GetMousePos());
		const u32 Parent = WidePreview ? 0 : PickWidget(Point);
		AddWidget((ELayoutWidget)*(const int*)Payload->Data, Parent, &Point);
	}
	ImGui::EndDragDropTarget();
}

void UILayoutEditorForm::HandleCanvasInput(const ImVec2& CanvasPos, const ImVec2& CanvasSize, bool Hovered)
{
	ImGuiIO& IO = ImGui::GetIO();
	const ImVec2 Mouse = IO.MousePos;

	// Zoom around the cursor
	if (Hovered && IO.MouseWheel != 0.0f)
	{
		const Fvector2 Anchor = ToLayout(Mouse);
		Zoom = std::clamp(Zoom * (IO.MouseWheel > 0.0f ? 1.15f : 1.0f / 1.15f), 0.1f, 8.0f);
		const ImVec2 NewScreen = ToScreen(Anchor);
		Pan.x += Mouse.x - NewScreen.x;
		Pan.y += Mouse.y - NewScreen.y;
	}

	// Pan with the right or middle button
	if (Hovered && (ImGui::IsMouseClicked(ImGuiMouseButton_Right) || ImGui::IsMouseClicked(ImGuiMouseButton_Middle)))
	{
		DragMode = EDragMode::Pan;
		DragMouseStart = Mouse;
		DragPanStart = Pan;
	}
	if (DragMode == EDragMode::Pan)
	{
		if (ImGui::IsMouseDown(ImGuiMouseButton_Right) || ImGui::IsMouseDown(ImGuiMouseButton_Middle))
		{
			Pan = { DragPanStart.x + Mouse.x - DragMouseStart.x, DragPanStart.y + Mouse.y - DragMouseStart.y };
		}
		else
		{
			DragMode = EDragMode::None;
		}
		return;
	}

	if (WidePreview)
	{
		if (Hovered && ImGui::IsMouseClicked(ImGuiMouseButton_Left))
		{
			Select(PickWidget(ToLayout(Mouse)));
		}
		return;
	}

	SLayoutWidget* Widget = Doc.Find(Selected);

	// Resize handle under the cursor
	u32 HoverEdges = 0;
	if (Widget != nullptr && !Widget->Locked && DragMode == EDragMode::None)
	{
		const Fvector2 AbsPos = Doc.GetAbsPos(Selected);
		const ImVec2 Min = ToScreen(AbsPos);
		const ImVec2 Max = ToScreen({ AbsPos.x + Widget->Size.x, AbsPos.y + Widget->Size.y });
		const float Reach = HandleRadius + 2.0f;
		const bool InX = Mouse.x >= Min.x - Reach && Mouse.x <= Max.x + Reach;
		const bool InY = Mouse.y >= Min.y - Reach && Mouse.y <= Max.y + Reach;
		const float MidX = (Min.x + Max.x) * 0.5f;
		const float MidY = (Min.y + Max.y) * 0.5f;
		const bool OnCornerX = std::abs(Mouse.x - Min.x) <= Reach || std::abs(Mouse.x - Max.x) <= Reach || std::abs(Mouse.x - MidX) <= Reach;
		const bool OnCornerY = std::abs(Mouse.y - Min.y) <= Reach || std::abs(Mouse.y - Max.y) <= Reach || std::abs(Mouse.y - MidY) <= Reach;

		if (InX && InY && OnCornerX && OnCornerY)
		{
			if (std::abs(Mouse.x - Min.x) <= Reach)
			{
				HoverEdges |= 1;
			}
			else if (std::abs(Mouse.x - Max.x) <= Reach)
			{
				HoverEdges |= 2;
			}
			if (std::abs(Mouse.y - Min.y) <= Reach)
			{
				HoverEdges |= 4;
			}
			else if (std::abs(Mouse.y - Max.y) <= Reach)
			{
				HoverEdges |= 8;
			}
		}
	}

	const u32 CursorEdges = DragMode == EDragMode::Resize ? DragEdges : HoverEdges;
	if (CursorEdges != 0 && (Hovered || DragMode == EDragMode::Resize))
	{
		const bool Horizontal = (CursorEdges & 3) != 0;
		const bool Vertical = (CursorEdges & 12) != 0;
		if (Horizontal && Vertical)
		{
			const bool Main = (CursorEdges == (1 | 4)) || (CursorEdges == (2 | 8));
			ImGui::SetMouseCursor(Main ? ImGuiMouseCursor_ResizeNWSE : ImGuiMouseCursor_ResizeNESW);
		}
		else
		{
			ImGui::SetMouseCursor(Horizontal ? ImGuiMouseCursor_ResizeEW : ImGuiMouseCursor_ResizeNS);
		}
	}

	if (Hovered && ImGui::IsMouseDoubleClicked(ImGuiMouseButton_Left))
	{
		const u32 Picked = PickWidget(ToLayout(Mouse));
		if (Picked != 0)
		{
			DragMode = EDragMode::None;
			GoToHandler(Picked);
			return;
		}
	}

	if (Hovered && ImGui::IsMouseClicked(ImGuiMouseButton_Left))
	{
		if (HoverEdges != 0)
		{
			DragMode = EDragMode::Resize;
			DragEdges = HoverEdges;
		}
		else
		{
			Select(PickWidget(ToLayout(Mouse)));
			Widget = Doc.Find(Selected);
			DragMode = Widget != nullptr && !Widget->Locked ? EDragMode::Move : EDragMode::None;
		}

		if (Widget != nullptr)
		{
			DragMouseStart = Mouse;
			DragPosStart = Doc.GetAbsPos(Selected);
			DragSizeStart = Widget->Size;
			DragChanged = false;
		}
	}

	if ((DragMode == EDragMode::Move || DragMode == EDragMode::Resize) && Widget != nullptr)
	{
		if (!ImGui::IsMouseDown(ImGuiMouseButton_Left))
		{
			DragMode = EDragMode::None;
		}
		else
		{
			const Fvector2 Delta = { (Mouse.x - DragMouseStart.x) / Zoom, (Mouse.y - DragMouseStart.y) / Zoom };
			const Fvector2 ParentPos = Widget->ParentId != 0 ? Doc.GetAbsPos(Widget->ParentId) : Fvector2{ 0.0f, 0.0f };

			Fvector2 NewPos = Widget->Pos;
			Fvector2 NewSize = Widget->Size;
			if (DragMode == EDragMode::Move)
			{
				NewPos = { Snap(DragPosStart.x + Delta.x) - ParentPos.x, Snap(DragPosStart.y + Delta.y) - ParentPos.y };
			}
			else
			{
				float Left = DragPosStart.x;
				float Top = DragPosStart.y;
				float Right = DragPosStart.x + DragSizeStart.x;
				float Bottom = DragPosStart.y + DragSizeStart.y;
				if (DragEdges & 1)
				{
					Left = std::min(Snap(Left + Delta.x), Right - 1.0f);
				}
				if (DragEdges & 2)
				{
					Right = std::max(Snap(Right + Delta.x), Left + 1.0f);
				}
				if (DragEdges & 4)
				{
					Top = std::min(Snap(Top + Delta.y), Bottom - 1.0f);
				}
				if (DragEdges & 8)
				{
					Bottom = std::max(Snap(Bottom + Delta.y), Top + 1.0f);
				}
				NewPos = { Left - ParentPos.x, Top - ParentPos.y };
				NewSize = { Right - Left, Bottom - Top };
			}

			if (!NewPos.similar(Widget->Pos) || !NewSize.similar(Widget->Size))
			{
				if (!DragChanged)
				{
					PushUndo();
					DragChanged = true;
					Widget = Doc.Find(Selected);
				}
				Widget->Pos = NewPos;
				Widget->Size = NewSize;
				MarkModified();
			}
		}
	}

	// Keyboard nudge
	if (ImGui::IsWindowFocused() && !IO.WantTextInput && Widget != nullptr && !Widget->Locked)
	{
		const float Step = IO.KeyShift ? 10.0f : 1.0f;
		Fvector2 Offset = { 0.0f, 0.0f };
		if (ImGui::IsKeyPressed(ImGuiKey_LeftArrow))
		{
			Offset.x -= Step;
		}
		if (ImGui::IsKeyPressed(ImGuiKey_RightArrow))
		{
			Offset.x += Step;
		}
		if (ImGui::IsKeyPressed(ImGuiKey_UpArrow))
		{
			Offset.y -= Step;
		}
		if (ImGui::IsKeyPressed(ImGuiKey_DownArrow))
		{
			Offset.y += Step;
		}

		if (Offset.x != 0.0f || Offset.y != 0.0f)
		{
			PushUndo();
			Doc.Find(Selected)->Pos.add(Offset);
			MarkModified();
		}

		if (ImGui::IsKeyPressed(ImGuiKey_Delete, false))
		{
			DeleteSelected();
		}
	}
}

void UILayoutEditorForm::DrawTexturePicker(SLayoutWidget& Widget)
{
	if (!TexturesLoaded)
	{
		LoadTextureDescriptions();
	}

	ImGui::SetNextItemWidth(-1.0f);
	ImGui::InputTextWithHint("##texture_filter", "Filter", TextureFilter, sizeof(TextureFilter));

	if (ImGui::BeginListBox("##textures", { -1.0f, 260.0f }))
	{
		xr_vector<const xr_string*> Matches;
		for (const xr_string& Id : TextureIds)
		{
			if (TextureFilter[0] == 0 || strstr(Id.c_str(), TextureFilter) != nullptr)
			{
				Matches.push_back(&Id);
			}
		}

		ImGuiListClipper Clipper;
		Clipper.Begin((int)Matches.size());
		while (Clipper.Step())
		{
			for (int Index = Clipper.DisplayStart; Index < Clipper.DisplayEnd; ++Index)
			{
				const xr_string& Id = *Matches[Index];
				if (ImGui::Selectable(Id.c_str(), Id == Widget.Texture))
				{
					Widget.Texture = Id;
					MarkModified();
					ImGui::CloseCurrentPopup();
				}

				if (ImGui::IsItemHovered())
				{
					if (const STextureCache* Texture = GetTexture(Id))
					{
						xr_string Key = Id;
						xr_strlwr(Key);
						const STextureRegion& Region = TextureRegions[Key];

						ImGui::BeginTooltip();
						const float Scale = std::min(1.0f, 256.0f / std::max(1.0f, std::max(Region.Rect.z, Region.Rect.w)));
						ImGui::Image(Texture->Texture->get_SRView()->GetRawSRV(), { std::max(1.0f, Region.Rect.z) * Scale, std::max(1.0f, Region.Rect.w) * Scale }, Texture->UV0, Texture->UV1);
						ImGui::Text("%s  %d x %d", Region.File.c_str(), (int)Region.Rect.z, (int)Region.Rect.w);
						ImGui::EndTooltip();
					}
				}
			}
		}
		ImGui::EndListBox();
	}
}

void UILayoutEditorForm::RebuildProperties()
{
	PropsDirty = false;
	PropsWidget = Selected;
	PropsSnapshot = Doc;

	PropItemVec Items;
	SLayoutWidget* Widget = Doc.Find(Selected);
	if (Widget == nullptr)
	{
		PHelper().CreateSText(Items, "Dialog\\Layout Name", &Doc.Name)->OnChangeEvent = xr_make_delegate(this, &UILayoutEditorForm::OnLayoutNameChange);
		PHelper().CreateCaption(Items, "Dialog\\Lua Class", Doc.GetClassName().c_str());
		PHelper().CreateCaption(Items, "Files\\Layout", ("configs\\ui\\" + Doc.Name + ".xml").c_str());
		PHelper().CreateCaption(Items, "Files\\Widescreen", ("configs\\ui\\" + Doc.Name + "_16.xml").c_str());
		PHelper().CreateCaption(Items, "Files\\Script", ("scripts\\" + Doc.Name + ".script").c_str());
		Props->AssignItems(Items);
		return;
	}

	const ELayoutWidget Type = Widget->Type;
	PHelper().CreateSText(Items, "Widget\\Name", &Widget->Name)->OnChangeEvent = xr_make_delegate(this, &UILayoutEditorForm::OnNameChange);
	PHelper().CreateToken8(Items, "Widget\\Type", (u8*)&Widget->Type, TypeTokens)->OnChangeEvent = xr_make_delegate(this, &UILayoutEditorForm::OnTypeChange);
	const SLayoutWidget* Parent = Doc.Find(Widget->ParentId);
	PHelper().CreateCaption(Items, "Widget\\Parent", Parent != nullptr ? Parent->Name.c_str() : "self");

	PHelper().CreateFloat(Items, "Rect\\X", &Widget->Pos.x, -4096.0f, 4096.0f, 1.0f, 0);
	PHelper().CreateFloat(Items, "Rect\\Y", &Widget->Pos.y, -4096.0f, 4096.0f, 1.0f, 0);
	PHelper().CreateFloat(Items, "Rect\\Width", &Widget->Size.x, 1.0f, 4096.0f, 1.0f, 0);
	PHelper().CreateFloat(Items, "Rect\\Height", &Widget->Size.y, 1.0f, 4096.0f, 1.0f, 0);
	PHelper().CreateButton(Items, "Rect\\Center", Parent != nullptr ? "In Parent" : "On Screen", 0)->OnBtnClickEvent = xr_make_delegate(this, &UILayoutEditorForm::OnCenterClick);

	if (LayoutWidget::HasTexture(Type))
	{
		PHelper().CreateSText(Items, "Texture\\Name", &Widget->Texture);
		PHelper().CreateButton(Items, "Texture\\Choose", "Pick", 0)->OnBtnClickEvent = xr_make_delegate(this, &UILayoutEditorForm::OnTextureClick);
		PHelper().CreateBool(Items, "Texture\\Stretch", &Widget->Stretch);
	}

	if (LayoutWidget::HasText(Type))
	{
		const bool IsList = Type == ELayoutWidget::ListBox || Type == ELayoutWidget::ComboBox;
		if (!IsList)
		{
			PHelper().CreateSText(Items, "Text\\Text Or String ID", &Widget->Text);
			PHelper().CreateButton(Items, "Text\\Localization", ICON_FA_LANGUAGE " Localize Text...", 0)->OnBtnClickEvent = xr_make_delegate(this, &UILayoutEditorForm::OnLocalizeClick);
		}

		PropsFont = 0;
		for (u32 Index = 0; Index < std::size(Fonts); ++Index)
		{
			if (Widget->Font == Fonts[Index])
			{
				PropsFont = Index;
			}
		}
		PHelper().CreateToken32(Items, "Text\\Font", &PropsFont, FontTokens)->OnChangeEvent = xr_make_delegate(this, &UILayoutEditorForm::OnFontChange);
		PHelper().CreateColor(Items, "Text\\Color", &Widget->TextColor);

		if (!IsList)
		{
			PHelper().CreateToken8(Items, "Text\\Align", (u8*)&Widget->Align, AlignTokens);
			PHelper().CreateToken8(Items, "Text\\Vertical Align", (u8*)&Widget->VertAlign, VertAlignTokens);
		}
	}

	if (Type == ELayoutWidget::TrackBar || Type == ELayoutWidget::ProgressBar)
	{
		PHelper().CreateFloat(Items, "Range\\Min", &Widget->Min, -100000.0f, 100000.0f, 0.1f, 2);
		PHelper().CreateFloat(Items, "Range\\Max", &Widget->Max, -100000.0f, 100000.0f, 0.1f, 2);
		PHelper().CreateFloat(Items, Type == ELayoutWidget::TrackBar ? "Range\\Preview Value" : "Range\\Position", &Widget->Value, -100000.0f, 100000.0f, 0.1f, 2);
		if (Type == ELayoutWidget::TrackBar)
		{
			PHelper().CreateFloat(Items, "Range\\Step", &Widget->Step, 0.0f, 1000.0f, 0.01f, 2);
			PHelper().CreateBool(Items, "Range\\Integer", &Widget->IsInteger);
		}
		else
		{
			PHelper().CreateBool(Items, "Range\\Horizontal", &Widget->Horizontal);
		}
	}

	if (Type == ELayoutWidget::ListBox)
	{
		PHelper().CreateFloat(Items, "List\\Item Height", &Widget->ItemHeight, 1.0f, 512.0f, 1.0f, 0);
	}

	if (LayoutWidget::HasOptions(Type))
	{
		PHelper().CreateSText(Items, "Options Item\\Entry", &Widget->OptionsEntry);
		PHelper().CreateSText(Items, "Options Item\\Group", &Widget->OptionsGroup);
	}

	// One folder per event: subscribe it and name the handler, empty takes the default one
	const std::span<const SLayoutEventInfo> Events = LayoutWidget::GetEvents(Type);
	for (u32 Index = 0; Index < Events.size(); ++Index)
	{
		const xr_string Folder = xr_string("Events\\") + Events[Index].Event + "\\";
		PHelper().CreateBool(Items, (Folder + "Subscribe").c_str(), &Widget->Events[Index].Enabled)->OnChangeEvent = xr_make_delegate(this, &UILayoutEditorForm::OnHandlerChange);
		if (Widget->Events[Index].Enabled)
		{
			PHelper().CreateSText(Items, (Folder + "Handler").c_str(), &Widget->Events[Index].Handler)->OnChangeEvent = xr_make_delegate(this, &UILayoutEditorForm::OnHandlerChange);
			PHelper().CreateCaption(Items, (Folder + "Function").c_str(), (Doc.GetClassName() + ":" + Doc.GetHandlerName(*Widget, Index) + "()").c_str());
		}
	}

	PHelper().CreateBool(Items, "Editor\\Hidden", &Widget->Hidden);
	PHelper().CreateBool(Items, "Editor\\Locked", &Widget->Locked);

	Props->AssignItems(Items);
}

void UILayoutEditorForm::OnPropsModified()
{
	// One undo step per edit: dragging a value changes it every frame
	if (!PropsEditing)
	{
		UndoStack.push_back(PropsSnapshot);
		if (UndoStack.size() > MaxUndo)
		{
			UndoStack.erase(UndoStack.begin());
		}
		RedoStack.clear();
		PropsEditing = true;
	}

	if (SLayoutWidget* Widget = Doc.Find(Selected))
	{
		Widget->Size = { std::max(1.0f, Widget->Size.x), std::max(1.0f, Widget->Size.y) };
	}
	Modified = true;
}

void UILayoutEditorForm::OnNameChange(PropValue* Value)
{
	SLayoutWidget* Widget = Doc.Find(Selected);
	const SLayoutWidget* Before = PropsSnapshot.Find(Selected);
	if (Widget == nullptr || Before == nullptr || Widget->Name == Before->Name)
	{
		return;
	}

	if (!Doc.IsValidName(Widget->Name.c_str()))
	{
		ELog.DlgMsg(mtError, "Widget name must contain only latin letters, digits and '_'");
		Widget->Name = Before->Name;
	}
	else if (Doc.MakeUniqueName(Widget->Name.c_str(), Selected) != Widget->Name)
	{
		ELog.DlgMsg(mtError, "Widget name '%s' is already used", Widget->Name.c_str());
		Widget->Name = Before->Name;
	}
	// The handler caption follows the name
	PropsDirty = true;
}

void UILayoutEditorForm::OnLayoutNameChange(PropValue* Value)
{
	if (!Doc.IsValidName(Doc.Name.c_str()))
	{
		ELog.DlgMsg(mtError, "Layout name must contain only latin letters, digits and '_'");
		Doc.Name = PropsSnapshot.Name;
	}
	PropsDirty = true;
}

void UILayoutEditorForm::OnTypeChange(PropValue* Value)
{
	// Event indices belong to the type, the new one starts with its main event
	if (SLayoutWidget* Widget = Doc.Find(Selected))
	{
		for (SLayoutEventBinding& Binding : Widget->Events)
		{
			Binding = SLayoutEventBinding();
		}
		Widget->Events[0].Enabled = !LayoutWidget::GetEvents(Widget->Type).empty();
	}
	// Each type has its own set of properties
	PropsDirty = true;
}

void UILayoutEditorForm::OnHandlerChange(PropValue* Value)
{
	SLayoutWidget* Widget = Doc.Find(Selected);
	const SLayoutWidget* Before = PropsSnapshot.Find(Selected);
	if (Widget != nullptr && Before != nullptr)
	{
		for (u32 Index = 0; Index < MaxLayoutEvents; ++Index)
		{
			xr_string& Handler = Widget->Events[Index].Handler;
			if (!Handler.empty() && !Doc.IsValidName(Handler.c_str()))
			{
				ELog.DlgMsg(mtError, "Handler name must contain only latin letters, digits and '_'");
				Handler = Before->Events[Index].Handler;
			}
		}
	}
	// The handler fields follow the subscription and the function caption follows the name
	PropsDirty = true;
}

void UILayoutEditorForm::OnFontChange(PropValue* Value)
{
	SLayoutWidget* Widget = Doc.Find(Selected);
	if (Widget != nullptr && PropsFont < std::size(Fonts))
	{
		Widget->Font = Fonts[PropsFont];
	}
}

void UILayoutEditorForm::OnCenterClick(ButtonValue* Value, bool& IsModified, bool& Safe)
{
	IsModified = false;
	const SLayoutWidget* Widget = Doc.Find(Selected);
	if (Widget == nullptr)
	{
		return;
	}

	const SLayoutWidget* Parent = Doc.Find(Widget->ParentId);
	const Fvector2 Area = Parent != nullptr ? Parent->Size : Fvector2{ CUILayoutDocument::ScreenWidth, CUILayoutDocument::ScreenHeight };
	const Fvector2 Size = Widget->Size;

	PushUndo();
	Doc.Find(Selected)->Pos = { std::round((Area.x - Size.x) * 0.5f), std::round((Area.y - Size.y) * 0.5f) };
	Modified = true;
}

void UILayoutEditorForm::OnTextureClick(ButtonValue* Value, bool& IsModified, bool& Safe)
{
	IsModified = false;
	TexturePickerRequest = true;
}

void UILayoutEditorForm::OnLocalizeClick(ButtonValue* Value, bool& IsModified, bool& Safe)
{
	IsModified = false;
	if (const SLayoutWidget* Widget = Doc.Find(Selected))
	{
		OpenLocalization(*Widget);
	}
}

void UILayoutEditorForm::DrawProperties()
{
	if (PropsDirty || PropsWidget != Selected)
	{
		RebuildProperties();
	}

	Props->Draw();

	// The edit ends once the mouse and keyboard let go of the item
	if (!ImGui::IsAnyItemActive())
	{
		PropsEditing = false;
		PropsSnapshot = Doc;
	}

	if (TexturePickerRequest)
	{
		TexturePickerRequest = false;
		ImGui::OpenPopup("##texture_picker");
	}

	ImGui::SetNextWindowSize({ 420.0f, 0.0f }, ImGuiCond_Appearing);
	if (ImGui::BeginPopup("##texture_picker"))
	{
		if (SLayoutWidget* Widget = Doc.Find(Selected))
		{
			const xr_string Before = Widget->Texture;
			DrawTexturePicker(*Widget);
			if (Widget->Texture != Before)
			{
				const xr_string After = Widget->Texture;
				Widget->Texture = Before;
				PushUndo();
				Doc.Find(Selected)->Texture = After;
				Modified = true;
			}
		}
		ImGui::EndPopup();
	}
}

// The editors work with "\n" only
void UILayoutEditorForm::SyncScript()
{
	// The generated region follows the layout, the rest is the user's code
	xr_string Merged;
	if (Doc.MergeScript(ScriptSource, Merged))
	{
		ScriptRegionLost = false;
		if (Merged != ScriptSource)
		{
			ScriptSource = Merged;
		}
	}
	else
	{
		ScriptRegionLost = true;
	}
}

bool UILayoutEditorForm::ApplyXmlEdit()
{
	if (!XmlEdited)
	{
		return true;
	}

	CUILayoutDocument Edited = Doc;
	xr_string Error;
	if (!Edited.ApplyXml(CodeEditor::GetText(*XmlView), ScriptSource, Error))
	{
		ELog.DlgMsg(mtError, "UI Layout: the xml has errors: %s", Error.c_str());
		return false;
	}

	const SLayoutWidget* Current = Doc.Find(Selected);
	const xr_string SelectedName = Current != nullptr ? Current->Name : xr_string();

	PushUndo();
	Doc = Edited;
	const SLayoutWidget* Reselected = Doc.FindByName(SelectedName.c_str());
	Selected = Reselected != nullptr ? Reselected->Id : 0;
	XmlEdited = false;
	XmlViewText.clear();
	MarkModified();
	return true;
}

void UILayoutEditorForm::GoToHandler(u32 Id)
{
	SLayoutWidget* Widget = Doc.Find(Id);
	if (Widget == nullptr)
	{
		return;
	}

	Select(Id);
	if (LayoutWidget::GetEvents(Widget->Type).empty())
	{
		StatusText = Widget->Name + " sends no events";
		return;
	}

	// The first subscribed event, or the main one gets subscribed
	u32 EventIndex = 0;
	while (EventIndex < LayoutWidget::GetEvents(Widget->Type).size() && !Widget->Events[EventIndex].Enabled)
	{
		++EventIndex;
	}
	if (EventIndex == LayoutWidget::GetEvents(Widget->Type).size())
	{
		EventIndex = 0;
		PushUndo();
		Doc.Find(Id)->Events[0].Enabled = true;
		MarkModified();
		Widget = Doc.Find(Id);
	}

	// The merge adds the handler when the script has none
	SyncScript();

	const xr_string Handler = Doc.GetHandlerName(*Widget, EventIndex);
	const xr_string Signature = "function " + Doc.GetClassName() + ":" + Handler + "(";
	const size_t Offset = ScriptSource.find(Signature);
	if (Offset == xr_string::npos)
	{
		StatusText = "Handler not found: " + Handler;
		return;
	}

	ScriptJumpLine = (int)std::count(ScriptSource.begin(), ScriptSource.begin() + Offset, '\n') + 1;
	PreviewTab = 2;
	PreviewTabRequest = true;
	ImGui::SetWindowFocus(CodeWindow);
}

xr_string UILayoutEditorForm::GetFieldClass(const xr_string& Field)
{
	const SLayoutWidget* Widget = Doc.FindByName(Field.c_str());
	if (Widget == nullptr)
	{
		return "";
	}
	return Widget->Type == ELayoutWidget::Window ? xr_string("CUIWindow") : EContext.Visor->GetInitClass(LayoutWidget::GetInitFunction(Widget->Type));
}

void UILayoutEditorForm::DrawCodePreview()
{
	static constexpr const char* Tabs[] = { ".xml", "_16.xml", ".script" };
	if (ImGui::BeginTabBar("##layout_code"))
	{
		for (int Index = 0; Index < (int)std::size(Tabs); ++Index)
		{
			string128 Label;
			xr_sprintf(Label, "%s%s%s", Doc.Name.c_str(), Tabs[Index], Index == 0 && XmlEdited ? " *" : "");

			ImGuiTabItemFlags Flags = ImGuiTabItemFlags_None;
			if (PreviewTabRequest && Index == PreviewTab)
			{
				Flags |= ImGuiTabItemFlags_SetSelected;
			}

			if (ImGui::BeginTabItem(Label, nullptr, Flags))
			{
				if (!PreviewTabRequest)
				{
					PreviewTab = Index;
				}
				ImGui::EndTabItem();
			}
		}
		PreviewTabRequest = false;
		ImGui::EndTabBar();
	}

	if (PreviewTab == 2)
	{
		if (ImGui::Button(ICON_FA_CLONE " Copy"))
		{
			ImGui::SetClipboardText(ScriptSource.c_str());
		}
		ImGui::SameLine();
		if (ScriptRegionLost)
		{
			ImGui::TextColored({ 1.0f, 0.4f, 0.3f, 1.0f }, "The ui_layout markers are gone: the layout can't update this script");
		}
		else
		{
			ImGui::TextDisabled("The code between the ui_layout markers is generated, the rest is yours");
		}

		if (ScriptViewText != ScriptSource)
		{
			ScriptViewText = ScriptSource;
			CodeEditor::SetText(*ScriptView, ScriptSource);
		}

		if (ScriptJumpLine >= 0)
		{
			// The handler body starts on the line after the signature
			ScriptView->SetCursorPosition({ ScriptJumpLine, 1 });
			ScriptJumpLine = -1;
		}


		EContext.Visor->Load();
		if (ScriptLanguageGeneration != EContext.Visor->GetGeneration())
		{
			ScriptLanguageGeneration = EContext.Visor->GetGeneration();
			TextEditor::LanguageDefinition Language;
			EContext.Visor->BuildLuaLanguage(Language);
			ScriptView->SetLanguageDefinition(Language);
		}
		Completion.BeforeRender(*ScriptView);
		ScriptView->Render("##script", ImGui::GetContentRegionAvail());
		const ImVec2 ScriptMin = ImGui::GetItemRectMin();
		const ImVec2 ScriptMax = ImGui::GetItemRectMax();
		Completion.AfterRender(*ScriptView, ScriptSource, xr_make_delegate(this, &UILayoutEditorForm::GetFieldClass));
		const bool Replaced = Search.Draw(*ScriptView, { XmlView.get(), ScriptView.get() }, ScriptMin, ScriptMax);
		if (ScriptView->IsTextChanged() || Replaced)
		{
			ScriptSource = CodeEditor::GetText(*ScriptView);
			ScriptViewText = ScriptSource;
			Modified = true;
		}
		return;
	}

	const bool Wide = PreviewTab == 1;
	if (!Wide && XmlEdited)
	{
		if (ImGui::Button(ICON_FA_CHECK " Apply"))
		{
			ApplyXmlEdit();
		}
		ImGui::SameLine();
		if (ImGui::Button(ICON_FA_ROTATE_LEFT " Revert"))
		{
			XmlEdited = false;
			XmlViewText.clear();
		}
		ImGui::SameLine();
		ImGui::TextColored({ 1.0f, 0.75f, 0.3f, 1.0f }, "Edited by hand: apply it to the layout");
	}
	else
	{
		if (ImGui::Button(ICON_FA_CLONE " Copy"))
		{
			ImGui::SetClipboardText(Doc.BuildXml(Wide).c_str());
		}
		ImGui::SameLine();
		ImGui::TextDisabled(Wide ? "Made from the layout on save" : "Edit and apply, or change the layout");
	}

	// Rebuilt only on changes, so the view keeps its scroll and selection
	if (XmlViewTab != PreviewTab)
	{
		XmlViewTab = PreviewTab;
		XmlViewText.clear();
	}
	XmlView->SetReadOnly(Wide);

	if (Wide || !XmlEdited)
	{
		const xr_string Code = Doc.BuildXml(Wide);
		if (XmlViewText != Code)
		{
			XmlViewText = Code;
			CodeEditor::SetText(*XmlView, Code);
		}
	}

	XmlView->Render("##xml", ImGui::GetContentRegionAvail());
	const bool Replaced = Search.Draw(*XmlView, { XmlView.get(), ScriptView.get() }, ImGui::GetItemRectMin(), ImGui::GetItemRectMax());
	if (!Wide && (XmlView->IsTextChanged() || Replaced))
	{
		XmlEdited = true;
		Modified = true;
	}
}

void UILayoutEditorForm::DrawNameDialog()
{
	if (NameDialogOpen)
	{
		NameDialogOpen = false;
		xr_strcpy(NameBuffer, NameDialogImport ? "" : Doc.Name.c_str());

		ExistingLayouts.clear();
		if (NameDialogImport)
		{
			CUILayoutDocument::ListImportable(ExistingLayouts);
			std::sort(ExistingLayouts.begin(), ExistingLayouts.end());
		}

		else
		{
			FS_FileSet Files;
			FS.file_list(Files, _game_config_, FS_ListFiles, "*.xml");
			for (const FS_File& File : Files)
			{
				if (!File.name.starts_with("ui\\") || File.name.find('\\', 3) != xr_string::npos || File.name.ends_with("_16.xml"))
				{
					continue;
				}
				ExistingLayouts.push_back(File.name.substr(3, File.name.size() - 3 - 4));
			}

			// Working widgets come first: they open without parsing the xml
			xr_vector<xr_string> SavedWidgets;
			CUILayoutDocument::ListWidgets(SavedWidgets);
			for (const xr_string& Widget : SavedWidgets)
			{
				std::erase(ExistingLayouts, Widget);
			}
			std::sort(SavedWidgets.begin(), SavedWidgets.end());
			ExistingLayouts.insert(ExistingLayouts.begin(), SavedWidgets.begin(), SavedWidgets.end());
		}

		ImGui::OpenPopup("UI Layout##name_dialog");
	}

	ImGui::SetNextWindowSize({ 420.0f, 460.0f }, ImGuiCond_Appearing);
	if (!ImGui::BeginPopupModal("UI Layout##name_dialog"))
	{
		return;
	}

	ImGui::TextUnformatted(NameDialogSave ? "Save the layout as" : NameDialogImport ? "Import a stock window" : "Open the layout");
	ImGui::SetNextItemWidth(-1.0f);
	if (ImGui::IsWindowAppearing())
	{
		ImGui::SetKeyboardFocusHere();
	}
	bool Accept = ImGui::InputText("##layout_name", NameBuffer, sizeof(NameBuffer), ImGuiInputTextFlags_EnterReturnsTrue | ImGuiInputTextFlags_CharsNoBlank);

	if (ImGui::BeginListBox("##layouts", { -1.0f, -ImGui::GetFrameHeightWithSpacing() - 4.0f }))
	{
		for (const xr_string& Layout : ExistingLayouts)
		{
			if (NameBuffer[0] != 0 && Layout.find(NameBuffer) == xr_string::npos && !NameDialogSave)
			{
				continue;
			}

			if (ImGui::Selectable(Layout.c_str(), Layout == NameBuffer, ImGuiSelectableFlags_AllowDoubleClick))
			{
				xr_strcpy(NameBuffer, Layout.c_str());
				Accept |= ImGui::IsMouseDoubleClicked(ImGuiMouseButton_Left);
			}
		}
		ImGui::EndListBox();
	}

	Accept |= ImGui::Button(NameDialogSave ? "Save" : NameDialogImport ? "Import" : "Open", { 120.0f, 0.0f });
	ImGui::SameLine();
	if (ImGui::Button("Cancel", { 120.0f, 0.0f }))
	{
		ImGui::CloseCurrentPopup();
	}

	if (Accept && NameBuffer[0] != 0)
	{
		if (NameDialogSave)
		{
			const bool Exists = FS.exist(_game_config_, (xr_string("ui\\") + NameBuffer + ".xml").c_str());
			if (!Doc.IsValidName(NameBuffer))
			{
				ELog.DlgMsg(mtError, "Layout name must contain only latin letters, digits and '_'");
			}
			else if (!Exists || NameBuffer == Doc.Name || ELog.DlgMsg(mtConfirmation, TMsgDlgButtons() | mbYes | mbNo, "%s.xml already exists. Overwrite it?", NameBuffer) == mrYes)
			{
				const xr_string OldName = Doc.Name;
				const xr_string OldClass = Doc.GetClassName();
				const xr_string OldScript = ScriptSource;
				Doc.Name = NameBuffer;

				// The user's code refers to the old class and layout names
				const std::regex ClassWord("\\b" + std::string(OldClass.c_str()) + "\\b");
				const std::regex LayoutWord("\\b" + std::string(OldName.c_str()) + "\\b");
				std::string Renamed = std::regex_replace(std::string(ScriptSource.c_str()), LayoutWord, Doc.Name.c_str());
				Renamed = std::regex_replace(Renamed, ClassWord, Doc.GetClassName().c_str());
				ScriptSource = Renamed.c_str();

				if (SaveLayout())
				{
					ImGui::CloseCurrentPopup();
				}
				else
				{
					Doc.Name = OldName;
					ScriptSource = OldScript;
				}
			}
		}
		else if (!Modified || ELog.DlgMsg(mtConfirmation, TMsgDlgButtons() | mbYes | mbNo, "The layout has unsaved changes. Discard them?") == mrYes)
		{
			if (NameDialogImport ? ImportLayout(NameBuffer) : OpenLayout(NameBuffer))
			{
				ImGui::CloseCurrentPopup();
			}
		}
	}

	ImGui::EndPopup();
}
