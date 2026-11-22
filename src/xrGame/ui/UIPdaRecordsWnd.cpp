#include "StdAfx.h"
#include "pch_script.h"
#include "UIPdaRecordsWnd.h"

#include "../../xrUI/UIXmlInit.h"
#include "../../xrUI/UIHelper.h"
#include "../../xrUI/Widgets/UIStatic.h"
#include "../../xrUI/Widgets/UIScrollView.h"
#include "../../xrUI/Widgets/UITabControl.h"
#include "../../xrUI/Widgets/UITabButton.h"
#include "../../xrUI/Widgets/UI3tButton.h"
#include "../../xrEngine/string_table.h"
#include "../../xrEngine/xr_input.h"
#include "../../xrScripts/script_engine.h"
#include "../ai_space.h"
#include "UIHotkeysHint.h"
#include "PdaUiSound.h"

namespace PdaRecordsWndHelpers
{
constexpr const char* RecordsXml = "pda_records.xml";
constexpr const char* ItemPath = "record_item";
constexpr const char* GroupHeaderPath = "group_header";

xr_string ParseText(const char* Text)
{
	if (!Text || !Text[0])
	{
		return {};
	}
	return g_pStringTable->ParseStringFromScript(Text).c_str();
}

void ApplyEntryIcon(CUIStatic* Static, const CPdaRecordEntry& Entry)
{
	if (!Static)
	{
		return;
	}
	if (!Entry.Icon.size())
	{
		Static->TextureOff();
		Static->Show(false);
		return;
	}
	Static->InitTexture(Entry.Icon.c_str());
	if (Entry.HasIconRect)
	{
		Frect TextureRect;
		TextureRect.lt.set(Entry.IconRect.x1, Entry.IconRect.y1);
		TextureRect.rb.set(Entry.IconRect.x2, Entry.IconRect.y2);
		TextureRect.rb.add(TextureRect.lt);
		Static->SetTextureRect(TextureRect);
	}
	Static->SetStretchTexture(true);
	Static->TextureOn();
	Static->Show(true);
}

bool ReadLayout(CUIXml& Xml, const char* Path, Frect& Rect)
{
	if (!Path || !Path[0] || !Xml.NavigateToNode(Path, 0))
	{
		return false;
	}
	Rect.x1 = Xml.ReadAttribFlt(Path, 0, "x", 0.f);
	Rect.y1 = Xml.ReadAttribFlt(Path, 0, "y", 0.f);
	Rect.x2 = Xml.ReadAttribFlt(Path, 0, "width", 0.f);
	Rect.y2 = Xml.ReadAttribFlt(Path, 0, "height", 0.f);
	return Rect.x2 > 0.f && Rect.y2 > 0.f;
}

Frect FitIconLayout(const Frect& Layout, const CPdaRecordEntry& Entry, bool KeepAspect)
{
	if (!KeepAspect || !Entry.HasIconRect || Entry.IconRect.x2 <= 0.f || Entry.IconRect.y2 <= 0.f)
	{
		return Layout;
	}
	const float Scale = std::min(Layout.x2 / Entry.IconRect.x2, Layout.y2 / Entry.IconRect.y2);
	Frect Result = Layout;
	Result.x2 = Entry.IconRect.x2 * Scale;
	Result.y2 = Entry.IconRect.y2 * Scale;
	return Result;
}

void ApplyLayout(CUIWindow* Wnd, const Frect& Rect)
{
	if (!Wnd)
	{
		return;
	}
	Wnd->SetWndPos(Fvector2().set(Rect.x1, Rect.y1));
	Wnd->SetWndSize(Fvector2().set(Rect.x2, Rect.y2));
}

bool TextureExists(const char* Name)
{
	if (!Name || !Name[0])
	{
		return false;
	}
	string_path FileName;
	string_path FullPath;
	xr_strconcat(FileName, Name, ".dds");
	FS.update_path(FullPath, _game_textures_, FileName);
	return !!FS.exist(FullPath);
}
}

void UIPdaRecordItem::InitFromXml(CUIXml& Xml, const char* Path, CUIPdaRecordsWnd* OwnerWnd, const char* Category)
{
	Owner = OwnerWnd;
	CUIXmlInit::InitWindow(Xml, Path, 0, this);
	BaseHeight = GetHeight();
	Padding = Xml.ReadAttribFlt(Path, 0, "padding", 8.f);

	XML_NODE* StoredRoot = Xml.GetLocalRoot();
	Xml.SetLocalRoot(Xml.NavigateToNode(Path, 0));

	if (Xml.NavigateToNode("selection", 0))
	{
		Selection = UIHelper::CreateStatic(Xml, "selection", this, false);
		if (Selection)
		{
			Selection->Show(false);
		}
	}
	if (Xml.NavigateToNode("new_mark", 0))
	{
		NewMark = UIHelper::CreateStatic(Xml, "new_mark", this, false);
	}
	if (Xml.NavigateToNode("icon", 0))
	{
		Icon = UIHelper::CreateStatic(Xml, "icon", this, false);
		if (Icon)
		{
			IconKeepAspect = Xml.ReadAttribInt("icon", 0, "keep_aspect", 0) != 0;
			IconLayout.set(Icon->GetWndPos().x, Icon->GetWndPos().y, Icon->GetWidth(), Icon->GetHeight());
		}
		if (Icon && Category && Category[0])
		{
			string128 LayoutPath;
			xr_sprintf(LayoutPath, "icon_%s", Category);
			Frect Layout;
			if (PdaRecordsWndHelpers::ReadLayout(Xml, LayoutPath, Layout))
			{
				IconLayout = Layout;
				PdaRecordsWndHelpers::ApplyLayout(Icon, Layout);
			}
		}
	}
	if (Xml.NavigateToNode("title", 0))
	{
		Title = UIHelper::CreateStatic(Xml, "title", this, false);
	}
	if (Xml.NavigateToNode("subtitle", 0))
	{
		Subtitle = UIHelper::CreateStatic(Xml, "subtitle", this, false);
	}
	if (Xml.NavigateToNode("btn_focus", 0))
	{
		FocusButton = UIHelper::Create3tButton(Xml, "btn_focus", this);
		if (FocusButton)
		{
			FocusButton->SetMessageTarget(this);
		}
	}

	Xml.SetLocalRoot(StoredRoot);
}

void UIPdaRecordItem::Setup(const CPdaRecordEntry& Entry, u32 TitleColor)
{
	Id = Entry.Id;

	PdaRecordsWndHelpers::ApplyEntryIcon(Icon, Entry);
	if (Icon && IconKeepAspect)
	{
		PdaRecordsWndHelpers::ApplyLayout(Icon, PdaRecordsWndHelpers::FitIconLayout(IconLayout, Entry, true));
	}
	if (NewMark)
	{
		NewMark->Show(Entry.IsNew);
	}
	if (Subtitle)
	{
		Subtitle->SetText(PdaRecordsWndHelpers::ParseText(Entry.Subtitle.c_str()).c_str());
	}
	if (FocusButton)
	{
		FocusButton->Show(Entry.HasMapTarget);
	}

	float Height = BaseHeight;
	if (Title)
	{
		Title->SetText(PdaRecordsWndHelpers::ParseText(Entry.Title.c_str()).c_str());
		Title->AdjustHeightToText();
		Title->SetTextColor(TitleColor);
		Height = std::max(Height, Title->GetWndPos().y + Title->GetHeight() + Padding);
	}

	SetHeight(Height);
	if (Selection)
	{
		Selection->SetHeight(Height);
	}
}

void UIPdaRecordItem::SetSelected(bool Selected)
{
	if (Selection)
	{
		Selection->Show(Selected);
	}
}

bool UIPdaRecordItem::OnMouseAction(float x, float y, EUIMessages mouse_action)
{
	if (FocusButton && FocusButton->IsShown() && FocusButton->CursorOverWindow())
	{
		return inherited::OnMouseAction(x, y, mouse_action);
	}
	if (Owner && mouse_action == WINDOW_LBUTTON_DOWN)
	{
		Owner->OnItemSelected(this);
		return true;
	}
	if (Owner && mouse_action == WINDOW_LBUTTON_DB_CLICK)
	{
		Owner->OnItemSelected(this);
		Owner->OnItemActivated(this);
		return true;
	}
	return inherited::OnMouseAction(x, y, mouse_action);
}

void UIPdaRecordItem::SendMessage(CUIWindow* pWnd, s16 msg, void* pData)
{
	if (Owner && FocusButton && pWnd == FocusButton && msg == BUTTON_CLICKED)
	{
		Owner->OnItemSelected(this);
		Owner->OnItemActivated(this);
		return;
	}
	inherited::SendMessage(pWnd, msg, pData);
}

bool UIPdaRecordGroupHeader::OnMouseAction(float x, float y, EUIMessages mouse_action)
{
	if (Owner && mouse_action == WINDOW_LBUTTON_DOWN)
	{
		Owner->ToggleGroup(Key);
		return true;
	}
	return inherited::OnMouseAction(x, y, mouse_action);
}

void CUIPdaRecordsWnd::Init(const char* XmlName)
{
	Xml.Load(CONFIG_PATH, UI_PATH, XmlName && XmlName[0] ? XmlName : PdaRecordsWndHelpers::RecordsXml);
	CUIXmlInit::InitWindow(Xml, "main_wnd", 0, this);

	ProviderFunction = Xml.ReadAttrib("main_wnd", 0, "provider", "pda_records.collect");
	SelectFunction = Xml.ReadAttrib("main_wnd", 0, "on_select", "pda_records.on_select");
	ActionFunction = Xml.ReadAttrib("main_wnd", 0, "on_action", "pda_records.on_action");
	RefreshIntervalMs = u32(std::max(250, Xml.ReadAttribInt("main_wnd", 0, "refresh_ms", 2000)));

	if (Xml.NavigateToNode("category_tabs", 0) && Xml.GetNodesNum("category_tabs", 0, "button") > 0)
	{
		CategoryTabs = new CUITabControl();
		CategoryTabs->SetAutoDelete(true);
		AttachChild(CategoryTabs);
		CUIXmlInit::InitTabControl(Xml, "category_tabs", 0, CategoryTabs);
		CategoryTabs->SetMessageTarget(this);
		TabCounters = Xml.ReadAttribInt("category_tabs", 0, "counters", 0) != 0;

		for (CUITabButton* Button : *CategoryTabs->GetButtonsVector())
		{
			SCategory Category;
			Category.Id = Button->m_btn_id;
			const char* BaseText = Button->TextItemControl()->GetText();
			Category.BaseText = BaseText ? BaseText : "";
			Categories.push_back(Category);
		}
	}
	else
	{
		SCategory Category;
		Category.Id = Xml.ReadAttrib("main_wnd", 0, "category", "notes");
		Categories.push_back(Category);
	}

	CreateHint("category_hint_prev", this);
	CreateHint("category_hint_next", this);
	const int HintCount = Xml.GetNodesNum(Xml.GetRoot(), "hotkeys_hint");
	for (int Index = 0; Index < HintCount; ++Index)
	{
		CreateHint("hotkeys_hint", this, Index);
	}

	List = new CUIScrollView();
	List->SetAutoDelete(true);
	AttachChild(List);
	CUIXmlInit::InitScrollView(Xml, "record_list", 0, List);

	GroupHeaders = Xml.NavigateToNode(PdaRecordsWndHelpers::GroupHeaderPath, 0) != nullptr;
	if (GroupHeaders)
	{
		GroupCollapsible = Xml.ReadAttribInt(PdaRecordsWndHelpers::GroupHeaderPath, 0, "collapsible", 0) != 0;
		GroupPrefixOpen = Xml.ReadAttrib(PdaRecordsWndHelpers::GroupHeaderPath, 0, "prefix_open", "");
		GroupPrefixClosed = Xml.ReadAttrib(PdaRecordsWndHelpers::GroupHeaderPath, 0, "prefix_closed", "");
	}
	TitleColorDefault = CUIXmlInit::GetColor(Xml, "record_item:colors:normal", 0, 0xFFDCDCDC);

	if (Xml.NavigateToNode("t_empty", 0))
	{
		EmptyText = UIHelper::CreateStatic(Xml, "t_empty", this);
		EmptyTextDefault = Xml.ReadAttrib("t_empty", 0, "text", "");
		EmptyText->Show(false);
	}

	InitDetails();

	if (CategoryTabs && CategoryTabs->GetTabsCount() > 0)
	{
		CategoryTabs->SetActiveTabByIndex(0);
	}
	ActiveCategoryIndex = 0;

	HintsControllerMode = pInput && pInput->GetControllerMode();

	if (UiSounds)
	{
		UiSounds->LoadSubdialog(Xml, "main_wnd");
	}
}

CUIHotkeysHint* CUIPdaRecordsWnd::CreateHint(const char* Path, CUIWindow* Parent, int Index)
{
	if (!Parent || !Xml.NavigateToNode(Path, Index))
	{
		return nullptr;
	}

	CUIHotkeysHint* Hint = new CUIHotkeysHint();
	Hint->SetAutoDelete(true);
	Parent->AttachChild(Hint);
	Hint->InitFromXml(Xml, Path, Index);
	Hints.push_back(Hint);
	return Hint;
}

void CUIPdaRecordsWnd::RefreshHints()
{
	for (CUIHotkeysHint* Hint : Hints)
	{
		Hint->Refresh();
	}
}

void CUIPdaRecordsWnd::InitDetails()
{
	if (!Xml.NavigateToNode("details", 0))
	{
		return;
	}

	ImageLevelIntro = Xml.ReadAttribInt("details", 0, "image_level_intro", 0) != 0;
	DetailsAutoHeading = Xml.ReadAttribInt("details", 0, "auto_heading", 1) != 0;
	ImageDefault = Xml.ReadAttrib("details", 0, "image_default", "");

	Details = new CUIWindow();
	Details->SetAutoDelete(true);
	AttachChild(Details);
	CUIXmlInit::InitWindow(Xml, "details", 0, Details);

	XML_NODE* StoredRoot = Xml.GetLocalRoot();
	Xml.SetLocalRoot(Xml.NavigateToNode("details", 0));

	if (Xml.NavigateToNode("empty", 0))
	{
		DetailsEmpty = UIHelper::CreateStatic(Xml, "empty", Details, false);
	}
	if (Xml.NavigateToNode("image", 0))
	{
		DetailsImage = UIHelper::CreateStatic(Xml, "image", Details, false);
		const char* Crop = Xml.ReadAttrib("image", 0, "crop_rect", "");
		float X = 0.f;
		float Y = 0.f;
		float W = 0.f;
		float H = 0.f;
		if (Crop && Crop[0] && sscanf(Crop, "%f,%f,%f,%f", &X, &Y, &W, &H) == 4 && W > 0.f && H > 0.f)
		{
			ImageCrop = true;
			ImageCropRect.set(X, Y, X + W, Y + H);
		}
	}
	if (Xml.NavigateToNode("icon", 0))
	{
		DetailsIcon = UIHelper::CreateStatic(Xml, "icon", Details, false);
		DetailsIconGap = Xml.ReadAttribFlt("icon", 0, "gap", 6.f);
		DetailsIconKeepAspect = Xml.ReadAttribInt("icon", 0, "keep_aspect", 0) != 0;
		DetailsIconPushBody = Xml.ReadAttribInt("icon", 0, "push_body", 1) != 0;
		if (DetailsIcon)
		{
			DetailsIconBase.set(DetailsIcon->GetWndPos().x, DetailsIcon->GetWndPos().y, DetailsIcon->GetWidth(), DetailsIcon->GetHeight());
			DetailsIcon->Show(false);
		}
	}
	if (Xml.NavigateToNode("title", 0))
	{
		DetailsTitle = UIHelper::CreateStatic(Xml, "title", Details, false);
	}
	if (Xml.NavigateToNode("subtitle", 0))
	{
		DetailsSubtitle = UIHelper::CreateStatic(Xml, "subtitle", Details, false);
	}
	if (Xml.NavigateToNode("body", 0))
	{
		DetailsBody = new CUIScrollView();
		DetailsBody->SetAutoDelete(true);
		Details->AttachChild(DetailsBody);
		CUIXmlInit::InitScrollView(Xml, "body", 0, DetailsBody);
		BodyBasePos = DetailsBody->GetWndPos();
		BodyBaseHeight = DetailsBody->GetHeight();
	}
	if (Xml.NavigateToNode("btn_map", 0))
	{
		BtnShowOnMap = UIHelper::Create3tButton(Xml, "btn_map", Details);
		CreateHint("btn_map:hint", BtnShowOnMap);
	}
	if (Xml.NavigateToNode("btn_track", 0))
	{
		BtnTrack = UIHelper::Create3tButton(Xml, "btn_track", Details);
		CreateHint("btn_track:hint", BtnTrack);
	}
	if (Xml.NavigateToNode("btn_action", 0))
	{
		BtnAction = UIHelper::Create3tButton(Xml, "btn_action", Details);
		CreateHint("btn_action:hint", BtnAction);
		if (BtnAction)
		{
			const char* Text = BtnAction->TextItemControl()->GetText();
			BtnActionDefaultText = Text ? Text : "";
		}
	}

	Xml.SetLocalRoot(StoredRoot);

	if (BtnShowOnMap)
	{
		Register(BtnShowOnMap);
		AddCallback(BtnShowOnMap, BUTTON_CLICKED, CUIWndCallback::void_function(this, &CUIPdaRecordsWnd::OnShowOnMapClicked));
	}
	if (BtnTrack)
	{
		Register(BtnTrack);
		AddCallback(BtnTrack, BUTTON_CLICKED, CUIWndCallback::void_function(this, &CUIPdaRecordsWnd::OnTrackClicked));
	}
	if (BtnAction)
	{
		Register(BtnAction);
		AddCallback(BtnAction, BUTTON_CLICKED, CUIWndCallback::void_function(this, &CUIPdaRecordsWnd::OnActionClicked));
	}
}

void CUIPdaRecordsWnd::Show(bool status)
{
	inherited::Show(status);
	if (status)
	{
		NeedRefresh = true;
		NeedRebuild = true;
		HintsControllerMode = pInput && pInput->GetControllerMode();
		RefreshHints();
	}
}

void CUIPdaRecordsWnd::Update()
{
	inherited::Update();
	if (!IsShown())
	{
		return;
	}

	const bool ControllerMode = pInput && pInput->GetControllerMode();
	if (ControllerMode != HintsControllerMode)
	{
		HintsControllerMode = ControllerMode;
		RefreshHints();
	}

	ApplyPendingActions();

	if (NeedRefresh || Device.dwTimeGlobal >= NextRefreshTime)
	{
		RefreshAll();
	}
	if (NeedRebuild)
	{
		RebuildList();
	}
}

void CUIPdaRecordsWnd::SendMessage(CUIWindow* pWnd, s16 msg, void* pData)
{
	if (msg == TAB_CHANGED && CategoryTabs && pWnd == CategoryTabs)
	{
		const shared_str ActiveId = CategoryTabs->GetActiveId();
		for (u32 Index = 0; Index < Categories.size(); ++Index)
		{
			if (Categories[Index].Id == ActiveId && Index != ActiveCategoryIndex)
			{
				ActiveCategoryIndex = Index;
				SelectedId = shared_str();
				NeedRebuild = true;
				if (UiSounds && IsShown())
				{
					UiSounds->Play(EPdaUiSound::Tab);
				}
				break;
			}
		}
		return;
	}

	inherited::SendMessage(pWnd, msg, pData);
	CUIWndCallback::OnEvent(pWnd, msg, pData);
}

bool CUIPdaRecordsWnd::SwitchCategory(bool Next)
{
	if (!CategoryTabs || CategoryTabs->GetTabsCount() < 2)
	{
		return false;
	}
	if (Next)
	{
		CategoryTabs->NextTab(true);
	}
	else
	{
		CategoryTabs->PrevTab(true);
	}
	return true;
}

bool CUIPdaRecordsWnd::HandleAction(EGameActions Action)
{
	switch (Action)
	{
	case kPDA_RECORDS_CATEGORY_PREV:
		SwitchCategory(false);
		return true;
	case kPDA_RECORDS_CATEGORY_NEXT:
		SwitchCategory(true);
		return true;
	case kPDA_RECORDS_PREV:
		MoveSelection(false);
		return true;
	case kPDA_RECORDS_NEXT:
		MoveSelection(true);
		return true;
	case kPDA_RECORDS_SHOW_ON_MAP:
		PendingShowOnMap = true;
		return true;
	case kPDA_RECORDS_TRACK:
		PendingTrack = true;
		return true;
	case kPDA_RECORDS_ACTION:
		PendingAction = true;
		return true;
	default:
		return false;
	}
}

bool CUIPdaRecordsWnd::OnKeyboardAction(int dik, EUIMessages keyboard_action)
{
	if (keyboard_action == WINDOW_KEY_PRESSED)
	{
		if (HandleAction(get_binded_action(dik, agUIRecords)))
		{
			return true;
		}

		switch (dik)
		{
		case SDL_SCANCODE_UP:
			MoveSelection(false);
			return true;
		case SDL_SCANCODE_DOWN:
			MoveSelection(true);
			return true;
		case SDL_SCANCODE_RETURN:
		case SDL_SCANCODE_KP_ENTER:
			PendingShowOnMap = true;
			return true;
		default:
			break;
		}
	}
	return inherited::OnKeyboardAction(dik, keyboard_action);
}

bool CUIPdaRecordsWnd::OnGamepadKeyAction(int key, EUIMessages gamepad_action)
{
	if (gamepad_action == WINDOW_KEY_PRESSED && HandleAction(get_binded_action(key, agUIRecords)))
	{
		return true;
	}
	return inherited::OnGamepadKeyAction(key, gamepad_action);
}

void CUIPdaRecordsWnd::OnItemSelected(UIPdaRecordItem* Item)
{
	if (!Item)
	{
		return;
	}
	if (UiSounds && SelectedId != Item->GetId())
	{
		UiSounds->Play(EPdaUiSound::ListSelect);
	}
	SelectEntry(Item->GetId(), true);
}

void CUIPdaRecordsWnd::OnItemActivated(UIPdaRecordItem* Item)
{
	if (Item)
	{
		PendingShowOnMap = true;
	}
}

void CUIPdaRecordsWnd::OnShowOnMapClicked(CUIWindow* w, void* d)
{
	PendingShowOnMap = true;
}

void CUIPdaRecordsWnd::OnTrackClicked(CUIWindow* w, void* d)
{
	PendingTrack = true;
}

void CUIPdaRecordsWnd::OnActionClicked(CUIWindow* w, void* d)
{
	PendingAction = true;
}

CUIPdaRecordsWnd::SCategory* CUIPdaRecordsWnd::ActiveCategory()
{
	return ActiveCategoryIndex < Categories.size() ? &Categories[ActiveCategoryIndex] : nullptr;
}

const CPdaRecordEntry* CUIPdaRecordsWnd::SelectedEntry()
{
	SCategory* Category = ActiveCategory();
	return Category && SelectedId.size() ? Category->List.Find(SelectedId) : nullptr;
}

void CUIPdaRecordsWnd::CallScript(const shared_str& Function, const char* Category, const char* Id, const char* Action)
{
	if (!Function.size())
	{
		return;
	}

	luabind::functor<void> Callback;
	if (!ai().script_engine().functor(Function.c_str(), Callback))
	{
		return;
	}

	try
	{
		if (Action)
		{
			Callback(Category, Id, Action);
		}
		else
		{
			Callback(Category, Id);
		}
	}
	catch (...)
	{
		Msg("! [PDA records] script callback [%s] failed", Function.c_str());
	}
}

void CUIPdaRecordsWnd::ApplyPendingActions()
{
	SCategory* Category = ActiveCategory();

	if (PendingSelectNotify.size())
	{
		const shared_str Id = PendingSelectNotify;
		PendingSelectNotify = shared_str();
		if (Category)
		{
			CallScript(SelectFunction, Category->Id.c_str(), Id.c_str(), nullptr);
			NeedRefresh = true;
		}
	}

	const CPdaRecordEntry* Entry = SelectedEntry();

	if (PendingAction)
	{
		PendingAction = false;
		if (Category && Entry && Entry->ActionId.size())
		{
			const shared_str Id = Entry->Id;
			const shared_str ActionId = Entry->ActionId;
			CallScript(ActionFunction, Category->Id.c_str(), Id.c_str(), ActionId.c_str());
			if (UiSounds)
			{
				UiSounds->Play(EPdaUiSound::ListSelect);
			}
			NeedRefresh = true;
			Entry = SelectedEntry();
		}
	}

	if (PendingTrack)
	{
		PendingTrack = false;
		if (Entry && Entry->Trackable && Entry->HasMapTarget && OnTrack)
		{
			const xr_string Hint = PdaRecordsWndHelpers::ParseText(Entry->Title.c_str());
			OnTrack(Entry->MapLevel, Entry->MapPosition, Hint.c_str());
			if (UiSounds)
			{
				UiSounds->Play(EPdaUiSound::ListSelect);
			}
		}
	}

	if (PendingShowOnMap)
	{
		PendingShowOnMap = false;
		if (Entry && Entry->HasMapTarget && OnShowOnMap)
		{
			OnShowOnMap(Entry->MapLevel, Entry->MapPosition);
		}
	}
}

void CUIPdaRecordsWnd::CollectCategory(SCategory& Category)
{
	Category.List.Clear();
	if (!ProviderFunction.size())
	{
		return;
	}

	luabind::functor<void> Provider;
	if (!ai().script_engine().functor(ProviderFunction.c_str(), Provider))
	{
		return;
	}

	try
	{
		Provider(Category.Id.c_str(), &Category.List);
	}
	catch (...)
	{
		Msg("! [PDA records] provider [%s] failed for category [%s]", ProviderFunction.c_str(), Category.Id.c_str());
	}
}

void CUIPdaRecordsWnd::RefreshAll()
{
	NeedRefresh = false;
	NextRefreshTime = Device.dwTimeGlobal + RefreshIntervalMs;

	for (SCategory& Category : Categories)
	{
		CollectCategory(Category);
	}
	UpdateTabCounters();

	SCategory* Category = ActiveCategory();
	if (Category && Category->List.Hash() != ListHash)
	{
		NeedRebuild = true;
	}
}

void CUIPdaRecordsWnd::UpdateTabCounters()
{
	if (!CategoryTabs || !TabCounters)
	{
		return;
	}

	for (const SCategory& Category : Categories)
	{
		CUITabButton* Button = CategoryTabs->GetButtonById(Category.Id);
		if (!Button)
		{
			continue;
		}

		const u32 NewCount = Category.List.NewCount();
		if (!NewCount)
		{
			Button->TextItemControl()->SetText(Category.BaseText.c_str());
			continue;
		}

		string256 Text;
		xr_sprintf(Text, "%s %u", Category.BaseText.c_str(), NewCount);
		Button->TextItemControl()->SetText(Text);
	}
}

u32 CUIPdaRecordsWnd::StateColor(const shared_str& State, bool IsNew)
{
	const shared_str Key = IsNew ? shared_str("new") : (State.size() ? State : shared_str("normal"));
	const auto Found = StateColors.find(Key);
	if (Found != StateColors.end())
	{
		return Found->second;
	}

	string256 Path;
	xr_strconcat(Path, "record_item:colors:", Key.c_str());
	u32 Color = TitleColorDefault;
	if (Xml.NavigateToNode(Path, 0))
	{
		Color = CUIXmlInit::GetColor(Xml, Path, 0, TitleColorDefault);
	}
	else if (IsNew && State.size())
	{
		Color = StateColor(State, false);
	}
	StateColors[Key] = Color;
	return Color;
}

shared_str CUIPdaRecordsWnd::GroupStateKey(const shared_str& GroupKey)
{
	const SCategory* Category = ActiveCategory();
	xr_string Key = Category ? Category->Id.c_str() : "";
	Key += "|";
	Key += GroupKey.c_str() ? GroupKey.c_str() : "";
	return shared_str(Key.c_str());
}

void CUIPdaRecordsWnd::AddGroupHeader(const CPdaRecordEntry& Entry, bool Collapsed)
{
	UIPdaRecordGroupHeader* Header = new UIPdaRecordGroupHeader();
	CUIXmlInit::InitStatic(Xml, PdaRecordsWndHelpers::GroupHeaderPath, 0, Header);
	xr_string Text;
	if (GroupCollapsible)
	{
		Header->SetOwner(this, Entry.GroupKey);
		Text = Collapsed ? GroupPrefixClosed : GroupPrefixOpen;
	}
	Text += PdaRecordsWndHelpers::ParseText(Entry.GroupCaption.c_str());
	Header->SetText(Text.c_str());
	List->AddWindow(Header, true);
}

void CUIPdaRecordsWnd::ToggleGroup(const shared_str& GroupKey)
{
	if (!GroupCollapsible)
	{
		return;
	}
	const shared_str Key = GroupStateKey(GroupKey);
	const auto Found = CollapsedGroups.find(Key);
	if (Found != CollapsedGroups.end())
	{
		CollapsedGroups.erase(Found);
	}
	else
	{
		CollapsedGroups.insert(Key);
	}
	NeedRebuild = true;
	if (UiSounds)
	{
		UiSounds->Play(EPdaUiSound::ListSelect);
	}
}

void CUIPdaRecordsWnd::RebuildList()
{
	NeedRebuild = false;
	SCategory* Category = ActiveCategory();
	const int ScrollPos = List->GetCurrentScrollPos();
	List->Clear();

	if (!Category)
	{
		ListHash = 0;
		UpdateEmptyText(true);
		SelectEntry(shared_str(), false);
		return;
	}

	ListHash = Category->List.Hash();

	bool HasGroup = false;
	bool Collapsed = false;
	shared_str CurrentGroup;
	for (const CPdaRecordEntry& Entry : Category->List.Entries())
	{
		if (!Entry.GroupKey.size())
		{
			Collapsed = false;
		}
		if (GroupHeaders && Entry.GroupKey.size() && (!HasGroup || Entry.GroupKey != CurrentGroup))
		{
			Collapsed = GroupCollapsible && CollapsedGroups.contains(GroupStateKey(Entry.GroupKey));
			AddGroupHeader(Entry, Collapsed);
			HasGroup = true;
			CurrentGroup = Entry.GroupKey;
		}
		if (Collapsed)
		{
			continue;
		}

		UIPdaRecordItem* Item = new UIPdaRecordItem();
		Item->InitFromXml(Xml, PdaRecordsWndHelpers::ItemPath, this, Category->Id.c_str());
		Item->Setup(Entry, StateColor(Entry.State, Entry.IsNew));
		List->AddWindow(Item, true);
	}

	UpdateEmptyText(Category->List.Count() == 0);

	shared_str Selected = SelectedId;
	if (!Selected.size() || !Category->List.Find(Selected))
	{
		Selected = Category->List.Count() ? Category->List.Entries().front().Id : shared_str();
	}
	SelectEntry(Selected, false);

	List->ForceUpdate();
	List->SetScrollPos(ScrollPos);
}

void CUIPdaRecordsWnd::UpdateEmptyText(bool Empty)
{
	if (!EmptyText)
	{
		return;
	}

	EmptyText->Show(Empty);
	if (!Empty)
	{
		return;
	}

	shared_str TextId = EmptyTextDefault;
	if (SCategory* Category = ActiveCategory())
	{
		string64 AttribName;
		xr_sprintf(AttribName, "text_%s", Category->Id.c_str());
		const char* CategoryText = Xml.ReadAttrib("t_empty", 0, AttribName, "");
		if (CategoryText && CategoryText[0])
		{
			TextId = CategoryText;
		}
	}
	if (TextId.size())
	{
		EmptyText->TextItemControl()->SetTextST(TextId.c_str());
	}
}

void CUIPdaRecordsWnd::SelectEntry(const shared_str& Id, bool Notify)
{
	const bool Changed = SelectedId != Id;
	SelectedId = Id;

	for (CUIWindow* Window : List->Items())
	{
		if (UIPdaRecordItem* Item = smart_cast<UIPdaRecordItem*>(Window))
		{
			Item->SetSelected(Id.size() && Item->GetId() == Id);
		}
	}

	if (Notify && Changed && Id.size())
	{
		PendingSelectNotify = Id;
	}

	UpdateDetails();
}

void CUIPdaRecordsWnd::MoveSelection(bool Next)
{
	xr_vector<UIPdaRecordItem*> Items;
	for (CUIWindow* Window : List->Items())
	{
		if (UIPdaRecordItem* Item = smart_cast<UIPdaRecordItem*>(Window))
		{
			Items.push_back(Item);
		}
	}
	if (Items.empty())
	{
		return;
	}

	size_t Index = 0;
	bool Found = false;
	for (size_t i = 0; i < Items.size(); ++i)
	{
		if (Items[i]->GetId() == SelectedId)
		{
			Index = i;
			Found = true;
			break;
		}
	}

	if (Found)
	{
		if (Next && Index + 1 < Items.size())
		{
			++Index;
		}
		else if (!Next && Index > 0)
		{
			--Index;
		}
		else
		{
			return;
		}
	}

	OnItemSelected(Items[Index]);
	List->ScrollToItem(Items[Index], 0);
}

shared_str CUIPdaRecordsWnd::ResolveImage(const CPdaRecordEntry& Entry) const
{
	if (Entry.Image == "none")
	{
		return shared_str();
	}
	if (Entry.Image.size())
	{
		return Entry.Image;
	}
	if (ImageLevelIntro && Entry.LevelName.size())
	{
		string_path Intro;
		xr_strconcat(Intro, "intro\\intro_", Entry.LevelName.c_str());
		if (PdaRecordsWndHelpers::TextureExists(Intro))
		{
			return Intro;
		}
	}
	return ImageDefault;
}

void CUIPdaRecordsWnd::UpdateImage(const CPdaRecordEntry* Entry)
{
	const shared_str Texture = Entry ? ResolveImage(*Entry) : shared_str();
	const bool HasImage = DetailsImage && Texture.size() != 0;
	if (DetailsImage)
	{
		DetailsImage->Show(HasImage);
		if (HasImage)
		{
			DetailsImage->InitTexture(Texture.c_str());
			if (ImageCrop)
			{
				DetailsImage->SetTextureRect(ImageCropRect);
			}
			DetailsImage->SetStretchTexture(true);
			DetailsImage->TextureOn();
		}
	}

	const bool ShowIcon = DetailsIcon && Entry && !HasImage && Entry->Icon.size();
	if (DetailsIcon)
	{
		if (ShowIcon)
		{
			Frect Layout = DetailsIconBase;
			if (const SCategory* Category = ActiveCategory())
			{
				string128 LayoutPath;
				xr_sprintf(LayoutPath, "details:icon_%s", Category->Id.c_str());
				Frect Override;
				if (PdaRecordsWndHelpers::ReadLayout(Xml, LayoutPath, Override))
				{
					Layout = Override;
				}
			}
			PdaRecordsWndHelpers::ApplyLayout(DetailsIcon, PdaRecordsWndHelpers::FitIconLayout(Layout, *Entry, DetailsIconKeepAspect));
			PdaRecordsWndHelpers::ApplyEntryIcon(DetailsIcon, *Entry);
		}
		else
		{
			DetailsIcon->Show(false);
		}
	}

	if (!DetailsBody)
	{
		return;
	}

	const bool IconPushesBody = ShowIcon && DetailsIconPushBody;
	if (HasImage || !Entry || !DetailsImage)
	{
		DetailsBody->SetWndPos(BodyBasePos);
		DetailsBody->SetHeight(BodyBaseHeight);
		if (!IconPushesBody)
		{
			return;
		}
	}

	float Top = DetailsImage ? DetailsImage->GetWndPos().y : BodyBasePos.y;
	if (IconPushesBody)
	{
		Top = std::max(Top, DetailsIcon->GetWndPos().y + DetailsIcon->GetHeight() + DetailsIconGap);
	}
	const float Bottom = BodyBasePos.y + BodyBaseHeight;
	DetailsBody->SetWndPos(Fvector2().set(BodyBasePos.x, Top));
	DetailsBody->SetHeight(std::max(0.f, Bottom - Top));
}

CUIStatic* CUIPdaRecordsWnd::AddDetailsLine(const shared_str& Style, const char* Text)
{
	if (!DetailsBody)
	{
		return nullptr;
	}

	string256 Path;
	xr_strconcat(Path, "details:line_", Style.size() ? Style.c_str() : "text");
	if (!Xml.NavigateToNode(Path, 0))
	{
		xr_strcpy(Path, "details:line_text");
		if (!Xml.NavigateToNode(Path, 0))
		{
			return nullptr;
		}
	}

	CUIStatic* Static = new CUIStatic();
	CUIXmlInit::InitStatic(Xml, Path, 0, Static);
	Static->SetText(Text);
	Static->AdjustHeightToText();
	DetailsBody->AddWindow(Static, true);
	return Static;
}

void CUIPdaRecordsWnd::UpdateDetails()
{
	if (!Details)
	{
		return;
	}

	const CPdaRecordEntry* Entry = SelectedEntry();
	const bool HasEntry = Entry != nullptr;

	if (DetailsEmpty)
	{
		DetailsEmpty->Show(!HasEntry);
	}
	for (CUIWindow* Window : { static_cast<CUIWindow*>(DetailsTitle), static_cast<CUIWindow*>(DetailsSubtitle),
		static_cast<CUIWindow*>(DetailsBody) })
	{
		if (Window)
		{
			Window->Show(HasEntry);
		}
	}

	UpdateImage(Entry);

	if (DetailsBody)
	{
		DetailsBody->Clear();
	}

	if (BtnShowOnMap)
	{
		BtnShowOnMap->Show(HasEntry && Entry->HasMapTarget && OnShowOnMap);
	}
	if (BtnTrack)
	{
		BtnTrack->Show(HasEntry && Entry->HasMapTarget && Entry->Trackable && OnTrack);
	}
	if (BtnAction)
	{
		const bool HasAction = HasEntry && Entry->ActionId.size();
		BtnAction->Show(HasAction);
		if (HasAction)
		{
			const xr_string Caption = Entry->ActionCaption.empty()
				? BtnActionDefaultText : PdaRecordsWndHelpers::ParseText(Entry->ActionCaption.c_str());
			BtnAction->TextItemControl()->SetText(Caption.c_str());
		}
	}

	if (!HasEntry)
	{
		return;
	}

	if (DetailsTitle)
	{
		DetailsTitle->SetText(PdaRecordsWndHelpers::ParseText(Entry->Title.c_str()).c_str());
	}
	if (DetailsSubtitle)
	{
		DetailsSubtitle->SetText(PdaRecordsWndHelpers::ParseText(Entry->Subtitle.c_str()).c_str());
	}

	if (!DetailsTitle && DetailsAutoHeading && Xml.NavigateToNode("details:line_heading", 0))
	{
		AddDetailsLine("heading", PdaRecordsWndHelpers::ParseText(Entry->Title.c_str()).c_str());
	}
	for (const SPdaRecordLine& Line : Entry->Lines)
	{
		AddDetailsLine(Line.Style, PdaRecordsWndHelpers::ParseText(Line.Text.c_str()).c_str());
	}

	if (DetailsBody)
	{
		DetailsBody->ForceUpdate();
		if (Entry->ScrollToEnd)
		{
			DetailsBody->ScrollToEnd();
		}
		else
		{
			DetailsBody->ScrollToBegin();
		}
	}
}
