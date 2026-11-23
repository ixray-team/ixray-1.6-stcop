#include "StdAfx.h"
#include "UITaskJournalWnd.h"

#include "../../xrUI/UIXmlInit.h"
#include "../../xrUI/UIHelper.h"
#include "../../xrUI/Widgets/UIStatic.h"
#include "../../xrUI/Widgets/UIFrameLineWnd.h"
#include "../../xrUI/Widgets/UIFrameWindow.h"
#include "../../xrUI/Widgets/UIScrollView.h"
#include "../../xrUI/Widgets/UITabControl.h"
#include "../../xrUI/Widgets/UITabButton.h"
#include "../../xrUI/Widgets/UI3tButton.h"
#include "../../xrEngine/string_table.h"
#include "../../xrEngine/xr_input.h"
#include "../GameTask.h"
#include "../GametaskManager.h"
#include "../Level.h"
#include "../map_location.h"
#include "UIInventoryUtilities.h"
#include "UIHotkeysHint.h"
#include "PdaUiSound.h"

namespace TaskJournalHelpers
{
constexpr const char* TaskJournalXml = "pda_task_journal.xml";
constexpr const char* ItemPath = "task_item";
constexpr const char* GroupHeaderPath = "group_header";
constexpr u32 RefreshIntervalMs = 1000;

void ReplaceToken(xr_string& Text, const char* Token, const char* Value)
{
	const size_t TokenLength = xr_strlen(Token);
	size_t Pos = Text.find(Token);
	while (Pos != xr_string::npos)
	{
		Text.replace(Pos, TokenLength, Value);
		Pos = Text.find(Token, Pos + xr_strlen(Value));
	}
}

void ApplyTaskIcon(CUIStatic* Static, const CGameTask* Task)
{
	if (!Static)
	{
		return;
	}

	if (!Task || !Task->m_icon_texture_name.size())
	{
		Static->TextureOff();
		Static->Show(false);
		return;
	}

	Static->InitTexture(Task->m_icon_texture_name.c_str());
	Frect IconRect = Task->m_icon_rect;
	Frect EmptyRect = Frect().set(0.f, 0.f, 0.f, 0.f);
	if (!IconRect.cmp(EmptyRect))
	{
		Frect TextureRect;
		TextureRect.lt.set(IconRect.x1, IconRect.y1);
		TextureRect.rb.set(IconRect.x2, IconRect.y2);
		TextureRect.rb.add(TextureRect.lt);
		Static->SetTextureRect(TextureRect);
	}
	Static->TextureOn();
	Static->SetStretchTexture(true);
	Static->Show(true);
}

shared_str TranslateOptional(const shared_str& Id)
{
	return Id.size() ? g_pStringTable->translate(Id) : shared_str();
}

bool IsFinished(const CGameTask* Task)
{
	return Task->GetTaskState() == eTaskStateCompleted || Task->GetTaskState() == eTaskStateFail;
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

shared_str TaskLevelName(CGameTask* Task)
{
	CMapLocation* Location = Task ? Task->LinkedMapLocation() : nullptr;
	return Location ? Location->GetLevelName() : shared_str();
}
}

void UITaskJournalItem::InitFromXml(CUIXml& Xml, const char* Path, CUITaskJournalWnd* OwnerWnd)
{
	Owner = OwnerWnd;
	CUIXmlInit::InitWindow(Xml, Path, 0, this);
	BaseHeight = GetHeight();
	Padding = Xml.ReadAttribFlt(Path, 0, "padding", 8.f);

	XML_NODE* StoredRoot = Xml.GetLocalRoot();
	Xml.SetLocalRoot(Xml.NavigateToNode(Path, 0));

	if (Xml.NavigateToNode("selection", 0))
	{
		const char* SelectionType = Xml.ReadAttrib("selection", 0, "type", "static");
		if (!xr_strcmp(SelectionType, "line"))
		{
			Selection = UIHelper::CreateFrameLine(Xml, "selection", this, false);
		}
		else if (!xr_strcmp(SelectionType, "frame"))
		{
			Selection = UIHelper::CreateFrameWindow(Xml, "selection", this, false);
		}
		else
		{
			Selection = UIHelper::CreateStatic(Xml, "selection", this, false);
		}
		if (Selection)
		{
			Selection->Show(false);
		}
	}
	if (Xml.NavigateToNode("icon", 0))
	{
		Icon = UIHelper::CreateStatic(Xml, "icon", this, false);
	}
	if (Xml.NavigateToNode("type_icon", 0))
	{
		TypeIcon = UIHelper::CreateStatic(Xml, "type_icon", this, false);
		TypeIconStory = Xml.ReadAttrib("type_icon", 0, "texture_story", "ui_inGame2_PDA_icon_Primary_mission");
		TypeIconSide = Xml.ReadAttrib("type_icon", 0, "texture_side", "ui_inGame2_PDA_icon_Secondary_mission");
	}
	if (Xml.NavigateToNode("title", 0))
	{
		Title = UIHelper::CreateStatic(Xml, "title", this, false);
	}
	if (Xml.NavigateToNode("status", 0))
	{
		Status = UIHelper::CreateStatic(Xml, "status", this, false);
		StatusCompleted = Xml.ReadAttrib("status", 0, "text_completed", "");
		StatusFailed = Xml.ReadAttrib("status", 0, "text_failed", "");
	}
	if (Xml.NavigateToNode("time", 0))
	{
		Time = UIHelper::CreateStatic(Xml, "time", this, false);
	}
	if (Xml.NavigateToNode("active_mark", 0))
	{
		ActiveMark = UIHelper::CreateStatic(Xml, "active_mark", this, false);
	}
	if (Xml.NavigateToNode("btn_focus", 0))
	{
		FocusButton = UIHelper::Create3tButton(Xml, "btn_focus", this);
		if (FocusButton)
		{
			FocusButton->SetMessageTarget(this);
		}
	}

	const u32 TitleColor = Title ? Title->GetTextColor() : 0xFFFFFFFF;
	ColorNormal = CUIXmlInit::GetColor(Xml, "colors:normal", 0, TitleColor);
	ColorActive = CUIXmlInit::GetColor(Xml, "colors:active", 0, ColorNormal);
	ColorUnread = CUIXmlInit::GetColor(Xml, "colors:unread", 0, ColorNormal);
	ColorCompleted = CUIXmlInit::GetColor(Xml, "colors:completed", 0, ColorNormal);
	ColorFailed = CUIXmlInit::GetColor(Xml, "colors:failed", 0, ColorNormal);

	Xml.SetLocalRoot(StoredRoot);
}

void UITaskJournalItem::Setup(CGameTask* TaskPtr, bool Active, const char* TimeText)
{
	Task = TaskPtr;
	const ETaskState State = Task->GetTaskState();

	TaskJournalHelpers::ApplyTaskIcon(Icon, Task);

	if (TypeIcon)
	{
		TypeIcon->InitTexture(Task->GetTaskType() == eTaskTypeStoryline ? TypeIconStory.c_str() : TypeIconSide.c_str());
	}

	if (Status)
	{
		shared_str Text;
		if (State == eTaskStateCompleted)
		{
			Text = StatusCompleted;
		}
		else if (State == eTaskStateFail)
		{
			Text = StatusFailed;
		}
		Status->TextItemControl()->SetTextST(Text.size() ? Text.c_str() : "");
		Status->SetTextColor(State == eTaskStateFail ? ColorFailed : ColorCompleted);
	}

	if (Time)
	{
		Time->SetText(TimeText ? TimeText : "");
	}

	if (ActiveMark)
	{
		ActiveMark->Show(Active);
	}

	if (FocusButton)
	{
		FocusButton->Show(State == eTaskStateInProgress && Task->HasActiveMapTarget());
	}

	float Height = BaseHeight;
	if (Title)
	{
		Title->TextItemControl()->SetTextST(g_pStringTable->ParseStringFromScript(Task->m_Title).c_str());
		Title->AdjustHeightToText();

		u32 Color = ColorNormal;
		if (State == eTaskStateCompleted)
		{
			Color = ColorCompleted;
		}
		else if (State == eTaskStateFail)
		{
			Color = ColorFailed;
		}
		else if (Active)
		{
			Color = ColorActive;
		}
		else if (!Task->m_read)
		{
			Color = ColorUnread;
		}
		Title->SetTextColor(Color);

		Height = std::max(Height, Title->GetWndPos().y + Title->GetHeight() + Padding);
	}

	SetHeight(Height);
	if (Selection)
	{
		Selection->SetHeight(Height);
	}
}

void UITaskJournalItem::SetSelected(bool Selected)
{
	if (Selection)
	{
		Selection->Show(Selected);
	}
}

bool UITaskJournalItem::OnMouseAction(float x, float y, EUIMessages mouse_action)
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

void UITaskJournalItem::SendMessage(CUIWindow* pWnd, s16 msg, void* pData)
{
	if (Owner && FocusButton && pWnd == FocusButton && msg == BUTTON_CLICKED)
	{
		Owner->OnItemSelected(this);
		Owner->OnItemActivated(this);
		return;
	}
	inherited::SendMessage(pWnd, msg, pData);
}

void CUITaskJournalWnd::Init()
{
	HotkeyContext.SetContext("pda.journal");
	HotkeyContext.SetHandler({shared_str("pda.journal")}, Hotkeys::LayerHandler(this, &CUITaskJournalWnd::HandleLayerAction));
	Xml.Load(CONFIG_PATH, UI_PATH, TaskJournalHelpers::TaskJournalXml);
	CUIXmlInit::InitWindow(Xml, "main_wnd", 0, this);
	ReplaceTaskList = Xml.ReadAttribInt("main_wnd", 0, "replace_task_list", 0) != 0;

	if (Xml.NavigateToNode("filter_tabs", 0) && Xml.GetNodesNum("filter_tabs", 0, "button") > 0)
	{
		FilterTabs = new CUITabControl();
		FilterTabs->SetAutoDelete(true);
		AttachChild(FilterTabs);
		CUIXmlInit::InitTabControl(Xml, "filter_tabs", 0, FilterTabs);
		FilterTabs->SetMessageTarget(this);

		if (Xml.ReadAttribInt("filter_tabs", 0, "counters", 0) != 0)
		{
			for (CUITabButton* Button : *FilterTabs->GetButtonsVector())
			{
				const char* BaseText = Button->TextItemControl()->GetText();
				TabTexts.push_back({ Button->m_btn_id, BaseText ? BaseText : "" });
			}
		}
	}

	CreateHint("filter_hint_prev", this);
	CreateHint("filter_hint_next", this);
	CUIHotkeysHint::CreateAll(Xml, this, Hints);

	List = new CUIScrollView();
	List->SetAutoDelete(true);
	AttachChild(List);
	CUIXmlInit::InitScrollView(Xml, "task_list", 0, List);

	if (Xml.NavigateToNode(TaskJournalHelpers::GroupHeaderPath, 0))
	{
		GroupHeaders = true;
		GroupTextStory = Xml.ReadAttrib(TaskJournalHelpers::GroupHeaderPath, 0, "text_story", "");
		GroupTextSide = Xml.ReadAttrib(TaskJournalHelpers::GroupHeaderPath, 0, "text_side", "");
	}

	if (Xml.NavigateToNode("t_empty", 0))
	{
		EmptyText = UIHelper::CreateStatic(Xml, "t_empty", this);
		EmptyTextDefault = Xml.ReadAttrib("t_empty", 0, "text", "");
		for (u32 FilterIndex = 0; FilterIndex < static_cast<u32>(ETaskListFilter::Count); ++FilterIndex)
		{
			string64 AttribName;
			xr_sprintf(AttribName, "text_%s", UITaskListWnd::TabIdFromFilter(static_cast<ETaskListFilter>(FilterIndex)));
			EmptyTexts[FilterIndex] = Xml.ReadAttrib("t_empty", 0, AttribName, "");
		}
		EmptyText->Show(false);
	}

	InitDetails();

	if (FilterTabs && FilterTabs->GetTabsCount() > 0)
	{
		Filter = UITaskListWnd::FilterFromTabId(FilterTabs->GetButtonByIndex(0)->m_btn_id);
		FilterTabs->SetActiveTabByIndex(0);
	}

	HintsControllerMode = pInput && pInput->GetControllerMode();

	if (UiSounds)
	{
		UiSounds->LoadSubdialog(Xml, "main_wnd");
	}
}

CUIHotkeysHint* CUITaskJournalWnd::CreateHint(const char* Path, CUIWindow* Parent, int Index)
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

void CUITaskJournalWnd::RefreshHints()
{
	for (CUIHotkeysHint* Hint : Hints)
	{
		Hint->Refresh();
	}
}

void CUITaskJournalWnd::InitDetails()
{
	if (!Xml.NavigateToNode("details", 0))
	{
		return;
	}

	TextStory = Xml.ReadAttrib("details", 0, "text_story", "");
	TextSide = Xml.ReadAttrib("details", 0, "text_side", "");
	TextInProgress = Xml.ReadAttrib("details", 0, "text_in_progress", "");
	TextActive = Xml.ReadAttrib("details", 0, "text_active", "");
	TextCompleted = Xml.ReadAttrib("details", 0, "text_completed", "");
	TextFailed = Xml.ReadAttrib("details", 0, "text_failed", "");
	FormatReceived = Xml.ReadAttrib("details", 0, "received_format", "");
	FormatFinished = Xml.ReadAttrib("details", 0, "finished_format", "");
	FormatLocation = Xml.ReadAttrib("details", 0, "location_format", "");
	ObjectivesCaption = Xml.ReadAttrib("details", 0, "objectives_caption", "");
	HeadingFromDescription = !xr_strcmp(Xml.ReadAttrib("details", 0, "heading_source", "title"), "description");
	ImageSection = Xml.ReadAttrib("details", 0, "image_section", "task_journal_images");
	ImageDefault = Xml.ReadAttrib("details", 0, "image_default", "");
	ImageLevelIntro = Xml.ReadAttribInt("details", 0, "image_level_intro", 0) != 0;

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
		if (Crop && Crop[0])
		{
			float X = 0.f;
			float Y = 0.f;
			float W = 0.f;
			float H = 0.f;
			if (sscanf(Crop, "%f,%f,%f,%f", &X, &Y, &W, &H) == 4 && W > 0.f && H > 0.f)
			{
				ImageCrop = true;
				ImageCropRect.set(X, Y, X + W, Y + H);
			}
		}
	}
	if (Xml.NavigateToNode("icon", 0))
	{
		DetailsIcon = UIHelper::CreateStatic(Xml, "icon", Details, false);
	}
	if (Xml.NavigateToNode("title", 0))
	{
		DetailsTitle = UIHelper::CreateStatic(Xml, "title", Details, false);
	}
	if (Xml.NavigateToNode("type", 0))
	{
		DetailsType = UIHelper::CreateStatic(Xml, "type", Details, false);
	}
	if (Xml.NavigateToNode("status", 0))
	{
		DetailsStatus = UIHelper::CreateStatic(Xml, "status", Details, false);
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
	if (Xml.NavigateToNode("btn_activate", 0))
	{
		BtnActivate = UIHelper::Create3tButton(Xml, "btn_activate", Details);
		CreateHint("btn_activate:hint", BtnActivate);
	}

	ColorObjectiveProgress = CUIXmlInit::GetColor(Xml, "colors:progress", 0, 0xFFDCDCDC);
	ColorObjectiveCompleted = CUIXmlInit::GetColor(Xml, "colors:completed", 0, ColorObjectiveProgress);
	ColorObjectiveFailed = CUIXmlInit::GetColor(Xml, "colors:failed", 0, ColorObjectiveProgress);

	Xml.SetLocalRoot(StoredRoot);

	if (BtnShowOnMap)
	{
		Register(BtnShowOnMap);
		AddCallback(BtnShowOnMap, BUTTON_CLICKED, CUIWndCallback::void_function(this, &CUITaskJournalWnd::OnShowOnMapClicked));
	}
	if (BtnActivate)
	{
		Register(BtnActivate);
		AddCallback(BtnActivate, BUTTON_CLICKED, CUIWndCallback::void_function(this, &CUITaskJournalWnd::OnActivateClicked));
	}
}

void CUITaskJournalWnd::Show(bool status)
{
	HotkeyContext.SetActive(status);
	inherited::Show(status);
	if (status)
	{
		NeedRebuild = true;
		HintsControllerMode = pInput && pInput->GetControllerMode();
		RefreshHints();
	}
}

void CUITaskJournalWnd::Update()
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

	if (!NeedRebuild && Device.dwTimeGlobal >= NextRefreshTime)
	{
		NextRefreshTime = Device.dwTimeGlobal + TaskJournalHelpers::RefreshIntervalMs;
		NeedRebuild = TasksSignature() != Signature;
	}

	if (NeedRebuild)
	{
		RebuildList();
	}
}

void CUITaskJournalWnd::SendMessage(CUIWindow* pWnd, s16 msg, void* pData)
{
	if (msg == TAB_CHANGED && FilterTabs && pWnd == FilterTabs)
	{
		if (UiSounds && IsShown())
		{
			UiSounds->Play(EPdaUiSound::Tab);
		}
		Filter = UITaskListWnd::FilterFromTabId(FilterTabs->GetActiveId());
		NeedRebuild = true;
		return;
	}

	inherited::SendMessage(pWnd, msg, pData);
	CUIWndCallback::OnEvent(pWnd, msg, pData);
}

bool CUITaskJournalWnd::SwitchFilter(bool Next)
{
	if (!FilterTabs || FilterTabs->GetTabsCount() < 2)
	{
		return false;
	}
	if (Next)
	{
		FilterTabs->NextTab(true);
	}
	else
	{
		FilterTabs->PrevTab(true);
	}
	return true;
}

bool CUITaskJournalWnd::HandleAction(EGameActions Action)
{
	switch (Action)
	{
	case kPDA_TASK_JOURNAL_FILTER_PREV:
		SwitchFilter(false);
		return true;
	case kPDA_TASK_JOURNAL_FILTER_NEXT:
		SwitchFilter(true);
		return true;
	case kPDA_TASK_JOURNAL_PREV:
		MoveSelection(false);
		return true;
	case kPDA_TASK_JOURNAL_NEXT:
		MoveSelection(true);
		return true;
	case kPDA_TASK_JOURNAL_SHOW_ON_MAP:
		PendingShowOnMap = SelectedId;
		return true;
	case kPDA_TASK_JOURNAL_TRACK:
		PendingActivate = SelectedId;
		return true;
	default:
		return false;
	}
}

bool CUITaskJournalWnd::HandleLayerAction(EGameActions Action, int Dik)
{
	return IsShown() && HandleAction(Action);
}

bool CUITaskJournalWnd::OnKeyboardAction(int dik, EUIMessages keyboard_action)
{
	if (keyboard_action == WINDOW_KEY_PRESSED)
	{
		if (HandleAction(get_binded_action(dik, agUITaskJournal)))
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
			PendingShowOnMap = SelectedId;
			return true;
		default:
			break;
		}
	}
	return inherited::OnKeyboardAction(dik, keyboard_action);
}

bool CUITaskJournalWnd::OnGamepadKeyAction(int key, EUIMessages gamepad_action)
{
	if (gamepad_action == WINDOW_KEY_PRESSED && HandleAction(get_binded_action(key, agUITaskJournal)))
	{
		return true;
	}
	return inherited::OnGamepadKeyAction(key, gamepad_action);
}

void CUITaskJournalWnd::OnItemSelected(UITaskJournalItem* Item)
{
	if (!Item || !Item->GetTask())
	{
		return;
	}
	if (UiSounds && SelectedId != Item->GetTask()->m_ID)
	{
		UiSounds->Play(EPdaUiSound::ListSelect);
	}
	SelectTask(Item->GetTask());
}

void CUITaskJournalWnd::OnItemActivated(UITaskJournalItem* Item)
{
	if (Item && Item->GetTask())
	{
		PendingShowOnMap = Item->GetTask()->m_ID;
	}
}

void CUITaskJournalWnd::OnShowOnMapClicked(CUIWindow* w, void* d)
{
	PendingShowOnMap = SelectedId;
}

void CUITaskJournalWnd::OnActivateClicked(CUIWindow* w, void* d)
{
	PendingActivate = SelectedId;
}

void CUITaskJournalWnd::ApplyPendingActions()
{
	if (PendingActivate.size())
	{
		CGameTask* Task = FindTask(PendingActivate);
		PendingActivate = shared_str();
		if (Task && Task->GetTaskState() == eTaskStateInProgress && !IsActiveTask(Task))
		{
			Level().GameTaskManager()->SetActiveTask(Task);
			if (UiSounds)
			{
				UiSounds->Play(EPdaUiSound::ListSelect);
			}
			NeedRebuild = true;
		}
	}

	if (PendingShowOnMap.size())
	{
		CGameTask* Task = FindTask(PendingShowOnMap);
		PendingShowOnMap = shared_str();
		if (Task && Task->GetTaskState() == eTaskStateInProgress && Task->HasActiveMapTarget() && OnShowOnMap)
		{
			OnShowOnMap(Task);
		}
	}
}

CGameTask* CUITaskJournalWnd::FindTask(const shared_str& Id) const
{
	if (!Id.size())
	{
		return nullptr;
	}
	for (SGameTaskKey& Key : Level().GameTaskManager()->GetGameTasks())
	{
		CGameTask* Task = Key.getGameTask();
		if (Task && Task->m_ID == Id)
		{
			return Task;
		}
	}
	return nullptr;
}

bool CUITaskJournalWnd::IsActiveTask(const CGameTask* Task) const
{
	CGameTaskManager* Manager = Level().GameTaskManager();
	return Task && (Task == Manager->ActiveTask(eTaskTypeStoryline) || Task == Manager->ActiveTask(eTaskTypeAdditional));
}

u32 CUITaskJournalWnd::TasksSignature() const
{
	CGameTaskManager* Manager = Level().GameTaskManager();
	u32 Hash = 2166136261u;
	const auto Mix = [&Hash](u32 Value)
	{
		Hash = (Hash ^ Value) * 16777619u;
	};

	for (SGameTaskKey& Key : Manager->GetGameTasks())
	{
		const CGameTask* Task = Key.getGameTask();
		if (!Task)
		{
			continue;
		}
		Mix(u32(Task->GetTaskState()));
		Mix(Task->m_read ? 1u : 0u);
		Mix(u32(reinterpret_cast<uintptr_t>(Task)));
		for (u16 Index = 1; Index <= Task->GetObjectivesCount(true); ++Index)
		{
			Mix(u32(Task->ObjectiveState(Index)));
		}
	}

	const CGameTask* Story = Manager->ActiveTask(eTaskTypeStoryline);
	const CGameTask* Side = Manager->ActiveTask(eTaskTypeAdditional);
	Mix(u32(reinterpret_cast<uintptr_t>(Story)));
	Mix(u32(reinterpret_cast<uintptr_t>(Side)));
	return Hash;
}

void CUITaskJournalWnd::SortTasks(xr_vector<CGameTask*>& Tasks) const
{
	const bool Finished = UITaskListWnd::IsFinishedFilter(Filter);
	std::stable_sort(Tasks.begin(), Tasks.end(), [this, Finished](const CGameTask* Left, const CGameTask* Right)
	{
		if (GroupHeaders)
		{
			const bool LeftStory = Left->GetTaskType() == eTaskTypeStoryline;
			const bool RightStory = Right->GetTaskType() == eTaskTypeStoryline;
			if (LeftStory != RightStory)
			{
				return LeftStory;
			}
		}
		if (Finished)
		{
			return Left->m_FinishTime > Right->m_FinishTime;
		}

		const bool LeftActive = IsActiveTask(Left);
		const bool RightActive = IsActiveTask(Right);
		if (LeftActive != RightActive)
		{
			return LeftActive;
		}
		if (!GroupHeaders)
		{
			const bool LeftStory = Left->GetTaskType() == eTaskTypeStoryline;
			const bool RightStory = Right->GetTaskType() == eTaskTypeStoryline;
			if (LeftStory != RightStory)
			{
				return LeftStory;
			}
		}
		if (Left->m_priority != Right->m_priority)
		{
			return Left->m_priority > Right->m_priority;
		}
		return Left->m_ReceiveTime > Right->m_ReceiveTime;
	});
}

void CUITaskJournalWnd::AddGroupHeader(bool Story)
{
	CUIStatic* Header = new CUIStatic();
	CUIXmlInit::InitStatic(Xml, TaskJournalHelpers::GroupHeaderPath, 0, Header);
	const shared_str& TextId = Story ? GroupTextStory : GroupTextSide;
	Header->SetText(TaskJournalHelpers::TranslateOptional(TextId).c_str());
	List->AddWindow(Header, true);
}

void CUITaskJournalWnd::RebuildList()
{
	NeedRebuild = false;
	Signature = TasksSignature();
	NextRefreshTime = Device.dwTimeGlobal + TaskJournalHelpers::RefreshIntervalMs;

	const int ScrollPos = List->GetCurrentScrollPos();
	List->Clear();

	xr_vector<CGameTask*> Tasks;
	for (SGameTaskKey& Key : Level().GameTaskManager()->GetGameTasks())
	{
		CGameTask* Task = Key.getGameTask();
		if (UITaskListWnd::TaskMatchesFilter(Task, Filter))
		{
			Tasks.push_back(Task);
		}
	}
	SortTasks(Tasks);

	CGameTask* Selected = nullptr;
	bool HasGroup = false;
	bool CurrentStory = false;
	for (CGameTask* Task : Tasks)
	{
		const bool Story = Task->GetTaskType() == eTaskTypeStoryline;
		if (GroupHeaders && (!HasGroup || Story != CurrentStory))
		{
			AddGroupHeader(Story);
			HasGroup = true;
			CurrentStory = Story;
		}

		const bool Finished = TaskJournalHelpers::IsFinished(Task);
		const xr_string TimeText = FormatTime(Finished ? Task->m_FinishTime : Task->m_ReceiveTime);

		UITaskJournalItem* Item = new UITaskJournalItem();
		Item->InitFromXml(Xml, TaskJournalHelpers::ItemPath, this);
		Item->Setup(Task, IsActiveTask(Task), TimeText.c_str());
		List->AddWindow(Item, true);

		if (Task->m_ID == SelectedId)
		{
			Selected = Task;
		}
	}

	if (!Selected && !Tasks.empty())
	{
		Selected = Tasks.front();
		for (CGameTask* Task : Tasks)
		{
			if (IsActiveTask(Task))
			{
				Selected = Task;
				break;
			}
		}
	}

	UpdateTabCounters();
	UpdateEmptyText(Tasks.empty());
	SelectTask(Selected);

	List->ForceUpdate();
	List->SetScrollPos(ScrollPos);
}

void CUITaskJournalWnd::UpdateTabCounters()
{
	if (!FilterTabs)
	{
		return;
	}

	for (const STabText& TabText : TabTexts)
	{
		CUITabButton* Button = FilterTabs->GetButtonById(TabText.Id);
		if (!Button)
		{
			continue;
		}

		const ETaskListFilter TabFilter = UITaskListWnd::FilterFromTabId(TabText.Id);
		u32 Count = 0;
		for (SGameTaskKey& Key : Level().GameTaskManager()->GetGameTasks())
		{
			if (UITaskListWnd::TaskMatchesFilter(Key.getGameTask(), TabFilter))
			{
				++Count;
			}
		}

		string256 Text;
		xr_sprintf(Text, "%s (%u)", TabText.BaseText.c_str(), Count);
		Button->TextItemControl()->SetText(Text);
	}
}

void CUITaskJournalWnd::UpdateEmptyText(bool Empty)
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

	shared_str TextId = EmptyTexts[static_cast<u32>(Filter)];
	if (!TextId.size())
	{
		TextId = EmptyTextDefault;
	}
	if (TextId.size())
	{
		EmptyText->TextItemControl()->SetTextST(TextId.c_str());
	}
}

void CUITaskJournalWnd::SelectTask(CGameTask* Task)
{
	SelectedId = Task ? Task->m_ID : shared_str();
	if (Task)
	{
		Task->m_read = true;
	}

	for (CUIWindow* Window : List->Items())
	{
		if (UITaskJournalItem* Item = smart_cast<UITaskJournalItem*>(Window))
		{
			Item->SetSelected(Task && Item->GetTask() == Task);
		}
	}

	UpdateDetails();
}

void CUITaskJournalWnd::MoveSelection(bool Next)
{
	xr_vector<UITaskJournalItem*> Items;
	for (CUIWindow* Window : List->Items())
	{
		if (UITaskJournalItem* Item = smart_cast<UITaskJournalItem*>(Window))
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
		if (Items[i]->GetTask() && Items[i]->GetTask()->m_ID == SelectedId)
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

xr_string CUITaskJournalWnd::FormatTime(ALife::_TIME_ID Time) const
{
	if (!Time)
	{
		return {};
	}

	const shared_str Date = InventoryUtilities::GetDateAsString(Time, InventoryUtilities::edpDateToDay);
	const shared_str Clock = InventoryUtilities::GetTimeAsString(Time, InventoryUtilities::etpTimeToMinutes);
	xr_string Result = Date.c_str();
	Result += ", ";
	Result += Clock.c_str();
	return Result;
}

xr_string CUITaskJournalWnd::FormatLine(const shared_str& FormatId, const char* Value) const
{
	xr_string Text = "{value}";
	if (FormatId.size())
	{
		const shared_str Format = g_pStringTable->translate(FormatId);
		if (Format != FormatId)
		{
			Text = Format.c_str();
		}
	}
	TaskJournalHelpers::ReplaceToken(Text, "{value}", Value ? Value : "");
	return Text;
}

CUIStatic* CUITaskJournalWnd::AddDetailsText(const char* Path, const char* Text)
{
	if (!DetailsBody || !Xml.NavigateToNode(Path, 0))
	{
		return nullptr;
	}

	CUIStatic* Static = new CUIStatic();
	CUIXmlInit::InitStatic(Xml, Path, 0, Static);
	Static->SetText(Text);
	Static->AdjustHeightToText();
	DetailsBody->AddWindow(Static, true);
	return Static;
}

shared_str CUITaskJournalWnd::ResolveImage(CGameTask* Task) const
{
	const shared_str LevelName = TaskJournalHelpers::TaskLevelName(Task);
	const char* Section = ImageSection.c_str();
	const bool HasSection = ImageSection.size() && pSettings->section_exist(Section);

	if (HasSection && pSettings->line_exist(Section, Task->m_ID.c_str()))
	{
		return pSettings->r_string(Section, Task->m_ID.c_str());
	}
	if (HasSection && LevelName.size() && pSettings->line_exist(Section, LevelName.c_str()))
	{
		return pSettings->r_string(Section, LevelName.c_str());
	}
	if (ImageLevelIntro && LevelName.size())
	{
		string_path Intro;
		xr_strconcat(Intro, "intro\\intro_", LevelName.c_str());
		if (TaskJournalHelpers::TextureExists(Intro))
		{
			return Intro;
		}
	}
	if (HasSection && pSettings->line_exist(Section, "default"))
	{
		return pSettings->r_string(Section, "default");
	}
	return ImageDefault;
}

void CUITaskJournalWnd::UpdateImage(CGameTask* Task)
{
	if (!DetailsImage)
	{
		return;
	}

	const shared_str Texture = Task ? ResolveImage(Task) : shared_str();
	const bool HasImage = Texture.size() != 0;
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

	if (!DetailsBody)
	{
		return;
	}

	if (HasImage || !Task)
	{
		DetailsBody->SetWndPos(BodyBasePos);
		DetailsBody->SetHeight(BodyBaseHeight);
	}
	else
	{
		const float Top = DetailsImage->GetWndPos().y;
		DetailsBody->SetWndPos(Fvector2().set(BodyBasePos.x, Top));
		DetailsBody->SetHeight(BodyBaseHeight + (BodyBasePos.y - Top));
	}
}

void CUITaskJournalWnd::UpdateDetails()
{
	if (!Details)
	{
		return;
	}

	CGameTask* Task = FindTask(SelectedId);
	const bool HasTask = Task != nullptr;

	if (DetailsEmpty)
	{
		DetailsEmpty->Show(!HasTask);
	}
	for (CUIWindow* Window : { static_cast<CUIWindow*>(DetailsIcon), static_cast<CUIWindow*>(DetailsTitle),
		static_cast<CUIWindow*>(DetailsType), static_cast<CUIWindow*>(DetailsStatus), static_cast<CUIWindow*>(DetailsBody) })
	{
		if (Window)
		{
			Window->Show(HasTask);
		}
	}

	UpdateImage(Task);

	if (DetailsBody)
	{
		DetailsBody->Clear();
	}

	const ETaskState State = HasTask ? Task->GetTaskState() : eTaskStateDummy;
	const bool InProgress = State == eTaskStateInProgress;
	const bool Active = HasTask && IsActiveTask(Task);

	if (BtnShowOnMap)
	{
		BtnShowOnMap->Show(InProgress && Task->HasActiveMapTarget() && OnShowOnMap);
	}
	if (BtnActivate)
	{
		BtnActivate->Show(InProgress && !Active);
	}

	if (!HasTask)
	{
		return;
	}

	TaskJournalHelpers::ApplyTaskIcon(DetailsIcon, Task);

	const xr_string TitleText = g_pStringTable->ParseStringFromScript(Task->m_Title).c_str();
	const xr_string DescriptionText = Task->m_Description.size()
		? g_pStringTable->ParseStringFromScript(Task->m_Description).c_str() : "";

	if (DetailsTitle)
	{
		DetailsTitle->SetText(TitleText.c_str());
	}
	if (DetailsType)
	{
		const shared_str& TypeText = Task->GetTaskType() == eTaskTypeStoryline ? TextStory : TextSide;
		DetailsType->SetText(TaskJournalHelpers::TranslateOptional(TypeText).c_str());
	}
	if (DetailsStatus)
	{
		shared_str StatusText = TextInProgress;
		u32 Color = ColorObjectiveProgress;
		if (State == eTaskStateCompleted)
		{
			StatusText = TextCompleted;
			Color = ColorObjectiveCompleted;
		}
		else if (State == eTaskStateFail)
		{
			StatusText = TextFailed;
			Color = ColorObjectiveFailed;
		}
		else if (Active && TextActive.size())
		{
			StatusText = TextActive;
		}
		DetailsStatus->SetText(TaskJournalHelpers::TranslateOptional(StatusText).c_str());
		DetailsStatus->SetTextColor(Color);
	}

	if (!DetailsBody)
	{
		return;
	}

	bool DescriptionShown = false;
	if (Xml.NavigateToNode("details:heading", 0))
	{
		const bool UseDescription = HeadingFromDescription && !DescriptionText.empty();
		AddDetailsText("details:heading", UseDescription ? DescriptionText.c_str() : TitleText.c_str());
		DescriptionShown = UseDescription;
	}
	if (!DescriptionShown && !DescriptionText.empty())
	{
		AddDetailsText("details:descr", DescriptionText.c_str());
	}

	const xr_string Received = FormatTime(Task->m_ReceiveTime);
	if (!Received.empty())
	{
		AddDetailsText("details:info", FormatLine(FormatReceived, Received.c_str()).c_str());
	}
	if (!InProgress)
	{
		const xr_string Finished = FormatTime(Task->m_FinishTime);
		if (!Finished.empty())
		{
			AddDetailsText("details:info", FormatLine(FormatFinished, Finished.c_str()).c_str());
		}
	}
	else
	{
		const shared_str LevelName = TaskJournalHelpers::TaskLevelName(Task);
		if (LevelName.size())
		{
			AddDetailsText("details:info", FormatLine(FormatLocation, g_pStringTable->translate(LevelName).c_str()).c_str());
		}
	}

	const u16 ObjectiveCount = Task->GetObjectivesCount(true);
	if (ObjectiveCount > 0)
	{
		if (ObjectivesCaption.size())
		{
			AddDetailsText("details:objectives_caption", g_pStringTable->translate(ObjectivesCaption).c_str());
		}
		for (u16 Index = 1; Index <= ObjectiveCount; ++Index)
		{
			const SGameTaskObjective& Objective = Task->Objective(Index);
			CUIStatic* Line = AddDetailsText("details:objective", g_pStringTable->ParseStringFromScript(Objective.m_Title).c_str());
			if (!Line)
			{
				continue;
			}
			const ETaskState ObjectiveState = Objective.GetTaskState();
			Line->SetTextColor(ObjectiveState == eTaskStateCompleted ? ColorObjectiveCompleted
				: (ObjectiveState == eTaskStateFail ? ColorObjectiveFailed : ColorObjectiveProgress));
		}
	}

	DetailsBody->ScrollToBegin();
}
