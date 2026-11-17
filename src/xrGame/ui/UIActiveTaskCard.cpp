#include "StdAfx.h"
#include "UIActiveTaskCard.h"
#include "UITaskWnd.h"

#include "../../xrUI/xrUIXmlParser.h"
#include "../../xrUI/UIXmlInit.h"
#include "../../xrUI/UIHelper.h"
#include "../../xrUI/Widgets/UIFrameWindow.h"
#include "../../xrUI/Widgets/UIStatic.h"
#include "../../xrUI/Widgets/UI3tButton.h"
#include "../../xrEngine/string_table.h"
#include "../../xrEngine/xr_level_controller.h"

#include "../Level.h"
#include "../map_manager.h"
#include "../map_location.h"
#include "../GameTask.h"
#include "../GametaskManager.h"

namespace ActiveTaskCard
{
constexpr u32 RefreshIntervalMs = 500;

CUIStatic* CreateOptionalStatic(CUIXml& Xml, const char* Path, CUIWindow* Parent)
{
	return Xml.NavigateToNode(Path, 0) ? UIHelper::CreateStatic(Xml, Path, Parent) : nullptr;
}

CUIFrameWindow* CreateOptionalFrame(CUIXml& Xml, const char* Path, CUIWindow* Parent)
{
	return Xml.NavigateToNode(Path, 0) ? UIHelper::CreateFrameWindow(Xml, Path, Parent) : nullptr;
}

CUI3tButton* CreateOptionalButton(CUIXml& Xml, const char* Path, CUIWindow* Parent)
{
	return Xml.NavigateToNode(Path, 0) ? UIHelper::Create3tButton(Xml, Path, Parent) : nullptr;
}

void ShowWindow(CUIWindow* Window, bool Visible)
{
	if (Window)
	{
		Window->Show(Visible);
	}
}
} // namespace ActiveTaskCard

void UIActiveTaskCard::InitFromXml(CUIXml& Xml, const char* Path)
{
	using namespace ActiveTaskCard;

	CUIXmlInit::InitWindow(Xml, Path, 0, this);
	FullHeight = GetHeight();

	XML_NODE* StoredRoot = Xml.GetLocalRoot();
	Xml.SetLocalRoot(Xml.NavigateToNode(Path, 0));

	Frame = CreateOptionalFrame(Xml, "background_frame", this);
	Background = CreateOptionalStatic(Xml, "background", this);
	HeaderFrame = CreateOptionalFrame(Xml, "header_frame", this);
	HeaderIcon = CreateOptionalStatic(Xml, "header_icon", this);
	HeaderText = CreateOptionalStatic(Xml, "t_header", this);
	ListButton = CreateOptionalButton(Xml, "btn_list", this);
	BodyFrame = CreateOptionalFrame(Xml, "body_frame", this);
	TaskIcon = CreateOptionalStatic(Xml, "t_icon", this);
	TypeText = CreateOptionalStatic(Xml, "t_type", this);
	TitleText = CreateOptionalStatic(Xml, "t_title", this);
	ObjectiveText = CreateOptionalStatic(Xml, "t_objective", this);
	InfoText = CreateOptionalStatic(Xml, "t_info", this);
	FocusButton = CreateOptionalButton(Xml, "btn_focus", this);

	Xml.SetLocalRoot(StoredRoot);

	TextStory = Xml.ReadAttrib(Path, 0, "text_story", "");
	TextSide = Xml.ReadAttrib(Path, 0, "text_side", "");
	TextMark = Xml.ReadAttrib(Path, 0, "text_mark", "");
	TextNone = Xml.ReadAttrib(Path, 0, "text_none", "");
	DistanceFormat = Xml.ReadAttrib(Path, 0, "distance_format", "");
	HeaderTextId = Xml.ReadAttrib(Path, 0, "header_text", "");
	HeaderCounter = Xml.ReadAttribInt(Path, 0, "header_counter", 0) != 0;
	ButtonTextShow = Xml.ReadAttrib(Path, 0, "button_show", "");
	ButtonTextHide = Xml.ReadAttrib(Path, 0, "button_hide", "");
	ButtonFormat = Xml.ReadAttrib(Path, 0, "button_format", "");
	IconStory = Xml.ReadAttrib(Path, 0, "icon_story", "");
	IconSide = Xml.ReadAttrib(Path, 0, "icon_side", "");
	IconMark = Xml.ReadAttrib(Path, 0, "icon_mark", "");
	HideWithTaskList = Xml.ReadAttribInt(Path, 0, "hide_with_task_list", 1) != 0;
	HeaderHeight = static_cast<float>(Xml.ReadAttribInt(Path, 0, "header_height", 0));
}

void UIActiveTaskCard::Tick(bool TaskListOpened)
{
	const bool ListStateChanged = TaskListOpened != TaskListShown;
	TaskListShown = TaskListOpened;

	if (ListStateChanged || Device.dwTimeGlobal >= NextRefreshTime)
	{
		Refresh();
	}

	if (HasHeader())
	{
		Show(true);
		UpdateLayout();
		return;
	}

	Show(HasContent && !(TaskListOpened && HideWithTaskList));
}

void UIActiveTaskCard::Refresh()
{
	NextRefreshTime = Device.dwTimeGlobal + ActiveTaskCard::RefreshIntervalMs;
	HasContent = false;
	HasTarget = false;
	TaskCount = 0;
	if (!g_pGameLevel)
	{
		return;
	}

	CGameTaskManager* TaskManager = Level().GameTaskManager();
	if (TaskManager)
	{
		TaskCount = TaskManager->GetTaskCount(eTaskStateInProgress, eTaskTypeStoryline) + TaskManager->GetTaskCount(eTaskStateInProgress, eTaskTypeAdditional);
	}
	UpdateHeader();

	CMapManager& MapManager = Level().MapManager();
	if (CMapLocation* Mark = MapManager.GetActiveUserNavigationLocation())
	{
		const char* Hint = Mark->GetHint();
		SetText(TypeText, TextMark.size() ? g_pStringTable->translate(TextMark).c_str() : "");
		SetText(TitleText, Hint ? Hint : "");
		SetText(ObjectiveText, "");
		SetText(InfoText, MakeInfoText(Mark).c_str());
		SetIcon(nullptr, IconMark);
		ActiveTaskCard::ShowWindow(FocusButton, true);
		HasContent = true;
		HasTarget = true;
		return;
	}

	CGameTask* Task = TaskManager ? TaskManager->ActiveTask(eTaskTypeStoryline) : nullptr;
	if (!Task && TaskManager)
	{
		Task = TaskManager->ActiveTask(eTaskTypeAdditional);
	}

	if (!Task)
	{
		if (TextNone.size() && !HasHeader())
		{
			SetText(TypeText, "");
			SetText(TitleText, g_pStringTable->translate(TextNone).c_str());
			SetText(ObjectiveText, "");
			SetText(InfoText, "");
			ActiveTaskCard::ShowWindow(FocusButton, false);
			HasContent = true;
		}
		return;
	}

	const ETaskType TaskType = Task->GetTaskType();
	const shared_str& TypeId = TaskType == eTaskTypeStoryline ? TextStory : TextSide;
	string256 TypeLine = "";
	if (TypeId.size())
	{
		const u32 Count = TaskManager->GetTaskCount(eTaskStateInProgress, TaskType);
		const u32 Index = TaskManager->GetTaskIndex(Task, eTaskStateInProgress, TaskType);
		if (Count > 1)
		{
			xr_sprintf(TypeLine, "%s \xB7 %u / %u", g_pStringTable->translate(TypeId).c_str(), Index, Count);
		}
		else
		{
			xr_strcpy(TypeLine, g_pStringTable->translate(TypeId).c_str());
		}
	}
	SetText(TypeText, TypeLine);
	SetText(TitleText, g_pStringTable->ParseStringFromScript(Task->m_Title).c_str());

	const SGameTaskObjective& Objective = Task->ActiveObjective();
	const shared_str& ObjectiveId = Task->ActiveObjectiveIdx() > 0 && Objective.m_Title.size() ? Objective.m_Title : Task->m_Description;
	SetText(ObjectiveText, ObjectiveId.size() ? g_pStringTable->ParseStringFromScript(ObjectiveId).c_str() : "");

	CMapLocation* Location = Task->HasActiveMapTarget() ? Task->LinkedMapLocation() : nullptr;
	SetText(InfoText, Location ? MakeInfoText(Location).c_str() : "");
	SetIcon(Task, TaskType == eTaskTypeStoryline ? IconStory : IconSide);
	ActiveTaskCard::ShowWindow(FocusButton, Location != nullptr);
	HasContent = true;
	HasTarget = true;
}

void UIActiveTaskCard::UpdateHeader()
{
	if (HeaderText && HeaderTextId.size())
	{
		string256 Text;
		if (HeaderCounter)
		{
			xr_sprintf(Text, "%s \xB7 %u", g_pStringTable->translate(HeaderTextId).c_str(), TaskCount);
		}
		else
		{
			xr_strcpy(Text, g_pStringTable->translate(HeaderTextId).c_str());
		}
		SetText(HeaderText, Text);
	}

	if (!ListButton)
	{
		return;
	}

	const shared_str& LabelId = TaskListShown ? ButtonTextHide : ButtonTextShow;
	const xr_string Label = LabelId.size() ? g_pStringTable->translate(LabelId).c_str() : "";

	string128 Binding = "";
	GetActionAllBinding("pda_tasks_toggle_list", Binding, sizeof(Binding));
	const shared_str NotBound = g_pStringTable->translate("st_key_notbinded");
	const bool HasBinding = Binding[0] && !(NotBound.size() && !xr_strcmp(Binding, NotBound.c_str()));

	string256 Text;
	if (HasBinding && ButtonFormat.find("%s") != xr_string::npos && ButtonFormat.find("%s") != ButtonFormat.rfind("%s"))
	{
		xr_sprintf(Text, ButtonFormat.c_str(), Label.c_str(), Binding);
	}
	else
	{
		xr_strcpy(Text, Label.c_str());
	}
	ListButton->TextItemControl()->SetText(Text);
}

void UIActiveTaskCard::UpdateLayout()
{
	const bool Collapsed = !HasTarget || (TaskListShown && HideWithTaskList);
	CUIWindow* BodyWindows[] = { BodyFrame, TaskIcon, TypeText, TitleText, ObjectiveText, InfoText };
	for (CUIWindow* Window : BodyWindows)
	{
		ActiveTaskCard::ShowWindow(Window, !Collapsed);
	}
	if (Collapsed)
	{
		ActiveTaskCard::ShowWindow(FocusButton, false);
	}

	if (HeaderHeight > 0.0f)
	{
		SetHeight(Collapsed ? HeaderHeight : FullHeight);
	}
}

bool UIActiveTaskCard::OnMouseAction(float x, float y, EUIMessages mouse_action)
{
	if (inherited::OnMouseAction(x, y, mouse_action))
	{
		return true;
	}

	if (!Owner || mouse_action != WINDOW_LBUTTON_DOWN)
	{
		return false;
	}

	if (HasHeader() && HeaderHeight > 0.0f && y < HeaderHeight)
	{
		Owner->ToggleTaskList();
		return true;
	}

	Owner->FocusActiveTarget();
	return true;
}

void UIActiveTaskCard::SendMessage(CUIWindow* pWnd, s16 msg, void* pData)
{
	if (Owner && msg == BUTTON_DOWN)
	{
		if (FocusButton && pWnd == FocusButton)
		{
			Owner->FocusActiveTarget();
			return;
		}
		if (ListButton && pWnd == ListButton)
		{
			Owner->ToggleTaskList();
			return;
		}
	}
	inherited::SendMessage(pWnd, msg, pData);
}

void UIActiveTaskCard::SetText(CUIStatic* Static, const char* Text)
{
	if (Static)
	{
		Static->TextItemControl()->SetText(Text ? Text : "");
	}
}

void UIActiveTaskCard::SetIcon(CGameTask* Task, const shared_str& Fallback)
{
	if (!TaskIcon)
	{
		return;
	}

	if (Task && Task->m_icon_texture_name.size())
	{
		TaskIcon->InitTexture(Task->m_icon_texture_name.c_str());
		Frect EmptyRect = Frect().set(0.0f, 0.0f, 0.0f, 0.0f);
		if (!Task->m_icon_rect.cmp(EmptyRect))
		{
			const Frect& IconRect = Task->m_icon_rect;
			Frect TextureRect;
			TextureRect.lt.set(IconRect.x1, IconRect.y1);
			TextureRect.rb.set(IconRect.x2, IconRect.y2);
			TextureRect.rb.add(TextureRect.lt);
			TaskIcon->SetTextureRect(TextureRect);
		}
	}
	else if (Fallback.size())
	{
		TaskIcon->InitTexture(Fallback.c_str());
	}
	else
	{
		TaskIcon->TextureOff();
		return;
	}

	TaskIcon->TextureOn();
	TaskIcon->SetStretchTexture(true);
}

xr_string UIActiveTaskCard::MakeInfoText(CMapLocation* Location) const
{
	if (!Location)
	{
		return "";
	}

	const shared_str& LevelName = Location->GetLevelName();
	xr_string Result = LevelName.size() ? g_pStringTable->translate(LevelName).c_str() : "";

	CObject* Actor = Level().CurrentEntity();
	if (!Actor || LevelName != Level().name() || !DistanceFormat.size())
	{
		return Result;
	}

	const Fvector& ActorPosition = Actor->Position();
	const Fvector TargetPosition = Location->GetLastPosition();
	const float Distance = _sqrt(_sqr(ActorPosition.x - TargetPosition.x) + _sqr(ActorPosition.z - TargetPosition.z));

	string256 Text;
	xr_sprintf(Text, g_pStringTable->translate(DistanceFormat).c_str(), Result.c_str(), iFloor(Distance));
	return Text;
}
