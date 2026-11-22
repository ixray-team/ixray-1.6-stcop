#include "StdAfx.h"
#include "UIPersonalSpotsWnd.h"
#include "UIHotkeysHint.h"
#include "UIMapWnd.h"
#include "UISecondTaskWnd.h"
#include "PdaUiSound.h"

#include "../../xrUI/xrUIXmlParser.h"
#include "../../xrUI/UIXmlInit.h"
#include "../../xrUI/UIHelper.h"
#include "../../xrUI/Widgets/UIFrameWindow.h"
#include "../../xrUI/Widgets/UIScrollView.h"
#include "../../xrUI/Widgets/UIStatic.h"
#include "../../xrUI/Widgets/UI3tButton.h"
#include "../../xrEngine/string_table.h"

#include "../Level.h"
#include "../map_manager.h"
#include "../map_location.h"

namespace PersonalSpotsWnd
{
constexpr u32 RefreshIntervalMs = 500;
constexpr u32 MessageDurationMs = 4000;

void SortSpots(xr_vector<CMapLocation*>& Spots, const shared_str& CurrentLevel)
{
	std::stable_sort(Spots.begin(), Spots.end(), [&CurrentLevel](CMapLocation* Left, CMapLocation* Right)
	{
		const bool LeftHere = Left->GetLevelName() == CurrentLevel;
		const bool RightHere = Right->GetLevelName() == CurrentLevel;
		if (LeftHere != RightHere)
		{
			return LeftHere;
		}
		return Left->ObjectID() < Right->ObjectID();
	});
}
} // namespace PersonalSpotsWnd

void UIPersonalSpotItem::InitFromXml(CUIXml& Xml, const char* Path, UIPersonalSpotsWnd* Panel, ALife::_OBJECT_ID Id)
{
	Owner = Panel;
	SpotId = Id;
	CUIXmlInit::InitWindow(Xml, Path, 0, this);

	const xr_string Base = Path;
	if (Xml.NavigateToNode((Base + ":highlight").c_str(), 0))
	{
		Background = UIHelper::CreateStatic(Xml, (Base + ":highlight").c_str(), this);
		Background->Show(false);
	}
	Name = UIHelper::CreateStatic(Xml, (Base + ":name").c_str(), this);
	if (Xml.NavigateToNode((Base + ":info").c_str(), 0))
	{
		Info = UIHelper::CreateStatic(Xml, (Base + ":info").c_str(), this);
	}
	if (Xml.NavigateToNode((Base + ":btn_track").c_str(), 0))
	{
		TrackButton = UIHelper::Create3tButton(Xml, (Base + ":btn_track").c_str(), this);
	}
	if (Xml.NavigateToNode((Base + ":btn_remove").c_str(), 0))
	{
		RemoveButton = UIHelper::Create3tButton(Xml, (Base + ":btn_remove").c_str(), this);
	}

	NameColor = CUIXmlInit::GetColor(Xml, (Base + ":normal").c_str(), 0, Name->GetTextColor());
	TrackedColor = CUIXmlInit::GetColor(Xml, (Base + ":tracked").c_str(), 0, NameColor);
}

void UIPersonalSpotItem::Refresh(CMapLocation& Location, bool Tracked, const char* InfoText)
{
	const char* Hint = Location.GetHint();
	Name->TextItemControl()->SetText(Hint ? Hint : "");
	Name->TextItemControl()->SetTextColor(Tracked ? TrackedColor : NameColor);

	if (Info)
	{
		Info->TextItemControl()->SetText(InfoText ? InfoText : "");
	}
	if (Background)
	{
		Background->Show(Tracked);
	}
}

bool UIPersonalSpotItem::OnMouseAction(float x, float y, EUIMessages mouse_action)
{
	if (inherited::OnMouseAction(x, y, mouse_action))
	{
		return true;
	}

	if (!Owner)
	{
		return false;
	}

	if (mouse_action == WINDOW_LBUTTON_DB_CLICK)
	{
		Owner->OnSpotRename(SpotId);
		return true;
	}
	if (mouse_action == WINDOW_LBUTTON_DOWN)
	{
		Owner->OnSpotFocus(SpotId);
		return true;
	}
	return false;
}

void UIPersonalSpotItem::SendMessage(CUIWindow* pWnd, s16 msg, void* pData)
{
	if (Owner && msg == BUTTON_DOWN)
	{
		if (TrackButton && pWnd == TrackButton)
		{
			Owner->OnSpotTrack(SpotId);
			return;
		}
		if (RemoveButton && pWnd == RemoveButton)
		{
			Owner->OnSpotRemove(SpotId);
			return;
		}
	}
	inherited::SendMessage(pWnd, msg, pData);
}

void UIPersonalSpotsWnd::InitFromXml(CUIXml& Xml, const char* Path)
{
	CUIXmlInit::InitWindow(Xml, Path, 0, this);

	XML_NODE* StoredRoot = Xml.GetLocalRoot();
	Xml.SetLocalRoot(Xml.NavigateToNode(Path, 0));

	Frame = UIHelper::CreateFrameWindow(Xml, "background_frame", this);
	Caption = UIHelper::CreateStatic(Xml, "t_caption", this);
	if (Xml.NavigateToNode("t_counter", 0))
	{
		Counter = UIHelper::CreateStatic(Xml, "t_counter", this);
	}
	CloseButton = UIHelper::Create3tButton(Xml, "btn_close", this);

	List = new CUIScrollView();
	List->SetAutoDelete(true);
	AttachChild(List);
	CUIXmlInit::InitScrollView(Xml, "spot_list", 0, List);
	DistanceFormat = Xml.ReadAttrib("spot_list", 0, "distance_format", "");

	if (Xml.NavigateToNode("t_empty", 0))
	{
		EmptyText = UIHelper::CreateStatic(Xml, "t_empty", this);
		EmptyText->Show(false);
	}
	if (Xml.NavigateToNode("t_message", 0))
	{
		MessageText = UIHelper::CreateStatic(Xml, "t_message", this);
		LimitMessage = Xml.ReadAttrib("t_message", 0, "text_limit", "");
		MessageText->Show(false);
	}
	if (Xml.NavigateToNode("t_tip", 0))
	{
		TipText = UIHelper::CreateStatic(Xml, "t_tip", this);
	}
	CUIHotkeysHint::CreateAll(Xml, this, HotkeysHints);

	Xml.SetLocalRoot(StoredRoot);
	ItemPath = xr_string(Path) + ":spot_item";
}

void UIPersonalSpotsWnd::Show(bool status)
{
	inherited::Show(status);
	if (status)
	{
		Rebuild();
		RefreshHints();
	}
}

void UIPersonalSpotsWnd::Update()
{
	inherited::Update();
	if (!IsShown())
	{
		return;
	}

	if (MessageText && MessageText->IsShown() && Device.dwTimeGlobal >= MessageHideTime)
	{
		MessageText->Show(false);
	}

	if (RebuildPending || HasSpotSetChanged())
	{
		Rebuild();
		return;
	}

	if (Device.dwTimeGlobal >= NextRefreshTime)
	{
		RefreshItems();
	}
}

void UIPersonalSpotsWnd::SendMessage(CUIWindow* pWnd, s16 msg, void* pData)
{
	if (pWnd == CloseButton && msg == BUTTON_DOWN)
	{
		if (UiSounds)
		{
			UiSounds->Play(EPdaUiSound::PanelClose);
		}
		Show(false);
		return;
	}
	inherited::SendMessage(pWnd, msg, pData);
}

void UIPersonalSpotsWnd::ShowLimitMessage()
{
	if (UiSounds)
	{
		UiSounds->Play(EPdaUiSound::Cancel);
	}

	if (!MessageText || !g_pGameLevel)
	{
		return;
	}

	string512 Text;
	xr_sprintf(Text, g_pStringTable->translate(LimitMessage).c_str(), Level().MapManager().GetPersonalSpotLimit());
	MessageText->TextItemControl()->SetText(Text);
	MessageText->Show(true);
	MessageHideTime = Device.dwTimeGlobal + PersonalSpotsWnd::MessageDurationMs;
}

void UIPersonalSpotsWnd::RefreshHints()
{
	for (CUIHotkeysHint* Hint : HotkeysHints)
	{
		Hint->Refresh();
	}
}

void UIPersonalSpotsWnd::OnSpotFocus(ALife::_OBJECT_ID Id)
{
	CMapLocation* Location = Level().MapManager().FindPersonalSpot(Id);
	if (!Location || !MapWnd)
	{
		return;
	}

	const Fvector2 Position = Location->CalcPosition();
	MapWnd->SetTargetMap(Location->GetLevelName(), Position, true);
	if (UiSounds)
	{
		UiSounds->Play(EPdaUiSound::ListSelect);
	}
}

void UIPersonalSpotsWnd::OnSpotRename(ALife::_OBJECT_ID Id)
{
	CMapLocation* Location = Level().MapManager().FindPersonalSpot(Id);
	if (!Location || !MapWnd)
	{
		return;
	}
	MapWnd->ShowSettingsWindow(Id, Location->GetLastPosition(), Location->GetLevelName());
}

void UIPersonalSpotsWnd::OnSpotTrack(ALife::_OBJECT_ID Id)
{
	CMapManager& MapManager = Level().MapManager();
	CMapLocation* Location = MapManager.FindPersonalSpot(Id);
	if (!Location)
	{
		return;
	}

	if (MapManager.IsUserNavigationLocation(Location))
	{
		MapManager.ClearActiveUserNavigationLocation();
	}
	else
	{
		MapManager.SetActiveUserNavigationLocation(Location);
	}

	if (UiSounds)
	{
		UiSounds->Play(EPdaUiSound::Confirm);
	}
	RefreshItems();
}

void UIPersonalSpotsWnd::OnSpotRemove(ALife::_OBJECT_ID Id)
{
	CMapManager& MapManager = Level().MapManager();
	if (!MapManager.RemovePersonalSpot(MapManager.FindPersonalSpot(Id)))
	{
		return;
	}

	if (UiSounds)
	{
		UiSounds->Play(EPdaUiSound::Cancel);
	}
	RebuildPending = true;
}

void UIPersonalSpotsWnd::Rebuild()
{
	RebuildPending = false;
	List->Clear();
	ShownIds.clear();

	if (!g_pGameLevel)
	{
		return;
	}

	CMapManager& MapManager = Level().MapManager();
	xr_vector<CMapLocation*> Spots;
	MapManager.GetUserLocations(Spots);
	PersonalSpotsWnd::SortSpots(Spots, Level().name());

	if (!Spots.empty())
	{
		CUIXml Xml;
		Xml.Load(CONFIG_PATH, UI_PATH, PDA_TASK_XML);
		for (CMapLocation* Location : Spots)
		{
			UIPersonalSpotItem* Item = new UIPersonalSpotItem();
			Item->InitFromXml(Xml, ItemPath.c_str(), this, Location->ObjectID());
			List->AddWindow(Item, true);
			ShownIds.push_back(Location->ObjectID());
		}
	}

	std::sort(ShownIds.begin(), ShownIds.end());
	RefreshItems();
	UpdateCounter(static_cast<u32>(Spots.size()));
	if (EmptyText)
	{
		EmptyText->Show(Spots.empty());
	}
}

void UIPersonalSpotsWnd::RefreshItems()
{
	NextRefreshTime = Device.dwTimeGlobal + PersonalSpotsWnd::RefreshIntervalMs;
	if (!g_pGameLevel)
	{
		return;
	}

	CMapManager& MapManager = Level().MapManager();
	for (CUIWindow* Window : List->Items())
	{
		UIPersonalSpotItem* Item = smart_cast<UIPersonalSpotItem*>(Window);
		if (!Item)
		{
			continue;
		}

		CMapLocation* Location = MapManager.FindPersonalSpot(Item->GetSpotId());
		if (!Location)
		{
			RebuildPending = true;
			return;
		}
		Item->Refresh(*Location, MapManager.IsUserNavigationLocation(Location), MakeInfoText(*Location).c_str());
	}
}

void UIPersonalSpotsWnd::UpdateCounter(u32 Count)
{
	if (!Counter || !g_pGameLevel)
	{
		return;
	}

	string64 Text;
	const u32 Limit = Level().MapManager().GetPersonalSpotLimit();
	if (Limit > 0)
	{
		xr_sprintf(Text, "%u / %u", Count, Limit);
	}
	else
	{
		xr_sprintf(Text, "%u", Count);
	}
	Counter->TextItemControl()->SetText(Text);
}

bool UIPersonalSpotsWnd::HasSpotSetChanged() const
{
	if (!g_pGameLevel)
	{
		return false;
	}

	xr_vector<CMapLocation*> Spots;
	Level().MapManager().GetUserLocations(Spots);
	if (Spots.size() != ShownIds.size())
	{
		return true;
	}

	for (size_t i = 0; i < Spots.size(); ++i)
	{
		if (Spots[i]->ObjectID() != ShownIds[i])
		{
			return true;
		}
	}
	return false;
}

xr_string UIPersonalSpotsWnd::MakeInfoText(CMapLocation& Location) const
{
	const shared_str& LevelName = Location.GetLevelName();
	xr_string Result = LevelName.size() ? g_pStringTable->translate(LevelName).c_str() : "";

	CObject* Actor = Level().CurrentEntity();
	if (!Actor || LevelName != Level().name() || !DistanceFormat.size())
	{
		return Result;
	}

	const Fvector& ActorPosition = Actor->Position();
	const Fvector SpotPosition = Location.GetLastPosition();
	const float Distance = _sqrt(_sqr(ActorPosition.x - SpotPosition.x) + _sqr(ActorPosition.z - SpotPosition.z));

	string256 Text;
	xr_sprintf(Text, g_pStringTable->translate(DistanceFormat).c_str(), Result.c_str(), iFloor(Distance));
	return Text;
}
