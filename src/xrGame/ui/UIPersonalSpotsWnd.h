#pragma once
#include "../../xrUI/Widgets/UIWindow.h"

class CUIXml;
class CUIFrameWindow;
class CUIScrollView;
class CUIStatic;
class CUI3tButton;
class CUIMapWnd;
class CUIHotkeysHint;
class CPdaUiSounds;
class CMapLocation;
class UIPersonalSpotsWnd;

class UIPersonalSpotItem final : public CUIWindow
{
	using inherited = CUIWindow;

public:
	void InitFromXml(CUIXml& Xml, const char* Path, UIPersonalSpotsWnd* Panel, ALife::_OBJECT_ID Id);
	void Refresh(CMapLocation& Location, bool Tracked, const char* InfoText);
	ALife::_OBJECT_ID GetSpotId() const { return SpotId; }

	virtual bool OnMouseAction(float x, float y, EUIMessages mouse_action) override;
	virtual void SendMessage(CUIWindow* pWnd, s16 msg, void* pData) override;
	virtual CUIWindow* ui_cast_window() override { return this; }

private:
	UIPersonalSpotsWnd* Owner = nullptr;
	ALife::_OBJECT_ID SpotId = ALife::INVALID_OBJECT_ID;
	CUIStatic* Background = nullptr;
	CUIStatic* Name = nullptr;
	CUIStatic* Info = nullptr;
	CUI3tButton* TrackButton = nullptr;
	CUI3tButton* RemoveButton = nullptr;
	u32 NameColor = u32(-1);
	u32 TrackedColor = u32(-1);
};

class UIPersonalSpotsWnd final : public CUIWindow
{
	using inherited = CUIWindow;

public:
	void InitFromXml(CUIXml& Xml, const char* Path);
	void SetMapWnd(CUIMapWnd* Wnd) { MapWnd = Wnd; }
	void SetUiSounds(CPdaUiSounds* Sounds) { UiSounds = Sounds; }

	virtual void Show(bool status) override;
	virtual void Update() override;
	virtual void SendMessage(CUIWindow* pWnd, s16 msg, void* pData) override;
	virtual CUIWindow* ui_cast_window() override { return this; }

	void ShowLimitMessage();
	void RefreshHints();

	void OnSpotFocus(ALife::_OBJECT_ID Id);
	void OnSpotRename(ALife::_OBJECT_ID Id);
	void OnSpotTrack(ALife::_OBJECT_ID Id);
	void OnSpotRemove(ALife::_OBJECT_ID Id);

private:
	void Rebuild();
	void RefreshItems();
	void UpdateCounter(u32 Count);
	bool HasSpotSetChanged() const;
	xr_string MakeInfoText(CMapLocation& Location) const;

	CUIMapWnd* MapWnd = nullptr;
	CPdaUiSounds* UiSounds = nullptr;
	CUIFrameWindow* Frame = nullptr;
	CUIStatic* Caption = nullptr;
	CUIStatic* Counter = nullptr;
	CUI3tButton* CloseButton = nullptr;
	CUIScrollView* List = nullptr;
	CUIStatic* EmptyText = nullptr;
	CUIStatic* MessageText = nullptr;
	CUIStatic* TipText = nullptr;
	xr_vector<CUIHotkeysHint*> HotkeysHints;

	xr_string ItemPath;
	shared_str DistanceFormat;
	shared_str LimitMessage;
	xr_vector<ALife::_OBJECT_ID> ShownIds;
	bool RebuildPending = false;
	u32 NextRefreshTime = 0;
	u32 MessageHideTime = 0;
};
