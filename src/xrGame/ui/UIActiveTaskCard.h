#pragma once
#include "../../xrUI/Widgets/UIWindow.h"

class CUIXml;
class CUIStatic;
class CUIFrameWindow;
class CUI3tButton;
class CUITaskWnd;
class CMapLocation;
class CGameTask;

class UIActiveTaskCard final : public CUIWindow
{
	using inherited = CUIWindow;

public:
	void InitFromXml(CUIXml& Xml, const char* Path);
	void SetOwner(CUITaskWnd* Wnd) { Owner = Wnd; }
	void Tick(bool TaskListOpened);
	void Refresh();

	virtual bool OnMouseAction(float x, float y, EUIMessages mouse_action) override;
	virtual void SendMessage(CUIWindow* pWnd, s16 msg, void* pData) override;
	virtual CUIWindow* ui_cast_window() override { return this; }

private:
	void SetText(CUIStatic* Static, const char* Text);
	void SetIcon(CGameTask* Task, const shared_str& Fallback);
	void UpdateHeader();
	void UpdateLayout();
	bool HasHeader() const { return HeaderText || ListButton; }
	xr_string MakeInfoText(CMapLocation* Location) const;

	CUITaskWnd* Owner = nullptr;

	CUIFrameWindow* Frame = nullptr;
	CUIStatic* Background = nullptr;
	CUIFrameWindow* HeaderFrame = nullptr;
	CUIStatic* HeaderIcon = nullptr;
	CUIStatic* HeaderText = nullptr;
	CUI3tButton* ListButton = nullptr;

	CUIFrameWindow* BodyFrame = nullptr;
	CUIStatic* TaskIcon = nullptr;
	CUIStatic* TypeText = nullptr;
	CUIStatic* TitleText = nullptr;
	CUIStatic* ObjectiveText = nullptr;
	CUIStatic* InfoText = nullptr;
	CUI3tButton* FocusButton = nullptr;

	shared_str TextStory;
	shared_str TextSide;
	shared_str TextMark;
	shared_str TextNone;
	shared_str DistanceFormat;
	shared_str HeaderTextId;
	shared_str ButtonTextShow;
	shared_str ButtonTextHide;
	xr_string ButtonFormat;
	shared_str IconStory;
	shared_str IconSide;
	shared_str IconMark;
	float FullHeight = 0.0f;
	float HeaderHeight = 0.0f;
	bool HideWithTaskList = true;
	bool HeaderCounter = false;
	bool HasContent = false;
	bool HasTarget = false;
	bool TaskListShown = false;
	u32 TaskCount = 0;
	u32 NextRefreshTime = 0;
};
