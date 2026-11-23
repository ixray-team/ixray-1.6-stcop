#pragma once

#include "../../xrUI/Widgets/UIWindow.h"
#include "../../xrUI/Widgets/UIWndCallback.h"
#include "../../xrUI/xrUIXmlParser.h"
#include "../../xrEngine/xr_level_controller.h"
#include "UISecondTaskWnd.h"

class CUIScrollView;
class CUITabControl;
class CUIStatic;
class CUI3tButton;
class CUIHotkeysHint;
class CGameTask;
class CPdaUiSounds;
class CUITaskJournalWnd;

class UITaskJournalItem final : public CUIWindow
{
	using inherited = CUIWindow;

public:
	void InitFromXml(CUIXml& Xml, const char* Path, CUITaskJournalWnd* Owner);
	void Setup(CGameTask* Task, bool Active, const char* TimeText);
	void SetSelected(bool Selected);
	CGameTask* GetTask() const { return Task; }

	bool OnMouseAction(float x, float y, EUIMessages mouse_action) override;
	void SendMessage(CUIWindow* pWnd, s16 msg, void* pData) override;
	CUIWindow* ui_cast_window() override { return this; }

private:
	CUITaskJournalWnd* Owner = nullptr;
	CGameTask* Task = nullptr;

	CUIWindow* Selection = nullptr;
	CUIStatic* Icon = nullptr;
	CUIStatic* TypeIcon = nullptr;
	CUIStatic* Title = nullptr;
	CUIStatic* Status = nullptr;
	CUIStatic* Time = nullptr;
	CUIStatic* ActiveMark = nullptr;
	CUI3tButton* FocusButton = nullptr;

	shared_str TypeIconStory;
	shared_str TypeIconSide;
	shared_str StatusCompleted;
	shared_str StatusFailed;
	u32 ColorNormal = 0xFFFFFFFF;
	u32 ColorActive = 0xFFFFFFFF;
	u32 ColorUnread = 0xFFFFFFFF;
	u32 ColorCompleted = 0xFFFFFFFF;
	u32 ColorFailed = 0xFFFFFFFF;
	float BaseHeight = 0.f;
	float Padding = 8.f;
};

class CUITaskJournalWnd final : public CUIWindow, public CUIWndCallback
{
	using inherited = CUIWindow;

public:
	using ShowOnMapCallback = xr_delegate<void(CGameTask*)>;

	void Init();
	void SetUiSounds(CPdaUiSounds* Sounds) { UiSounds = Sounds; }
	void SetShowOnMapCallback(const ShowOnMapCallback& Callback) { OnShowOnMap = Callback; }
	bool SwitchFilter(bool Next);
	bool ReplacesTaskList() const { return ReplaceTaskList; }

	void Show(bool status) override;
	void Update() override;
	void SendMessage(CUIWindow* pWnd, s16 msg, void* pData) override;
	bool OnKeyboardAction(int dik, EUIMessages keyboard_action) override;
	bool OnGamepadKeyAction(int key, EUIMessages gamepad_action) override;
	CUIWindow* ui_cast_window() override { return this; }

	void OnItemSelected(UITaskJournalItem* Item);
	void OnItemActivated(UITaskJournalItem* Item);

private:
	struct STabText
	{
		shared_str Id;
		xr_string BaseText;
	};

	void InitDetails();
	CUIHotkeysHint* CreateHint(const char* Path, CUIWindow* Parent, int Index = 0);
	void RefreshHints();
	bool HandleAction(EGameActions Action);
	bool HandleLayerAction(EGameActions Action, int Dik);
	void RebuildList();
	void SortTasks(xr_vector<CGameTask*>& Tasks) const;
	void AddGroupHeader(bool Story);
	void UpdateTabCounters();
	void UpdateEmptyText(bool Empty);
	void SelectTask(CGameTask* Task);
	void MoveSelection(bool Next);
	void UpdateDetails();
	void UpdateImage(CGameTask* Task);
	shared_str ResolveImage(CGameTask* Task) const;
	void ApplyPendingActions();
	CGameTask* FindTask(const shared_str& Id) const;
	bool IsActiveTask(const CGameTask* Task) const;
	u32 TasksSignature() const;
	xr_string FormatTime(ALife::_TIME_ID Time) const;
	xr_string FormatLine(const shared_str& FormatId, const char* Value) const;
	CUIStatic* AddDetailsText(const char* Path, const char* Text);
	void OnShowOnMapClicked(CUIWindow* w, void* d);
	void OnActivateClicked(CUIWindow* w, void* d);

	CUIXml Xml;
	CPdaUiSounds* UiSounds = nullptr;
	ShowOnMapCallback OnShowOnMap;

	CUITabControl* FilterTabs = nullptr;
	xr_vector<STabText> TabTexts;
	ETaskListFilter Filter = ETaskListFilter::Active;
	xr_vector<CUIHotkeysHint*> Hints;
	CHotkeyContextScope HotkeyContext;
	bool HintsControllerMode = false;

	CUIScrollView* List = nullptr;
	CUIStatic* EmptyText = nullptr;
	shared_str EmptyTextDefault;
	shared_str EmptyTexts[static_cast<u32>(ETaskListFilter::Count)];
	bool GroupHeaders = false;
	shared_str GroupTextStory;
	shared_str GroupTextSide;

	CUIWindow* Details = nullptr;
	CUIStatic* DetailsEmpty = nullptr;
	CUIStatic* DetailsImage = nullptr;
	CUIStatic* DetailsIcon = nullptr;
	CUIStatic* DetailsTitle = nullptr;
	CUIStatic* DetailsType = nullptr;
	CUIStatic* DetailsStatus = nullptr;
	CUIScrollView* DetailsBody = nullptr;
	CUI3tButton* BtnShowOnMap = nullptr;
	CUI3tButton* BtnActivate = nullptr;
	Fvector2 BodyBasePos = {};
	float BodyBaseHeight = 0.f;
	Frect ImageCropRect = {};
	bool ImageCrop = false;

	shared_str TextStory;
	shared_str TextSide;
	shared_str TextInProgress;
	shared_str TextActive;
	shared_str TextCompleted;
	shared_str TextFailed;
	shared_str FormatReceived;
	shared_str FormatFinished;
	shared_str FormatLocation;
	shared_str ObjectivesCaption;
	bool HeadingFromDescription = false;
	shared_str ImageSection;
	shared_str ImageDefault;
	bool ImageLevelIntro = false;
	u32 ColorObjectiveProgress = 0xFFFFFFFF;
	u32 ColorObjectiveCompleted = 0xFFFFFFFF;
	u32 ColorObjectiveFailed = 0xFFFFFFFF;

	shared_str SelectedId;
	shared_str PendingShowOnMap;
	shared_str PendingActivate;
	u32 Signature = 0;
	u32 NextRefreshTime = 0;
	bool NeedRebuild = true;
	bool ReplaceTaskList = false;
};
