#pragma once

#include "../../xrUI/Widgets/UIWindow.h"
#include "../../xrUI/Widgets/UIStatic.h"
#include "../../xrUI/Widgets/UIWndCallback.h"
#include "../../xrUI/xrUIXmlParser.h"
#include "../../xrEngine/xr_level_controller.h"
#include "PdaRecords.h"

class CUIScrollView;
class CUITabControl;
class CUIStatic;
class CUI3tButton;
class CUIHotkeysHint;
class CPdaUiSounds;
class CUIPdaRecordsWnd;

class UIPdaRecordItem final : public CUIWindow
{
	using inherited = CUIWindow;

public:
	void InitFromXml(CUIXml& Xml, const char* Path, CUIPdaRecordsWnd* Owner, const char* Category);
	void Setup(const CPdaRecordEntry& Entry, u32 TitleColor);
	void SetSelected(bool Selected);
	const shared_str& GetId() const { return Id; }

	bool OnMouseAction(float x, float y, EUIMessages mouse_action) override;
	void SendMessage(CUIWindow* pWnd, s16 msg, void* pData) override;
	CUIWindow* ui_cast_window() override { return this; }

private:
	CUIPdaRecordsWnd* Owner = nullptr;
	shared_str Id;

	CUIStatic* Selection = nullptr;
	CUIStatic* NewMark = nullptr;
	CUIStatic* Icon = nullptr;
	CUIStatic* Title = nullptr;
	CUIStatic* Subtitle = nullptr;
	CUI3tButton* FocusButton = nullptr;
	Frect IconLayout = {};
	bool IconKeepAspect = false;
	float BaseHeight = 0.f;
	float Padding = 8.f;
};

class UIPdaRecordGroupHeader final : public CUIStatic
{
	using inherited = CUIStatic;

public:
	void SetOwner(CUIPdaRecordsWnd* OwnerWnd, const shared_str& GroupKey)
	{
		Owner = OwnerWnd;
		Key = GroupKey;
	}

	bool OnMouseAction(float x, float y, EUIMessages mouse_action) override;

private:
	CUIPdaRecordsWnd* Owner = nullptr;
	shared_str Key;
};

class CUIPdaRecordsWnd final : public CUIWindow, public CUIWndCallback
{
	using inherited = CUIWindow;

public:
	using ShowOnMapCallback = xr_delegate<void(const shared_str&, const Fvector&)>;
	using TrackCallback = xr_delegate<void(const shared_str&, const Fvector&, const char*)>;

	void Init(const char* XmlName = "pda_records.xml");
	void SetUiSounds(CPdaUiSounds* Sounds) { UiSounds = Sounds; }
	void SetShowOnMapCallback(const ShowOnMapCallback& Callback) { OnShowOnMap = Callback; }
	void SetTrackCallback(const TrackCallback& Callback) { OnTrack = Callback; }
	bool SwitchCategory(bool Next);

	void Show(bool status) override;
	void Update() override;
	void SendMessage(CUIWindow* pWnd, s16 msg, void* pData) override;
	bool OnKeyboardAction(int dik, EUIMessages keyboard_action) override;
	bool OnGamepadKeyAction(int key, EUIMessages gamepad_action) override;
	CUIWindow* ui_cast_window() override { return this; }

	void OnItemSelected(UIPdaRecordItem* Item);
	void OnItemActivated(UIPdaRecordItem* Item);
	void ToggleGroup(const shared_str& GroupKey);

private:
	struct SCategory
	{
		shared_str Id;
		xr_string BaseText;
		CPdaRecordList List;
	};

	void InitDetails();
	CUIHotkeysHint* CreateHint(const char* Path, CUIWindow* Parent, int Index = 0);
	void RefreshHints();
	bool HandleAction(EGameActions Action);
	void RefreshAll();
	void CollectCategory(SCategory& Category);
	void UpdateTabCounters();
	void RebuildList();
	void AddGroupHeader(const CPdaRecordEntry& Entry, bool Collapsed);
	shared_str GroupStateKey(const shared_str& GroupKey);
	void UpdateEmptyText(bool Empty);
	void SelectEntry(const shared_str& Id, bool Notify);
	void MoveSelection(bool Next);
	void UpdateDetails();
	void UpdateImage(const CPdaRecordEntry* Entry);
	shared_str ResolveImage(const CPdaRecordEntry& Entry) const;
	CUIStatic* AddDetailsLine(const shared_str& Style, const char* Text);
	u32 StateColor(const shared_str& State, bool IsNew);
	void ApplyPendingActions();
	SCategory* ActiveCategory();
	const CPdaRecordEntry* SelectedEntry();
	void CallScript(const shared_str& Function, const char* Category, const char* Id, const char* Action);
	void OnShowOnMapClicked(CUIWindow* w, void* d);
	void OnTrackClicked(CUIWindow* w, void* d);
	void OnActionClicked(CUIWindow* w, void* d);

	CUIXml Xml;
	CPdaUiSounds* UiSounds = nullptr;
	ShowOnMapCallback OnShowOnMap;
	TrackCallback OnTrack;

	shared_str ProviderFunction;
	shared_str SelectFunction;
	shared_str ActionFunction;
	u32 RefreshIntervalMs = 2000;
	bool TabCounters = false;

	CUITabControl* CategoryTabs = nullptr;
	xr_vector<SCategory> Categories;
	u32 ActiveCategoryIndex = 0;
	xr_vector<CUIHotkeysHint*> Hints;
	bool HintsControllerMode = false;

	CUIScrollView* List = nullptr;
	CUIStatic* EmptyText = nullptr;
	shared_str EmptyTextDefault;
	bool GroupHeaders = false;
	bool GroupCollapsible = false;
	xr_string GroupPrefixOpen;
	xr_string GroupPrefixClosed;
	xr_set<shared_str> CollapsedGroups;
	xr_map<shared_str, u32> StateColors;
	u32 TitleColorDefault = 0xFFFFFFFF;

	CUIWindow* Details = nullptr;
	CUIStatic* DetailsEmpty = nullptr;
	CUIStatic* DetailsImage = nullptr;
	CUIStatic* DetailsIcon = nullptr;
	Frect DetailsIconBase = {};
	float DetailsIconGap = 6.f;
	bool DetailsIconKeepAspect = false;
	bool DetailsIconPushBody = true;
	bool DetailsAutoHeading = true;
	CUIStatic* DetailsTitle = nullptr;
	CUIStatic* DetailsSubtitle = nullptr;
	CUIScrollView* DetailsBody = nullptr;
	CUI3tButton* BtnShowOnMap = nullptr;
	CUI3tButton* BtnTrack = nullptr;
	CUI3tButton* BtnAction = nullptr;
	xr_string BtnActionDefaultText;
	Fvector2 BodyBasePos = {};
	float BodyBaseHeight = 0.f;
	Frect ImageCropRect = {};
	bool ImageCrop = false;
	bool ImageLevelIntro = false;
	shared_str ImageDefault;

	shared_str SelectedId;
	u32 ListHash = 0;
	u32 NextRefreshTime = 0;
	bool NeedRefresh = true;
	bool NeedRebuild = true;
	bool PendingShowOnMap = false;
	bool PendingTrack = false;
	bool PendingAction = false;
	shared_str PendingSelectNotify;
};
