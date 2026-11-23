#pragma once

#include "../../xrUI/Widgets/UIWindow.h"
#include "../../xrUI/Widgets/UIStatic.h"
#include "../../xrUI/xrUIXmlParser.h"
#include "../EncyclopediaCatalog.h"

class CUI3dStatic;
class CUIScrollView;
class CUIEncyclopediaCatalogWnd;

class UIEncyclopediaCatalogItem final : public CUIWindow
{
	using inherited = CUIWindow;

public:
	void InitFromXml(CUIXml& Xml, const char* Path, CUIEncyclopediaCatalogWnd* OwnerWnd);
	void Setup(const SEncyclopediaEntry& Entry, u32 TitleColor);
	void SetSelected(bool Selected);
	void SetNew(bool IsNew, u32 TitleColor);
	bool OnMouseAction(float X, float Y, EUIMessages MouseAction) override;

	shared_str Key;

private:
	CUIEncyclopediaCatalogWnd* Owner = nullptr;
	CUIWindow* Selection = nullptr;
	CUIStatic* NewMark = nullptr;
	CUIStatic* Icon = nullptr;
	CUIStatic* Title = nullptr;
	Frect IconLayout;
	bool IconKeepAspect = false;
	float BaseHeight = 0.f;
	float Padding = 6.f;
};

class UIEncyclopediaGroupHeader final : public CUIStatic
{
	using inherited = CUIStatic;

public:
	void SetOwner(CUIEncyclopediaCatalogWnd* OwnerWnd, const shared_str& GroupId)
	{
		Owner = OwnerWnd;
		Id = GroupId;
	}
	bool OnMouseAction(float X, float Y, EUIMessages MouseAction) override;

private:
	CUIEncyclopediaCatalogWnd* Owner = nullptr;
	shared_str Id;
};

class CUIEncyclopediaCatalogWnd final : public CUIWindow
{
	using inherited = CUIWindow;

public:
	void Init(const char* XmlName);
	void Show(bool Status) override;
	void Update() override;
	bool OnKeyboardAction(int Dik, EUIMessages KeyboardAction) override;

	void OnItemClicked(UIEncyclopediaCatalogItem* Item);
	void ToggleGroup(const shared_str& GroupId);

private:
	void Refresh();
	void RebuildList();
	void SelectEntry(const shared_str& EntryKey);
	void MoveSelection(bool Next);
	void UpdateDetails();
	void UpdateVisuals(const SEncyclopediaEntry* Entry);
	bool Apply3dIcon(const shared_str& Section);
	CUIStatic* AddLine(const shared_str& Style, const char* Text);
	SEncyclopediaEntry* FindEntry(const shared_str& EntryKey);
	u32 TitleColor(bool IsNew) const;

	CUIXml Xml;
	xr_vector<SEncyclopediaGroup> Groups;
	xr_vector<shared_str> CollapsedGroups;
	shared_str SelectedKey;
	bool NeedRebuild = false;

	CUIScrollView* List = nullptr;
	CUIStatic* EmptyText = nullptr;
	CUIWindow* Details = nullptr;
	CUIStatic* DetailsEmpty = nullptr;
	CUIStatic* DetailsSubtitle = nullptr;
	CUIStatic* DetailsImage = nullptr;
	CUIStatic* DetailsIcon = nullptr;
	CUI3dStatic* DetailsIcon3d = nullptr;
	CUIScrollView* DetailsBody = nullptr;

	Fvector2 BodyBasePos = {};
	float BodyBaseHeight = 0.f;
	Frect IconBase = {};
	float IconGap = 6.f;
	bool IconKeepAspect = true;
	bool IconPushBody = true;
	bool Use3dIcons = false;

	bool GroupCollapsible = false;
	bool GroupCounters = false;
	xr_string GroupPrefixOpen;
	xr_string GroupPrefixClosed;
	u32 ColorNormal = 0xFFD2D2D2;
	u32 ColorNew = 0xFFE1D75A;
};
