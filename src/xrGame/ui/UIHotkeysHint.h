#pragma once
#include "../../xrUI/Widgets/UIStatic.h"
#include "../../xrUI/xrUIXmlParser.h"

class CUIHotkeysHint final : public CUIStatic
{
	using inherited = CUIStatic;

public:
	static void CreateAll(CUIXml& Xml, CUIWindow* Parent, xr_vector<CUIHotkeysHint*>& Result);

	void InitFromXml(CUIXml& Xml, const char* Path, int Index = 0);
	void Refresh();
	void SetActiveContexts(const xr_vector<shared_str>& Contexts);
	void Update() override;

	virtual CUIWindow* ui_cast_window() override { return this; }

private:
	static constexpr u32 MaxKeysPerItem = 4;
	static constexpr u32 MaxAutoItems = 16;

	struct SHotkeyItem
	{
		shared_str Action;
		shared_str GamepadAction;
		shared_str FixedKey;
		shared_str Label;
		shared_str Context;
	};

	struct SResolvedItem
	{
		const SHotkeyItem* Source = nullptr;
		u32 Index = 0;
		xr_vector<xr_string> Keys;
	};

	struct SChip
	{
		CUIWindow* Back[MaxKeysPerItem] = {};
		CUIStatic* Key[MaxKeysPerItem] = {};
		CUIStatic* Label = nullptr;
	};

	void ReadLayout(CUIXml& Xml, XML_NODE* Root);
	void ReadItems(CUIXml& Xml, XML_NODE* Root);
	void RebuildAutoItems();
	bool IsContextActive(const shared_str& Context) const;
	void ResolveItems(xr_vector<SResolvedItem>& Result) const;

	xr_string BuildText(const xr_vector<SResolvedItem>& Items, size_t ItemCount) const;
	bool FitsWidth(const xr_string& Text) const;

	void InitChips(CUIXml& Xml, XML_NODE* TemplateRoot, u32 Count);
	void RefreshChips(const xr_vector<SResolvedItem>& Items);
	void HideChips();
	float ChipKeyWidth(CUIStatic& Key, const xr_string& Text) const;
	float ChipLabelWidth(CUIStatic& Label, const char* Text) const;
	float MeasureItem(const SResolvedItem& Item) const;

	xr_vector<SHotkeyItem> HotkeyItems;
	xr_vector<SHotkeyItem> FixedItems;
	shared_str AutoContexts;
	u32 SeenRevision = u32(-1);
	bool SeenControllerMode = false;
	xr_vector<shared_str> ActiveContexts;
	xr_string Separator = " \xB7 ";
	xr_string LabelSeparator = " ";
	xr_string KeyFormat = "[%s]";
	xr_string GamepadKeyFormat = "%s";

	bool ChipMode = false;
	CUIWindow* ChipsLayer = nullptr;
	xr_vector<SChip> Chips;
	float KeyPadding = 8.0f;
	float KeyMinWidth = 0.0f;
	float KeySpacing = 4.0f;
	float LabelSpacing = 8.0f;
	float ItemSpacing = 20.0f;
	char ChipsAlign = 'l';
};
