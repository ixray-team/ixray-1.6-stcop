#pragma once
class XREPROPS_API UIItemListForm :public XrUI
{
	friend class UIItemListFormItem;
	TOnILItemsFocused	OnItemsFocusedEvent;
	TOnILItemFocused	OnItemFocusedEvent;
public:
	UIItemListForm();
	virtual ~UIItemListForm();
public:
	virtual void Draw();
	void ClearList();
	void ClearSelected();
	void SelectItem(const char* name);
	void AssignItems(ListItemsVec& items, const char* name_selection = nullptr, bool clear_floder = true, bool save_selected = false);
	IC const ListItemsVec& GetItems()const {return m_Items;}
	bool GetSelected(RStringVec& items)const;
	//int GetSelected(LPCSTR pref, ListItemsVec& items, bool bOnlyObject);???
public:
	IC void		SetOnItemsFocusedEvent(TOnILItemsFocused e) { OnItemsFocusedEvent = e; }
	IC void		SetOnItemFocusedEvent(TOnILItemFocused e) { OnItemFocusedEvent = e; }
public:
	enum {
		fMenuEdit =			(1<<0),
		fMultiSelect =		(1<<1),
	};
	Flags32 m_Flags;
private:
	void UpdateSelected(UIItemListFormItem* NewSelected);
	ListItemsVec m_Items;
	UIItemListFormItem m_RootItem;
	UIItemListFormItem* m_SelectedItem;
};

