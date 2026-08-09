#pragma once
#include "FolderLib.h"

class XREPROPS_API UIItemListForm : 
	public IEditorWnd, 
	protected FolderHelper<ListItem, true>
{
public:
	using Node = FolderHelper<ListItem, true>::Node;
	DECLARE_XR_DELEGATE(OnItemRename, void, Node&, const char*, const char*, EItemType);
	DECLARE_XR_DELEGATE(OnItemRemove, void, Node& Node);
	DECLARE_XR_DELEGATE(OnItemPreRemove, bool, Node& Node);
	DECLARE_XR_DELEGATE(OnILItemsFocused, void, ListItemsVec&);
	DECLARE_XR_DELEGATE(OnILItemFocused, void, ListItem*);
	DECLARE_XR_DELEGATE(OnItemCreate, void, const char*);
	DECLARE_XR_DELEGATE(OnItemClone, void, const char*, const char*);
	DECLARE_XR_DELEGATE(VerifyItem, bool, Node*);
	DECLARE_XR_DELEGATE(GetItemMoveActionSlot, ENodeMoveActionSlot, Node*);
	DECLARE_XR_DELEGATE(OnMoveItem, bool, Node*);

private:
	TOnILItemsFocused OnItemsFocusedEvent;
	TOnILItemFocused OnItemFocusedEvent;
	TOnILItemFocused OnItemUnfocusedEvent;
	TOnItemPreRemove OnItemPreRemoveEvent;
	TOnItemRemove OnItemRemoveEvent;
	TVerifyItem VerifyItemRename;
	TVerifyItem VerifyItemMove;
	TOnItemRename OnItemRenameEvent;
	TVerifyItem VerifyItemCreate;
	TVerifyItem VerifyFolderCreate;
	TOnItemCreate OnItemCreateEvent;
	TVerifyItem VerifyItemClone;
	TOnItemClone OnItemCloneEvent;
	TGetItemMoveActionSlot GetItemMoveActionSlot;
	xr_map<ENodeMoveActionSlot, TOnMoveItem> ItemMoveActionSlots = {
		{ENodeMoveActionSlot::Default, {this, &UIItemListForm::ItemMoveActionDefault}}
	};

public:
	UIItemListForm();
	virtual ~UIItemListForm();

public:
	virtual void           Draw();
	void                   ClearList();
	void                   RemoveSelectItem();
	void                   ClearSelected();
	void                   SelectItem(const char* name, bool ClearOld = true);
	void                   AssignItems(ListItemsVec& items, const char* name_selection = nullptr, bool clear_Folder = true, bool save_selected = false);
	IC const ListItemsVec& GetItems() const
	{
		return m_Items;
	}
	bool GetSelected(RStringVec& items) const;
	int  GetSelected(const char* pref, ListItemsVec& items, bool bOnlyObject);

public:
	enum
	{
		fMenuEdit = (1 << 0),
		fMultiSelect = (1 << 1),
	};
	Flags32 m_Flags;

private:

	virtual bool IsNodeTrueFolder(Node& node) override
	{
		if (node.Object && node.Object->m_Object)
		{
			return false;
		}
		return node.IsFolder();
	}
	
	void       DrawMenuEdit();
	string4096 m_edit_name;
	string4096 m_edit_path;
	Node* m_edit_node;

	bool ItemMoveActionDefault(Node* Node);

public:
	IC void SetOnItemsFocusedEvent(TOnILItemsFocused e)
	{
		OnItemsFocusedEvent = e;
	}
	IC void SetOnItemFocusedEvent(TOnILItemFocused e)
	{
		OnItemFocusedEvent = e;
	}
	IC void SetOnItemUnfocusedEvent(TOnILItemFocused e)
	{
		OnItemUnfocusedEvent = e;
	}
	IC void SetOnItemPreRemoveEvent(TOnItemPreRemove e)
	{
		OnItemPreRemoveEvent = e;
	}
	IC void SetOnItemRemoveEvent(TOnItemRemove e)
	{
		OnItemRemoveEvent = e;
	}
	IC void SetOnItemRenameEvent(TOnItemRename e)
	{
		OnItemRenameEvent = e;
	}
	IC void SetVerifyItemCreate(TVerifyItem e)
	{
		VerifyItemCreate = e;
	}
	IC void SetVerifyFolderCreate(TVerifyItem e)
	{
		VerifyFolderCreate = e;
	}
	IC void SetOnItemCreaetEvent(TOnItemCreate e)
	{
		OnItemCreateEvent = e;
	}
	IC void SetVerifyItemRename(TVerifyItem e)
	{
		VerifyItemRename = e;
	}
	IC void SetVerifyItemMove(TVerifyItem e)
	{
		VerifyItemMove = e;
	}
	IC void SetVerifyItemClone(TVerifyItem e)
	{
		VerifyItemClone = e;
	}
	IC void SetOnItemCloneEvent(TOnItemClone e)
	{
		OnItemCloneEvent = e;
	}
	IC void SetGetItemMoveActionSlot(TGetItemMoveActionSlot e)
	{
		GetItemMoveActionSlot = e;
	}
	IC void SetOnMoveItemEvent(ENodeMoveActionSlot Slot, TOnMoveItem e)
	{
		R_ASSERT(ItemMoveActionSlots.find(Slot) == ItemMoveActionSlots.end());
		ItemMoveActionSlots[Slot] = e;
	}
	IC void SetFilter(const char* filter)
	{
		m_Filter = filter ? filter : "";
	}

private:
	virtual void DrawAfterFolderNode(bool is_open, Node* Node = 0);
	virtual void DrawItem(Node* Node);
	virtual void DrawNode(Node* N) override;
	virtual bool IsDrawFolder(Node* Node);
	virtual void IsItemClicked(Node* Node);
	virtual bool IsFolderBullet(Node* Node);
	virtual bool IsFolderSelected(Node* Node);

	bool VerifyItemCloneFunc(UIItemListForm::Node* Node);
	bool VerifyItemCreateFunc(UIItemListForm::Node* Node);
	bool VerifyFolderCreateFunc(UIItemListForm::Node* Node);
	bool VerifyItemRenameFunc(UIItemListForm::Node* Node);
	bool VerifyItemMoveFunc(UIItemListForm::Node* Node);
	virtual void EventRenameNode(Node* Node, const char* old_path, const char* new_path) override;
	virtual void EventRemoveNode(Node* Node, const char* path) override;
	virtual bool EventPreRemoveNode(Node* Node) override;

	void ResetAutoExpand(Node* N);
	bool SetAutoExpandForFilter(Node* N);

public:
	Node         m_GeneralNode;
	ListItemsVec m_Items;
	ListItemsVec m_SelectedItems;
	void         ClearSelectedItems();
	bool         m_UseMenuEdit;
	void         ClearObject(Node* Node);
	xr_string    m_Filter;
};