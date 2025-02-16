#ifndef __UI_TREE_VIEW_BOX_ITEM_H__
#define __UI_TREE_VIEW_BOX_ITEM_H__

#pragma once

#include "uilistboxitemex.h"

class CUIListBox;

class CUITreeViewBoxItem : public CUIListBoxItem
{
	typedef CUIListBoxItem inherited;
	
	bool			isRoot;

	int				m_value;
	
	bool			isOpened;
	int				iTextShift;

	CUITreeViewBoxItem *pOwner;
public:
	void			SetRoot(bool set);
	bool			IsRoot() const						{ return isRoot; }

	int				GetArticleValue() const					{ return m_value; }
	void			SetArticleValue(int val)					{ m_value = val; }

	void			SetTextShift(int delta)				{ iTextShift += delta; }

	virtual bool	OnMouseDown							(int mouse_btn);
	virtual void	OnFocusReceive						();
	virtual void	OnFocusLost						();

	CUITreeViewBoxItem * GetOwner() const					{ return pOwner; }
	void			SetOwner(CUITreeViewBoxItem *owner)	{ pOwner = owner; }
protected:

	virtual void	OnRootChanged();
public:

	void			Open();
	void			Close();
	bool			IsOpened() const					{ return isOpened; }
protected:

	virtual void	OnOpenClose();
public:
    
	
	typedef			xr_vector<CUITreeViewBoxItem*>		SubItems;
	typedef			SubItems::iterator				SubItems_it;
	SubItems		vSubItems;

	CUIStatic		UIBkg;

	void AddSubItem(CUITreeViewBoxItem *pItem);
	
	void DeleteAllSubItems();
	
	CUITreeViewBoxItem * Find(LPCSTR text) const;

	CUITreeViewBoxItem * Find(int value) const;
	
	CUITreeViewBoxItem * Find(CUITreeViewBoxItem *pItem) const;
	
	xr_string GetHierarchyAsText();

	
	virtual void SetText(LPCSTR str);
	//virtual void SendMessage(CUIWindow* pWnd, s16 msg, void* pData);


				CUITreeViewBoxItem			();
	virtual		~CUITreeViewBoxItem			();

	
	void	MarkArticleAsRead(bool value);
	bool	IsArticleReaded() { return m_bArticleRead; }
	
	void	SetReadedColor(u32 cl)		{ m_uReadedColor = cl;		}
	void	SetUnreadedColor(u32 cl)	{ m_uUnreadedColor = cl;	}
	void	SetManualSetColor(bool val)	{ m_bManualSetColor = val;	}
	
	void	SetItemColor()
	{
		m_bArticleRead ? SetTextColor(m_uReadedColor, m_uReadedColor) : SetTextColor(m_uUnreadedColor, m_uUnreadedColor);
	}

	void SetListParent(CUIListBox* p)
	{
		m_real_parent = p;
	}

private:
	
	void	CheckParentMark(CUITreeViewBoxItem *pOwner);
	
	u32		m_uUnreadedColor;
	
	u32		m_uReadedColor;
	
	bool	m_bArticleRead;

	bool	m_bManualSetColor;

	CUIListBox* m_real_parent;
};

//////////////////////////////////////////////////////////////////////////
//  Function for automatic tree hierarchy creation
//////////////////////////////////////////////////////////////////////////

using GroupStringTree = xr_vector<shared_str>;
using GroupStringTree_it = GroupStringTree::iterator;

//////////////////////////////////////////////////////////////////////////

void CreateTreeBranch(shared_str nestingTree, shared_str leafName, CUIListBox *pListToAdd, int leafProperty,
					  CGameFont *pRootFont, u32 rootColor, CGameFont *pLeafFont, u32 leafColor, bool markRead);

#endif	//UI_TREE_VIEW_ITEM_H_