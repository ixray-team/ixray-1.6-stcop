#pragma once

#include "UIScrollView.h"

class CUIListBoxItem;

class CUIListBox : 	public CUIScrollView
{
public:
					CUIListBox						();
	CUIListBoxItem* AddItem							();
	CUIListBoxItem* AddTextItem						(LPCSTR text);
	void			AddExistingItem					(CUIListBoxItem *item, int pos = -1);

	void			Remove							(CUIListBoxItem* itm);
	void 			RemoveItem				(int index) 
										{ CUIListBoxItem* item = GetItemByIDX(index);
										  Remove(item); }
	void			RemoveAll						();

	CUIListBoxItem* GetItemByTAG					(u32 tag_value);
	int				GetIdxByTAG						(u32 tag_value);
	CUIListBoxItem* GetItemByIDX					(int idx);
	CUIListBoxItem* GetItemByText					(LPCSTR text);
	CUIListBoxItem* GetSelectedItem					();

	LPCSTR			GetSelectedText					();
	LPCSTR			GetText							(int idx);
	void			MoveSelectedUp					();
	void			MoveSelectedDown				();
	void			SetSelectionTexture				(LPCSTR texture);
	void			SetItemHeight					(float h);
	float			GetItemHeight					();
	float			GetLongestLength				();

	virtual	void	SetSelected						(CUIWindow* w)	{CUIScrollView::SetSelected(w);};
		u32			GetSelectedIDX					();
		void		SetSelectedIDX					(u32 idx);
		void		SetSelectedTAG					(u32 tag_val);
		void		SetSelectedText					(LPCSTR txt);
		void		SetImmediateSelection			(bool f);

virtual bool		OnMouseAction							(float x, float y, EUIMessages mouse_action);
virtual void		SendMessage						(CUIWindow* pWnd, s16 msg, void* pData = 0);

			void			SetTextColor			(u32 color);
			void			SetTextColorS			(u32 color);
			u32				GetTextColor			();
			void			SetFont					(CGameFont* pFont);
			CGameFont*		GetFont					();
			void			SetTextAlignment			(ETextAlignment alignment);
			ETextAlignment		GetTextAlignment			();


protected:
	CGameFont*		m_pFont;
	float			m_def_item_height;
	u32				m_text_color;
	u32				m_text_color_s;
	ETextAlignment			m_text_al;
	shared_str		m_selection_texture;

	bool			m_bImmediateSelection;
	DECLARE_SCRIPT_REGISTER_FUNCTION
};