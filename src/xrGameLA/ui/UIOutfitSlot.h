#pragma once
#include "UIDragDropListEx.h"

class CUIOutfitDragDropList :public CUIDragDropListEx
{
	typedef CUIDragDropListEx						inherited;
	CUIStatic*										m_background;
	shared_str										m_default_outfit;
	void					SetOutfit				(CUICellItem* itm);
	
public:
							CUIOutfitDragDropList	();
	virtual					~CUIOutfitDragDropList	();

			void			SetItem					(CUICellItem* itm) override; //auto
			void			SetItem					(CUICellItem* itm, Fvector2 abs_pos) override;  // start at cursor pos
			void			SetItem					(CUICellItem* itm, Ivector2 cell_pos) override; // start at cell
			CUICellItem*	RemoveItem				(CUICellItem* itm, bool force_root) override;
			void			ClearAll				(bool) override;
			void			Draw					() override;
			void			SetDefaultOutfit		(LPCSTR default_outfit);
};
