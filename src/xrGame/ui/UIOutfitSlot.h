#pragma once
#include "UIDragDropListEx.h"

class CUI3dStatic;

class CUIOutfitDragDropList :public CUIDragDropListEx
{
	typedef CUIDragDropListEx						inherited;
	CUI3dStatic*									m_background;
	shared_str										m_default_outfit;
	
public:
							CUIOutfitDragDropList	();
	virtual					~CUIOutfitDragDropList	();

	virtual void			SetItem					(CUICellItem* itm); //auto
	virtual bool			SetItem					(CUICellItem* itm, Fvector2 abs_pos);  // start at cursor pos
	virtual void			SetItem					(CUICellItem* itm, Ivector2 cell_pos); // start at cell
	virtual CUICellItem*	RemoveItem				(CUICellItem* itm, bool force_root);
	void					SetOutfit				(CUICellItem* itm);
	virtual	void			Draw					();
			void			SetDefaultOutfit		(const char* default_outfit);
};
