#pragma once
#include "UIDragDropListEx.h"

class CUI3dStatic;

class CUIOutfitDragDropList :public CUIDragDropListEx
{
	typedef CUIDragDropListEx						inherited;
	CUI3dStatic*									m_background;
	shared_str										m_visual_animation = "$editor";

public:
							CUIOutfitDragDropList	();
	virtual					~CUIOutfitDragDropList	();

	virtual void			SetItem					(CUICellItem* itm); //auto
	virtual bool			SetItem					(CUICellItem* itm, Fvector2 abs_pos);  // start at cursor pos
	virtual void			SetItem					(CUICellItem* itm, Ivector2 cell_pos); // start at cell
	virtual CUICellItem*	RemoveItem				(CUICellItem* itm, bool force_root);
	void					SetOutfit				(CUICellItem* itm);
	virtual	void			Draw					();
			void			SetVisualAnimation		(const char* animation);

	virtual CUIOutfitDragDropList* ui_cast_outfit_dragdrop_list() { return this; }
};
