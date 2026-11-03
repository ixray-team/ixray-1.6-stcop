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
	virtual ~CUIOutfitDragDropList	() override;

	virtual void			SetItem					(CUICellItem* itm) override; //auto
	virtual bool			SetItem					(CUICellItem* itm, Fvector2 abs_pos) override;  // start at cursor pos
	virtual void			SetItem					(CUICellItem* itm, Ivector2 cell_pos) override; // start at cell
	virtual CUICellItem*	RemoveItem				(CUICellItem* itm, bool force_root) override;
	void					SetOutfit				(CUICellItem* itm);
	virtual void			Draw					() override;
			void			SetVisualAnimation		(const char* animation);

	virtual CUIOutfitDragDropList* ui_cast_outfit_dragdrop_list() override { return this; }
};
