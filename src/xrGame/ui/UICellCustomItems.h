#pragma once
#include "UICellItem.h"
#include "../Weapon.h"


class CUIInventoryCellItem :public CUICellItem
{
	typedef  CUICellItem	inherited;
public:
								CUIInventoryCellItem		(CInventoryItem* itm);
	virtual bool			EqualTo						(CUICellItem* itm) override;
	virtual void			UpdateItemText				() override;
	virtual CUIDragItem*	CreateDragItem				() override;
	virtual bool			IsHelper					() override;
	virtual void			SetIsHelper					(bool is_helper) override;
				bool			IsHelperOrHasHelperChild	();
	virtual void			Update						() override;
				CInventoryItem* object						() {return (CInventoryItem*)m_pData;}

	virtual CUIWindow* ui_cast_window() override { return this; }
	virtual CUIStatic* ui_cast_static() override { return this; }
	virtual CUICellItem* ui_cast_cell_item() override { return this; }
};

class CUIAmmoCellItem final :public CUIInventoryCellItem
{
	typedef  CUIInventoryCellItem	inherited;
protected:
	virtual void			 UpdateItemText				() override;
public:
								 CUIAmmoCellItem			(CWeaponAmmo* itm);

				u32				 CalculateAmmoCount			();
	virtual bool			 EqualTo						(CUICellItem* itm) override;
	virtual CUIDragItem*	 CreateDragItem				() override;
				CWeaponAmmo*	 object						() {return (CWeaponAmmo*)m_pData;}

	virtual CUIWindow* ui_cast_window() override { return this; }
	virtual CUIStatic* ui_cast_static() override { return this; }
	virtual CUICellItem* ui_cast_cell_item() override { return this; }
};

class CUIWeaponCellItem final :public CUIInventoryCellItem
{
	typedef  CUIInventoryCellItem	inherited;
public:
	enum eAddonType{	eSilencer=0, eScope, eLauncher, eMaxAddon};
protected:
	CUIStatic*					m_addons					[eMaxAddon];
	shared_str					m_last_icons				[eMaxAddon];
	Fvector2					m_addon_offset				[eMaxAddon];
	bool mScopeBack = false;
	void						CreateIcon					(eAddonType);

	void						RefreshOffset				();
	CUIStatic*					GetIcon						(eAddonType);
	void						InitAddon					(CUIStatic* s, const char* section, Fvector2 offset, bool use_heading);
	bool						is_scope					();
	bool						is_silencer					();
	bool						is_launcher					();
public:
								CUIWeaponCellItem			(CWeapon* itm);
	virtual ~CUIWeaponCellItem			() override;
	virtual void			Update						() override;
	virtual void			Draw						() override;
	virtual void			SetTextureColor				(u32 color) override;

				CWeapon*		object						() {return (CWeapon*)m_pData;}
	virtual void			OnAfterChild				(CUIDragDropListEx* parent_list) override;
	virtual CUIDragItem*	CreateDragItem				() override;
	virtual bool			EqualTo						(CUICellItem* itm) override;
	CUIStatic*					get_addon_static			(u32 idx)				{return m_addons[idx];}
	void						DestroyIcon					(eAddonType);
	virtual CUIWindow* ui_cast_window() override { return this; }
	virtual CUIStatic* ui_cast_static() override { return this; }
	virtual CUICellItem* ui_cast_cell_item() override { return this; }
};

class CBuyItemCustomDrawCell final :public ICustomDrawCellItem
{
	CGameFont*			m_pFont;
	string16			m_string;
public:
						CBuyItemCustomDrawCell	(const char* str, CGameFont* pFont);
	virtual void		OnDraw					(CUICellItem* cell) override;

};
