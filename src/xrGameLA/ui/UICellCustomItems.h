#pragma once
#include "UICellItem.h"
#include "../Weapon.h"


class CUIInventoryCellItem :public CUICellItem
{
	typedef  CUICellItem	inherited;
protected:
				CUIStatic*		m_upgrade;
				CUIStatic*		m_unique_mark;
				CUIStatic*		m_quest_mark;
				CUIStatic* 		CreateSpecialIcon			(LPCSTR name, bool smaller);
				float			UpdateSpecialIcon			(CUIStatic* icon, Fvector2 pos, bool enable);
public:
								CUIInventoryCellItem		(CInventoryItem* itm);
	virtual		bool			EqualTo						(CUICellItem* itm);
				CInventoryItem* object						() {return (CInventoryItem*)m_pData;}
				
				void			Update						() override;
};

class CUIAmmoCellItem :public CUIInventoryCellItem
{
	typedef  CUIInventoryCellItem	inherited;
protected:
	virtual		void			UpdateItemText			();
public:
								CUIAmmoCellItem				(CWeaponAmmo* itm);
	virtual		void			Update						();
	virtual		bool			EqualTo						(CUICellItem* itm);
				CWeaponAmmo*	object						() {return (CWeaponAmmo*)m_pData;}
};


class CUIWeaponCellItem :public CUIInventoryCellItem
{
	typedef  CUIInventoryCellItem	inherited;
public:
	enum eAddonType{	eSilencer=0, eScope, eLauncher, eMaxAddon};
	struct SAddon {
					SAddon() { icon = nullptr; }
		CUIStatic*	icon;
		Fvector2	offset;
		bool		drawBehind;
		shared_str	section;
	};
protected:
	SAddon						m_addons					[eMaxAddon];
	void						CreateIcon					(eAddonType);
	void						DestroyIcon					(eAddonType);
	void						RefreshOffset				();
	CUIStatic*					GetIcon						(eAddonType);
	void						InitAddon					(SAddon& addon, LPCSTR section, bool use_heading);
	void						InitAddon					(CUIStatic*, LPCSTR section, Fvector2 offset, bool use_heading);
	virtual		void			InitAddonForDrag			(CUIDragItem*, const SAddon&, int pos = -1);
	virtual		void			InitAddonsForDrag			(CUIDragItem*, bool behind);
	bool						is_scope					();
	bool						is_silencer					();
	bool						is_launcher					();
				void			DrawAddons					(bool behind);
public:
								CUIWeaponCellItem			(CWeapon* itm);
				virtual			~CUIWeaponCellItem			();
	virtual		void			Update						();
	virtual		void			SetTextureColor				(u32 color);
				void			DrawTexture					() override;

				CWeapon*		object						() {return (CWeapon*)m_pData;}
	virtual		void 			OnAfterChild				(CUIDragDropListEx* parent_list) override;
	virtual		CUIDragItem*	CreateDragItem				();
	virtual		bool			EqualTo						(CUICellItem* itm);
};

class CBuyItemCustomDrawCell :public ICustomDrawCellItem
{
	CGameFont*			m_pFont;
	string16			m_string;
public:
						CBuyItemCustomDrawCell	(LPCSTR str, CGameFont* pFont);
	virtual void		OnDraw					(CUICellItem* cell);

};
