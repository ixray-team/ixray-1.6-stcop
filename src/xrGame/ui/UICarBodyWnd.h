#pragma once

#include "../../xrUI/Widgets/UIDialogWnd.h"
#include "../../xrUI/Widgets/UIEditBox.h"
#include "inventory_space.h"
#include "UIOwnerPropertiesBox.h"

class CUIDragDropListEx;
class CUIItemInfo;
class CUICharacterInfo;
class CUIPropertiesBox;
class CUI3tButton;
class CUICellItem;
class CInventoryBox;
class CInventoryOwner;
class CUIItemDropAmountWnd;
class CWeaponMagazined;

class CUICarBodyWnd: public CUIDialogWnd, public CUIOwnerPropertiesBox
{
private:
	typedef CUIDialogWnd		inherited;
	bool						m_b_need_update;
public:
								CUICarBodyWnd				();
	virtual						~CUICarBodyWnd				();

	virtual void				Init						();
	virtual bool				StopAnyMove					() {return true;}
	virtual CInventory*			GetInventory				();
	virtual CInventoryOwner*	GetInventoryOwner			() { return m_pOurObject; }

	virtual void				SendMessage					(CUIWindow *pWnd, s16 msg, void *pData);

	void						InitCarBody					(CInventoryOwner* pOurInv, CInventoryOwner* pOthersInv);
	void						InitCarBody					(CInventoryOwner* pOur, CInventoryBox* pInvBox);
	virtual void				Draw						();
	virtual void				Update						();
		
	virtual void				Show						(bool status);

	void						DisableAll					();
	void						EnableAll					();
	virtual bool				OnKeyboardAction					(int dik, EUIMessages keyboard_action);

	void						UpdateLists_delayed			();

	void						clear_highlight_lists		();

	void						MoveAllCurrentItem			(u32 item_amount);
	void						TakeAllCurrentItem			(u32 item_amount);
protected:
	CInventoryOwner*			m_pOurObject;

	CInventoryOwner*			m_pOthersObject;
	CInventoryBox*				m_pInventoryBox;

	CUIDragDropListEx*			m_pUIOurBagList;
	CUIDragDropListEx*			m_pUIOthersBagList;

	CUIStatic*					m_pUIStaticTop;
	CUIStatic*					m_pUIStaticBottom;

	CUIFrameWindow*				m_pUIDescWnd;
	CUIStatic*					m_pUIStaticDesc;
	CUIItemInfo*				m_pUIItemInfo;

	CUIStatic*					m_pUIOurBagWnd;
	CUIStatic*					m_pUIOthersBagWnd;

	//информация о персонажах 
	CUIStatic*					m_pUIOurIcon;
	CUIStatic*					m_pUIOthersIcon;
	CUICharacterInfo*			m_pUICharacterInfoLeft;
	CUICharacterInfo*			m_pUICharacterInfoRight;
	CUI3tButton*				m_pUITakeAll;
	CUI3tButton*				m_pUIPutAll;

	CUIItemDropAmountWnd*		m_pItemDropAmountWnd;

	CUICellItem*				m_pCurrentCellItem;
	bool						m_highlight_clear;

	void						UpdateLists					();

	void						ActivatePropertiesBox		();
	void						ProcessPropertiesBoxClicked	();

	void						set_highlight_item			(CUICellItem* cell_item);
	void						highlight_armament			(PIItem item, CUIDragDropListEx* ddlist);
	void						highlight_ammo_for_weapon	(PIItem weapon_item, CUIDragDropListEx* ddlist);
	void						highlight_weapons_for_ammo	(PIItem ammo_item, CUIDragDropListEx* ddlist);
	bool						highlight_addons_for_weapon	(PIItem weapon_item, CUICellItem* ci);
	void						highlight_weapons_for_addon	(PIItem addon_item, CUIDragDropListEx* ddlist);

	void						EatItem						();

	bool						ToOurBag					();
	bool						ToOthersBag					();
	
	void						SetCurrentItem				(CUICellItem* itm);
	CUICellItem*				CurrentItem					();
	PIItem						CurrentIItem				();

	// Взять все
	void						TakeAll						();
	void						PutAll						();

	bool						OnItemDrop					(CUICellItem* itm);
	bool						OnItemStartDrag				(CUICellItem* itm);
	bool						OnItemDbClick				(CUICellItem* itm);
	bool						OnItemSelected				(CUICellItem* itm);
	bool						OnItemRButtonClick			(CUICellItem* itm);
	bool						OnItemFocusReceive			(CUICellItem* itm);
	bool						OnItemFocusLost				(CUICellItem* itm);
	bool						OnItemFocusedUpdate			(CUICellItem* itm);

	bool						ToDeadBodyBag				(CUICellItem* itm, bool b_use_cursor_pos);
	bool						TransferItem				(PIItem itm, CInventoryOwner* owner_from, CInventoryOwner* owner_to, bool b_check);
	void						BindDragDropListEnents		(CUIDragDropListEx* lst);
	void						ColorizeItem				(CUICellItem* itm);
private:
	const char* m_onCanTake = {};
	bool m_isCanTake = false;

	const char* m_onCanMoveToPartner = {};
	bool m_isCanMoveToPartner = false;

protected:
	virtual	void				PropertiesBoxForDrop		(CUICellItem* cell_item, PIItem item, bool& b_show);

	void						DetachAddon					(LPCSTR addon_name, PIItem itm = nullptr);

	void						UnloadWeapon				(CWeaponMagazined* pWnp);
	bool						TryUseItem					(CUICellItem* itm);

	void						SendEvent_Item_Eat			(PIItem	pItem, u16 parent);
	void						SendEvent_Item2Ruck			(PIItem	pItem, u16 parent);

	bool						ToBag						(CUICellItem* itm, bool b_use_cursor_pos);

	enum eCarBodySndAction{	eSndOpen	=0,
								eSndClose,
								eItemToSlot,
								eItemToBelt,
								eItemToRuck,
								eProperties,
								eDropItem,
								eAttachAddon,
								eDetachAddon,
								eItemUse,
								eSndMax};
	ref_sound					sounds					[eSndMax];
	void						PlaySnd					(eCarBodySndAction a);
};