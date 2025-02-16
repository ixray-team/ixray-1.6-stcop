#pragma once

class CInventory;

#include "UIDialogWnd.h"
#include "UIStatic.h"

#include "UIProgressBar.h"

#include "UIPropertiesBox.h"
#include "UIOutfitSlot.h"

#include "UIActorProtectionInfo.h"
#include "UIItemInfo.h"
#include "../inventory_space.h"
#include "../WeaponMagazined.h"

class CArtefact;
class CUI3tButton;
class CUIDragDropListEx;
class CUICellItem;
class CUIInventoryWnd: public CUIDialogWnd
{
private:
	typedef CUIDialogWnd	inherited;
	bool					m_b_need_reinit;
	xr_vector<ref_sound*>	use_sounds;
public:
							CUIInventoryWnd				();
	virtual					~CUIInventoryWnd			();

	virtual void			Init						();

	void					InitInventory				();
	void					InitInventory_delayed		();
	virtual bool			StopAnyMove					()					{return false;}

	virtual void			SendMessage					(CUIWindow *pWnd, s16 msg, void *pData);
	virtual bool			OnMouseAction						(float x, float y, EUIMessages mouse_action);
	virtual bool			OnKeyboardAction					(int dik, EUIMessages keyboard_action);


	IC CInventory*			GetInventory				()					{return m_pInv;}

	virtual void			Update						();
	virtual void			Draw						();

	virtual void			ShowDialog						(bool bDoHideIndicators);
	virtual void			HideDialog						();

	void					AddItemToBag				(PIItem pItem);
	void					DeleteFromInventory			(PIItem PIItem);


	void					PlayUseSound(LPCSTR sound_path);

protected:
	enum eInventorySndAction{	eInvSndOpen	=0,
								eInvSndClose,
								eInvItemToSlot,
								eInvItemToBelt,
								eInvItemToRuck,
								eInvProperties,
								eInvDropItem,
								eInvAttachAddon,
								eInvDetachAddon,
								eInvItemUse,
								eInvSndMax};

	ref_sound					sounds					[eInvSndMax];
	void						PlaySnd					(eInventorySndAction a);

	CUIStatic					UIBeltSlots;
	CUIStatic					UIBack;
	CUIStatic*					UIRankFrame;
	CUIStatic*					UIRank;

	CUIStatic					UICacoWnd;
	CUIStatic					UIBagWnd;
	CUIStatic					UIMoneyWnd;
	CUIStatic					UIDescrWnd;
	CUIFrameWindow				UIPersonalWnd;

	CUI3tButton*				UIExitButton;
	CUIStatic					UIStaticBottom;
	CUIStatic					UIStaticTime;
	CUITextWnd					UIStaticTimeString;

	CUIStatic					UIStaticPersonal;
		
	CUIDragDropListEx*			m_pUIBagList;
	CUIDragDropListEx*			m_pUIBeltList;
	CUIDragDropListEx*			m_pUIPistolList;
	CUIDragDropListEx*			m_pUIAutomaticList;
	CUIDragDropListEx*			m_pUIAutomatic2List;
	CUIDragDropListEx*			m_pUIKnifeList;
	CUIDragDropListEx*			m_pUIBinocularList;
	CUIDragDropListEx*			m_pUITorchList;
	CUIDragDropListEx*			m_pUIOutfitList;
	CUIDragDropListEx*			m_pUIDetectorList;
	CUIDragDropListEx*			m_pUIHelmetList;
	CUIDragDropListEx*			m_pUIPNVList;
	CUIDragDropListEx*			m_pUIAnomDetectorList;

	CUIDragDropListEx*			InitDragDropList			(CUIXml&, LPCSTR name, int index, CUIWindow* parent = nullptr, CUIDragDropListEx* list = nullptr);
	void						InitSlotItem				(CUIDragDropListEx* list, TSlotId slot);
	void						ClearAllLists				();
	void						BindDragDropListEvents		(CUIDragDropListEx* lst);
	
	EListType					GetType						(CUIDragDropListEx* l);
	TSlotId						GetSlot						(CUIDragDropListEx* l, CUICellItem* itm);
	CUIDragDropListEx*			GetSlotList					(TSlotId slot_idx);
	bool						SlotIsCompatible			(TSlotId desiredSlot, CUICellItem* itm);
	bool						SecondRifleSlotAvailable	() { return m_pUIAutomatic2List != nullptr; }
	bool						CanPutInSlot				(TSlotId desiredSlot, CUICellItem* itm);

	bool		xr_stdcall		OnItemDrop					(CUICellItem* itm);
	bool		xr_stdcall		OnItemStartDrag				(CUICellItem* itm);
	bool		xr_stdcall		OnItemDbClick				(CUICellItem* itm);
	bool		xr_stdcall		OnItemSelected				(CUICellItem* itm);
	bool		xr_stdcall		OnItemRButtonClick			(CUICellItem* itm);


	CUIStatic					UIProgressBack;
	CUIStatic					UIProgressBack_rank;

	CUIProgressBar				UIProgressBarHealth;	
	CUIProgressBar				UIProgressBarStamina;
	CUIProgressBar				UIProgressBarRadiation;
	CUIProgressBar				UIProgressBarHunger;
	CUIProgressBar				UIProgressBarThirst;
	CUIProgressBar				UIProgressBarArmor;
	CUIProgressBar				UIProgressBarMozg;

	CUIProgressBar				UIProgressBarRank;

	CUIPropertiesBox			UIPropertiesBox;
	
	//информация о персонаже
	UIActorProtectionInfo		UIOutfitInfo;
	CUIItemInfo					UIItemInfo;

	CInventory*					m_pInv;

	CUICellItem*				m_pCurrentCellItem;

	bool						DropItem					(PIItem itm, CUIDragDropListEx* lst);
	bool						TryUseItem					(PIItem itm);
	//----------------------	-----------------------------------------------
	void						SendEvent_Item2Slot			(PIItem	pItem, TSlotId slotId);
	void						SendEvent_Item2Belt			(PIItem	pItem);
	void						SendEvent_Item2Ruck			(PIItem	pItem);
	void						SendEvent_Item_Drop			(PIItem	pItem);
	void						SendEvent_Item_Eat			(PIItem	pItem);
	void						SendEvent_ActivateSlot		(PIItem	pItem, TSlotId slotId);

	//---------------------------------------------------------------------

	void						ProcessPropertiesBoxClicked	();
	void						ActivatePropertiesBox		();
	bool						AttachActionToPropertyBox	(TSlotId slot, CInventoryItem* addon, LPCSTR text);

	void						DropCurrentItem				(bool b_all);
	void						SumAmmoByDrop				(CUICellItem* cell_itm, CUIDragDropListEx* old_owner);
	void						EatItem						(PIItem itm);
	
	bool						ToSlot						(CUICellItem* itm, bool force_place, TSlotId slot);
	bool						ToBag						(CUICellItem* itm, bool b_use_cursor_pos);
	bool						ToBelt						(CUICellItem* itm, bool b_use_cursor_pos);


	void						AttachAddon					(PIItem item_to_upgrade);
	void						DetachAddon					(const char* addon_name);

	void						SetCurrentItem				(CUICellItem* itm);
	void						ColorizeAmmo				(CUICellItem* itm);
	CUICellItem*				CurrentItem					();
	PIItem						CurrentIItem				();

	TIItemContainer				ruck_list;
	u32							m_iCurrentActiveSlot;
};