#pragma once

#include "UIWindow.h"
#include "..\OutfitBase.h"

class CUIScrollView;
class CCustomOutfit;
class CHelmet;
class CUIStatic;
class CUIXml;

class UIActorProtectionInfo : public CUIWindow
{
CCustomOutfit*		m_outfit;
CHelmet*			m_helmet;
public:
					UIActorProtectionInfo			();
	virtual			~UIActorProtectionInfo			();

			void 	Update					(CCustomOutfit* outfit,  CHelmet* helmet);	
			void 	InitFromXml				(CUIXml& xml_doc);
protected:

	CUIScrollView*	m_listWnd;
	bool			m_showLabels;
	bool			m_showHints;
	bool			m_showHeadArmor;

	enum InfoType {
		_item_start						= 0,
		_item_burn_immunity				= _item_start,
		_item_strike_immunity,
		_item_shock_immunity,
		_item_wound_immunity,		
		_item_radiation_immunity,
		_item_telepatic_immunity,
		_item_chemical_burn_immunity,
		_item_explosion_immunity,
		_item_armor_body,
		_item_armor_head,

		_max_item_index,
	};
	CUIStatic*		m_items[_max_item_index];

	void			SetItem					(InfoType, u32 hitType, bool force_add = false);
	void			SetItem					(InfoType, LPCSTR text, bool force_add = false);
	void			SetItemArmor			(InfoType, float (COutfitBase::*getter)(), u32 hitType = (u32)-1);
};