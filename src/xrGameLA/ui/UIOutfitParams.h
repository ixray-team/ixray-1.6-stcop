#pragma once
#include "UIWindow.h"

class CUIXml;
class CUIStatic;

class CInventoryItem;
class COutfitBase;

#pragma todo("get rid of all this, move entire class into scripts")
class CUIOutfitParams :public CUIWindow
{
public:
	CUIOutfitParams();
	virtual						~CUIOutfitParams();
	void 						InitFromXml(CUIXml& xml_doc);
	bool 						Check(CInventoryItem&);
	void 						SetInfo(CInventoryItem&);

protected:
	enum InfoType {
		_item_start = 0,

		_item_burn_immunity = _item_start,
		_item_strike_immunity,
		_item_shock_immunity,
		_item_wound_immunity,
		_item_radiation_immunity,
		_item_telepatic_immunity,
		_item_chemical_burn_immunity,
		_item_explosion_immunity,
		_item_armor_body,
		_item_armor_head,
		_item_additional_inventory_weight2,
		_item_sprint_allowed,
		
		_item_health_restore_speed,
		_item_radiation_restore_speed,
		_item_satiety_restore_speed,
		_item_power_restore_speed,
		_item_bleeding_restore_speed,

		_max_item_index,

	};
	struct ProtectionDesc {
		u32 hitType;
		CUIOutfitParams::InfoType infoType;
	};
	struct LineDesc {
		LPCSTR staticName;
		LPCSTR paramName;
	};
	static LineDesc				infoLines[_max_item_index];
	static ProtectionDesc		protectionTypes[];
	bool						m_showHeadArmor;

	CUIStatic*					m_info_items[_max_item_index];
	
	void						DisplayProtecton(InfoType, u32 hitType, COutfitBase*, float& height);
	void						DisplayValue(InfoType, bool, float& height);
	void						DisplayValue(InfoType, float, float& height, bool revertSign = false);
	void						DisplayValue(InfoType, LPCSTR text, u32 color, float& height);
	void						DisplayArmor(InfoType, float, float& height);
};