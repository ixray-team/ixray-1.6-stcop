#pragma once

#include "../../xrUI/Widgets/UIWindow.h"
#include "../../xrUI/Widgets/UIDoubleProgressBar.h"

class CUIXml;
class CUI3dStatic;
class CInventoryItem;

#include "../../xrScripts/script_export_space.h"

struct SLuaWpnParams;

class CUIWpnParams final : 
	public CUIWindow 
{
public:
	CUIWpnParams();
	virtual ~CUIWpnParams() = default;

	void 					InitFromXml			(CUIXml& xml_doc);
	void					SetInfo				(CInventoryItem* slot_wpn, CInventoryItem& cur_wpn);
	bool 					Check				(CInventoryItem& wpn_section);

	virtual CUIWindow* ui_cast_window() { return this; }

protected:
	CUIDoubleProgressBar	m_progressAccuracy; // red or green
	CUIDoubleProgressBar	m_progressHandling;
	CUIDoubleProgressBar	m_progressDamage;
	CUIDoubleProgressBar	m_progressRPM;

	CUIStatic*				m_icon_acc;
	CUIStatic*				m_icon_dam;
	CUIStatic*				m_icon_han;
	CUIStatic*				m_icon_rpm;

	CUIStatic*				m_stAmmo;
	CUIStatic				m_textAccuracy;
	CUIStatic				m_textHandling;
	CUIStatic				m_textDamage;
	CUIStatic				m_textRPM;
	CUIStatic*				m_textAmmoTypes;
	CUIStatic*				m_textAmmoUsedType;
	CUIStatic*				m_textAmmoCount;
	CUIStatic*				m_textAmmoCount2;
	CUI3dStatic*			m_stAmmoType1;
	CUI3dStatic*			m_stAmmoType2;
	CUI3dStatic*			m_stAmmoType3;
	CUIStatic*				m_Prop_line;
};

// -------------------------------------------------------------------------------------------------

class CUIConditionParams : public CUIWindow 
{
public:
							CUIConditionParams	();
	virtual					~CUIConditionParams	();

	void 					InitFromXml			(CUIXml& xml_doc);
	void					SetInfo				(CInventoryItem const* slot_wpn, CInventoryItem const& cur_wpn);

	virtual CUIWindow* ui_cast_window() { return this; }

protected:
	CUIDoubleProgressBar	m_progress; // red or green
	CUIStatic				m_text;
};
