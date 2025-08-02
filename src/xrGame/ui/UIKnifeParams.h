#pragma once
#include "../../xrUI/Widgets/UIWindow.h"
#include "../../xrUI/Widgets/UIDoubleProgressBar.h"

class CUIXml;
class CInventoryItem;

#include "../../xrScripts/script_export_space.h"

struct SLuaKnifeParams;

class CUIKnifeParams final : public CUIWindow
{
public:
							CUIKnifeParams		();
	virtual					~CUIKnifeParams		();

	void 					InitFromXml			(CUIXml& xml_doc);
	void					SetInfo				(CInventoryItem* slot_wpn, CInventoryItem& cur_wpn);
	bool 					Check				(CInventoryItem& cur_wpn);

	virtual CUIWindow* ui_cast_window() { return this; }

protected:
	CUIDoubleProgressBar	m_progressHandling;
	CUIDoubleProgressBar	m_progressDamage;

	CUIStatic				m_icon_dam;
	CUIStatic				m_icon_han;
	CUIStatic				m_icon_dist;

	CUIStatic				m_textHandling;
	CUIStatic				m_textDamage;
	CUIStatic				m_textDist;
	CUIStatic				m_textDist1Value;
	CUIStatic				m_textDist2Value;
	CUIStatic				m_meters_name;
	CUIStatic				m_textDistDelimiter;
	CUIStatic				m_Prop_line;
};