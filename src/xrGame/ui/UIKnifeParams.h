#pragma once
#include "../../xrUI/Widgets/UIWindow.h"

#include "UIDoubleProgressBar.h"

class CUIXml;
class CInventoryItem;

#include "../../xrScripts/script_export_space.h"

struct SLuaKnifeParams;

class CUIKnifeParams : public CUIWindow 
{
public:
							CUIKnifeParams		();
	virtual					~CUIKnifeParams		();

	void 					InitFromXml			(CUIXml& xml_doc);
	void					SetInfo				(CInventoryItem* slot_wpn, CInventoryItem& cur_wpn);
	bool 					Check				(CInventoryItem& cur_wpn);

protected:
	CUIDoubleProgressBar	m_progressHandling;
	CUIDoubleProgressBar	m_progressDamage;

	CUIStatic				m_icon_dam;
	CUIStatic				m_icon_han;
	CUIStatic				m_icon_dist;

	CUITextWnd				m_textHandling;
	CUITextWnd				m_textDamage;
	CUITextWnd				m_textDist;
	CUITextWnd				m_textDist1Value;
	CUITextWnd				m_textDist2Value;
	CUITextWnd				m_meters_name;
	CUITextWnd				m_textDistDelimiter;
	CUIStatic				m_Prop_line;
};