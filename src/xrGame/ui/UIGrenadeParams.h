////////////////////////////////////////////////////////////////////////////
//	Module 		: UIGrenadeParams.h
//	Created 	: 03.08.2025
//	Author		: St4lker0k765
//	Description : Implementation for grenade params in inventory
////////////////////////////////////////////////////////////////////////////
#pragma once
#include "../../xrUI/Widgets/UIWindow.h"
#include "../../xrUI/Widgets/UIDoubleProgressBar.h"

class CUIXml;
class CInventoryItem;

#include "../../xrScripts/script_export_space.h"

class CUIGrenadeParams : public CUIWindow 
{
public:
							CUIGrenadeParams		();
	virtual					~CUIGrenadeParams();

	void 					InitFromXml			(CUIXml& xml_doc);
	void					SetInfo				(CInventoryItem* slot_wpn, CInventoryItem& cur_wpn);
	bool 					Check				(CInventoryItem& cur_wpn);

protected:
	bool					initialized = false;
	CUIStatic*				m_iconBlastHit = nullptr;
	CUIStatic*				m_iconBlastRadius = nullptr;
	CUIStatic*				m_iconFragsCount = nullptr;
	CUIStatic*				m_iconFragsRadius = nullptr;
	CUIStatic*				m_iconFragsHit = nullptr;

	CUIStatic*				m_captionBlastHit = nullptr;
	CUIStatic*				m_captionBlastRadius = nullptr;
	CUIStatic*				m_captionFragsCount = nullptr;
	CUIStatic*				m_captionFragsRadius = nullptr;
	CUIStatic*				m_captionFragsHit = nullptr;
	CUIStatic*				m_textBlastHit = nullptr;
	CUIStatic*				m_textBlastRadius = nullptr;
	CUIStatic*				m_textFragsCount = nullptr;
	CUIStatic*				m_textFragsRadius = nullptr;
	CUIStatic*				m_textFragsHit = nullptr;
	CUIStatic*				m_Prop_line = nullptr;
};