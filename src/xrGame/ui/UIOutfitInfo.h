#pragma once

#include "../../xrUI/Widgets/UIWindow.h"
#include "../../xrUI/Widgets/UIDoubleProgressBar.h"
#include "../../xrEngine/AI/alife_space.h"

class CCustomOutfit;
class CHelmet;
class CUIStatic;
class CUIDoubleProgressBar;
class CUIXml;
class CUIScrollView;

class CUIOutfitImmunity final : public CUIWindow
{
public:
					CUIOutfitImmunity	();
	virtual			~CUIOutfitImmunity	();

			bool	InitFromXml			( CUIXml& xml_doc, LPCSTR base_str, u32 hit_type );
			void	SetProgressValue	( float cur, float comp );

	virtual CUIWindow* ui_cast_window() { return this; }

protected:
	CUIStatic				m_name; // texture + name
	CUIDoubleProgressBar	m_progress;
	CUITextWnd*				m_value; // 100%
	float					m_magnitude;
	shared_str				m_unit_str;

}; // class CUIOutfitImmunity

// -------------------------------------------------------------------------------------

class CUIOutfitInfo final : public CUIWindow
{
public:
					CUIOutfitInfo		();
	virtual			~CUIOutfitInfo		();

			void 	InitFromXml			( CUIXml& xml_doc );
			void 	UpdateInfo			( CCustomOutfit* cur_outfit, CCustomOutfit* slot_outfit = NULL );	
			void 	UpdateInfo			( CHelmet* cur_helmet, CHelmet* slot_helmet = NULL );
			void	SetItem				(CCustomOutfit* outfit, u32 hitType, bool force_add);

	virtual CUIWindow* ui_cast_window() { return this; }

protected:
	enum				{ max_count = ALife::eHitTypeMax-2 };
	
	CUIStatic*			m_caption;
	CUIStatic*			m_Prop_line;
	CUIOutfitImmunity*	m_items[max_count];
	CUIStatic*			m_items_legacy[max_count];
	CUIScrollView*		m_listWnd;

}; // class CUIOutfitInfo
