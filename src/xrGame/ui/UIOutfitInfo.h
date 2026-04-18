#pragma once

#include "../../xrUI/Widgets/UIWindow.h"
#include "../../xrUI/Widgets/UIStatic.h"
#include "../../xrUI/Widgets/UIDoubleProgressBar.h"
#include "../../xrEngine/AI/alife_space.h"

class CCustomOutfit;
class CHelmet;
class CUIDoubleProgressBar;
class CUIXml;
class CUIScrollView;

class CUIOutfitImmunity final : public CUIStatic
{
public:
					CUIOutfitImmunity	();
	virtual			~CUIOutfitImmunity	();

	enum class InitResult
	{
		Failed,
		Normal,
		Plain
	};

		InitResult	Init				( CUIXml& xml_doc, const char* section );
			void	SetCaption			( const char* name );
			void	SetProgressValue	( float cur, float comp );
			void	SetDefaultValuesPlain(float magnitude, const shared_str& unit);
			bool	GetLegacyMode		() { return m_legacy_mode; }
			void	SetAfValue			( float val ) { m_af_value = val;}

	virtual CUIWindow* ui_cast_window() { return this; }

protected:
	bool					m_legacy_mode = false;
	CUIStatic*				m_name; // texture + name
	CUIDoubleProgressBar*	m_progress;
	CUIStatic*				m_value; // 100%
	CUIStatic*				m_text_legacy; // 100%
	float					m_magnitude;
	float					m_af_value = 0.0f;
	shared_str				m_unit_str;

	InitResult	InitPlain(CUIXml& xml, const char* section);

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
//			void	SetItem				(CCustomOutfit* outfit, u32 hitType, bool force_add);

    CUIOutfitImmunity* CreateItem(CUIXml& uiXml, const char* section,
        float magnitude, const shared_str& unit,
        shared_str translationId);

	virtual CUIWindow* ui_cast_window() { return this; }

protected:
	enum				{ max_count = ALife::eHitTypeMax-2 };
	
	CUIStatic*			m_caption;
	CUIStatic*			m_Prop_line;
	CUIOutfitImmunity*	m_items[max_count];
	CUIScrollView*		m_listWnd;

}; // class CUIOutfitInfo
