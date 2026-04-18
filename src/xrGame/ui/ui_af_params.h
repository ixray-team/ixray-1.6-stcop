#pragma once
#include "../../xrUI/Widgets/UIWindow.h"
#include "../../xrEngine/AI/alife_space.h"
#include "../../xrUI/Widgets/UIStatic.h"

class CUIXml;
class UIArtefactParamItem;
class CInventoryItem;

class CUIArtefactParams final : public CUIWindow
{
public:
	enum class CParamType
	{
		eParamTypeOutfit,
		eParamTypeArtefact,
		eParamTypeBackpack
	};

					CUIArtefactParams		(const CParamType& type);
	virtual			~CUIArtefactParams		();
			void	InitFromXml				(CUIXml& xml);
			bool	Check					(const shared_str& af_section);
			void	SetInfo					(CInventoryItem& pInvItem);

	virtual CUIWindow* ui_cast_window() { return this; }

protected:
    UIArtefactParamItem* CreateItem(CUIXml& uiXml, pcstr section,
        shared_str translationId, shared_str translationId2 = nullptr);
	
    UIArtefactParamItem* CreateItem(CUIXml& uiXml, pcstr section,
        float magnitude, bool isSignInverse, const shared_str& unit,
        shared_str translationId, shared_str translationId2 = nullptr);

	UIArtefactParamItem*	m_immunity_item[ALife::eHitTypeWound_2];
	UIArtefactParamItem*	m_restore_item[ALife::eRestoreTypeMax];
	UIArtefactParamItem*	m_disp_condition;
	UIArtefactParamItem*	m_additional_weight;
	UIArtefactParamItem*	m_af_slots;

	CUIStatic*				m_Prop_line;

	CParamType				object_type;

	bool is_artefact() const { return object_type == CParamType::eParamTypeArtefact; }
	bool is_backpack() const { return object_type == CParamType::eParamTypeBackpack; }

}; // class CUIArtefactParams

// -----------------------------------

class UIArtefactParamItem final : public CUIStatic
{
public:
				UIArtefactParamItem	();
	virtual		~UIArtefactParamItem();
	
    enum class InitResult
    {
        Failed,
        Normal,
        Plain
    };
	
	InitResult	Init				( CUIXml& xml, LPCSTR section );
	
		void	SetDefaultValuesPlain(float magnitude, bool isSignInverse, const shared_str& unit);
		void	SetCaption			( LPCSTR name );
		void	SetValue			( float value );
	
	virtual CUIWindow* ui_cast_window() { return this; }
		bool	GetLegacyMode		() { return !(m_caption && m_caption->IsShown() && m_value && m_value->IsShown()); }

protected:
	InitResult	InitPlain			(CUIXml& xml, pcstr section);

private:
	CUIStatic*	m_caption;
	CUIStatic*	m_value;
	CUIStatic*	m_text_legacy; // 100%
	float		m_magnitude;
	bool		m_sign_inverse;
	shared_str	m_unit_str;
	shared_str	m_texture_minus;
	shared_str	m_texture_plus;

}; // class UIArtefactParamItem
