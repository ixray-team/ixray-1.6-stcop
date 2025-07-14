#pragma once
#include "../../xrUI/Widgets/UIWindow.h"
#include "../../xrEngine/AI/alife_space.h"


class CUIXml;
class CUIStatic;
class CUITextWnd;
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
	UIArtefactParamItem*	m_immunity_item[ALife::eHitTypeWound_2];
	UIArtefactParamItem*	m_restore_item[ALife::eRestoreTypeMax];
	UIArtefactParamItem*	m_disp_condition;
	UIArtefactParamItem*	m_additional_weight;
	UIArtefactParamItem*	m_af_slots;

	CUIStatic*				m_Prop_line;

	CParamType				object_type;

	bool is_artefact() const { return object_type == CParamType::eParamTypeArtefact; }
	bool is_backpack() const { return object_type == CParamType::eParamTypeBackpack; }

protected: // SoC
	enum {
		_item_start = 0,
		_item_health_restore_speed = _item_start,
		_item_radiation_restore_speed,
		_item_satiety_restore_speed,
		_item_power_restore_speed,
		_item_bleeding_restore_speed,

		_max_item_index1,

		_item_burn_immunity = _max_item_index1,
		_item_strike_immunity,
		_item_shock_immunity,
		_item_wound_immunity,
		_item_radiation_immunity,
		_item_telepatic_immunity,
		_item_chemical_burn_immunity,
		_item_explosion_immunit,
		_item_fire_wound_immunity,

		_max_item_index,

	};
	CUIStatic* m_info_items[_max_item_index];
}; // class CUIArtefactParams

// -----------------------------------

class UIArtefactParamItem final : public CUIWindow
{
public:
				UIArtefactParamItem	();
	virtual		~UIArtefactParamItem();
		
		bool	Init				( CUIXml& xml, LPCSTR section );
		void	SetCaption			( LPCSTR name );
		void	SetValue			( float value );
	
	virtual CUIWindow* ui_cast_window() { return this; }

private:
	CUIStatic*	m_caption;
	CUITextWnd*	m_value;
	float		m_magnitude;
	bool		m_sign_inverse;
	shared_str	m_unit_str;
	shared_str	m_texture_minus;
	shared_str	m_texture_plus;

}; // class UIArtefactParamItem
