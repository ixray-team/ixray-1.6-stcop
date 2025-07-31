#pragma once

#include "../../xrUI/Widgets/UIWindow.h"
#include "../../xrEngine/AI/alife_space.h"
#include "../../xrServerEntities/inventory_space.h"
#include "../actor_defs.h"

class CUIStatic;
class CUITextWnd;
class CUIProgressBar;
class CUIProgressShape;
class CUIXml;
class CUIArrow;
class CActor;

int const it_max = ALife::infl_max_count - 1;

class CUIHudStatesWnd : public CUIWindow
{
private:
	typedef CUIWindow						inherited;
//-	typedef ALife::EInfluenceType	EIndicatorType;

	CUIStatic*			m_back_v = nullptr;
	CUIStatic*			m_back_over_arrow = nullptr;
	CUIStatic*			m_static_armor = nullptr;
	CUIStatic*			m_bleeding = nullptr;

	CUIStatic*			m_resist_back[it_max];
	CUIStatic*			m_indik[it_max];
	CUIStatic*			m_ind_starvation = nullptr;
	CUIStatic*			m_resist_back_starvation = nullptr;

	CUITextWnd*			m_ui_weapon_sign_ammo = nullptr;
	Frect				m_ui_weapon_icon_rect;

	CUIProgressShape*	m_progress_self = nullptr;
	CUIArrow*			m_arrow = nullptr;
	CUIArrow*			m_arrow_shadow = nullptr;
	
	float				m_last_health;
	float				m_health_blink;

	float				m_radia_self;
//	float				m_actor_radia_factor;
	float				m_radia_hit;
	shared_str			m_lanim_name;
	
	float				m_zone_cur_power[ALife::infl_max_count];
//--	float				m_zone_max_power[hud_it_max];//<-- CActorCondition
	float				m_zone_feel_radius[ALife::infl_max_count ];
	ALife::EHitType		m_zone_hit_type[ALife::infl_max_count ];
	float				m_zone_threshold[ALife::infl_max_count ];

	float				m_zone_feel_radius_max;
	u32					m_timer_1sec;
	
	bool				m_fake_indicators_update;
	bool				m_cur_state_LA[it_max];
	bool				m_b_force_update;

	bool				m_isZoneTouch = false;
	const char*			m_onZoneTouch = {};
public:
	
	CUITextWnd*			m_ui_weapon_cur_ammo = nullptr;
	CUITextWnd*			m_ui_weapon_fmj_ammo = nullptr;
	CUITextWnd*			m_ui_weapon_ap_ammo = nullptr;
	CUITextWnd*			m_fire_mode = nullptr;
	CUITextWnd*			m_ui_grenade = nullptr;
	II_BriefInfo		m_item_info;
	CUIStatic*			m_radia_damage = nullptr;
	
	CUIProgressBar*		m_ui_health_bar = nullptr;
	CUIProgressBar*		m_ui_armor_bar = nullptr;
	CUIProgressBar*		m_ui_stamina_bar = nullptr;
	CUIStatic*			m_ui_weapon_icon = nullptr;
	CUIStatic*			m_back = nullptr;
	CUITextWnd*			m_ui_weapon_third_ammo = nullptr; //Alundaio

	u32					m_ui_weapon_ammo_color_active;
	u32					m_ui_weapon_ammo_color_inactive;

					CUIHudStatesWnd		();
	virtual			~CUIHudStatesWnd	();

			void	InitFromXml			( CUIXml& xml, LPCSTR path );
			void	Load_section		();
	virtual void	Update				();
//	virtual void	Draw				();

			void	on_connected		();
			void	reset_ui			();
			void	UpdateHealth		( CActor* actor );
			void	SetAmmoIcon			( const shared_str& sect_name );
			void	UpdateActiveItemInfo( CActor* actor );

			void 	UpdateZones			();
			void	UpdateIndicators	( CActor* actor );
			void	UpdateSatiety		(CActor* actor);

			float	get_zone_cur_power	( ALife::EHitType hit_type );
			float	get_main_sensor_value()	{ return m_radia_hit; }

			void	DrawZoneIndicators	();
			void	FakeUpdateIndicatorType(u8 t, float power);
			void	EnableFakeIndicators(bool enable);

	virtual CUIWindow* ui_cast_window() { return this; }

protected:
	static	ALife::EInfluenceType	get_indik_type( ALife::EHitType hit_type );

			void	Load_section_type	( ALife::EInfluenceType type, LPCSTR section );
			void	UpdateIndicatorType	( CActor* actor, ALife::EInfluenceType type );
			void	SwitchLA			( bool state, ALife::EInfluenceType type );

}; // class CUIHudStatesWnd
