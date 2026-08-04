#pragma once

#include "../../xrUI/Widgets/UIWindow.h"
#include "../../xrEngine/AI/alife_space.h"
#include "../../xrServerEntities/inventory_space.h"
#include "../actor_defs.h"

class CUIStatic;
class CUI3dStatic;
class CUIProgressBar;
class CUIProgressShape;
class CUIXml;
class CUIArrow;
class CActor;
class CUIStackPanel;
class CInventoryItem;
class CUIHudGroupCatalog;

int const it_max = ALife::infl_max_count - 1;

class CUIHudStatesWnd final : public CUIWindow
{
private:
	enum class EWpnIconHudMode : u8
	{
		Legacy,
		AmmoText,
		Caliber,
	};

	typedef CUIWindow						inherited;
//-	typedef ALife::EInfluenceType	EIndicatorType;

	CUIStatic*			m_back_v = nullptr;
	CUIStatic*			m_back_over_arrow = nullptr;
	CUIStatic*			m_bleeding = nullptr;

	CUIStatic*			m_resist_back[it_max];
	CUIStatic*			m_indik[it_max];
	CUIStatic*			m_ind_starvation = nullptr;
	CUIStatic*			m_resist_back_starvation = nullptr;

	CUIProgressShape*	m_progress_self = nullptr;
	CUIArrow*			m_arrow = nullptr;
	CUIArrow*			m_arrow_shadow = nullptr;

	float				m_last_health;
	float				m_health_blink;

	float				m_radia_self;
//	float				m_actor_radia_factor;
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
	bool				m_weapon_icon_show_weapon_name = false;
	bool				m_use_adaptive_ammo_widget = false;
	bool				m_use_fire_mode_icons = false;
	bool				m_use_fire_mode_text_labels = false;
	EWpnIconHudMode		m_wpnIconHudMode = EWpnIconHudMode::Legacy;

	bool				m_isZoneTouch = false;
	const char*			m_onZoneTouch = {};

	CUIStatic*			m_ui_adaptive_clip = nullptr;
	CUIStatic*			m_ui_adaptive_total = nullptr;
	string32			m_adaptive_total_separator = "/";
	CUIStatic*			m_ui_fire_mode_icon = nullptr;
	CUIStatic*			m_ui_caliber_text = nullptr;
	CUIStatic*			m_ui_caliber_icon = nullptr;

	CUIHudGroupCatalog*		m_hud_group_catalog = nullptr;

	xr_map<shared_str, shared_str>	m_fire_mode_icon_map;
	xr_map<shared_str, shared_str>	m_fire_mode_label_map;

	CUIStackPanel*		UIStackPanelDangers;

	struct SContextualColorCache
	{
		u32 texture = color_rgba(255, 255, 255, 255);
		u32 text = color_rgba(255, 255, 255, 255);
	};

	// Contextual display (health/stamina + weapon blocks)
	bool				m_health_context_active = false;
	bool				m_weapon_context_active = false;
	float				m_health_context_last_time = 0.f;
	float				m_weapon_context_last_time = 0.f;
	float				m_health_block_alpha = 0.f;
	float				m_weapon_block_alpha = 0.f;
	float				m_context_show_speed = 4.f;
	float				m_context_hide_speed = 3.f;
	float				m_context_hide_delay = 2.5f;
	float				m_context_health_threshold = 0.002f;
	float				m_context_stamina_for_track = -1.f;
	shared_str			m_context_active_item_sect;
	shared_str			m_context_fire_mode;
	string64			m_context_ammo_signature = {};
	u8					m_context_weapon_state = 0xff;
	bool				m_contextual_was_enabled = false;
	u32					m_back_base_color = color_rgba(255, 255, 255, 255);
	u32					m_bleeding_base_color = color_rgba(255, 255, 255, 255);
	SContextualColorCache m_cache_health_progress;
	SContextualColorCache m_cache_health_background;
	SContextualColorCache m_cache_stamina_progress;
	SContextualColorCache m_cache_stamina_background;
	SContextualColorCache m_cache_armor_progress;
	SContextualColorCache m_cache_armor_background;
	SContextualColorCache m_cache_static_health;
	SContextualColorCache m_cache_static_armor;
	SContextualColorCache m_cache_static_weapon;
	SContextualColorCache m_cache_cur_ammo;
	SContextualColorCache m_cache_fmj_ammo;
	SContextualColorCache m_cache_ap_ammo;
	SContextualColorCache m_cache_third_ammo;
	SContextualColorCache m_cache_sign_ammo;
	SContextualColorCache m_cache_adaptive_clip;
	SContextualColorCache m_cache_adaptive_total;
	SContextualColorCache m_cache_fire_mode;
	SContextualColorCache m_cache_fire_mode_icon;
	SContextualColorCache m_cache_caliber_text;
	SContextualColorCache m_cache_caliber_icon;
	SContextualColorCache m_cache_weapon_icon;
	SContextualColorCache m_cache_grenade;

public:
	float				m_radia_hit;

	CUIStatic*			m_ui_weapon_cur_ammo = nullptr;
	CUIStatic*			m_ui_weapon_fmj_ammo = nullptr;
	CUIStatic*			m_ui_weapon_ap_ammo = nullptr;
	CUIStatic*			m_fire_mode = nullptr;
	CUIStatic*			m_ui_grenade = nullptr;
	II_BriefInfo		m_item_info;
	CUIStatic*			m_radia_damage = nullptr;
	Frect				m_ui_weapon_icon_rect;
	CUIStatic*			m_ui_weapon_sign_ammo = nullptr;
	CUIStatic*			m_static_armor = nullptr;
	CUIStatic*			m_static_health = nullptr;
	CUIStatic*			m_static_weapon = nullptr;

	CUIProgressBar*		m_ui_health_bar = nullptr;
	CUIProgressBar*		m_ui_armor_bar = nullptr;
	CUIProgressBar*		m_ui_stamina_bar = nullptr;
	CUI3dStatic*		m_ui_weapon_icon = nullptr;
	CUIStatic*			m_back = nullptr;
	CUIStatic*			m_ui_weapon_third_ammo = nullptr; //Alundaio

	u32					m_ui_weapon_ammo_color_active;
	u32					m_ui_weapon_ammo_color_inactive;

					CUIHudStatesWnd		();
	virtual			~CUIHudStatesWnd	();

			void	InitFromXml			( CUIXml& xml, const char* path );
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

			void	Load_section_type	( ALife::EInfluenceType type, const char* section );
			void	UpdateIndicatorType	( CActor* actor, ALife::EInfluenceType type );
			void	SwitchLA			( bool state, ALife::EInfluenceType type );

			void	HideCaliberHudWidgets	();
			void	UpdateCaliberHudForItem	( CInventoryItem* item );
			shared_str ResolveFireModeDisplayText(const shared_str& fireModeCode) const;

			bool	IsContextualDisplayEnabled	() const;
			void	LoadContextualDisplaySettings(CUIXml& xml, const char* path);
			void	TriggerHealthContext		();
			void	TriggerWeaponContext		();
			void	UpdateContextualTriggers	(CActor* actor);
			void	TickContextualDisplay		();
			void	SyncDynamicWeaponColorCaches();
			void	CaptureStaticColorCache		(CUIStatic* wnd, SContextualColorCache& cache) const;
			void	CaptureProgressColorCache	(CUIProgressBar* bar, SContextualColorCache& progress, SContextualColorCache& background) const;
			void	CaptureContextualBaseColors	();
			void	ApplyStaticFromColorCache	(CUIStatic* wnd, const SContextualColorCache& cache, float alpha) const;
			void	ApplyProgressFromColorCache	(CUIProgressBar* bar, const SContextualColorCache& progress, const SContextualColorCache& background, float alpha) const;
			void	RestoreContextualColorsFromCache();
			void	ApplyContextualAlpha		();

}; // class CUIHudStatesWnd
