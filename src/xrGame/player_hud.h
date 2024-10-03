#pragma once
#include "firedeps.h"

#include "../Include/xrRender/Kinematics.h"
#include "../Include/xrRender/KinematicsAnimated.h"
#include "actor_defs.h"

class player_hud;
class CHudItem;
class CMotionDef;

struct motion_descr
{
	MotionID		mid;
	shared_str		name;
};

struct player_hud_motion
{
	shared_str				m_alias_name;
	shared_str				m_base_name;
	shared_str				m_additional_name;
	float					m_anim_speed;
	xr_vector<motion_descr>	m_animations;
};

struct player_hud_motion_container
{
	xr_vector<player_hud_motion>	m_anims;
	player_hud_motion*				find_motion(const shared_str& name);
	void		load				(IKinematicsAnimated* model, const shared_str& sect);
};

struct hud_item_measures
{
	enum{e_fire_point=(1<<0), e_fire_point2=(1<<1), e_shell_point=(1<<2), e_16x9_mode_now=(1<<3)};
	Flags8							m_prop_flags;

	Fvector							m_item_attach[2];//pos,rot

	Fvector							m_hands_offset[2][3];//pos,rot/ normal,aim,GL
	Fvector							m_strafe_offset[4][2]; // pos,rot,data1,data2/ normal,aim-GL	 --#SM+#--

	struct inertion_params
	{
		float m_tendto_speed;
		float m_tendto_speed_aim;
		float m_tendto_ret_speed;
		float m_tendto_ret_speed_aim;

		float m_min_angle;
		float m_min_angle_aim;

		Fvector4 m_offset_LRUD;
		Fvector4 m_offset_LRUD_aim;
	};

	inertion_params m_inertion_params; //--#SM+#--

	u16								m_fire_bone;
	Fvector							m_fire_point_offset;
	u16								m_fire_bone2;
	Fvector							m_fire_point2_offset;
	u16								m_shell_bone;
	Fvector							m_shell_point_offset;

	Fvector							m_hands_attach[2];//pos,rot

	void load						(const shared_str& sect_name, IKinematics* K);
};

struct attachable_hud_item
{
	player_hud*						m_parent;
	CHudItem*						m_parent_hud_item;
	shared_str						m_sect_name;
	IKinematics*					m_model;
	u16								m_attach_place_idx;
	hud_item_measures				m_measures;

	//runtime positioning
	Fmatrix							m_attach_offset;
	Fmatrix							m_item_transform;

	player_hud_motion_container		m_hand_motions;
			
			attachable_hud_item		(player_hud* pparent):m_parent(pparent),m_upd_firedeps_frame(u32(-1)),m_parent_hud_item(NULL){}
			~attachable_hud_item	();
	void load						(const shared_str& sect_name);
	void update						(bool bForce);
	void update_hud_additional		(Fmatrix& trans);
	void setup_firedeps				(firedeps& fd);
	void render						();	
	void render_item_ui				();
	bool render_item_ui_query		();
	bool need_renderable			();
	void set_bone_visible			(const shared_str& bone_name, BOOL bVisibility, BOOL bSilent=FALSE);
	void debug_draw_firedeps		();

	//hands bind position
	Fvector&						hands_attach_pos();
	Fvector&						hands_attach_rot();

	//hands runtime offset
	Fvector&						hands_offset_pos();
	Fvector&						hands_offset_rot();

	void							set_hands_offset_pos(Fvector3 offset);
	void							set_hands_offset_rot(Fvector3 offset);

//props
	u32								m_upd_firedeps_frame;
	void		tune				(Ivector values);
	void		anim_play			(const shared_str& item_anm_name, BOOL bMixIn, float speed);
	u32			anim_play			(const shared_str& anim_name, BOOL bMixIn, const CMotionDef*& md, u8& rnd);

};

class player_hud
{
public: 
					player_hud			(bool invert = false);
					~player_hud			();
	void			load				(const shared_str& model_name);
	void			load_default		(){load("actor_hud");};
	void			update				(const Fmatrix& trans);
	void			render_hud			();	
	void			render_item_ui		();
	bool			render_item_ui_query();

	u32				anim_play			(u16 part, const MotionID& M, BOOL bMixIn, const CMotionDef*& md, float speed);
	bool			check_anim			(const shared_str& anim_name, u16 place_idx);

	bool			animator_play			(const shared_str& anim_name, u16 place_idx = u16(-1), u16 part_id = u16(-1), BOOL bMixIn = FALSE, float speed = 1.0f, u8 anm_idx = u8(0), bool impact_on_item = false, bool similar_check = false, PlayCallback Callback = PlayCallback(0), LPVOID CallbackParam = LPVOID(0), BOOL UpdateCallbackType = 0);
	void			animator_fx_play		(const shared_str& anim_name, u16 place_idx = u16(-1), u16 part_id = u16(-1), u8 anm_idx = u8(0), float blendAccrue = 1.f, float blendFalloff = 1.f, float Speed = 1.f, float Power = 1.f);

	const shared_str& section_name		() const {return m_sect_name;}

	attachable_hud_item* create_hud_item(const shared_str& sect);
	void			RemoveHudItem		(const shared_str& sect);
	void			attach_item			(CHudItem* item);
	bool			allow_activation	(CHudItem* item);
	attachable_hud_item* attached_item	(u16 item_idx)	{return m_attached_items[item_idx];};
	void			detach_item_idx		(u16 idx);
	void			detach_item			(CHudItem* item);
	void			detach_all_items	(){m_attached_items[0]=NULL; m_attached_items[1]=NULL;};

	void			calc_transform		(u16 attach_slot_idx, const Fmatrix& offset, Fmatrix& result);
	void			tune				(Ivector values);
	u32				motion_length		(const MotionID& M, const CMotionDef*& md, float speed);
	u32				motion_length		(const shared_str& anim_name, const shared_str& hud_name, const CMotionDef*& md);
	void			OnMovementChanged	(ACTOR_DEFS::EMoveCommand cmd)	;
	void			RestoreHandBlends(LPCSTR ignored_part);

	struct default_hud_coords_params
	{
		shared_str hud_sect;
		Fvector3 hands_position = {0,0,0};
		Fvector3 hands_orientation = {0,0,0};
		bool is16x9 = true;
	};

	struct cached_cfg_param_float
	{
		shared_str last_section;
		float value = 0.f;
		bool is_default = true;
	};

	void			ResetBlockedPartID(){m_blocked_part_idx=u16(-1); };
	void			SetHandsVisible(bool val){m_bhands_visible=val;};
	bool			GetHandsVisible(){return m_bhands_visible;};
	void			UpdateWeaponOffset(u32 delta);
	default_hud_coords_params GetDefaultHudCoords(shared_str hud_sect);
	default_hud_coords_params _last_default_hud_params;
	float GetCachedCfgParamFloatDef(cached_cfg_param_float& cached, const shared_str section, const shared_str key, float def);
	void GetCurrentTargetOffset_aim(shared_str section, Fvector3& pos, Fvector3& rot, float& factor);
	void GetCurrentTargetOffset(shared_str section, Fvector3& pos, Fvector3& rot, float& factor);
	void AddOffsets(const xr_string base, shared_str section, Fvector3& pos, Fvector3& rot, float koef = 1.0f);
	void AddSuicideOffset(shared_str section, Fvector3& pos, Fvector3& rot);
	void ResetItmHudOffset(CHudItem* itm);

	cached_cfg_param_float cached_hud_move_weaponhide_factor;
	cached_cfg_param_float cached_hud_move_unzoom_factor;

	cached_cfg_param_float cached_hud_move_speed_rot;
	cached_cfg_param_float cached_hud_move_speed_pos;

	cached_cfg_param_float cached_suicide_speed_rot;
	cached_cfg_param_float cached_suicide_speed_pos;

	cached_cfg_param_float cached_hud_move_stabilize_factor;
	cached_cfg_param_float cached_hud_move_slow_crouch_factor;
	cached_cfg_param_float cached_hud_move_crouch_factor;
	cached_cfg_param_float cached_hud_move_slow_factor;

	u32 time_accumulator;

	u32 tocrouch_time_remains;
	u32 fromcrouch_time_remains;
	u32 toslowcrouch_time_remains;
	u32 fromslowcrouch_time_remains;

	u32 torlookout_time_remains;
	u32 fromrlookout_time_remains;
	u32 tollookout_time_remains;
	u32 fromllookout_time_remains;

	IKinematics* m_legs_model;
private:
	void			update_inertion		(Fmatrix& trans);
	void			update_additional	(Fmatrix& trans);
private:
	const Fvector&	attach_rot			() const;
	const Fvector&	attach_pos			() const;

	shared_str							m_sect_name;

	Fmatrix								m_attach_offsetr, m_attach_offsetl;

	Fmatrix								m_transform;
	Fmatrix								m_transformL;

	IKinematicsAnimated*				m_model;
	xr_vector<u16>						m_ancors;
	attachable_hud_item*				m_attached_items[2];
	xr_vector<attachable_hud_item*>		m_pool;

	u16									m_blocked_part_idx;
	bool								m_bhands_visible;
	bool								m_binverted;
	int									item_idx_priority;
	void  LeftArmCallback(CBoneInstance* B);
};

extern player_hud* g_player_hud;