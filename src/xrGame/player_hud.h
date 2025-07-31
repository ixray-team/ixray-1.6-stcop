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
	xr_vector<shared_str>	m_bone_parts;
	float					m_anim_speed;
	xr_vector<motion_descr>	m_animations;
};

struct player_hud_motion_container
{
	xr_vector<player_hud_motion>	m_anims;
	xr_hash_map<shared_str, bool>	m_names;
	player_hud_motion*				find_motion(const shared_str& name);
	bool		has_motion			(const shared_str& name);
	void		load				(IKinematicsAnimated* model, const shared_str& sect);
};

struct weapon_inertion
{
	struct base_params
	{
		Fvector position;
		Fvector rotation;
		void Load(const shared_str& section, const shared_str& str, bool is_16x9);
	};

	base_params move_suicide_offset;

	base_params move_to_crouch_offset;
	base_params move_from_crouch_offset;
	base_params move_to_slow_crouch_offset;
	base_params move_from_slow_crouch_offset;

	base_params move_to_rlookout_offset;
	base_params move_from_rlookout_offset;
	base_params move_to_llookout_offset;
	base_params move_from_llookout_offset;

	base_params aim_move_to_crouch_offset;
	base_params aim_move_from_crouch_offset;
	base_params aim_move_to_slow_crouch_offset;
	base_params aim_move_from_slow_crouch_offset;

	base_params aim_move_to_rlookout_offset;
	base_params aim_move_from_rlookout_offset;
	base_params aim_move_to_llookout_offset;
	base_params aim_move_from_llookout_offset;

	base_params move_rlookout_offset;
	base_params move_llookout_offset;

	base_params move_left_offset;
	base_params move_right_offset;
	base_params move_forward_offset;
	base_params move_back_offset;

	base_params move_crouch_offset;
	base_params move_slow_crouch_offset;

	base_params move_jump_offset;
	base_params move_fall_offset;
	base_params move_landing_offset;
	base_params move_landing2_offset;

	float move_rlookout_offset_speed_factor = 1.0f;
	float move_llookout_offset_speed_factor = 1.0f;

	float aim_move_slow_crouch_factor = 1.0f;
	float aim_move_crouch_factor = 1.0f;
	float aim_move_slow_factor = 1.0f;

	bool no_other_hud_moving_while_suicide = false;

	u32 to_crouch_time = 0;
	u32 from_crouch_time = 0;
	u32 to_slow_crouch_time = 0;
	u32 from_slow_crouch_time = 0;

	u32 to_rlookout_time = 0;
	u32 from_rlookout_time = 0;
	u32 to_llookout_time = 0;
	u32 from_llookout_time = 0;

	float move_weaponhide_factor = 1.0f;
	float move_unzoom_factor = 1.0f;

	float move_speed_pos = 0.1f;
	float move_speed_rot = 0.4f;

	float move_suicide_speed_pos = 0.2f;
	float move_suicide_speed_rot = 0.002f;

	float move_stabilize_factor = 2.0f;

	void Load(const shared_str& section, bool is_16x9);
};

struct hud_item_measures
{
	enum{e_fire_point=(1<<0), e_fire_point2=(1<<1), e_shell_point=(1<<2), e_16x9_mode_now=(1<<3)};
	Flags8							m_prop_flags;

	Fvector							m_item_attach[2];//pos,rot

	Fvector							m_hands_offset[2][3];//pos,rot/ normal,aim,GL

	struct inertion_params
	{
		float m_tendto_speed;
		float m_tendto_speed_aim;
		float m_tendto_ret_speed;
		float m_tendto_ret_speed_aim;
	};

	inertion_params m_inertion_params; //--#SM+#--
	weapon_inertion m_weapon_inertion;

	u16								m_fire_bone;
	Fvector							m_fire_point_offset;
	u16								m_fire_bone2;
	Fvector							m_fire_point2_offset;
	u16								m_shell_bone;
	Fvector							m_shell_point_offset;

	Fvector							m_hands_attach[2], m_hands_attach_real[2];//pos,rot

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
			
	u32 time_accumulator = 0;

	u32 tocrouch_time_remains = 0;
	u32 fromcrouch_time_remains = 0;
	u32 toslowcrouch_time_remains = 0;
	u32 fromslowcrouch_time_remains = 0;

	u32 torlookout_time_remains = 0;
	u32 fromrlookout_time_remains = 0;
	u32 tollookout_time_remains = 0;
	u32 fromllookout_time_remains = 0;

	void GetCurrentTargetOffset_aim(weapon_inertion& inertion_params, Fvector& pos, Fvector& rot, float& factor, u32& real);
	void GetCurrentTargetOffset(weapon_inertion& inertion_params, Fvector& pos, Fvector& rot, float& factor, u32& real);
	void AddOffsets(weapon_inertion::base_params& base, Fvector& pos, Fvector& rot, float koef = 1.0f);
	void AddSuicideOffset(weapon_inertion& inertion_params, const shared_str& section, Fvector& pos, Fvector& rot);

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
	void UpdateInertion				(u32 delta, CActor* actor);

	//hands bind position
	Fvector&						hands_attach_pos();
	Fvector&						hands_attach_rot();

	//hands runtime offset
	Fvector&						hands_offset_pos();
	Fvector&						hands_offset_rot();

//props
	u32								m_upd_firedeps_frame;
	void		tune				(Ivector values);
	void		anim_play			(const shared_str& item_anm_name, BOOL bMixIn, float speed);
	u32			anim_play			(const shared_str& anim_name, BOOL bMixIn, const CMotionDef*& md, u8& rnd);

};

struct animator_item
{
	player_hud* m_parent = nullptr;
	IKinematics* m_item = nullptr;

	Fmatrix m_attach_offset;
	Fmatrix m_item_transform;
	Fvector m_item_attach[2];
	Fvector m_hands_attach[2];
	u32	m_upd_firedeps_frame = u32(-1);
	bool IsPlaying = false;

	player_hud_motion_container	m_hand_motions;

	shared_str m_section;

	animator_item(player_hud* pParent, const shared_str& section);
	~animator_item();
	void update(bool bForce);
	void render();

	void anim_play(const shared_str& item_anm_name, BOOL bMixIn, float speed);
	u32 anim_play(const shared_str& anim_name, BOOL bMixIn, const CMotionDef*& md);
};

class player_hud
{
public: 
					player_hud			(bool invert = false);
					~player_hud			();
	void			load				(const shared_str& model_name);
	void			load_default		();
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

	void			ResetBlockedPartID(){m_blocked_part_idx=u16(-1); };
	void			SetBlockedPartID(u16 val){m_blocked_part_idx = val; }
	void			SetHandsVisible(bool val){m_bhands_visible=val;};
	bool			GetHandsVisible(){return m_bhands_visible;};

	void			UpdateWeaponOffset(u32 delta);

	IKinematics*	m_legs_model;
	bool			m_show_legs = true;
	bool			m_need_reload = true;

	IKinematicsAnimated* GetModel() { return m_model; }
	animator_item* create_animator_item(const shared_str& section);
	void			delete_animator_item();
	animator_item* GetAnimator() { return m_animator_item; }

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
	animator_item*						m_animator_item = nullptr;
	xr_vector<attachable_hud_item*>		m_pool;

	u16									m_blocked_part_idx;
	bool								m_bhands_visible;
	bool								m_binverted;
	int									item_idx_priority;
	void  LeftArmCallback(CBoneInstance* B);
};

extern player_hud* g_player_hud;