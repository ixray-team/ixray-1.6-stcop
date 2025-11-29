// GameObject.h: interface for the CGameObject class.
//
//////////////////////////////////////////////////////////////////////
#pragma once
#include "../xrEngine/xr_object.h"
#include "../xrCore/ECS/EntityOwner.h"
#include "../xrNetServer/NET_DeprecatedConstants.h"

#include "xrServer_Space.h"
#include "alife_space.h"
#include "UsableScriptObject.h"
#include "script_binder.h"
#include "Hit.h"
#include "game_object_space.h"

class CMincer;
class CZoneCampfire;
class CTorridZone;
class CAmebaZone;
class CHairsZone;
class CGraviZone;
class CBaseGraviZone;
class CMosquitoBald;
class CNoGravityZone;
class CVisualZone;
class CPhysicsShell;
class CSE_Abstract;
class CPHSynchronize;
class CScriptGameObject;
class CCar;
class CInventoryItem;
class CEntity;
class CEntityAlive;
class CInventoryOwner;
class CActor;
class CPhysicsShellHolder;
class IInputReceiver;
class CArtefact;
class CCreature;
class CAI_Stalker;
class CScriptEntity;
class CAI_ObjectLocation;
class CWeapon;
class CExplosive;
class CHolderCustom;
class CAttachmentOwner;
class CBaseMonster;
class CSpaceRestrictor;
class CAttachableItem;
class animation_movement_controller;
class CBlend;
class ai_obstacle;
class CMissile;
class CExplosiveRocket;
class CCustomRocket;
class CRocketLauncher;
class CGrenade;
class CUsableScriptObject;
class CBreakableObject;
class CHudItem;
class CCustomOutfit;
class CHelmet;
class CCustomDetector;
class CCustomDevice;
class CWeaponAmmo;
class CWeaponBinoculars;
class CWeaponKnife;
class CWeaponMagazined;
class CWeaponMagazinedWGrenade;
class CWeaponBM16;
class CWeaponRPG7;
class CWeaponRG6;
class CTorch;
class CBolt;
class CPda;
class CInventoryBox;
class CSpectator;
class CSilencer;
class CScope;
class CGrenadeLauncher;
class CProjector;
class CLevelChanger;
class CPhysicItem;
class CEatableItem;
class CScriptZone;
class CHelicopter;
class CHangingLamp;
class CPhraseDialogManager;
class CBackpack;
class CClimableObject;
class CPhysicObject;
class CTeamBaseZone;
class CWeaponShotgun;
class CWeaponAutomaticShotgun;

class IKinematics;
class CAI_Trader;

template <typename _return_type>
class CScriptCallbackEx;

class CGameObject : 
	public CObject, 
	public CUsableScriptObject,
	public CScriptBinder,
	public IECSOwner
{
	typedef CObject inherited;
protected:

	CAI_ObjectLocation				*m_ai_location;
	animation_movement_controller	*m_anim_mov_ctrl;
	ALife::_STORY_ID				m_story_id;
	bool							m_spawned;
	//время удаления объекта
	bool					m_bObjectRemoved;
	bool					m_bCrPr_Activated;
	u32						m_dwCrPr_ActivationStep;
	Flags32					m_server_flags;
	int						m_script_clsid;
	u32						m_spawn_time;
public:
	CGameObject();
	virtual ~CGameObject();
public:
	//functions used for avoiding most of the smart_cast
	virtual CGameObject*				cast_game_object			()						{return this;}
	virtual CAttachmentOwner*			cast_attachment_owner		()						{return nullptr;}
	virtual CInventoryOwner*			cast_inventory_owner		()						{return nullptr;}
	virtual CInventoryItem*				cast_inventory_item			()						{return nullptr;}
	virtual CEntity*					cast_entity					()						{return nullptr;}
	virtual CEntityAlive*				cast_entity_alive			()						{return nullptr;}
	virtual CActor*						cast_actor					()						{return nullptr;}
	virtual CAI_Trader*					cast_trader					()						{return nullptr;}
	virtual CPhysicsShellHolder*		cast_physics_shell_holder	()						{return nullptr;}
	virtual IInputReceiver*				cast_input_receiver			()						{return nullptr;}
	virtual CArtefact*					cast_artefact				()						{return nullptr;}
	virtual CCreature*				cast_creature			()						{return nullptr;}
	virtual CAI_Stalker*				cast_stalker				()						{return nullptr;}
	virtual CScriptEntity*				cast_script_entity			()						{return nullptr;}
	virtual CWeapon*					cast_weapon					()						{return nullptr;}
	virtual CExplosive*					cast_explosive				()						{return nullptr;}
	virtual CSpaceRestrictor*			cast_restrictor				()						{return nullptr;}
	virtual CAttachableItem*			cast_attachable_item		()						{return nullptr;}
	virtual CHolderCustom*				cast_holder_custom			()						{return nullptr;}
	virtual CBaseMonster*				cast_base_monster			()						{return nullptr;}
	virtual CCar*						cast_car					()						{return nullptr;}
	virtual CMissile					*cast_missile				()						{return nullptr;}
	virtual CExplosiveRocket			*cast_explosive_rocket		()						{return nullptr;}
	virtual CGrenade					*cast_grenade				()						{return nullptr;}
	virtual CUsableScriptObject			*cast_usable_script_object	()						{return this;}
	virtual CBreakableObject			*cast_breakable_object		()						{return nullptr;}
	virtual CHudItem					*cast_hud_item				()						{return nullptr;}
	virtual CCustomOutfit				*cast_outfit				()						{return nullptr;}
	virtual CHelmet						*cast_helmet				()						{return nullptr;}
	virtual CCustomDetector				*cast_custom_detector		()						{return nullptr;}
	virtual CCustomDevice				*cast_custom_device			()						{return nullptr;}
	virtual CWeaponAmmo					*cast_weapon_ammo			()						{return nullptr;}
	virtual CWeaponBinoculars			*cast_weapon_binoculars		()						{return nullptr;}
	virtual CWeaponKnife				*cast_weapon_knife			()						{return nullptr;}
	virtual CWeaponMagazined			*cast_weapon_magazined		()						{return nullptr;}
	virtual CWeaponMagazinedWGrenade	*cast_weapon_magazined_w_grenade()					{return nullptr;}
	virtual CWeaponBM16					*cast_weapon_bm16			()						{return nullptr;}
	virtual CWeaponRPG7					*cast_weapon_rpg7			()						{return nullptr;}
	virtual CWeaponRG6					*cast_weapon_rg6			()						{return nullptr;}
	virtual CTorch						*cast_torch					()						{return nullptr;}
	virtual CBolt* cast_bolt() { return nullptr; }
	virtual CPda* cast_pda() { return nullptr; }
	virtual CInventoryBox* cast_inventory_box() { return nullptr; }
	virtual CSpectator* cast_spectator() { return nullptr; }
	virtual CSilencer* cast_addon_silencer() { return nullptr; }
	virtual CScope* cast_addon_scope() { return nullptr; }
	virtual CGrenadeLauncher* cast_addon_grenade_launcher() { return nullptr; }
	virtual CProjector* cast_projector() { return nullptr; }
	virtual CLevelChanger* cast_level_changer() { return nullptr; }
	virtual CPhysicItem* cast_physics_item() { return nullptr; }
	virtual CEatableItem* cast_eatable_item() { return nullptr; }
	virtual CScriptZone* cast_script_zone() { return nullptr; }
	virtual CHelicopter* cast_helicopter() { return nullptr; }
	virtual CHangingLamp* cast_hanging_lamp() { return nullptr; }
	virtual CPhraseDialogManager* cast_phrase_dialog_manager() { return nullptr; }
	virtual CBackpack* cast_backpack() { return nullptr; }
	virtual CClimableObject* cast_climable_object() { return nullptr; }
	virtual CPhysicObject* cast_physics_object() { return nullptr; }
	virtual CTeamBaseZone* cast_team_base_zone() { return nullptr; }
	virtual CCustomRocket* cast_custom_rocket() { return nullptr; }
	virtual CRocketLauncher* cast_rocket_launcher() { return nullptr; }
	virtual CWeaponShotgun* cast_weapon_shotgun() { return nullptr; }
	virtual CWeaponAutomaticShotgun* cast_weapon_auto_shotgun() { return nullptr; }
	virtual CVisualZone* cast_visual_zone() { return nullptr; }
	virtual CNoGravityZone* cast_no_gravity_zone() { return nullptr; }
	virtual CMosquitoBald* cast_mosquito_bald_zone() { return nullptr; }
	virtual CBaseGraviZone* cast_base_gravi_zone() { return nullptr; }
	virtual CGraviZone* cast_gravi_zone() { return nullptr; }
	virtual CHairsZone* cast_hairs_zone() { return nullptr; }
	virtual CAmebaZone* cast_ameba_zone() { return nullptr; }
	virtual CTorridZone* cast_torrid_zone() { return nullptr; }
	virtual CZoneCampfire* cast_zone_campfire() { return nullptr; }
	virtual CMincer* cast_mincer_zone() { return nullptr; }
	virtual CRadioactiveZone* cast_radioactive_zone() { return nullptr; }
	virtual CAnomalyZone* cast_anomaly_zone() { return nullptr; }

public:
	virtual bool						feel_touch_on_contact	(CObject *)					{return true;}
	virtual bool						use						(CGameObject* who_use)		{return CUsableScriptObject::use(who_use);};

public:
	CInifile				*m_ini_file;

	// Utilities
	static void				u_EventGen			(NET_Packet& P, u32 type, ALife::_OBJECT_ID dest	);
	static void				u_EventSend			(NET_Packet& P, u32 dwFlags = DPNSEND_GUARANTEED	);
	
	// Methods
	virtual void			Load				(const char* section);
	bool			net_Spawn			(CSE_Abstract* DC) override;
	virtual void			net_Destroy			();
	virtual	void			net_Relcase			( CObject* O );	
	virtual void			UpdateCL			( );
	virtual void			OnChangeVisual		( );
	//object serialization
	virtual void			net_Save			(NET_Packet &net_packet);
	virtual void			net_Load			(IReader	&ireader);
	virtual void			net_Serialize		(ISaveObject& Object);
	virtual bool			net_SaveRelevant	();
	virtual void			save				(NET_Packet &output_packet);
	virtual void			load				(IReader &input_packet);
	virtual void			Serialize			(ISaveObject& Object);

	virtual bool			net_Relevant		()	{ return getLocal();	}	// send messages only if active and local
	virtual void			spatial_move		();
	virtual bool			Ready				()	{ return getReady();	}	// update only if active and fully initialized by/for network
//	virtual float			renderable_Ambient	();

	virtual void			shedule_Update		(u32 dt);	
	virtual bool			shedule_Needed		();
	virtual float			shedule_Scale_Base	();
	virtual void			renderable_Render	();
	virtual void			OnEvent				(NET_Packet& P, u16 type);
	virtual	void			Hit					(SHit* pHDS) {};
	virtual void			SetHitInfo				(CObject* who, CObject* weapon, s16 element, Fvector Pos, Fvector Dir)	{};
	virtual	bool			BonePassBullet		(u16 boneID) { return false; }


	//игровое имя объекта
	virtual const char*			Name                () const;
	
	//virtual void			OnH_A_Independent	();
	virtual void			OnH_B_Chield		();
	virtual void			OnH_B_Independent	(bool just_before_destroy);

	virtual bool			IsVisibleForZones	() { return true; }
///////////////////////////////////////////////////////////////////////
	virtual bool			NeedToDestroyObject	() const;
	virtual void			DestroyObject		();
///////////////////////////////////////////////////////////////////////

	// Position stack
	virtual	SavedPosition	ps_Element			(u32 ID) const;

			void			setup_parent_ai_locations(bool assign_position = true);
			void			validate_ai_locations(bool decrement_reference = true);

	//animation_movement_controller
	virtual	void			create_anim_mov_ctrl			( CBlend *b, Fmatrix *start_pose, bool local_animation  );
	virtual	void			destroy_anim_mov_ctrl			( );
			void			update_animation_movement_controller();
			bool			animation_movement_controlled	( ) const	;
const animation_movement_controller* animation_movement		( ) const	{ return	m_anim_mov_ctrl; }
	  animation_movement_controller* animation_movement		( )			{ return	m_anim_mov_ctrl; }
	// Game-specific events

	virtual bool			UsedAI_Locations				();
			bool			TestServerFlag					(u32 Flag) const;
	virtual	bool			can_validate_position_on_spawn	(){return true;}
#ifdef DEBUG_DRAW
	virtual void			OnRender			();
#endif

			void			init				();
	virtual	void			reinit				();
	virtual	void			reload				(const char* section);
	///////////////////// network /////////////////////////////////////////
	bool					object_removed		() const { return m_bObjectRemoved; };
public:
	virtual void			make_Interpolation	() {}; // interpolation from last visible to corrected position/rotation
	virtual void			PH_B_CrPr			() {}; // actions & operations before physic correction-prediction steps
	virtual void			PH_I_CrPr			() {}; // actions & operations after correction before prediction steps
#ifdef DEBUG
	virtual void			PH_Ch_CrPr			() {}; // 
	virtual	void			dbg_DrawSkeleton	();
#endif
	virtual void			PH_A_CrPr			() {}; // actions & operations after phisic correction-prediction steps
	virtual void			CrPr_SetActivationStep	(u32 Step)	{m_dwCrPr_ActivationStep = Step; };
	virtual u32				CrPr_GetActivationStep	()	{ return m_dwCrPr_ActivationStep; };
	virtual void			CrPr_SetActivated		(bool Activate)	{ m_bCrPr_Activated = Activate; };
	virtual bool			CrPr_IsActivated		()				{ return m_bCrPr_Activated; };
	///////////////////////////////////////////////////////////////////////
	virtual const SRotation	Orientation			() const
	{
		SRotation			rotation;
		float				h,p,b;
		XFORM().getHPB		(h,p,b);
		rotation.yaw		= h;
		rotation.pitch		= p;
		return				(rotation);
	};

	virtual bool			use_parent_ai_locations	() const
	{
		return				(true);
	}

public:
	typedef void  visual_callback(IKinematics *);
	typedef FixedVector<visual_callback*,6>			CALLBACK_VECTOR;
	typedef CALLBACK_VECTOR::iterator			CALLBACK_VECTOR_IT;

	CALLBACK_VECTOR			m_visual_callback;

public:
			void			add_visual_callback		(visual_callback *callback);
			void			remove_visual_callback	(visual_callback *callback);
			void			SetKinematicsCallback	(bool set);

	IC		CALLBACK_VECTOR &visual_callbacks	()
	{
		return				(m_visual_callback);
	}


private:
	mutable CScriptGameObject	*m_lua_game_object;
public:
			CScriptGameObject	*lua_game_object() const;
			int				clsid			() const
	{
		THROW				(m_script_clsid >= 0);
		return				(m_script_clsid);
	}
public:
	IC		CInifile		*spawn_ini			()
	{
		return				(m_ini_file);
	}
protected:
	virtual	void			spawn_supplies		();

public:
	IC		CAI_ObjectLocation	&ai_location		() const
	{
		VERIFY				(m_ai_location);
		return				(*m_ai_location);
	}

public:
	IC		u32				spawn_time			() const
	{
		VERIFY				(m_spawned);
		return				(m_spawn_time);
	}

	IC		const ALife::_STORY_ID &story_id	() const
	{
		return				(m_story_id);
	}
	
	virtual void FootStepCallback(float power, bool b_play, bool b_on_ground, bool b_hud_view);

public:
	virtual u32				ef_creature_type	() const;
	virtual u32				ef_equipment_type	() const;
	virtual u32				ef_main_weapon_type	() const;
	virtual u32				ef_anomaly_type		() const;
	virtual u32				ef_weapon_type		() const;
	virtual u32				ef_detector_type	() const;
	virtual bool			natural_weapon		() const {return true;}
	virtual bool			natural_detector	() const {return true;}
	virtual bool			use_center_to_aim	() const {return false;}
	// [12.11.07] Alexander Maniluk: added this method for moving object
	virtual void MoveTo(Fvector const & position) {};

public:
	
	typedef CScriptCallbackEx<void> CScriptCallbackExVoid;

private:
	using CALLBACK_MAP = xr_map<GameObject::ECallbackType, CScriptCallbackExVoid>;
	using CALLBACK_MAP_IT = CALLBACK_MAP::iterator;

	CALLBACK_MAP			*m_callbacks;

public:
	CScriptCallbackExVoid	&callback			(GameObject::ECallbackType type) const;
	virtual	const char*			visual_name			(CSE_Abstract *server_entity);

	virtual	void			On_B_NotCurrentEntity () {};

	// for moving objects
private:
			u32				new_level_vertex_id	() const;
			void			update_ai_locations	(bool decrement_reference);

private:
	ai_obstacle				*m_ai_obstacle;
	Fmatrix					m_previous_matrix;

public:
	virtual	bool			is_ai_obstacle		() const;

public:
	IC		ai_obstacle		&obstacle			() const
	{
		VERIFY				(m_ai_obstacle);
		return				(*m_ai_obstacle);
	}

	virtual void			on_matrix_change	(const Fmatrix &previous);
};