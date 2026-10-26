// Creature.h: interface for the CCreature class.
//
//////////////////////////////////////////////////////////////////////

#pragma once

#include "entity_alive.h"
#include "script_entity.h"
#include "../xrEngine/Feel_Vision.h"
#include "../xrEngine/Feel_Sound.h"
#include "../xrEngine/Feel_Touch.h"
#include "../Include/xrRender/Kinematics.h"
#include "../xrCore/Containers/associative_vector.h"
#include "trajectories.h"


namespace MonsterSpace {
	struct SBoneRotation;
};

class CMotionDef;
class IKinematicsAnimated;
class CMemoryManager;
class CItemManager;
class CEnemyManager;
class CDangerManager;
class CMovementManager;
class CSoundPlayer;
class CAI_Stalker;
class CDangerObject;
class moving_object;

class CCreature : 
	public CEntityAlive, 
	public CScriptEntity,
	public Feel::Vision,
	public Feel::Sound,
	public Feel::Touch
{
protected:
	typedef	CEntityAlive	inherited;

private:
	CMemoryManager		*m_memory_manager;
	CMovementManager	*m_movement_manager;
	CSoundPlayer		*m_sound_player;

private:
	u32					m_client_update_delta;
	u32					m_last_client_update_time;

protected:
	
	struct				SAnimState
	{
		MotionID		fwd;
		MotionID		back;
		MotionID		ls;
		MotionID		rs;

		void			Create(IKinematicsAnimated* K, const char* base);
	};

private:
	xr_vector<CLASS_ID>	m_killer_clsids;

public:
	// Eyes
	Fmatrix				eye_matrix;
	int					eye_bone;
	float				eye_fov;
	float				eye_range;

	float				m_fCurSpeed;

	u32					eye_pp_stage;
	u32					eye_pp_timestamp;
	Fvector				m_tEyeShift;
	float				m_fEyeShiftYaw;
	bool				NET_WasExtrapolating;

	Fvector				tWatchDirection;

	virtual void		Think() = 0;

	float				m_fTimeUpdateDelta;
	u32					m_dwLastUpdateTime;
//	Fmatrix				m_tServerTransform;
	
	u32					m_dwCurrentTime;		// time updated in UpdateCL 

	struct net_update	{
		u32				dwTimeStamp;			// server(game) timestamp
		float			o_model;				// model yaw
		SRotation		o_torso;				// torso in world coords
		Fvector			p_pos;					// in world coords
		float			fHealth;

		// non-exported (temporal)

		net_update()	{
			dwTimeStamp		= 0;
			o_model			= 0;
			o_torso.yaw		= 0;
			o_torso.pitch	= 0;
			p_pos.set		(0,0,0);
			fHealth			= 0.f;
		}
		void	lerp	(net_update& A,net_update& B, float f);
	};
	xr_deque<net_update>	NET;
	net_update				NET_Last;
	bool					NET_WasInterpolating;	// previous update was by interpolation or by extrapolation
	u32						NET_Time;				// server time of last update
//------------------------------

	virtual bool		feel_touch_on_contact	(CObject *) override;
	virtual bool		feel_touch_contact		(CObject *) override;
	// utils
	void				mk_orientation			( Fvector& dir, Fmatrix& mR );
	void				mk_rotation				( Fvector& dir, SRotation &R);

	// stream executors
	virtual void		Exec_Action				( float dt );
	virtual void		Exec_Look				( float dt );
	void		Exec_Visibility			( );
	virtual void		eye_pp_s0				( );
	void				eye_pp_s1				( );
	void				eye_pp_s2				( );

	virtual void		UpdateCamera			( );

public:
						CCreature			( );
	virtual ~CCreature			( ) override;

public:
	virtual CEntityAlive*				cast_entity_alive		() override {return this;}
	virtual CEntity*					cast_entity				() override {return this;}

public:
	virtual DLL_Pure	*_construct				() override;
	virtual bool		net_Spawn				( CSE_Abstract* DC) override;
	virtual void		Die						( CObject* who) override;

	virtual void		HitSignal				( float P,	Fvector& vLocalDir, CObject* who);
	virtual void		shedule_Update					( u32		DT		) override;
	virtual void		UpdateCL				( ) override;

	// Network
	virtual void		net_Export				(NET_Packet& P) override;				// export to server
	virtual void		net_Import				(NET_Packet& P) override;				// import from server
	virtual void		net_Relcase				(CObject*	 O) override;

	virtual void		SelectAnimation			( const Fvector& _view, const Fvector& _move, float speed ) = 0;

	virtual bool		is_base_monster_with_enemy	() { return false; }

	// debug
#ifdef DEBUG_DRAW
	virtual void		OnRender				( ) override;
	virtual void		OnHUDDraw				(CCustomHUD* hud) override;
#endif

	virtual bool		bfExecMovement			(){return(false);};


	IC	bool					angle_lerp_bounds		(float &a, float b, float c, float d);
	IC	void					vfNormalizeSafe			(Fvector& Vector);

public:
	virtual float				ffGetFov				()	const override {return eye_fov;}
	virtual float				ffGetRange				()	const override {return eye_range;}
			void				set_fov					(float new_fov);
			void				set_range				(float new_range);
//	virtual	void				feel_touch_new			(CObject	*O);
	virtual bool				feel_visible_isRelevant	(CObject		*O);
	virtual Feel::Sound*		dcast_FeelSound			() override { return this;	}
	virtual void				Hit						(SHit* pHDS) override;

	virtual void				OnEvent					( NET_Packet& P, u16 type		) override;
	virtual void				net_Destroy				() override;
	virtual bool				UsedAI_Locations		() override;
	///////////////////////////////////////////////////////////////////////
	virtual u16					PHGetSyncItemsNumber	() override {return inherited ::PHGetSyncItemsNumber();}
	virtual CPHSynchronize*		PHGetSyncItem			(u16 item) override {return inherited ::PHGetSyncItem(item);}
	virtual void				PHUnFreeze				() override {return inherited ::PHUnFreeze();}
	virtual void				PHFreeze				() override {return inherited ::PHFreeze();}
	///////////////////////////////////////////////////////////////////////
public:
	virtual void				Load					(const char*	section) override;
	virtual void				reinit					() override;
	virtual void				reload					(const char*	section) override;
	virtual const SRotation		Orientation				() const override;
	virtual float				get_custom_pitch_speed	(float def_speed) {return def_speed;}

	virtual bool				human_being				() const override
	{
		return					(false);
	}
	virtual	void				PitchCorrection			();

	virtual void				save					(NET_Packet &output_packet) override;
	virtual void				load					(IReader &input_packet) override;
	virtual void Serialize(ISaveObject& Object) override;
	virtual bool				net_SaveRelevant		()	override						{return inherited::net_SaveRelevant();}
	
	virtual	const MonsterSpace::SBoneRotation &head_orientation	() const;
	
	virtual void				UpdatePositionAnimation	();
	virtual void				set_ready_to_save		() override;
	virtual CPhysicsShellHolder*cast_physics_shell_holder	() override {return this;}
	virtual CCreature*		cast_creature		() override {return this;}
	virtual CScriptEntity*		cast_script_entity		() override {return this;}
	virtual CGameObject*		cast_game_object		() override	{return this;}

			void				load_killer_clsids		(const char* section);
			bool				is_special_killer		(CObject *obj);

	IC		CMemoryManager		&memory					() const;
	CMemoryManager*				GetMemoryManager		() const { return m_memory_manager; }
	virtual float				feel_vision_mtl_transp	(const collide::rq_result& result) override;
	virtual void				feel_sound_new			(CObject* who, int type, CSound_UserDataPtr user_data, const Fvector &Position, float power) override;

	virtual bool				useful					(const CItemManager *manager, const CGameObject *object) const;
	virtual float				evaluate				(const CItemManager *manager, const CGameObject *object) const;
	virtual bool				useful					(const CEnemyManager *manager, const CEntityAlive *object) const;
	virtual float				evaluate				(const CEnemyManager *manager, const CEntityAlive *object) const;
	virtual bool				useful					(const CDangerManager *manager, const CDangerObject &object) const;
	virtual float				evaluate				(const CDangerManager *manager, const CDangerObject &object) const;

protected:
	float						m_panic_threshold;

public:
	IC		float				panic_threshold			() const;


private:
	CSound_UserDataVisitor			*m_sound_user_data_visitor;

protected:
	virtual CSound_UserDataVisitor	*create_sound_visitor	();
	virtual CMemoryManager			*create_memory_manager	();
	virtual CMovementManager		*create_movement_manager();

public:
	IC		CMovementManager		&movement				() const;
	IC		CSoundPlayer			&sound					() const;
	IC		CSound_UserDataVisitor	*sound_user_data_visitor() const;

protected:
	float							m_far_plane_factor;
	float							m_fog_density_factor;

public:
	virtual	void					update_range_fov		(float &new_range, float &new_fov, float start_range, float start_fov);

public:
			void 			update_sound_player		();
	virtual	void					on_restrictions_change	();

	virtual bool					should_wait_to_use_corspe_visual () { return true; }
	virtual const char*					visual_name				(CSE_Abstract *server_entity) override;

private:
	bool							m_already_dead;

public:
	IC		const bool				&already_dead			() const {return (m_already_dead);};
	virtual void					on_enemy_change			(const CEntityAlive *enemy);
	virtual CVisualMemoryManager	*visual_memory			() const override;

public:
	IC		float					client_update_fdelta	() const;
	IC		const u32				&client_update_delta	() const;
	IC		const u32				&last_client_update_time() const;

	//////////////////////////////////////////////////////////////////////////
	// Critical Wounds
	//////////////////////////////////////////////////////////////////////////
public:
	typedef u32											CriticalWoundType;
private:
	typedef associative_vector<u16,CriticalWoundType>	BODY_PART;

protected:
	u32								m_last_hit_time;
	float							m_critical_wound_threshold;
	float							m_critical_wound_decrease_quant;
	float							m_critical_wound_accumulator;
	CriticalWoundType				m_critical_wound_type;
	BODY_PART						m_bones_body_parts;

protected:
	virtual void					load_critical_wound_bones					() {}

	virtual bool					critical_wound_external_conditions_suitable	() {return true;}
	virtual void					critical_wounded_state_start				() {}

			bool					update_critical_wounded						(const u16 &bone_id, const float &power);

public:
	IC		void					critical_wounded_state_stop					();
public:
	IC		bool					critically_wounded							();
	IC	const u32					&critical_wound_type						() const;

	//////////////////////////////////////////////////////////////////////////
private:
	bool							m_invulnerable;

public:
	IC		void					invulnerable								(const bool &invulnerable);
	IC		bool					invulnerable								() const;

private:
	moving_object					*m_moving_object;

public:
	IC			moving_object		*get_moving_object	() const;
	virtual void				spatial_move		() override;
	virtual Fvector				predict_position	(const float &time_to_check) const override;
	virtual Fvector				target_position		() const override;

protected:
	bool	m_update_rotation_on_frame;

private:
	bool	m_movement_enabled_before_animation_controller;

public:
	virtual void					create_anim_mov_ctrl	( CBlend *b, Fmatrix *start_pose, bool local_animation  ) override;
	virtual void					destroy_anim_mov_ctrl	( ) override;
	virtual void					ForceTransform			( Fmatrix const& m ) override;
	
public:
	virtual Fvector					spatial_sector_point	( ) override;

#ifdef DEBUG
	xr_vector<trajectory_pick>			m_jump_picks;
	xr_vector<Fvector>				m_jump_collide_tris;
#endif // #ifdef DEBUG
};

#include "Creature_inline.h"