#pragma once
#include "PHCharacter.h"
#include "Physics.h"
#include "MathUtils.h"
#include "ElevatorState.h"
#include "IColisiondamageInfo.h"
#include "../xrEngine/GameMtlLib.h"
namespace ALife {
	enum EHitType;
};
#ifdef DEBUG
#include "debug_output.h"
#endif
class ICollisionHitCallback;

class CPHSimpleCharacter : 
	public CPHCharacter,
	private ICollisionDamageInfo
{
	typedef CPHCharacter	inherited;
private:
	
	collide::rq_results RQR;

protected:
	CElevatorState m_elevator_state;

public:
	////////////////////////////damage////////////////////////////////////////
	struct SCollisionDamageInfo
	{
										SCollisionDamageInfo		()										;
				void					Construct					()										;
				float					ContactVelocity				()				const					;
				void					HitDir						(Fvector &dir)	const					;
			IC	const Fvector&			HitPos						()				const					{return cast_fv(m_damege_contact.geom.pos);}
				void					Reinit						()										;
				dContact				m_damege_contact;
				ICollisionHitCallback	*m_hit_callback;
				ALife::_OBJECT_ID m_obj_id;
				float					m_dmc_signum;
				enum{ctStatic,ctObject}	m_dmc_type;
				ALife::EHitType			m_hit_type;
				bool					is_initiated;
		mutable	float					m_contact_velocity;
	};
#ifdef	DEBUG
	SCollisionDamageInfo& dbg_get_collision_dmg_info(){ return m_collision_damage_info ;}
#endif

protected:
	SCollisionDamageInfo		m_collision_damage_info;
	ObjectContactCallbackFun*	m_object_contact_callback;
	////////////////////////// geometry
	Fvector m_last_move;
	dGeomID m_geom_shell;
	dGeomID m_wheel;
	dGeomID m_hat;
	dGeomID	m_cap;

	dGeomID m_hat_transform;
	dGeomID m_wheel_transform;
	dGeomID m_shell_transform;
	dGeomID m_cap_transform;

	dSpaceID m_space;
	
	dReal m_radius;
	dReal m_cyl_hight;

	////////////////////////// movement
	dVector3 m_control_force;
	Fvector	 m_acceleration;	
	Fvector  m_cam_dir;
	dVector3 m_wall_contact_normal;
	dVector3 m_ground_contact_normal;
	dVector3 m_clamb_depart_position;
	dVector3 m_depart_position;
	dVector3 m_wall_contact_position;
	dVector3 m_ground_contact_position;
	dReal	 jump_up_velocity;
	dReal	 m_collision_damage_factor;
	dReal	 m_max_velocity;

	float   m_air_control_factor;

	dVector3 m_jump_depart_position;
	dVector3 m_death_position;
	Fvector  m_jump_accel;

	Fvector  m_last_environment_update;
	u16		 m_last_picked_material;

	//movement state
	bool is_contact;
	bool was_contact;
	bool b_depart;
	bool b_meet;
	bool b_side_contact;
	bool b_was_side_contact;
	bool b_any_contacts;
	bool b_air_contact_state;

	bool b_valide_ground_contact;
	bool b_valide_wall_contact;
	bool b_on_object;
	bool b_was_on_object;
	bool b_on_ground;
	bool b_lose_ground;
	bool b_collision_restrictor_touch;
	u32  m_contact_count;

	bool	is_control;
	bool	b_meet_control;
	bool	b_lose_control;
	bool	was_control;
	bool	b_stop_control;
	bool	b_depart_control;
	bool	b_jump;
	bool	b_jumping;
	bool	b_clamb_jump;
	bool	b_external_impulse;
	u64		m_ext_impuls_stop_step;
	Fvector m_ext_imulse;
	bool	b_death_pos;
	bool	b_foot_mtl_check;
	dReal	m_friction_factor;
	bool	b_non_interactive;
public:
							CPHSimpleCharacter					()									;
	virtual	~CPHSimpleCharacter					() override {Destroy();}

	/////////////////CPHObject//////////////////////////////////////////////
	virtual	void		PhDataUpdate						(dReal step) override;
	virtual	void		PhTune								(dReal step) override;
	virtual	void		InitContact							(dContact* c,bool &do_collide,u16 /*material_idx_1*/,u16 /*material_idx_2*/) override;
	virtual	dSpaceID	dSpace								() override {return m_space;}
	virtual	dGeomID		dSpacedGeom							() override {return (dGeomID)m_space;}
	virtual	void		get_spatial_params					() override;
	/////////////////CPHCharacter////////////////////////////////////////////
public:
	//Check state
	virtual	bool			 		ContactWas						() override {if(b_meet_control) {b_meet_control=false;return true;} else return false;}
	virtual	EEnvironment	 		CheckInvironment				() override;
	virtual	void			 		GroundNormal					(Fvector &norm) override;
	virtual	const ICollisionDamageInfo	*CollisionDamageInfo ()const override {return this;}
	virtual	ICollisionDamageInfo	*CollisionDamageInfo () override {return this;}
private:
	virtual	float			 	ContactVelocity				()const override {return m_collision_damage_info.ContactVelocity();}
	virtual	void			 	HitDir							(Fvector& dir)const override {return m_collision_damage_info.HitDir(dir);}
	virtual	const Fvector&	 	HitPos							()const override {return m_collision_damage_info.HitPos();}
	ALife::_OBJECT_ID				 	DamageInitiatorID				()const override;
	virtual	CObject			 	*DamageInitiator				()const override;
	virtual	ALife::EHitType	 	HitType							()const override {return m_collision_damage_info.m_hit_type; };
	virtual	void				SetInitiated					() override;
	virtual	bool				IsInitiated						()const override;
	virtual	bool				GetAndResetInitiated			() override;
	virtual	void				SetHitType						(ALife::EHitType type) override { m_collision_damage_info.m_hit_type = type; };
	virtual	ICollisionHitCallback	*HitCallback					()const override;
	virtual	void				Reinit							() override {m_collision_damage_info.Reinit();};
public:
	//Creating
	virtual	void		Create								(dVector3 sizes) override;
	virtual	void		Destroy								(void) override;
	virtual	void		Disable								() override;
	virtual	void 		EnableObject						(CPHObject* obj) override;
	virtual	void		Enable								() override;
	virtual	void		SetBox								(const dVector3 &sizes) override;
	virtual	bool		UpdateRestrictionType				(CPHCharacter* ach) override;
	//get-set
	virtual	void		SetObjectContactCallback			(ObjectContactCallbackFun* callback) override;
	virtual	void		SetObjectContactCallbackData		( void* data ) override;
	virtual	void		SetWheelContactCallback				(ObjectContactCallbackFun* callback) override;
private:
				void		RemoveObjectContactCallback			(ObjectContactCallbackFun* callback);
				void		AddObjectContactCallback			(ObjectContactCallbackFun* callback);
static			void		TestRestrictorContactCallbackFun	(bool& do_colide,bool bo1,dContact& c,SGameMtl* material_1,SGameMtl* material_2);
public:
	virtual	ObjectContactCallbackFun* ObjectContactCallBack	() override;
	virtual	void		SetStaticContactCallBack			(ContactCallbackFun* calback) override;
	virtual	void		SwitchOFFInitContact				() override;
	virtual	void		SwitchInInitContact					() override;
	virtual	void		SetAcceleration						(Fvector accel) override;
	virtual	Fvector		GetAcceleration						() override { return m_acceleration; };
	virtual	void		SetCamDir							(const Fvector& cam_dir) override;
	virtual	const Fvector&	CamDir								()const override {return m_cam_dir;}
	virtual	void		SetMaterial							(u16 material) override;
	virtual	void		SetPosition							(const Fvector &pos) override;
	virtual	void		GetVelocity							(Fvector& vvel)const override;
	virtual	void		GetSmothedVelocity					(Fvector& vvel) override;
	virtual	void		SetVelocity							(Fvector vel) override;
	virtual	void		SetAirControlFactor					(float factor) override {m_air_control_factor=factor;}
	virtual	void		SetElevator							(IClimableObject* climable) override {m_elevator_state.SetElevator(climable);};
	virtual	CElevatorState	*ElevatorState						() override;
	virtual	void		SetCollisionDamageFactor			(float f) override {m_collision_damage_factor=f;}
	virtual	void		GetPosition							(Fvector& vpos) override;
	virtual		void		GetPreviousPosition					(Fvector& pos)		;
	virtual	float		FootRadius							() override;
	virtual	void		DeathPosition						(Fvector& deathPos) override;
	virtual	void		IPosition							(Fvector& pos) override;
	virtual	u16			ContactBone							() override;
	virtual	void		ApplyImpulse						(const Fvector& dir, const dReal P) override;
	virtual	void		ApplyForce							(const Fvector& force) override;
	virtual	void		ApplyForce							(const Fvector& dir,float force) override;
	virtual	void		ApplyForce							(float x,float y, float z) override;
	virtual	void		AddControlVel						(const Fvector& vel) override;
	virtual	void		SetMaximumVelocity					(dReal vel) override {m_max_velocity=vel;}
	virtual	dReal		GetMaximumVelocity					() override { return m_max_velocity;}
	virtual	void		SetJupmUpVelocity					(dReal velocity) override {jump_up_velocity=velocity;}

	virtual	bool		JumpState							() override
	{
																				return b_jumping||b_jump;
																					};
	virtual	const Fvector&  ControlAccel						()const override {return m_acceleration;}
	virtual	bool		TouchRestrictor						(ERestrictionType rttype) override;
	virtual	float		&FrictionFactor						() override {return m_friction_factor;}
	virtual	void		SetMas								(dReal mass) override;
	virtual	float		Mass								() override {return m_mass;};
	virtual	void		SetPhysicsRefObject					(IPhysicsShellHolder* ref_object) override;
	virtual	void		SetNonInteractive					(bool v) override;
	virtual	bool		IsEnabled							() override
	{ if(!b_exist)return false; return !!dBodyIsEnabled(m_body);}

	virtual	void		GetBodyPosition						(Fvector& vpos) override
	{ VERIFY(b_exist); vpos = cast_fv(dBodyGetPosition(m_body));  }
	virtual	const Fvector		&BodyPosition						()const override
		{VERIFY(b_exist && m_body); return cast_fv(dBodyGetPosition(m_body)); }

	virtual	void		get_State							(		SPHNetState&	state) override;
	virtual	void		set_State							(const	SPHNetState&	state) override;
	virtual		void		ValidateWalkOn						();
	bool		ValidateWalkOnMesh					()			;
	bool		ValidateWalkOnObject				()			;
private:
			void		ApplyAcceleration					();

			u16			RetriveContactBone					();
			void		SafeAndLimitVelocity				();
virtual		void		UpdateStaticDamage					(dContact* c,SGameMtl* tri_material,bool bo1);
			void		UpdateDynamicDamage					(dContact* c,u16 obj_material_idx,dBodyID b,bool bo1);
IC			void 		FootProcess							(dContact* c,bool &do_collide ,bool bo);
IC			void		foot_material_update				(u16	tri_material,u16	foot_material_idx);
			static void	TestPathCallback(bool& do_colide,bool bo1,dContact& c,SGameMtl * /*material_1*/,SGameMtl * /*material_2*/);
	virtual	void		Collide								() override;
			void		OnStartCollidePhase					();
private:
	virtual	void		Freeze							() override { CPHObject::Freeze();		}
	virtual	void		UnFreeze						() override { CPHObject::UnFreeze();	}
	virtual	void		step							(float dt) override { CPHObject::step( dt ); }
	virtual	void		collision_disable				() override { CPHObject::collision_disable(); }
	virtual	void		collision_enable				() override { CPHObject::collision_enable(); }
	virtual	void		collision_dynamic_enable() override { CPHObject::collision_dynamic_enable(); }
	virtual	void		collision_dynamic_disable() override { CPHObject::collision_dynamic_disable(); }
	virtual	void		NetRelcase						( IPhysicsShellHolder* O ) override;
protected:
	virtual	void	get_Box								( Fvector&	sz, Fvector& c )const override;
	protected:
	virtual	void	update_last_material						() override;
public:	
#ifdef DEBUG_DRAW
	virtual		void		OnRender						();
#endif
};

constexpr dReal def_spring_rate = 0.5f;
constexpr dReal def_dumping_rate = 20.1f;

IC bool ignore_material(u16 material_idx)
{
	SGameMtl* material = GMLibrary().GetMaterialByIdx(material_idx);
	return !!material->Flags.test(SGameMtl::flActorObstacle);
}