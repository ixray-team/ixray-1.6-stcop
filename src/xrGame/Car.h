#pragma once
#include "Entity.h"
#include "../xrPhysics/ODE/PHDynamicData.h"
#include "../xrPhysics/PhysicsShell.h"
#include "../xrPhysics/PHUpdateObject.h"
#include "script_entity.h"
#include "CarLights.h"
#include "../xrPhysics/ODE/PHObject.h"
#include "holder_custom.h"
#include "PHSkeleton.h"
#include "DamagableItem.h"
#include "PHCollisionDamageReceiver.h"
#include "CarDamageParticles.h"
#include "xrServer_Objects_ALife.h"
#include "CarDamageParticles.h"
#include "hit_immunity.h"
#include "Explosive.h"
#include "PHDestroyable.h"
#include "DelayedActionFuse.h"
#include "InventoryOwner.h"
#include "inventory_space.h"
#include "PHCommander.h"
#include "PHContact.h"
#include "ParticlesPlayer.h"
#include "gamepersistent.h"
#include "ParticlesObject.h"
#include "RocketLauncher.h"
#include "../xrEngine/GameMtlLib.h"
#include "ui_base.h"
#include "../Include/xrRender/Kinematics.h"
#include "../xrEngine/CameraBase.h"

// Cars-Ext compatibility: IXRAY exposes CInifile::read_if_exists<T> instead of the LA READ_IF_EXISTS macro.
#define READ_IF_EXISTS(ini, getter, section, key, def) READ_IF_EXISTS_IMPL_##getter(ini, section, key, def)
#define READ_IF_EXISTS_IMPL_r_float(ini, section, key, def) (ini)->read_if_exists<float>(section, key, def)
#define READ_IF_EXISTS_IMPL_r_bool(ini, section, key, def) (ini)->read_if_exists<bool>(section, key, def)
#define READ_IF_EXISTS_IMPL_r_u8(ini, section, key, def) (ini)->read_if_exists<u8>(section, key, def)
#define READ_IF_EXISTS_IMPL_r_u16(ini, section, key, def) (ini)->read_if_exists<u16>(section, key, def)
#define READ_IF_EXISTS_IMPL_r_u32(ini, section, key, def) (ini)->read_if_exists<u32>(section, key, def)
#define READ_IF_EXISTS_IMPL_r_fvector3(ini, section, key, def) (ini)->read_if_exists<Fvector3>(section, key, def)
#define READ_IF_EXISTS_IMPL_r_fcolor(ini, section, key, def) (ini)->read_if_exists<Fcolor>(section, key, def)
#define READ_IF_EXISTS_IMPL_r_string(ini, section, key, def) ((ini)->line_exist(section, key) ? (ini)->r_string(section, key) : (def))

#ifndef _abs
#define _abs(x) ((x) < 0 ? -(x) : (x))
#endif

// Cars-Ext normalizes several direction vectors that can legitimately be zero
// (stationary car, coincident bones, closed doors), which asserts in Debug.
IC bool safe_normalize(Fvector& V)
{
	if (V.square_magnitude() <= EPS_L)
		return false;

	V.normalize();
	return true;
}

static Fmatrix set_pos_dir_rel(Fmatrix world_mat, Fmatrix loc_mat, Fvector pos, Fvector dir)
{
	loc_mat.c.mad(loc_mat.k, pos.z);
	loc_mat.c.mad(loc_mat.i, pos.x);
	loc_mat.c.mad(loc_mat.j, pos.y);

	Fvector Center = loc_mat.c;
	float X, Y, Z;
	loc_mat.getXYZ(X, Y, Z);
	X += dir.x;
	Y += dir.y;
	Z += dir.z;
	loc_mat.setXYZ(X, Y, Z);
	loc_mat.c.set(Center);

	return Fmatrix().mul(world_mat, loc_mat);
}

class CCarParticlesPlayCall :
	public CPHAction
{
	LPCSTR ps_name;
	Fmatrix pos;
	Fvector vel;
public:
	CCarParticlesPlayCall(Fmatrix pos_, Fvector vel_, LPCSTR psn)
	{
		ps_name = psn;
		pos = pos_;
		vel = vel_;
	}
	virtual void run()
	{
		xr_shared_ptr<CParticlesObject> ps = Particles::Details::Create(ps_name, true);
		if (ps)
		{
			ps->UpdateParent(pos, vel);
			GamePersistent().ps_needtoplay.push_back(ps);
		}
	};
	virtual bool obsolete()const { return false; }
};

class CPHWallMarksCallStatic :
	public CPHAction
{
	CDB::TRI* pTri;
	IWallMarkArray* marks;
	Fvector pos;
	Fvector dir;
	float wsize;
	float simil;
public:
	CPHWallMarksCallStatic(IWallMarkArray* marks_, Fvector pos_, float wsize_, CDB::TRI* pTri_, float simil_, Fvector dir_)
	{
		marks = marks_;
		pos = pos_;
		dir = dir_;
		wsize = wsize_;
		pTri = pTri_;
		simil = simil_;
	}
	virtual void run()
	{
		::Render->add_StaticWallmark(marks, pos, wsize, pTri, Level().ObjectSpace.GetStaticVerts().data());
	};
	virtual bool obsolete()const{return false;}
};

class CPHWallMarksCallDynamic:
	public CPHAction
{
	Fmatrix xform;
	IKinematics *kinematics;
	IWallMarkArray* marks;
	Fvector pos;
	Fvector dir;
	float wsize;
public:
	CPHWallMarksCallDynamic(Fmatrix xform_, IKinematics* kin_, IWallMarkArray* marks_, Fvector pos_, Fvector dir_, float wsize_)
	{
		xform = xform_;
		kinematics = kin_;
		marks = marks_;
		pos = pos_;
		dir = dir_;
		wsize = wsize_;
	}
	virtual void run()
	{
		::Render->add_SkeletonWallmark(&xform, kinematics, marks, pos, dir, wsize);
	};
	virtual bool obsolete()const{return false;}
};

enum eState_Steer
{
	wheel_idle = 0,
	wheel_limit,
	wheel_left,
	wheel_right,
	wheel_drive,
	wheel_brake
};
// refs
class ENGINE_API			CBoneInstance;
class						CActor;
class						CInventory;
class						CSE_PHSkeleton;
class						CCarTurret;
struct						dxGeomUserData;
struct						dSurfaceParameters;
// defs

#ifdef DEBUG
	#include "../xrEngine/StatGraph.h"
	#include "PHDebug.h"
#endif

class CScriptEntityAction;
class car_memory;

XREFLECT(LuaExport, Bases = "CGameObject,CHolderCustom")
class CCar final :
	public CEntity, 
	public CScriptEntity,
	public CPHUpdateObject,
	public CHolderCustom,
	public CPHSkeleton,
	public CDamagableItem,
	public CPHDestroyable,
	public CPHCollisionDamageReceiver,
	public CHitImmunity,
	public CExplosive,
	public CInventoryOwner,
	public CRocketLauncher,
	public CDelayedActionFuse
{
	XGENERATED_BODY()
private:
	collide::rq_results		RQR;

#ifdef DEBUG
// 	CFunctionGraph 					m_dbg_power_rpm			;
// 	CFunctionGraph 					m_dbg_torque_rpm		;
// 	CStatGraph	   					*m_dbg_dynamic_plot		;
// 	bool							b_plots					;
	float _stdcall			TorqueRpmFun		(float rpm)		{return Parabola(rpm)/rpm;}
	void 					InitDebug			()				{}
	void 					DbgSheduleUpdate	()				{}
	void 					DbgUbdateCl			()				{}
	void 					DbgCreatePlots		()				{}
	void 					DBgClearPlots		()				{}
#endif
////////////////////////////////////////////////////////////////////
	Flags16						async_calls						;
static	const u16				cAsCallsnum						= 3;
	enum EAsyncCalls
	{
		ascSndTransmission			=			1<<0,
		ascSndStall					=			1<<1,
		ascExhoustStop				=			1<<2,
		ascLast 					=			1<<cAsCallsnum
	};
	void					ASCUpdate			()				;
	void					ASCUpdate			(EAsyncCalls c)	;
	void					AscCall				(EAsyncCalls c)	;
////////////////////////////////////////////////////////////////////////////////////////
	virtual bool						CanRemoveObject				();
	void								DropTrunkContents			();
////////////////////////////////////////////////////////////////////////
	CPhysicsShell*						characters_scraper_shell;
	BONE_P_MAP							bone_map;					//interface for PhysicsShell
	xr_set<u16>							pribor_arrows;
	static	void 						ActorObstacleCallback		(bool& do_colide,bool bo1,CPHContact& cphc,SGameMtl* material_1,SGameMtl* material_2);
	static	void 						CollisionDetectionCallback		(bool& do_colide,bool bo1,CPHContact& cphc,SGameMtl* material_1,SGameMtl* material_2);
	virtual void						PhDataUpdate				(float step)			;
	virtual void						PhTune						(float step)			;
/////////////////////////////////////////////////////////////////////////
	virtual void						ApplyDamage					(u16 level)				;
	void								UpdateDamageEffects			()						;
	void								RepairDamageItems			()						;
	virtual	float						Health						()						{return GetfHealth();}
	virtual void						ChangeCondition				(float fDeltaCondition)	;
	virtual void						StartTimerEffects			()						{};
//////////////////////////////////////////////////////////////////////////////////////////////////////////////
	virtual CPhysicsShellHolder*		PPhysicsShellHolder			()						{return static_cast<CPhysicsShellHolder*>(this);}
	virtual CPHCollisionDamageReceiver	*PHCollisionDamageReceiver	()						{return static_cast<CPHCollisionDamageReceiver*>(this);}

////////////////////////////////////////////////////////////////////////
	CCarDamageParticles					m_damage_particles;
///////////////////////////////////////////////////////////////////////
public:
	bool								calc_bones_at_once = false;

	enum ECarCamType
	{
		EctFirstCam = 0,
		EctSecondCam,
		EctThridCam,
		EctFourthCam,
		EctFiveCam,
		EctMaxCam
	};
	ECarCamType saved_camera;
	bool rsp,lsp,fwp,bkp,brp,fwpbkp;
	Fmatrix m_root_transform;
	Fvector m_exit_position;
	Fvector m_last_enter_position;
	enum eStateDrive
	{
		drive,
		neutral
	};

	eStateDrive e_state_drive;

	enum eStateSteer
	{
		right,
		idle,
		left
	};

	eStateSteer e_state_steer;

	bool b_wheels_limited;
	bool b_engine_on;
	bool b_clutch;
	bool b_starting;
// 	bool b_stalling;
	bool b_breaks;
	bool b_transmission_switching;

	u32	 m_dwStartTime;
	u32	 m_dwAttachTime;
	XPROPERTY(LuaReadWrite, Name = "fuel")
	float m_fuel;
	XPROPERTY(LuaReadWrite, Name = "fuel_tank")
	float m_fuel_tank;
	float m_fuel_consumption;
	float m_fuel_consumption_current;
	float m_fuel_speed_factor;
	float m_fuel_speed_optimal;
	float m_fuel_rpm_factor;
	float m_fuel_gas_factor;
	u16	  m_driver_anim_type;

	float   m_break_start;
	float	m_handbreak_start;
	float	m_break_time;
// 	float	m_breaks_to_back_rate;
	float	m_power_neutral_factor;//multiplier for power when accelerator is not pressed (0-1,0.25)
	bool	b_exploded;
	bool	b_event_exit_car;
	u32		m_exit_car_time;
	u16		m_exit_door_bone;
	bool	b_event_drag_activated;
	bool	b_awd;
	bool	m_firsteye_panel;
	bool	m_allow_inventory_control;
	float	m_interior_volume;
	float	m_hit_stall_threshold;
	float	m_hit_anomaly_scale;

	struct SHeadlightHideaway
	{
		CPhysicsJoint* joint;
		float velocity;
		float force;
		int axis_num;
		SHeadlightHideaway(CPhysicsJoint* joint_ = NULL, float vel = 0.0f, float force_ = 0.0f, int axis = -1)
		{
			joint = joint_;
			velocity = vel;
			force = force_;
			axis_num = axis;
			Switch();
		}
		void Switch(bool hideaways_up = false);
	};
	xr_vector<SHeadlightHideaway> headlight_hideaways;

	struct SEngineDoor
	{
		CPhysicsJoint* joint;
		float velocity;
		float force;
		int axis_num;
		SEngineDoor(CPhysicsJoint* joint_ = NULL, float vel = 0.0f, float force_ = 0.0f, int axis = -1)
		{
			joint = joint_;
			velocity = vel;
			force = force_;
			axis_num = axis;
			Switch();
		}
		void Switch(bool doors_up = false);
	};
	xr_vector<SEngineDoor> engine_door;

	struct SSuspension
	{
		CPhysicsJoint* joint;
		float velocity;
		float force;
		int axis_num;
		bool b_independent;
		SSuspension(CPhysicsJoint* joint_ = NULL, float vel = 0.0f, float force_ = 0.0f, int axis = -1, bool indpnd = false)
		{
			joint = joint_;
			velocity = vel;
			force = force_;
			axis_num = axis;
			b_independent = indpnd;
		}
		void Update();
	};
	xr_vector<SSuspension> suspension_joints;

	struct SWheel: 
	public CDamagableHealthItem
	{
		typedef		CDamagableHealthItem inherited;
		u16									bone_id				;
		bool								inited				;
		float								radius				;
		float								wheel_vel_factor	;
		float								wheel_trq_factor	;
		float								wheel_diff_power	;
		float								suspension_distance ;
		float								suspension_force_inc;
		float								suspension_spring_factor;
		float								suspension_damping_factor;
		float								spring_last			;
		float								suspension_inertia	;
		float								damping_last		;
		CPhysicsJoint						*joint				;
		CCar								*car				;
		eState_Steer						wheel_state			;
		bool								drive_wheel			;
		bool								break_wheel			;
		bool								steer_wheel			;
		//wm_shader							tire_trail_wallmark ;
		LPCSTR								tire_trail_wallmark_name;
		float								tire_trail_wallmark_size;
		bool								tire_trail_wallmark_avlbl;

		float								spring_factor_default;
		float								damping_factor_default;
		Fvector								default_axis2;

		bool								tire_sounds;
		ref_sound							tire_drive;
		const ref_sound*					tire_drive_source;
		float								tire_trail_simulars_factor;
		float								tire_particle_scale;
		float								tire_particle_scale_min;
		float								tire_particle_scale_max;
		float								max_control_velocity;
		// The drive motor was switched to zero-velocity axle friction while
		// the wheel had no ground contact. Restore the drive target on landing.
		bool								AirborneFriction;
		struct			SWheelCollisionParams
		{
			float							spring_factor		;
			float							damping_factor		;
			float							mu_factor			;
			bool							slide				;
			bool							was_slide			;
			bool							buxe				;
			bool							was_buxe			;
			bool							breaking			;
			bool							breaking_abs		;
			bool							traction			;
			u32								last_contact_time	;
			float							friction_slip_factor;
			float							friction2_slip_factor;
			float							friction_rain_factor;
			float							friction_nitro_factor;
			float							current_friction_factor;
			float							current_friction2_factor;
			float							break_start;
			SWheel*							pwheel;
			SWheelCollisionParams			();
		}									collision_params	;

	IC	static	void			applywheelCollisionParams				(const dxGeomUserData *ud,bool& do_colide,CPHContact& cphc,SGameMtl* material_1,SGameMtl* material_2);
		static	void			WheellCollisionCallback					(bool& do_colide,bool bo1,CPHContact& cphc,SGameMtl* material_1,SGameMtl* material_2)							;
				
				void 			Init									();//asumptions: bone_map is 1. ini parsed 2. filled in 3. bone_id is set 
				void			Load									(LPCSTR section);
				void			Play									(Fvector c_pos, u16 m1_id, u16 m2_id, bool tire_fulleffects);
				void			UpdateSounds							();
				void 			RestoreNetState							(const CSE_ALifeCar::SWheelState& a_state)														;
				void 			SaveNetState							(NET_Packet& P)																					;
				void 			ApplyDriveAxisVel						(float vel)																						;
				void 			ApplyDriveAxisTorque					(float torque)																					;
				void 			ApplyDriveAxisVelTorque					(float vel,float torque)																		;
				void 			ApplySteerAxisVel						(float vel)																						;
				void 			ApplySteerAxisTorque					(float torque)																					;
				void 			ApplySteerAxisVelTorque					(float vel,float torque)																		;
				void 			SetSteerLoLimit							(float lo)																						;
				void			SetSteerHiLimit							(float hi)																						;
				void			SetSteerLimits							(float hi,float lo)																				;
				float			GetDriveAngleRate();
				float			GetSteerAngleRate();
				float			GetSteerAngle();
				void			Neutral();
				enum eWheelType
				{
					wheel          = (1 << 0),
					wheel_drive    = (1 << 1),
					wheel_breaking = (1 << 2),
					wheel_steering = (1 << 3),
					wheel_all	   = wheel | wheel_drive | wheel_breaking | wheel_steering
				};
				Flags32 WheelFlags;
virtual void ApplyDamage			(u16 level);
				virtual void Repair();
		SWheel(CCar* acar)
		{
			bone_id=BI_NONE;
			car=acar;
			joint=NULL;
			inited=false;
			drive_wheel = false;
			break_wheel = false;
			steer_wheel = false;
			tire_drive_source = nullptr;
			WheelFlags = {0};
			max_control_velocity = 0.f;
			AirborneFriction = false;
		}
	};
	struct SWheelDrive  
	{
		SWheel* pwheel;
		float	pos_fvd;
		float	gear_factor;
		void	Init		()						;
		void	Drive		()						;
		void	Neutral		()						;
		void	UpdatePower	()						;
		float	ASpeed		()						;
		float   ASpeedMaximum()						;
		float	DiffSpeed	()						;
		void	Load		(LPCSTR /*section*/){}	;
	};
	struct SWheelSteer 
	{
		SWheel* pwheel;
		float pos_right;
		float lo_limit;
		float hi_limit;
// 		float steering_velocity;
		float steering_torque;
		bool  limited;			//zero limited for idle steering drive
		float GetSteerAngle()
		{
			return -pos_right*dJointGetHinge2Angle1 (pwheel->joint->GetDJoint());
		}
		void	 Init		()						;
		void	 SteerRight	()						;
		void	 SteerLeft	()						;
		void	 SteerIdle	()						;
		void	 Limit		()						;
		void	 Load		(LPCSTR /*section*/){}	;

	};
	struct SWheelBreak 
	{
		SWheel			*pwheel							;
		float			break_torque					;
		float			hand_break_torque				;
		void		 Init			()					;
		void		 Break			(float k)			;
		void		 HandBreak		(float k)			;
		void		 Neutral		()					;
		void		 Load			(LPCSTR section)	;
	};

	struct SExhaust
	{
		u16					bone_id;
		xr_shared_ptr<CParticlesObject>	p_pgobject;
		xr_shared_ptr<CParticlesObject>	p_pgobject_nitro;
		CCar*				pcar;
		void Init();
		void Play();
		void Stop();
		void Update();
		void Clear ();
		void PlayNitro();
		void StopNitro();
		void PlayPop();
		SExhaust(CCar* acar)
		{
			bone_id=BI_NONE;
			pcar=acar;
			p_pgobject = nullptr;
			p_pgobject_nitro = nullptr;
		}
		~SExhaust();
	};

	struct SDoor;
	struct SDoor :
	public CDamagableHealthItem
	{
		typedef CDamagableHealthItem inherited;
		u16 bone_id;
		CCar* pcar;
		bool  update;
		CPhysicsJoint*  joint;
		float			torque;
		float			a_vel;
		float			torque_factor_opening;
		float			a_vel_factor_opening;
		float			torque_factor_closing;
		float			a_vel_factor_closing;
		float			pos_open;

		bool			non_enterable;
		bool			non_exitable;
		bool			broken_door_handle;
		bool			spring_loaded_door;
		bool			lamba_door;
		bool			vertical_door;

// 		float			dist_to_door;
		float			dist_to_door_avlbl;
		float			opened_angle;
		float			closed_angle;

		float			default_opened_angle;
		float			default_closed_angle;
		float			default_lin_resist;
		float			default_ang_resist;
		Fvector			default_hinge_axis;
		float			default_joint_sf;
		float			default_joint_df;
		float			default_axis_sf;
		float			default_axis_df;

		float			saved_mass;
		bool			recursive_door;
		ref_sound		snd_door_close;
		ref_sound		snd_door_open;
		u32				open_time;
		u32				real_open_time;
		u32				real_close_time;
// 		u32				switch_time;
		bool			no_deactivate;
		// UNUSED: legacy doorway helper
#if 0
		struct SDoorway
		{
		Fvector2		door_plane_ext;
		_vector2<int>	door_plane_axes;
			SDoor			*door;
				 SDoorway	();
			void SPass		();
			void Init		(SDoor	*adoor);
			void Trace		(const Fvector &point,const Fvector &dir);
		};
#endif
		Fvector2		door_plane_ext;
		_vector2<int>	door_plane_axes;
		Fvector			door_dir_in_door;
		Fmatrix			closed_door_form_in_object;
		void Use();
		void Switch();
		void Init();
		void Open();
		void Close();
		void Break();
virtual void ApplyDamage(u16 level);
		virtual void Repair();
		void Update();
		float GetAngleRate();
		float GetAngle();
		bool CanEnter(const Fvector& pos,const Fvector& dir,const Fvector& foot_pos);
		bool IsInArea(const Fvector& pos,const Fvector& dir);
		bool IsFront (const Fvector& pos,const Fvector& dir);
		bool CanExit(const Fvector& pos,const Fvector& dir);
		bool TestPass(const Fvector& pos,const Fvector& dir);
		//bool TestPass1(const Fvector& pos,const Fvector& dir);
		void GetExitPosition(Fvector& pos);
		Fvector GetWorldPosition();
		bool IsDistOfSight(const Fvector& pos);
		bool IsLineOfSight();
		void ApplyTorque(float atorque,float aa_vel);
		void NeutralTorque(float atorque);
		void ClosingToClosed(bool obuser = true);
		void Deactivate();
		void ClosedToOpening(bool obuser = true);
		void PlaceInUpdate();
		void RemoveFromUpdate();
		void SaveNetState(NET_Packet& P);
		void RestoreNetState(const CSE_ALifeCar::SDoorState& a_state);
		void SetDefaultNetState();
		enum eState
		{
			opening,
			closing,
			opened,
			closed,
			broken
		};
		eState state;
		SDoor(CCar* acar)
		{
			bone_id=BI_NONE;
			pcar=acar;
			joint=NULL;
			state=closed;
			torque=500.f;
			a_vel=M_PI;
			torque_factor_opening = 1.f;
			a_vel_factor_opening = 1.f;
			torque_factor_closing = 1.f;
			a_vel_factor_closing = 1.f;
// 			dist_to_door = 0.0f;
			dist_to_door_avlbl = 3.0f;

			non_enterable = false;
			non_exitable = false;
			broken_door_handle = false;
			spring_loaded_door = false;
			lamba_door = false;
			vertical_door = false;

			real_close_time = 0;
			real_open_time = 0;
			recursive_door = false;
			no_deactivate = false;
			default_lin_resist =0.f;
			default_ang_resist =0.f;
			saved_mass = 0.f;
		}
	};

	struct SCarSound
	{
		ref_sound					snd_engine							;
		ref_sound					snd_engine_start					;
		ref_sound					snd_engine_stop						;
		ref_sound					snd_transmission					;
		ref_sound					snd_gas								;
		ref_sound					snd_brake_release					;
		ref_sound					snd_valve							;
		ref_sound					snd_turbo							;
		ref_sound					snd_limiter							;
		ref_sound					snd_idle							;
		ref_sound					snd_engine_decel					;
		ref_sound					snd_engine_heavy					;
		ref_sound					snd_blower							;
		ref_sound					snd_nitro							;
		ref_sound					snd_nitro_start						;
		ref_sound					snd_nitro_stop						;
		ref_sound					snd_whine							;
		ref_sound					snd_horn							;
		ref_sound					snd_pop								;
		//ref_sound					snd_idle_exhaust					;
		//ref_sound					snd_engine_exhaust					;
		//ref_sound					snd_engine_decel_exhaust			;
		//ref_sound					snd_limiter_exhaust					;

		enum ESoundState
		{
			sndOff,
			sndStalling,
			sndStoping,
			sndStarting,
			sndDrive
		} eCarSound;
		void	Update				()							;
		void	UpdateStarting		()							;
		void	UpdateStoping		()							;
		void	UpdateStalling		()							;
		void	UpdateDrive			()							;
		void	SwitchState			(ESoundState new_state)		;
		void	SetSoundPosition	(ref_sound	&snd)			;
		void	SetEngineSoundPosition	(ref_sound &snd)		;
		//void	SetEngineExhaustSoundPosition (ref_sound& snd)	;
		void	SwitchOff			()							;
		void	SwitchOn			()							;
		void	Init				()							;
		void	Destroy				()							;
		void	Start				()							;
		void	Stop				()							;
		void	Stall				()							;
		void	Drive				()							;
		void	TransmissionSwitch	()							;
		void	Gas					()							;
		void	BrakeRelease		()							;
		void	Valve				()							;
		void	NitroStart			()							;
		void	NitroStop			()							;
		void	PopSound			()							;
		void	Whine				()							;
		void	Horn				(bool start)				;

				SCarSound			(CCar* car)					;
				~SCarSound			()							;
		Fvector	relative_pos									;
		Fvector relative_engine_pos								;
		//Fvector relative_engine_exhaust_pos						;
		float	volume											;
		u32		engine_start_delay								;//snd_engine starts after engine_start_delay ms by snd_engine_start
		u32		time_state_start								;
		CCar*	pcar											;
	} *m_car_sound;

private:
	typedef CEntity			inherited;
private:
// 	CCarTurret*				m_car_weapon_active;
	int						m_car_weapon_slots;
	xr_vector<CCarTurret*>	m_car_weapons;
	float					m_steer_angle;
	float					m_wheel_angle;
	float					m_steer_factor;
	bool					m_repairing;
	u16						m_bone_trunk;
	CCameraBase*			camera[EctMaxCam];
	CCameraBase*			active_camera;
	float					m_camera_inertion_n;
	float					m_camera_inertion_k;
	float					m_camera_angle_factor;
	float					break_start_dt;
	Fvector					m_camera_position[EctMaxCam];
	Fvector					CarCameraDirection;
// 	Fvector					carCameraNormal;
	float					m_camera_inert_x;
	float					m_camera_inert_z;
	u32						m_camera_last_mmove_time;

	////////////////////////////////////////////////////
	friend struct SWheel;
	friend struct SDoor;

	xr_map   <u16,SWheel>	m_wheels_map;
	xr_vector <SWheelDrive> m_driving_wheels;
	xr_vector <SWheelSteer> m_steering_wheels;
	xr_vector <SWheelBreak> m_breaking_wheels;
	xr_vector <SExhaust>	m_exhausts;
	xr_vector <u16>			m_steers;
	xr_vector <u16>			m_indoor_l_available_doors;
	shared_str				m_exhaust_particles;
	shared_str				m_nitro_particles;
	shared_str				m_pop_particles;
	xr_map	  <u16,SDoor>	m_doors;
	xr_map <int, SDoor*>	m_doors_recurs;
	xr_vector <SDoor*>		m_doors_update;
	xr_vector <Fvector4>	m_gear_ratious;
	Fmatrix					m_sits_transforms; // driver_place
	float					m_current_gear_ratio;

	/////////////////////////////////////////////////////////////
	bool					b_auto_switch_transmission;
	bool					b_auto_transmission_stop;
	bool					b_cvt_transmission;
	bool					b_nitro_engage;
	/////////////////////////////////////////////////////////////
	float					m_doors_torque_factor;
	/////////////////////////////////////////////////////////////
	// Cars-Ext tire trails use dedicated wallmark arrays because the stock
	// IXRAY material library has no collide marks for objects\car_wheel.
	FactoryPtr<IWallMarkArray> m_tire_wallmarks_ground;
	FactoryPtr<IWallMarkArray> m_tire_wallmarks_dirt;
	FactoryPtr<IWallMarkArray> m_tire_wallmarks_grass;

	enum ETireSurface
	{
		TireSurfaceRoad,
		TireSurfaceDirt,
		TireSurfaceGrass,
		TireSurfaceWater,
	};

	// Dedicated wheel sounds, loaded from vehicles.ltx with LA defaults.
	ref_sound				m_tire_sound_road;
	ref_sound				m_tire_sound_dirt;
	ref_sound				m_tire_sound_grass;
	ref_sound				m_tire_sound_water;
	ref_sound				m_tire_skid_sound_road;
	ref_sound				m_tire_skid_sound_dirt;
	ref_sound				m_tire_skid_sound_grass;
	ref_sound				m_tire_skid_sound_water;
	/////////////////////////////////////////////////////////////

	float					m_max_power					;//best rpm
	float					m_power_reverse_factor		;
	float					m_power_increment_factor	;
	float					m_power_decrement_factor	;
	float					m_rpm_increment_factor		;
	float					m_rpm_decrement_factor		;
	float					m_rpm_brp_increment_factor	;
	float					m_rpm_brp_decrement_factor	;
	float					m_wheels_spin_factor_start;
	float					m_wheels_spin_factor_stop;
	float					m_wheels_sound_delay;
	float					m_wheels_sound_timer;
	/////////////////////porabola
	float m_a,m_b,m_c;

	float					m_current_engine_power;
	float					m_current_rpm;
	float					m_current_rpm_brp_n;
	float					m_current_rpm_brp_n_max;
	float					m_current_rpm_brp_decrease;
	float					m_current_slip_sliding;
	float					m_slip_sliding_threshold;
	float					m_vel_sliding_threshold;
	float					m_axle_friction;
	u32						m_last_cotact_time;
	u32						m_hit_engine_time;
	float					m_slip_start_time;
	float					m_steer_boost;
	u32						m_steer_boost_frame;
	float					m_steer_turn_factor;
	float					m_steer_counter_turn_factor;
	float					m_steer_turn_compensation;
	float					m_steer_turn_compensation_max;
	float					m_steer_turn_delta;

	float					m_transmission_switch_rpm;
	u32						m_transmission_stage;
	float					m_transmission_switch_delay;
	float					m_transmission_switch_power_factor;
	float					m_transmission_switch_volume;
	bool					b_transmission_switch_drive;
	//float					m_transmission_switch_exhaust_volume;

	float					m_limiter_volume;
	//float					m_limiter_exhaust_volume;
// 	float					m_limiter_engine_volume;
	float					m_transmission_switch_time;
// 	float					m_transmission_switch_end_time;
	float					m_transmission_switch_sound_time;
// 	bool					m_transmission_switched;
	bool					m_transmission_sound_played;
	float					m_sound_state_after_switch;
	u32						m_engine_valve;
	bool					m_engine_valve_switch;
	float					m_turbo_scale;
	float					m_turbo_sound_scale;
	float					m_turbo_volume_scale;
	float					m_turbo_scale_min;
	float					m_turbo_scale_max;
	float					m_turbo_scale_speed;
	float					m_blower_scale_min;
	float					m_blower_volume_min;
	float					m_blower_rpm_scale; 
	float					m_blower_rpm_current;
	float					m_blower_inertia;
	float					m_blower_sound_forward;
	float					m_engine_idle_volume;
	float					m_sound_state_change_up;
	float					m_sound_state_change_down;
	float					m_sound_idle_change_up;
	float					m_sound_idle_change_down;
	float					m_sound_state_switch_factor;
	float					m_sound_effect;
	u32						m_sound_switch_effect_type;
	float					m_sound_switch_effect_power;
	float					m_sound_switch_effect_speed;
	float					m_sound_switch_effect_duration;
	//float					m_engine_exhaust_idle_volume_scale;
	float					m_engine_idle_volume_scale;
// 	float					m_engine_idle_volume_scale_current;
// 	float					m_engine_idle_volume_scale_current_drive;
	float					m_engine_rpm_scale;
	float					m_idle_state_scale;
	float					m_idle_state_scale_last;
	float					m_decel_sound_boost;
	float					m_heavy_sound_boost;
	float					m_engine_state_scale;
	float					m_engine_state_scale_last;
	float					m_engine_state_scale_min;
	float					m_engine_sound_scale;
	float					m_engine_sound_scale_absolute;
	//float					m_engine_exhaust_sound_scale;
	float					m_engine_sound_scale_max;
	float					m_engine_sound_scale_min;
	float					m_engine_sound_scale_factor;
	float					m_engine_sound_stop_dt;
	float					m_engine_decel_sound_stop_dt;
	float					m_engine_volume_scale;
	//float					m_engine_exhaust_volume_scale;
	float					m_engine_decel_volume_scale;
	//float					m_engine_decel_exhaust_volume_scale;
// 	float					m_engine_gas_sound_scale;
	bool					m_sound;
	//bool					m_exhaust_sound;
	float					m_engine_drive;
	float					m_engine_drive_noswitch;
	float					m_engine_drive_time;
	float					m_engine_heavy;
	float					m_engine_heavy_constant;
	float					m_engine_heavy_rpm;
	float					m_engine_heavy_switch;
	float					m_valve_volume_scale;
	float					m_valve_switch_volume;
	float					m_valve_time;
	float					m_gas_volume_scale;
	float					m_friction_spin_factor;
	float					m_friction2_spin_factor;
	float					m_friction_slip_factor;
	float					m_friction2_slip_factor;
	float					m_friction_slip_speed;
	float					m_friction2_steering_wheels;
	float					m_friction2_nonsteering_wheels;
	float					m_friction2_brp_boost;
	bool					b_drift_boost;
	float					m_drift_start_boost;
	float					m_drift_boost_time;
	float					m_steer_spin_boost;
	float					m_steer_speed_boost;
	float					m_brake_boost;
	float					m_switch_shake_boost;
	float					m_engine_shake_boost;
	bool					m_shake_stage;
	bool					m_nitro;
	float					m_nitro_power_factor;
	float					m_nitro_particle_scale;
	bool					b_nitro_particles_enabled;
	bool					m_nitro_engage;
	float					m_nitro_engage_time;
	float					m_nitro_fuel_factor;
	float					m_nitro_friction_factor;
	float					m_nitro_wheelie_boost;
	float					m_wheelie_boost;
	float					m_nitro_wheelie_friction_brp_factor;
	float					m_nitro_switch_boost;
	float					m_whine_sound_scale;
	bool					m_whine_reverse;
	float					m_whine_speed;
	float					m_whine_exterior_volume;
	bool					m_pop;
	float					m_pop_time;
	float					m_pop_probability;
	bool					m_pop_single;
	bool					m_pop_played;
	float					m_steer_start_time;
	bool					m_break_abs;
	float					m_break_abs_power;
	bool					m_break_abs_individual;
	float					m_break_abs_threshold;
	bool					m_enhanced_sound;
	bool					b_burnout;
	bool					m_steer_invert;
	bool					m_steer_nonlinear;
	float					m_steer_threshold;
	float					m_last_velocity;
	float					m_last_avelocity;
	float					m_last_avelocity_j;
	float					m_acceleration;
	float					m_camera_acceleration;
	bool					b_diff_lock;
	float					m_diff_lock_power;
	float					m_diff_lock_overpower;
	float					m_downforce;
	float					m_current_downforce;
	//bool					b_downforce_compensation;

	//gauge
	float					m_speedometer_factor;
	float					m_speedometer_max_speed;
	float					m_speedometer_min_speed;
	float					m_speedometer_current;
	float					m_speedometer_offset;
	float					m_tachometer_factor;
	float					m_tachometer_current;
	float					m_tachometer_min_rpm;
	float					m_fuel_current;
	float					m_fuel_angle;
	float					m_fuel_offset;
	float					m_temp_angle;
	float					m_temp_current;
	float					m_temperature;
	float					m_temp_offset;
	float					m_temp_offset_current;
	float					m_gauge_angle;
	float					m_gauge_current;
	float					m_fan_rotation;
	float					m_fan_speed;
	float					m_fan_speed_current;
	bool					m_fan_engine;
	float					m_pressure_current;
	float					m_pressure;
	float					m_pressure_idle_angle;
	float					m_pressure_max_angle;
	float					m_pressure_factor;
	float					m_pressure_offset;
	float					m_blower_angle;
	float					m_blower_current;
	float					m_engine_angle;
	float					m_engine_current;
	float					m_engine_speed;
	bool					m_engine_stage;
	bool					m_clock_seconds_real;
	float					m_clock_seconds_offset;
	float					m_clock_minutes_offset;
	float					m_clock_hours_offset;
	float					m_brakes_pressure_factor;
	float					m_brakes_pressure_max_angle;
	float					m_brakes_pressure_current;
	float					m_brakes_pressure;
	float					m_brakes_pressure_apply_angle;
	float					m_brakes_pressure_apply_current;
	float					m_battery_factor;
	float					m_battery_angle;
	float					m_battery_current;
	float					m_gear_park_angle_x;
	float					m_gear_drive_angle_x;
	float					m_gear_reverse_angle_x;
	float					m_gear_current_angle_x;
	float					m_gear_current_angle_z;
	float					m_gear_speed;
	bool					m_gear_manual;
	float					m_gear_left_angle;
	float					m_gear_middle_angle;
	float					m_gear_right_angle;
	bool					m_torch_usage;
	//gauge

// 	float					m_fSaveMaxRPM;
	float					m_max_rpm;
	float					m_min_rpm;
	float					m_power_rpm;//max power
	float					m_torque_rpm;//max torque
	float					m_sound_rpm;
	bool					mb_traction;
	xr_vector<Fmatrix>		bones_bind_forms;
	Fvector					bind_center;

// 	CGameFont*				pFontTest;
// 	ui_shader				hVideoShader;


	float					m_steering_speed;
	float					m_ref_radius;
	size_t					m_current_transmission_num;
	///////////////////////////////////////////////////
	CCarLights				m_lights;
	CCarLights				m_stoplights;
	CCarLights				m_indoorlights;
	CCarLights				m_reverselights;
	CCarLights				m_enginelights;
	////////////////////////////////////////////////////
	/////////////////////////////////////////////////
	void				InitParabola();
	float	_stdcall	Parabola(float rpm);
	//float GetSteerAngle();
	void				 LimitWheels						()	;
	void				 Drive								()	;
	void				 Starter							()	;
	void				 StartEngine						()	;
	void				 Stall								()	;
	void				 Clutch								()	;
	void				 Unclutch							()	;
	void				 SwitchEngine						()	;
	void				 SwitchEngineDoors					()  ;
	void				 NeutralDrive						()	;
	void				 UpdatePower						()	;
// 	void				 ReleasePedals						()	;
	void				 ResetKeys							()	;
	XPROPERTY(LuaCallable)
	void				 ShowTrunk							()	;
	////////////////////////////////////////////////////////////////////////////
	float				RefWheelMaxSpeed					()	;
	float				RefWheelMaxSpeedByLastGear			()	;
	float				EngineCurTorque						()	;
	float				RefWheelCurTorque					()	;
	float	 			EnginePower							()	;
	float	 			EngineDriveSpeed					()	;
	float	 			DriveWheelsMeanAngleRate			()	;
IC	float	 			EngineRpmFromWheels					(){return dFabs(DriveWheelsMeanAngleRate()*m_current_gear_ratio);}
	/////////////////////////////////////////////////////////////////////////	
	void				SteerRight							();
	void				SteerLeft							();
	void				SteerIdle							();
	void				Transmission						(size_t num);
// 	void				CircleSwitchTransmission			();
	void				TransmissionUp						();
	void				TransmissionDown					();
IC	size_t				CurrentTransmission					(){return m_current_transmission_num;}
	void				PressRight							();
	void				PressLeft							();
	void				PressForward						();
	void				PressBack							();
	void				PressBreaks							();

	void				ReleaseRight						();
	void				ReleaseLeft							();
	void				ReleaseForward						();
	void				ReleaseBack							();
	void				ReleaseBreaks						();
	void				Revert								();
	bool				ScriptUseTrunk						(u16 bone_id);
	float				EffectiveGravity					();
	float				AntiGravityAccel					();
	float				GravityFactorImpulse				();
	void StartBreaking									();
	void StopBreaking									();
	void UpdateBackForward								();
	void Burnout										();
	void HandBreak										();
	void ReleaseHandBreak								();
	void DriveForward									();
	void DriveBack										();
	void LoadBoneMap									();
	ETireSurface GetTireSurface							(u16 m1_id, u16 m2_id);
	IWallMarkArray* GetTireWallmarks						(u16 m1_id, u16 m2_id);
	ref_sound* GetTireSound								(u16 m1_id, u16 m2_id);
	ref_sound* GetTireSkidSound							(u16 m1_id, u16 m2_id);
	void CreateSkeleton									(CSE_Abstract	*po);//creates m_pPhysicsShell
	void Init											();

	void PlayExhausts									();
	void StopExhausts									();
	void PlayExhaustsNitro								();
	void StopExhaustsNitro								();
	void PlayExhaustsPop								();
	void UpdateExhausts									();
	void ClearExhausts									();
	void UpdateFuel										(float time_delta);
	XPROPERTY(LuaCallable, Name = "add_fuel")
	float AddFuel										(float ammount); //ammount - fuel to load, ret - fuel loaded
	XPROPERTY(LuaCallable)
	void Refuel											();
	XPROPERTY(LuaCallable)
	float GetFuel										() { return m_fuel;  }
	XPROPERTY(LuaCallable)
	void CarExplode										();
	void RemoveGrassSpheres								();
	////////////////////////////////////////////		////////

	void					OnCameraChange				(int type);





	bool					HUDview						( ) { return IsFocused(); }

	static void	__stdcall		cb_Steer				(CBoneInstance* B);
	static void	__stdcall		cb_Steer_factor			(CBoneInstance* B);
	static void __stdcall		cb_Gauge				(CBoneInstance* B);
	static void __stdcall		cb_Suspension			(CBoneInstance* B);

	enum EGaugeType
	{
		gauge_speedometer,
		gauge_tachometer,
		gauge_fuel,
		gauge_temp,
		gauge_battery,
		gauge_gauge,
		gauge_engine,
		gauge_blower,
		gauge_pressure,
		gauge_brakes_pressure,
		gauge_brakes_pressure_apply,
		gauge_fan,
		gauge_gear,
		clock_seconds,
		clock_minutes,
		clock_hours
	} eCarGauge;

	class Gauge_struct
	{
	public:
		Gauge_struct(CCar* car = NULL, int type_ = 0)
		{
			pcar = car;
			type = type_;
		}
		CCar* pcar;
		int type;
	};

	class Suspension_struct
	{
	public:
		Suspension_struct(CCar* car = NULL, u16 first_wheel_ = BI_NONE, u16 second_wheel_ = BI_NONE, float suspension_offset_ = 0.f, bool steer_ = true, bool move_ = true, float steer_factor_ = 1.f)
		{
			pcar = car;
			first_wheel = first_wheel_;
			second_wheel = second_wheel_;
			suspension_offset = suspension_offset_;
			steer = steer_;
			move = move_;
			steer_factor = steer_factor_;
		}
		CCar* pcar;
		u16 first_wheel;
		u16 second_wheel;
		float suspension_offset;
		bool steer;
		bool move;
		float steer_factor;
	};

	virtual	void			Hit							(SHit* pHDS);
	virtual void			Die							(CObject* who);
	virtual void PHHit									(float P,Fvector &dir, CObject *who,s16 element,Fvector p_in_object_space, float impulse, ALife::EHitType hit_type/* =ALife::eHitTypeWound */);
			bool WheelHit								(float P,s16 element,ALife::EHitType hit_type);
			bool DoorHit								(float P,s16 element,ALife::EHitType hit_type);
public:
	void					ConsoleRepair				()				{ SetfHealth(GetMaxHealth()); RepairDamageItems(); }
	void					ConsoleRefuel				()				{ Refuel(); }
	virtual bool			allowWeapon					() const		{return true;};
	virtual bool			HUDView						() const;
	virtual Fvector			ExitPosition				(){return m_exit_position;}
	virtual Fvector			ExitVelocity				();
	void					GetVelocity					(Fvector& vel)	{m_pPhysicsShell->get_LinearVel(vel);}
	Fvector					GetVelocity					()	{ Fvector vel = { 0.f, 0.f, 0.f }; if (m_pPhysicsShell)m_pPhysicsShell->get_LinearVel(vel); return vel; }
	float					GetVelocityM				()	{ return Fvector(GetVelocity()).magnitude(); }
	Fvector					GetAVelocity				() { Fvector vel = { 0.f, 0.f, 0.f }; if (m_pPhysicsShell)m_pPhysicsShell->get_AngularVel(vel); return vel; }
	float					GetAVelocityM				() { return Fvector(GetAVelocity()).magnitude(); }
	float					GetSpeedKmH					()	{ return GetVelocityM() * 3.6f; }
	void					cam_Update					(float dt, float fov);
	void					ZoomCamera					(int cmd, float free_step);
	void					detach_Actor				();
	bool					attach_Actor				(CGameObject* actor);
	bool					is_Door						(u16 id,xr_map<u16,SDoor>::iterator& i);
	bool					is_Door						(u16 id);
	bool					DoorOpen					(u16 id);
	bool					DoorClose					(u16 id);
// 	bool					DoorUse						(u16 id);
	bool					DoorSwitch					(u16 id);
	bool					Enter						(const Fvector& pos,const Fvector& dir,const Fvector& foot_pos);
	bool					Exit						(const Fvector& pos,const Fvector& dir);
	bool					IsHeadLightsAvailable		(){return m_lights.IsAvailable();};
	SDoor*					Get_Door					(u16 id, bool all = false);
// 	bool					All_Doors_closed			();
	SDoor*					GetNearestDoor				(Fvector look_pos, Fvector look_dir, bool special = false);
	u16						GetNearestBone				(Fvector look_pos, Fvector look_dir, float &range, bool trunk_test = false);
	Fvector					GetBindCenter				(){return bind_center;}

	bool					b_start_open_recursive;
	bool					b_start_close_recursive;
	bool					b_all_recurs_opened;
	void					UpdateRecursiveDoors		();
	bool					HasFirstCam(){return active_camera->tag == EctFirstCam;}
	bool					AllowInventoryControl() const { return m_allow_inventory_control; }

	bool					Use							(const Fvector& pos,const Fvector& dir,const Fvector& foot_pos);
	void					DoExit						();
// 	void					DoEnter						();
	XPROPERTY(LuaCallable)
	void					OpenTrunkBone				();
	XPROPERTY(LuaCallable)
	void					CloseTrunkBone				();
	XPROPERTY(LuaCallable)
	Fvector					TrunkBoneWorldPos			();
	void					StopEngine					();
	// stop engine due to external forces
	void					KillEngine					();
	void					HitEngine					(CObject *obj, float scale);
	float					HitStallThreshold			() const		{ return m_hit_stall_threshold; }
	float					HitAnomalyScale				() const		{ return m_hit_anomaly_scale; }
	u16						GetTrunkBone				() const		{return m_bone_trunk;};
	u16						DriverAnimationType 		();
	virtual	bool			register_schedule() const { return false; }
	// Core events
	virtual DLL_Pure		*_construct					();
	virtual void			Load						( LPCSTR section );
	void					ParseDefinitions			();
	virtual bool			net_Spawn					( CSE_Abstract* DC ) override;
	virtual void			net_Destroy					();
	virtual void			UpdateCL					( ); 
	virtual	void			UpdateEx					(float fov); //called by owner

	virtual void			shedule_Update				(u32 dt);
	virtual void			renderable_Render			( ); 
	virtual	bool			bfAssignMovement			(CScriptEntityAction *tpEntityAction);
	virtual	bool			bfAssignObject				(CScriptEntityAction *tpEntityAction);

	// Network
	virtual void			net_Export					(NET_Packet& P);				// export to server
	virtual void			net_Import					(NET_Packet& P);				// import from server
	virtual bool			net_Relevant				() override	{ return IsMyCar() || OnServer(); }		// relevant for export to server
	virtual bool			UsedAI_Locations			() override;
	virtual	void			net_Relcase					(CObject* O );
	// Input
	virtual void			OnMouseMove					(int x, int y);
	virtual void			OnMouseWheel				(int direction);
	virtual void			OnKeyboardPress				(int dik);
	virtual void			OnKeyboardRelease			(int dik);
	virtual void			OnKeyboardHold				(int dik);
	void					ReleaseInput				();
	virtual void			vfProcessInputKey			(int iCommand, bool bPressed);
	virtual void			OnEvent						( NET_Packet& P, u16 type);
	virtual void			OnAfterExplosion			();
	virtual void			OnBeforeExplosion			();
	virtual void			GetRayExplosionSourcePos	(Fvector &pos);
	virtual void			ActivateExplosionBox		(const Fvector &size,Fvector &in_out_pos){};
	virtual void			ResetScriptData				(void *P=0);

	XPROPERTY(LuaCallable, Name = "Action")
	virtual void			Action						(u16 id, u32 flags);
	
	virtual void			SetParam					(int id, Fvector2 val);
	XPROPERTY(LuaCallable, Name = "SetParam")
	virtual void			SetParam					(int id, Fvector val);
			XPROPERTY(LuaCallable, Name = "HasWeapon")
			bool			HasWeapon					();
			bool			HasWeaponActive				();
			float			HasWeaponActiveDisp			();
			CCarTurret		*GetWeaponActive			();
			XPROPERTY(LuaCallable, Name = "CanHit")
			bool			WpnCanHit					();
			XPROPERTY(LuaCallable, Name = "FireDirDiff")
			float			FireDirDiff					();
			XPROPERTY(LuaCallable, Name = "IsObjectVisible")
			bool			isObjectVisible				(CScriptGameObject* O);
			XPROPERTY(LuaCallable)
			Fvector			CurrentVel					();
	XPROPERTY(LuaCallable)
	virtual float			GetfHealth					() const		{return CEntity::GetfHealth();};
	XPROPERTY(LuaCallable)
	virtual float			SetfHealth					(float value);

	// Hits
	virtual void			HitSignal					(float /**HitAmount/**/,	Fvector& /**local_dir/**/, CObject* /**who/**/, s16 /**element/**/)	{};
	virtual void			HitImpulse					(float /**amount/**/,		Fvector& /**vWorldDir/**/, Fvector& /**vLocalDir/**/)			{};
	virtual void			g_fireParams				(const CHudItem* /**pHudItem/**/, Fvector& /**P/**/, Fvector& /**D/**/)											{};
	virtual ALife::_OBJECT_ID				Initiator					();
	// HUD
	virtual void			OnHUDDraw					(CCustomHUD* hud);
			void			DBGDraw						()																				;
	
	CCameraBase*			Camera						();
	XPROPERTY(LuaCallable)
	void					SetExplodeTime				(u32 et)	;
	XPROPERTY(LuaCallable)
			u32				ExplodeTime					()			;

	CInventory*				GetInventory				() override { return &inventory(); }
		  void				VisualUpdate				(float fov=90.0f);
		  void				DrawCarInfo				();
// 	Fvector					GetTorqAxes					(u16 elem_id);
// 	Fvector					GetForcAxes					(u16 elem_id);
	Fvector					GetAvelAxes					(u16 elem_id);
	Fvector					GetLvelAxes					(u16 elem_id);
// 	void					SetTorqAxes					(u16 elem_id, Fvector trq);
// 	void					SetForcAxes					(u16 elem_id, Fvector frc);
	void					SetAvelAxes					(u16 elem_id, Fvector avel);
	void					SetLvelAxes					(u16 elem_id, Fvector lvel);

// 	void					WheelsSetSpring(float spring = 0.f);
// 	void					WheelsSetDamping(float damping = 0.f);

// 	float					WheelsGetSpring();
// 	float					WheelsGetDamping();

protected:
	virtual void					SpawnInitPhysics					(CSE_Abstract	*D)																;
	virtual void					net_Save							(NET_Packet& P)																	;
	virtual bool					net_SaveRelevant					()																				;
			void					SaveNetState						(NET_Packet& P)																	;
	virtual	void					RestoreNetState						(CSE_PHSkeleton* po)															;
			void					SetDefaultNetState					(CSE_PHSkeleton* po)															;
	virtual void					save								(NET_Packet& output_packet)														;
	virtual void					load								(IReader& input_packet)															;
	virtual void					Serialize							(ISaveObject& Object)															;

	virtual bool					IsHudModeNow						(){return false;};
	
public:
	XPROPERTY(LuaConstructor)
	CCar(void);
	virtual ~CCar(void) override;
	virtual bool					AlwaysTheCrow						() override;
	
private:
	template <class T> IC void fill_wheel_vector(const char* S, xr_vector<T>& type_wheels);
	IC void fill_exhaust_vector(const char* S, xr_vector<SExhaust>& exhausts);
	IC void fill_doors_map(const char* S, xr_map<u16, SDoor>& doors);

public:
	virtual CEntity*			cast_entity				() override						{return this;}

	virtual	void reinit			() override;
	virtual	void reload			(const char* section) override;
	virtual CGameObject			*cast_game_object			() override	{return this;}
	virtual CExplosive			*cast_explosive				() override	{return this;}
	virtual CPhysicsShellHolder	*cast_physics_shell_holder	() override	{return this;}
	virtual CScriptEntity		*cast_script_entity			() override	{return this;}
	virtual IDamageSource		*cast_IDamageSource			() override	{return this;}
	virtual CHolderCustom		*cast_holder_custom			() override	{return this;}
	virtual CCar*				cast_car					() override	{return this;}
	virtual CInventoryOwner		*cast_inventory_owner		() override	{return this;}
	virtual bool				is_ai_obstacle				() const override {return true;}

	virtual bool				unlimited_ammo				() override {return false;}
	virtual bool				infinite_fire				() override {return false;}

	// Gamepad and gyroscope input, IXRAY contract
	virtual void				OnGamepadAxisMove			(int id, Fvector2 value) override;
	virtual void				OnGyroscopeMove				(Fvector3 value) override;
	virtual void				OnGamepadKeyPress			(int id) override;
	virtual void				OnGamepadKeyRelease			(int id) override;

	// Trunk and usable bones, IXRAY contract
	bool						TryTrunk					();
	bool						TryUsableBones				();

	// Multiplayer, IXRAY contract
	bool						IsMyCar						() const;
	virtual void				SyncRead					(NET_Packet& Packet) override;
	virtual void				SyncWrite					(NET_Packet& Packet) override;

private:
	struct SCarNetUpdate
	{
		u32	TimeStamp;
		xr_vector<SPHNetState> StateVec;
	};
	xr_deque<SCarNetUpdate> m_CarNetUpdates;

	void						Interpolate					();
	float						InterpolateStates			(u32 element, SCarNetUpdate const& first, SCarNetUpdate const& last, SPHNetState& current);
	xr_hash_map<u16, shared_str> UsableBones;
	u16							UsableBonesActive = BI_NONE;
	bool						m_roll_lock_second = false;

private:
	car_memory	*m_memory;
};
