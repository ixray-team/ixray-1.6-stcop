////////////////////////////////////////////////////////////////////////////
//	Module 		: mounted_turret.h
//	Created 	: 01/11/2011
//  Modified 	: 02/11/2011
//	Author		: lost alpha
//	Description : mounted turret
////////////////////////////////////////////////////////////////////////////
#pragma once

#include "../xrScripts/script_export_space.h"
#include "shootingobject.h"
#include "weaponammo.h"
#include "entity.h"
#include "phskeleton.h"
#include "holder_custom.h"
#include "hudsound.h"
#include "weaponammo.h"

class CScriptGameObject;

static const u16 MAX_FIRE_TEMP = 400;

class CMountedTurret :  public CEntity,
						public CPHSkeleton,
						public CHolderCustom,
						public CShootingObject
{
	public:
										CMountedTurret				();
		virtual							~CMountedTurret				();
		virtual		const Fvector		&get_CurrentFirePoint		()						{ return m_fire_pos; }
		virtual		const Fmatrix		&get_ParticlesXFORM			();
		virtual		CHolderCustom		*cast_holder_custom			()						{ return this; }
//		virtual		CScriptEntity		*cast_script_entity			()						{ return this; }
		virtual		CGameObject			*cast_game_object			()						{ return this; }
		virtual		CEntity				*cast_entity				()						{ return this; }
		virtual		CPhysicsShellHolder	*cast_physics_shell_holder	()						{ return this; }
		virtual		CPHSkeleton			*PHSkeleton					()						{ return this; }
		virtual		CMountedTurret		*cast_mounted_turret		()						{ return this; }
		virtual		void				Load						(LPCSTR section);

		virtual		BOOL				net_Spawn					(CSE_Abstract* DC);
		virtual		void				net_Destroy					();
		virtual		void				net_Save					(NET_Packet &P);	
		virtual		void				RestoreNetState				(CSE_PHSkeleton *ph);
		virtual		BOOL				net_SaveRelevant			()						{ return TRUE; }
		virtual     void				save						(NET_Packet &output_packet);
		virtual     void				load						(IReader &input_packet);
		virtual		void				reinit						();
	
		virtual		void				UpdateCL					();
		virtual		void				shedule_Update				(u32 dt);

		virtual		void				renderable_Render			();

		virtual		BOOL				UsedAI_Locations			()						{ return FALSE; }
//		virtual		void				ResetScriptData				(void *P = nullptr);
					Fvector3			GetFirePoint				() const;
		// control functions
		virtual		void				OnMouseMove					(int x, int y);
		virtual		void				OnKeyboardPress				(int dik);
		virtual		void				OnKeyboardRelease			(int dik);
		virtual		void				OnKeyboardHold				(int dik);

		virtual		CInventory			*GetInventory				()						{ return 0;	}

		virtual		void				cam_Update					(float dt, float fov = 90.0f);

		virtual		bool				Use							(const Fvector& pos,const Fvector& dir,const Fvector& foot_pos);
		virtual		bool				attach_Actor				(CGameObject* actor);
		virtual		void				detach_Actor				();
		virtual		Fvector				ExitPosition				();
		virtual		Fvector				ExitVelocity				();
		virtual		bool				allowWeapon					()	const				{ return false; };
		virtual		bool				HUDView						()  const				{ return true; };
		virtual		void				SpawnInitPhysics			(CSE_Abstract *D);
		virtual     DLL_Pure			*_construct					();
		virtual		CCameraBase			*Camera						();
		virtual		void				HitSignal					(float P, Fvector &local_dir, CObject *who, s16 element)	{}
		virtual		void				HitImpulse					(float P, Fvector &vWorldDir, Fvector &vLocalDir)			{}
		virtual		CPhysicsShellHolder	*PPhysicsShellHolder		()						{ return PhysicsShellHolder(); }
		virtual		void				SetParam					(int id, Fvector2 val);
		virtual		void				SetParam					(int id, Fvector3 val);
		virtual		void				Action						(int id, u32 flags);
					u16					GetTemperature				() const				{ return m_temperature; }
		virtual		void				SetNpcOwner					(CGameObject *obj);
					void				SetNpcOwner					(CScriptGameObject *script_game_object);
					void				SetEnemy					(CScriptGameObject *script_game_object);
					Fvector3			GetFireDir					() const				{ return m_selected_fire_dir; }

	private:
		static		void 		__stdcall	BoneCallbackX				(CBoneInstance *B);
		static		void		__stdcall	BoneCallbackY				(CBoneInstance *B);

	protected:
		virtual		void				FireStart					();
		virtual		void				FireEnd						();
		virtual		void				UpdateFire					();
		virtual		void				OnShot						();
					void				UpdateBarrelDir				();
					void				AddShotEffector				();
					void				RemoveShotEffector			();
					void				SetDesiredDir				(float h, float p);
					void				SetDesiredDir				(Fvector3 new_dir);
					void				SetDesiredEnemyPos			(Fvector3 pos);
					void				SetBoneCallbacks			();
					void				ResetBoneCallbacks			();

	private:
		CCameraBase						*m_camera;

		u16								m_fire_bone;
		u16								m_actor_bone;
		u16								m_rotate_x_bone;
		u16								m_rotate_y_bone;
		u16								m_camera_bone;

		u16								m_temperature;
		s8								m_temp_incr;
	
		float							m_tgt_x_rot, m_tgt_y_rot, m_cur_x_rot;
		float							m_cur_y_rot, m_bind_x_rot, m_bind_y_rot;
		Fvector3						m_bind_x, m_bind_y;
		Fvector3						m_fire_dir, m_fire_pos, m_fire_norm;
		Fmatrix							m_i_bind_x_xform, m_i_bind_y_xform, m_fire_bone_xform;
		Fvector2						m_lim_x_rot, m_lim_y_rot; //in bone space
		Fvector3						m_selected_fire_dir, m_initial_pos;
		bool							m_allow_fire;

	protected:
		shared_str						m_ammo_type;
		CCartridge						*m_current_ammo;
		HUD_SOUND_ITEM						m_snd_shot;
		float							m_cam_relax_speed;
		float							m_cam_max_angle;
	
	public:
		DECLARE_SCRIPT_REGISTER_FUNCTION

	public:
		enum ETurretParams
		{
			eActivate = 1,
			eDeactivate,
			eDesiredDir,
			eDesiredEnemyDir,
			eDesiredEnemyPos,
			eFireStart,
			eFireStop,
		};
};

