#pragma once

#include "holder_custom.h"
#include "ShootingObject.h"
#include "PhysicsShellHolder.h"
#include "HudSound.h"
class CCartridge;
class CCameraBase;

#define DESIRED_DIR 1

class CWeaponStatMgun final : public CPhysicsShellHolder, 
	public CHolderCustom,
	public CShootingObject
{
private:
	using inheritedPH = CPhysicsShellHolder;
	using inheritedHolder = CHolderCustom;
	using inheritedShooting = CShootingObject;

private:
	CCameraBase*			camera;
	// 
	static void		_BCL	BoneCallbackX		(CBoneInstance *B);
	static void		_BCL	BoneCallbackY		(CBoneInstance *B);
	void					SetBoneCallbacks	();
	void					ResetBoneCallbacks	();

	HUD_SOUND_COLLECTION_LAYERED m_sounds_layered;

//casts
public:
	virtual CHolderCustom*	cast_holder_custom	() override		{return this;}
	virtual CGameObject*	cast_game_object	() override		{return this;}
	virtual CPhysicsShellHolder* cast_physics_shell_holder() override { return this; }

//general
public:
							CWeaponStatMgun		();
	virtual ~CWeaponStatMgun	() override;

	virtual void			Load				(const char* section) override;

	virtual bool			net_Spawn			(CSE_Abstract* DC) override;
	virtual void			net_Destroy			() override;
	virtual void			net_Export			(NET_Packet& P) override;	// export to server
	virtual void			net_Import			(NET_Packet& P) override;	// import from server

	virtual void			UpdateCL			() override;

	virtual void			Hit					(SHit* pHDS) override;

//shooting
private:
	u16						m_rotate_x_bone, m_rotate_y_bone, m_fire_bone, m_camera_bone;
	float					m_tgt_x_rot, m_tgt_y_rot, m_cur_x_rot, m_cur_y_rot, m_bind_x_rot, m_bind_y_rot;
	Fvector					m_bind_x, m_bind_y;
	Fvector					m_fire_dir,m_fire_pos;

	Fmatrix					m_i_bind_x_xform, m_i_bind_y_xform, m_fire_bone_xform;
	Fvector2				m_lim_x_rot, m_lim_y_rot; //in bone space
	CCartridge*				m_Ammo;
	float					m_barrel_speed;
	Fvector2				m_dAngle;
	Fvector					m_destEnemyDir;
	bool					m_allow_fire;
	float					camRelaxSpeed;
	float					camMaxAngle;


	bool 					m_firing_disabled;
	bool					m_overheat_enabled;
	float 					m_overheat_value;
	float 					m_overheat_time_quant;
	float 					m_overheat_decr_quant;
	float 					m_overheat_threshold;
	shared_str				m_overheat_particles;
	xr_shared_ptr<CParticlesObject>		p_overheat;

	float					m_vibration_time = 0.1f;
	float					m_vibration_factor_left = 1.0f;
	float					m_vibration_factor_right = 1.0f;
protected:
	void					UpdateBarrelDir		();
	virtual const Fvector&	get_CurrentFirePoint();
	virtual const Fmatrix&	get_ParticlesXFORM	();

	virtual void			FireStart			() override;
	virtual void			FireEnd				() override;
	virtual	void			UpdateFire			();
	virtual	void			OnShot				();
			void			AddShotEffector		();
			void			RemoveShotEffector	();
			void			SetDesiredDir		(float h, float p);
	virtual bool			IsHudModeNow		() override {return false;};

//HolderCustom
public:
	virtual bool			Use					(const Fvector& pos,const Fvector& dir,const Fvector& foot_pos) override {return !Owner();};
	virtual void			OnGamepadAxisMove	(int id, Fvector2 value) override;
	virtual void			OnGyroscopeMove		(Fvector3 value) override;
	virtual void			OnMouseMove			(int x, int y) override;
	virtual void			OnGamepadKeyPress	(int id) override {};
	virtual void			OnGamepadKeyRelease	(int id) override {};
	virtual void			OnKeyboardPress		(int dik) override;
	virtual void			OnKeyboardRelease	(int dik) override;
	virtual void			OnKeyboardHold		(int dik) override;
	virtual CInventory*		GetInventory		() override {return NULL;};
	virtual void			cam_Update			(float dt, float fov=90.0f) override;

	virtual void			renderable_Render	() override;

	virtual bool			attach_Actor		(CGameObject* actor) override;
	virtual void			detach_Actor		() override;
	virtual bool			allowWeapon			()	const override {return false;};
	virtual bool			HUDView				()	const override {return true;};
	virtual Fvector			ExitPosition		() override {return Fvector().set(0.0f,0.0f,0.0f);};

	virtual CCameraBase*	Camera				() override {return camera;};

	virtual void			Action				(u16 id, u32 flags) override;
	virtual void			SetParam			(int id, Fvector2 val) override;
};