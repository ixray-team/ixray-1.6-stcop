#pragma once

#include "holder_custom.h"
#include "PhysicsShellHolder.h"
class CCameraBase;

class CHolderEntityObject:	public CPhysicsShellHolder, 
						public CHolderCustom
{
private:
	typedef CPhysicsShellHolder inheritedPH;
	typedef CHolderCustom		inheritedHolder;

private:
	CCameraBase*			camera;
	// 
	static void		_BCL	BoneCallbackX		(CBoneInstance *B);
	static void		_BCL	BoneCallbackY		(CBoneInstance *B);
	void					SetBoneCallbacks	();
	void					ResetBoneCallbacks	();

//casts
public:
	virtual CHolderCustom	*cast_holder_custom	() override {return this;}

//general
public:
							CHolderEntityObject		();
	virtual ~CHolderEntityObject	() override;

	virtual void			Load				(const char* section) override;

	virtual bool			net_Spawn			(CSE_Abstract* DC) override;
	virtual void			net_Destroy			() override;
	virtual void			net_Export			(NET_Packet& P) override;	// export to server
	virtual void			net_Import			(NET_Packet& P) override;	// import from server

	virtual void			UpdateCL			() override;

	virtual void			Hit					(SHit* pHDS) override;

//shooting
private:
	Fvector3				m_camera_position;
	Fvector3				m_exit_position;
	Fvector3				m_camera_angle;

	Fvector2				m_lim_x_rot, m_lim_y_rot; //in bone space
	bool 					m_bAllowWeapon;
protected:
	virtual bool			IsHudModeNow		(){return false;};

//HolderCustom
public:
	virtual bool			Use					(const Fvector& pos,const Fvector& dir,const Fvector& foot_pos) override {return !Owner();};
	virtual void			OnMouseMove			(int x, int y) override;
	virtual void			OnGamepadAxisMove	(int id, Fvector2 value) override;
	virtual void			OnGamepadKeyPress	(int id) override;
	virtual void			OnGamepadKeyRelease	(int id) override;
	virtual void			OnKeyboardPress		(int dik) override;
	virtual void			OnKeyboardRelease	(int dik) override;
	virtual void			OnKeyboardHold		(int dik) override;
	virtual void			OnGyroscopeMove		(Fvector3 value) override;
	virtual CInventory*		GetInventory		() override {return nullptr;};
	virtual void			cam_Update			(float dt, float fov=90.0f) override;

	virtual void			renderable_Render	() override;

	virtual void			attach_actor_script(bool bForce = false);
	virtual void			detach_actor_script(bool bForce = false);

	virtual bool			attach_Actor		(CGameObject* actor) override;
	virtual void			detach_Actor		() override;
	virtual bool			allowWeapon			()	const override {return m_bAllowWeapon;};
	virtual bool			HUDView				()	const override {return true;};
	virtual Fvector			ExitPosition() override { return m_exit_position; };

	virtual CCameraBase*	Camera				() override {return camera;};

	virtual void			Action				(u16 id, u32 flags) override;
	virtual void			SetParam			(int id, Fvector2 val) override;
};