#pragma once

#include "../../Entity.h"
#include "../../../Include/xrRender/KinematicsAnimated.h"

class CParticlesObject;

class CPhantom final : public CEntity {
	
	typedef	CEntity inherited;
private:
	enum EState{
		stInvalid		= -2,
		stIdle			= -1,
		stBirth			= 0,
		stFly			= 1,
		stContact		= 2,
		stShoot			= 3,
		stCount
	};
	EState				m_CurState;
	EState				m_TgtState;

	void				SwitchToState_internal		(EState new_state);
	void				SwitchToState				(EState new_state){m_TgtState=new_state;}
	void 		OnIdleState					();
	void 		OnFlyState					();
	void 		OnDeadState					();

	void				UpdateFlyMedia				();

	xr_delegate<void()> UpdateEvent;
private:
	struct SStateData{
		shared_str		particles;
		ref_sound		sound;
		MotionID		motion;
	};
	SStateData			m_state_data[stCount];
private:
	CParticlesObject*	m_fly_particles;
	static void			animation_end_callback	(CBlend* B);
private:
	CObject*			m_enemy;

	float				fSpeed;	
	float				fASpeed;
	Fvector2			vHP;

	float				fContactHit;

	Fmatrix				XFORM_center				();

	CParticlesObject*	PlayParticles				(const shared_str& name, bool bAutoRemove, const Fmatrix& xform);
//	void				PlayMotion					(MotionID);

	void				UpdatePosition				(const Fvector& tgt_pos);

	void				PsyHit						(const CObject *object, float value);
public:
						CPhantom					();
	virtual ~CPhantom					() override;

	virtual void		Load						( const char* section ) override;
	virtual bool		net_Spawn					( CSE_Abstract* DC ) override;
	virtual void		net_Destroy					() override;
	virtual void		net_Relcase					(CObject* O) override;

	virtual void		net_Export					(NET_Packet& P) override;
	virtual void		net_Import					(NET_Packet& P) override;
	virtual void		save						(NET_Packet &output_packet) override;
	virtual void		load						(IReader &input_packet) override;
	virtual void Serialize(ISaveObject& Object) override;

	virtual void		shedule_Update				(u32 DT) override;
	virtual void		UpdateCL					() override;

	virtual void		HitSignal					(float	HitAmount,	Fvector& local_dir, CObject* who, s16 element) override {}
	virtual void		HitImpulse					(float	amount,		Fvector& vWorldDir, Fvector& vLocalDir) override {}
	virtual void		Hit							(SHit* pHDS) override;

	virtual bool		IsVisibleForZones			() override {return false;}

	virtual bool		UsedAI_Locations			() override {return false;}

	virtual CEntity*	cast_entity					() override {return this;}

	void				SetEnemy(CObject* enemy) { m_enemy = enemy; } //Alundaio
};

