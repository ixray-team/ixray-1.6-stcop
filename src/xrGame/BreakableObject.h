// DummyObject.h: interface for the CHangingLamp class.
//
//////////////////////////////////////////////////////////////////////

#ifndef BreakableObjectH
#define BreakableObjectH

#pragma once

#include "PhysicsShellHolder.h"
#include "../xrPhysics/icollisiondamagereceiver.h"

class	IPHStaticGeomShell;
//struct	dContact;
//struct	SGameMtl;

class CBreakableObject: 
	public CPhysicsShellHolder,
	public ICollisionDamageReceiver
{
	typedef	CPhysicsShellHolder		inherited;
private:
		bool				b_resived_damage;
		float				m_max_frame_damage;
static	float				m_damage_threshold;
static	float				m_health_threshhold;
static	float				m_immunity_factor;
		Fvector				m_contact_damage_pos;
		Fvector				m_contact_damage_dir;

		float				fHealth;
		IPHStaticGeomShell	*m_pUnbrokenObject;
		CPhysicsShell		*m_Shell;
static	u32					m_remove_time;
		u32					m_break_time;
		bool				bRemoved;
//		Fbox				m_saved_box;
public:
					CBreakableObject	();
	virtual ~CBreakableObject	() override;

	virtual void	Load				( const char* section) override;
	virtual bool	net_Spawn			( CSE_Abstract* DC) override;
	virtual void	net_Destroy			() override;
	virtual void	shedule_Update		( u32 dt) override;							// Called by sheduler
	virtual void	UpdateCL			() override;
	virtual bool	renderable_ShadowGenerate	( ) override { return false;	}
	virtual bool	renderable_ShadowReceive	( ) override { return true;	}

	virtual void	Hit					(SHit* pHDS) override;

	virtual void	net_Export			(NET_Packet& P) override;
	virtual void	net_Import			(NET_Packet& P) override;
	virtual bool	UsedAI_Locations	() override;

	virtual CBreakableObject* cast_breakable_object() override { return this; }
private:
	void			Init				();
	void			CreateUnbroken		();
	void			CreateBroken		();
	void			DestroyUnbroken		();
	void			ActivateBroken		();
	void			Split				();
	void			Break				();
	void			ApplyExplosion		(const Fvector &dir,float impulse);
	void			CheckHitBreak		(float power,ALife::EHitType hit_type);
	void			ProcessDamage		();
	void			SendDestroy			();
	virtual void	_BCL		enable_notificate	() override;
//	static 	void	ObjectContactCallback(bool& /**do_colide/**/,bool bo1,dContact& c,SGameMtl * /*material_1*/,SGameMtl * /*material_2*/);
private:
	virtual ICollisionDamageReceiver	*PHCollisionDamageReceiver	() override {return (this);}
	virtual void					CollisionHit				( ALife::_OBJECT_ID source_id, u16 bone_id, float power, const Fvector &dir, Fvector &pos ) override;
};

#endif //BreakableObjectH
