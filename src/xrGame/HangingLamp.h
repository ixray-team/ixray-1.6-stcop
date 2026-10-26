// DummyObject.h: interface for the CHangingLamp class.
//
//////////////////////////////////////////////////////////////////////

#ifndef HangingLampH
#define HangingLampH
#pragma once

#include "GameObject.h"
#include "PhysicsShellHolder.h"
#include "PHSkeleton.h"
#include "../xrScripts/script_export_space.h"
// refs
class CLAItem;
class CPhysicsElement;
class CSE_ALifeObjectHangingLamp;
class CPHElement;
class CHangingLamp final : 
	public CPhysicsShellHolder,
	public CPHSkeleton
{//need m_pPhysicShell
	typedef	CPhysicsShellHolder		inherited;
	// Allow CScriptGameObject to access private members for script control (cutscenes, logic)	
	friend class CScriptGameObject;

private:
	u16				light_bone;
	u16				ambient_bone;

	ref_light		light_render;
	ref_light		light_ambient;
	CLAItem*		lanim;
	float			ambient_power;
	bool			m_bState;
	
	ref_glow		glow_render;
	
	float			fHealth;
	float			fBrightness;
	void			CreateBody		(CSE_ALifeObjectHangingLamp	*lamp);
	void			Init();
	void			RespawnInit		();
	bool			Alive			(){return fHealth>0.f;}


public:
					CHangingLamp	();
	virtual ~CHangingLamp	() override;
	void			TurnOn			();
	void			TurnOff			();
	bool			IsActive();
	virtual void	Load			( const char* section) override;
	virtual bool	net_Spawn		( CSE_Abstract* DC) override;
	virtual void	net_Destroy		() override;
	virtual void	shedule_Update	( u32 dt) override;							// Called by sheduler
	virtual void	UpdateCL		( ) override;								// Called each frame, so no need for dt


	virtual void SpawnInitPhysics(CSE_Abstract	*D) override;
	virtual CPhysicsShellHolder* PPhysicsShellHolder() override {return PhysicsShellHolder();};
	virtual void CopySpawnInit() override;
	virtual void net_Save(NET_Packet& P) override;
	virtual bool net_SaveRelevant() override;
	virtual void save(NET_Packet &output_packet) override;
	virtual void load(IReader &input_packet) override;
	virtual void Serialize(ISaveObject& Object) override;

	virtual bool	renderable_ShadowGenerate	( ) override { return true;	}
	virtual bool	renderable_ShadowReceive	( ) override { return true;	}

	virtual void	Hit				(SHit* pHDS) override;
	virtual void	net_Export		(NET_Packet& P) override;
	virtual void	net_Import		(NET_Packet& P) override;
	virtual bool	UsedAI_Locations() override;

	virtual CHangingLamp* cast_hanging_lamp() override { return this; }

	virtual void	Center			(Fvector& C)	const override;
	virtual float	Radius			()				const override;
	DECLARE_SCRIPT_REGISTER_FUNCTION
};
#endif //HangingLampH
