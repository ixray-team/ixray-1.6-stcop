#pragma once
#include "Missile.h"
#include "Explosive.h"
#include "../xrEngine/Feel_Touch.h"

class CGrenade : public CMissile,
	public CExplosive
{
	using inherited = CMissile;
public:
	CGrenade();
	virtual ~CGrenade() override = default;


	virtual void			Load								(const char* section) override;
	virtual void			LoadSounds							(const char* section) override;

	virtual bool 			net_Spawn							(CSE_Abstract* DC) override;
	virtual void 			net_Destroy							() override;
	virtual void 			net_Relcase							(CObject* O ) override;

	virtual void 			OnH_B_Independent					(bool just_before_destroy) override;
	virtual void 			OnH_A_Independent					() override;
	virtual void 			OnH_A_Chield						() override;
	virtual void			DiscardState						() override;

	virtual void 			OnEvent								(NET_Packet& P, u16 type) override;
	virtual bool			DropGrenade							();			//in this case if grenade state is eReady, it should Throw

	virtual void 			OnAnimationEnd						(u8 state) override;
	virtual void 			UpdateCL							() override;

	virtual void 			Throw() override;
	virtual void 			Destroy() override;


	virtual bool			Action								(u16 cmd, u32 flags) override;
	virtual bool			Useful								() const override;
	virtual void			State								(u8 state) override;

	virtual void			OnH_B_Chield						() override {inherited::OnH_B_Chield();}

	virtual bool			CheckGrenadeExplosionByHit			(SHit* SHit);
	virtual void			Hit									(SHit* pHDS) override;

	virtual bool			NeedToDestroyObject					() const override;
	virtual ALife::_TIME_ID	TimePassedAfterIndependant			() const override;

			void			PutNextToSlot						();

	virtual void			DeactivateItem						() override;
	virtual bool			GetBriefInfo						(II_BriefInfo& info) override;

	virtual bool			SendDeactivateItem					(bool Force) override;

	void FastThrow();

protected:
	ALife::_TIME_ID			m_dwGrenadeRemoveTime;
	ALife::_TIME_ID			m_dwGrenadeIndependencyTime;
	bool					m_bExplosionOnHit;
	bool					m_bExplosionWhileNotActivated;
	U32Vec					m_eExplosionHitTypes;

	struct SContactGrenadeParams
	{
		u32 SafeTime = 0, DelayTime = 0;
		bool ExplosionOnKick = false, DeactivateOnLowSpeedContact = false;
		float MinExplosionSpeed = 0.0f;
	} m_contact_grenade_params;

protected:
	ESoundTypes				m_eSoundCheckout;
private:
	float					m_grenade_detonation_threshold_hit;
	bool					m_thrown;

protected:
	virtual void			UpdateXForm							() override { CMissile::UpdateXForm(); };
public:
	virtual bool			UsedAI_Locations					() override;
	virtual CGrenade		*cast_grenade						() override {return this;}
	virtual CExplosive		*cast_explosive						() override {return this;}
	virtual CMissile		*cast_missile						() override {return this;}
	virtual CHudItem		*cast_hud_item						() override {return this;}
	virtual CGameObject		*cast_game_object					() override {return this;}
	virtual IDamageSource	*cast_IDamageSource					() override {return CExplosive::cast_IDamageSource();}

	SContactGrenadeParams& ContactParams						() { return m_contact_grenade_params; }

	typedef					xr_delegate< void (CGrenade*) >	destroy_callback;
	void					set_destroy_callback				(destroy_callback callback) 
																{ m_destroy_callback = callback; }
private:
	destroy_callback		m_destroy_callback;
};
