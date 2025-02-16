#pragma once

#include "WeaponCustomPistol.h"
#include "../xrScripts/script_export_space.h"

class CWeaponKnife: public CWeapon {
private:
	typedef CWeapon inherited;
protected:
	bool				m_attackStart;

protected:

	virtual void		switch2_Idle				();
	virtual void		switch2_Hiding				();
	virtual void		switch2_Hidden				();
	virtual void		switch2_Showing				();
			void		switch2_Attacking			(u32 state);

	virtual void		OnAnimationEnd				(u32 state);
	virtual void		OnStateSwitch				(u32 S);

	void				state_Attacking				(float dt);

	virtual void		KnifeStrike					(const Fvector& pos, const Fvector& dir);

	float				fWallmarkSize;
	u16					knife_material_idx;

protected:	
	ALife::EHitType		m_eHitType;

	ALife::EHitType		m_eHitType_1;
	//float				fHitPower_1;
	Fvector4			fvHitPower_1;
	float				fHitImpulse_1;
	float				fAP_1;

	ALife::EHitType		m_eHitType_2;
	//float				fHitPower_2;
	Fvector4			fvHitPower_2;
	float				fHitImpulse_2;
	float				fAP_2;

	// проигрывать звук удара с анимации начала удара, а не конца
	bool				bPlaySoundAtStart;
	bool				bHasShoot2Sound;

	float				fCurrentHit;
	float				fCurrentAP;

protected:
	virtual void		LoadFireParams					(LPCSTR section, LPCSTR prefix);
			void		PlayShootSound					();

public:
						CWeaponKnife(); 
	virtual				~CWeaponKnife(); 

	void				Load							(LPCSTR section);

	virtual void		Fire2Start						();
	virtual void		FireStart						();


	virtual bool		Action							(u16 cmd, u32 flags);
	virtual void		GetBriefInfo					(xr_string& str_name, xr_string& icon_sect_name, xr_string& str_count);

	DECLARE_SCRIPT_REGISTER_FUNCTION
};
