#include "stdafx.h"
#include "Fireball.h"
#include "../xrEngine/xr_level_controller.h"

CFireball::CFireball() : CWeaponKnife()
{
}

CFireball::~CFireball(void)
{
}

void CFireball::Load(LPCSTR section)
{
	//inherited::Load(section);
	CWeapon::Load(section);

	fWallmarkSize = pSettings->r_float(section,"wm_size");

	m_sounds.LoadSound(section,"snd_shoot", "sndShot"		, false, SOUND_TYPE_WEAPON_SHOOTING);
	
	knife_material_idx =  GMLib.GetMaterialIdx(pSettings->r_string(*hud_sect,"fireball_material"));
}

bool CFireball::Action(u16 cmd, u32 flags)
{
	switch(cmd){
		case kWPN_FIRE: {
			if (flags&CMD_START){
				SwitchState(ePreFire);
				return true;
			} else if (flags&CMD_STOP && GetState()==ePreFire) {
				SwitchState(eIdle);
				return true;
			}
		} return true;
		case kWPN_ZOOM: return true;

	}
	return inherited::Action(cmd, flags);
}

void CFireball::OnStateSwitch		(u32 S)
{
	if (S==eFire) {
		int t=10;
	}
	inherited::OnStateSwitch(S);
	switch (S) {
		case eFire: {
			StartFlameParticles();
			fTime			+=	fTimeToFire;
		}return;
		case eFire2: {
			//Msg("eFire2 CFireball called!!! WTF???");
					 }return;
		case ePreFire: {
			switch2_PreFire	();
		}break;
	}
}



void CFireball::switch2_PreFire	()
{
	if(IsPending())	return;

	PlayHUDMotion("anim_activate", FALSE, this, ePreFire);
	SetPending		(TRUE);
}

void CFireball::OnAnimationEnd		(u32 state)
{
	switch (state)
	{
	case eFire: {
				Fvector	p1, d; 
				p1.set(get_LastFP()); 
				d.set(get_LastFD());

				KnifeStrike(p1,d);

				SwitchState(eIdle);
		}return;
		case ePreFire:  SetPending	(FALSE); SwitchState(eFire); return;

	}
	inherited::OnAnimationEnd(state);
}


void CFireball::LoadFireParams(LPCSTR section, LPCSTR prefix)
{
	CWeapon::LoadFireParams(section, prefix);

	fvHitPower_1		= fvHitPower;
	fHitImpulse_1		= fHitImpulse;
	m_eHitType_1		= ALife::g_tfString2HitType(pSettings->r_string(section, "hit_type"));
}



void CFireball::UpdateCL			()
{
	inherited::UpdateCL	();
	float dt = Device.fTimeDelta;

	

	//когда происходит апдейт состояния оружия
	//ничего другого не делать
	if(GetNextState() == GetState())
	{
		switch (GetState())
		{
		case eShowing:
		case eHiding:
		case eIdle:
			fTime			-=	dt;
			if (fTime<0)	
				fTime = 0;
			break;
		case eFire:			
			//if(iAmmoElapsed>0)
			//	state_Fire		(dt);
			
			if(fTime<=0)	StopShooting();
			else			fTime -= dt;

			break;
		case eHidden:		break;
		}
	}
}




#include "pch_script.h"

using namespace luabind;

void CFireball::script_register	(lua_State *L)
{
	module(L)
	[
		class_<CFireball,CGameObject>("CFireball")
			.def(constructor<>())
	];
}



