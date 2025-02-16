#include "stdafx.h"

#include "WeaponKnife.h"
#include "player_hud.h"
#include "Entity.h"
#include "Actor.h"
#include "level.h"
#include "../xrEngine/xr_level_controller.h"
#include "game_cl_base.h"
#include "../Include/xrRender/Kinematics.h"
#include "../gamemtllib.h"
#include "level_bullet_manager.h"
#include "ai_sounds.h"
#include "game_cl_single.h"

#define KNIFE_MATERIAL_NAME "objects\\knife"

CWeaponKnife::CWeaponKnife() : CWeapon("KNIFE") 
{
	m_attackStart			= false;
	SetState				( eHidden );
	SetNextState			( eHidden );
	knife_material_idx		= (u16)-1;
	bPlaySoundAtStart		= false;
	bHasShoot2Sound			= false;
}
CWeaponKnife::~CWeaponKnife()
{
}

void CWeaponKnife::Load	(LPCSTR section)
{
	// verify class
	inherited::Load		(section);

	fWallmarkSize = pSettings->r_float(section,"wm_size");

	m_sounds.LoadSound(section,"snd_shoot"		, "sndShot"		, false, SOUND_TYPE_WEAPON_SHOOTING		);
	if (pSettings->line_exist(section, "snd_shoot_2"))
	{
		m_sounds.LoadSound(section,"snd_shoot_2", "sndShot2"	, false, SOUND_TYPE_WEAPON_SHOOTING		);
		bHasShoot2Sound = true;
	}

	knife_material_idx =  GMLib.GetMaterialIdx(KNIFE_MATERIAL_NAME);

	bPlaySoundAtStart = !!READ_IF_EXISTS(pSettings, r_bool, section, "snd_shoot_play_at_start", FALSE);
}

void CWeaponKnife::OnStateSwitch	(u32 S)
{
	inherited::OnStateSwitch(S);
	switch (S)
	{
	case eIdle:
		switch2_Idle	();
		break;
	case eShowing:
		switch2_Showing	();
		break;
	case eHiding:
		switch2_Hiding	();
		break;
	case eHidden:
		switch2_Hidden	();
		break;
	case eFire:
		{
			//-------------------------------------------
			m_eHitType		= m_eHitType_1;
			//fHitPower		= fHitPower_1;
			if (ParentIsActor())
			{
				if (GameID() == GAME_SINGLE)
				{
					fCurrentHit		= fvHitPower_1[g_SingleGameDifficulty];
				}
				else
				{
					fCurrentHit		= fvHitPower_1[egdMaster];
				}
			}
			else
			{
				fCurrentHit		= fvHitPower_1[egdMaster];
			}
			fHitImpulse		= fHitImpulse_1;
			fCurrentAP		= fAP_1;
			//-------------------------------------------
			switch2_Attacking	(S);
		}break;
	case eFire2:
		{
			//-------------------------------------------
			m_eHitType		= m_eHitType_2;
			//fHitPower		= fHitPower_2;
			if (ParentIsActor())
				{
				if (GameID() == GAME_SINGLE)
				{
					fCurrentHit		= fvHitPower_2[g_SingleGameDifficulty];
				}
				else
				{
					fCurrentHit		= fvHitPower_2[egdMaster];
				}
			}
			else
			{
				fCurrentHit		= fvHitPower_2[egdMaster];
			}
			fHitImpulse		= fHitImpulse_2;
			fCurrentAP		= fAP_2;
			//-------------------------------------------
			switch2_Attacking	(S);
		}break;
	}
}
	

void CWeaponKnife::KnifeStrike(const Fvector& pos, const Fvector& dir)
{
	CCartridge						cartridge; 
	cartridge.param_s.buckShot		= 1;				
	cartridge.param_s.impair		= 1.0f;
	cartridge.param_s.kDisp			= 1.0f;
	cartridge.param_s.kHit			= 1.0f;
	cartridge.param_s.kSpeed		= 1.0f;
//.	cartridge.param_s.kCritical		= 1.0f;
	cartridge.param_s.kImpulse		= 1.0f;
	cartridge.param_s.kAP			= fCurrentAP;
	cartridge.m_flags.set			(CCartridge::cfTracer, FALSE);
	cartridge.m_flags.set			(CCartridge::cfRicochet, FALSE);
	cartridge.param_s.fWallmarkSize	= fWallmarkSize;
	cartridge.bullet_material_idx	= knife_material_idx;

	while(m_magazine.size() < 2)	m_magazine.push_back(cartridge);
	iAmmoElapsed					= m_magazine.size();
	bool SendHit					= SendHitAllowed(H_Parent());

	if (!bPlaySoundAtStart)
	{
		PlayShootSound();
	}

	CActor* pActor = smart_cast<CActor*>(H_Parent());
	if (pActor && pActor->IsFirstEye())
		Level().BulletManager().AddBullet(	pos, 
											dir, 
											m_fStartBulletSpeed, 
											fCurrentHit, 
											fHitImpulse, 
											H_Parent()->ID(), 
											ID(), 
											m_eHitType, 
											(fireDistance), 
											cartridge, 
											1.f,
											SendHit);
	else
		Level().BulletManager().AddBullet(	pos, 
											dir, 
											m_fStartBulletSpeed, 
											fCurrentHit, 
											fHitImpulse, 
											H_Parent()->ID(), 
											ID(), 
											m_eHitType, 
											fireDistance+1.2f, 
											cartridge, 
											1.f,
											SendHit);
}


void CWeaponKnife::OnAnimationEnd(u32 state)
{
	switch (state)
	{
	case eHiding:	SwitchState(eHidden);	break;
	case eFire: 
	case eFire2: 
		{
            if(m_attackStart) 
			{
				m_attackStart = false;
				if(GetState()==eFire)
					PlayHUDMotion("anim_shoot1_end",		TRUE, this, GetState());
				else
					PlayHUDMotion("anim_shoot2_end",		TRUE, this, GetState());

				Fvector	p1, d; 
				p1.set(get_LastFP()); 
				d.set(get_LastFD());

				if(H_Parent()) 
					smart_cast<CEntity*>(H_Parent())->g_fireParams(this, p1,d);
				else break;

				KnifeStrike(p1,d);
			} 
			else 
				SwitchState(eIdle);
		}break;
	case eShowing:
	case eIdle:	
		SwitchState(eIdle);		break;	
	default:		inherited::OnAnimationEnd(state);
	}
}

void CWeaponKnife::state_Attacking	(float)
{
}

void CWeaponKnife::switch2_Attacking	(u32 state)
{
	if(IsPending())	return;

	if(state==eFire)
		PlayHUDMotion("anim_shoot1_start",		FALSE, this, state);
	else
		PlayHUDMotion("anim_shoot2_start",		FALSE, this, state);

	m_attackStart	= true;
	SetPending			(TRUE);

	if (bPlaySoundAtStart)
	{
		PlayShootSound();
	}
}

void CWeaponKnife::switch2_Idle	()
{
	VERIFY(GetState()==eIdle);

	PlayAnimIdle		();
	SetPending			(FALSE);
}

void CWeaponKnife::switch2_Hiding	()
{
	FireEnd					();
	VERIFY(GetState()==eHiding);
	PlayHUDMotion("anim_hide", TRUE, this, GetState());
}

void CWeaponKnife::switch2_Hidden()
{
	signal_HideComplete		();
	SetPending				(FALSE);
}

void CWeaponKnife::switch2_Showing	()
{
	VERIFY(GetState()==eShowing);
	PlayHUDMotion("anim_show", FALSE, this, GetState());
}


void CWeaponKnife::FireStart()
{	
	inherited::FireStart();
	SwitchState			(eFire);
}

void CWeaponKnife::Fire2Start () 
{
	CActor* pActor = smart_cast<CActor*>(H_Parent());
	if(pActor)
	{
		CEntity::SEntityState st;
		pActor->g_State(st);
			if(!st.bSprint)
			{
			inherited::Fire2Start();
			SwitchState(eFire2);
			}
	}
}


bool CWeaponKnife::Action(u16 cmd, u32 flags) 
{
	if(inherited::Action(cmd, flags)) return true;
	switch(cmd) 
	{

		case kWPN_ZOOM : 
			if(flags&CMD_START) Fire2Start();
			else Fire2End();
			return true;
	}
	return false;
}

void CWeaponKnife::LoadFireParams(LPCSTR section, LPCSTR prefix)
{
	inherited::LoadFireParams(section, prefix);

	string256			full_name;
	string32			buffer;
	shared_str			s_sHitPower_2;
	//fHitPower_1		= fHitPower;
	fvHitPower_1		= fvHitPower;
	fHitImpulse_1		= fHitImpulse;
	m_eHitType_1		= ALife::g_tfString2HitType(pSettings->r_string(section, "hit_type"));

	//fHitPower_2			= pSettings->r_float	(section,strconcat(full_name, prefix, "hit_power_2"));
	s_sHitPower_2		= pSettings->r_string_wb	(section,xr_strconcat(full_name, prefix, "hit_power_2"));
	fvHitPower_2[egdMaster]	= (float)atof(_GetItem(*s_sHitPower_2,0,buffer));//первый параметр - это хит для уровня игры мастер

	fvHitPower_2[egdVeteran]	= fvHitPower_2[egdMaster];//изначально параметры для других уровней
	fvHitPower_2[egdStalker]	= fvHitPower_2[egdMaster];//сложности
	fvHitPower_2[egdNovice]		= fvHitPower_2[egdMaster];//такие же

	int num_game_diff_param=_GetItemCount(*s_sHitPower_2);//узнаём колличество параметров для хитов
	if (num_game_diff_param>1)//если задан второй параметр хита
	{
		fvHitPower_2[egdVeteran]	= (float)atof(_GetItem(*s_sHitPower_2,1,buffer));//то вычитываем его для уровня ветерана
	}
	if (num_game_diff_param>2)//если задан третий параметр хита
	{
		fvHitPower_2[egdStalker]	= (float)atof(_GetItem(*s_sHitPower_2,2,buffer));//то вычитываем его для уровня сталкера
	}
	if (num_game_diff_param>3)//если задан четвёртый параметр хита
	{
		fvHitPower_2[egdNovice]	= (float)atof(_GetItem(*s_sHitPower_2,3,buffer));//то вычитываем его для уровня новичка
	}

	fHitImpulse_2		= pSettings->r_float	(section,xr_strconcat(full_name, prefix, "hit_impulse_2"));
	m_eHitType_2		= ALife::g_tfString2HitType(pSettings->r_string(section, "hit_type_2"));
	fAP_1				= READ_IF_EXISTS(pSettings, r_float, section, "hit_ap",   EPS_L);
	fAP_2				= READ_IF_EXISTS(pSettings, r_float, section, "hit_ap_2", EPS_L);
}

void CWeaponKnife::PlayShootSound()
{
	PlaySound((bHasShoot2Sound && GetState() == eFire2) ? "sndShot2" : "sndShot", get_LastFP());
}

void CWeaponKnife::GetBriefInfo(xr_string& str_name, xr_string& icon_sect_name, xr_string& str_count)
{
	str_name		= NameShort();
	str_count		= "";
	icon_sect_name	= *cNameSect();
}
