#include "StdAfx.h"
#include "Grenade.h"
#include "../xrPhysics/PhysicsShell.h"
//.#include "WeaponHUD.h"
#include "Entity.h"
#include "Actor.h"
#include "Inventory.h"
#include "Level.h"
#include "xrMessages.h"
#include "../xrEngine/xr_level_controller.h"
#include "game_cl_base.h"
#include "xrServer_Objects_ALife.h"

#define GRENADE_REMOVE_TIME		30000
const float default_grenade_detonation_threshold_hit=100;

CGrenade::CGrenade(void) 
{
	m_destroy_callback.clear();
	m_eSoundCheckout = ESoundTypes(SOUND_TYPE_WEAPON_RECHARGING);
	m_eExplosionHitTypes.clear();
}

CGrenade::~CGrenade(void) 
{
}

void CGrenade::Load(LPCSTR section) 
{
	inherited::Load(section);
	CExplosive::Load(section);

	m_bExplosionOnHit = READ_IF_EXISTS(pSettings, r_bool, section, "explosion_on_hit", false);
	m_bExplosionWhileNotActivated = READ_IF_EXISTS(pSettings, r_bool, section, "explosive_while_not_activated", false);

	if (m_bExplosionOnHit)
	{
		string128 S1;
		LPCSTR S_ = pSettings->r_string(section, "explosion_hit_types");
		int count = _GetItemCount(S_);
		for (int i = 0; i < count; ++i)
		{
			_GetItem(S_, i, S1);
			m_eExplosionHitTypes.push_back(static_cast<u32>(atoi(S1)));
		}
	}

	m_contact_grenade_params.SafeTime = READ_IF_EXISTS(pSettings, r_u32, section, "safe_time", 0);
	m_contact_grenade_params.DelayTime = READ_IF_EXISTS(pSettings, r_u32, section, "delay_time", 0);
	m_contact_grenade_params.ExplosionOnKick = READ_IF_EXISTS(pSettings, r_bool, section, "explosion_on_kick", false);
	m_contact_grenade_params.MinExplosionSpeed = READ_IF_EXISTS(pSettings, r_float, section, "min_explosion_speed", 0.0f);
	m_contact_grenade_params.DeactivateOnLowSpeedContact = READ_IF_EXISTS(pSettings, r_bool, section, "deactivate_on_minimal_speed_contact", false);

	//////////////////////////////////////
	//время убирания оружия с уровня
	if(pSettings->line_exist(section,"grenade_remove_time"))
		m_dwGrenadeRemoveTime = pSettings->r_u32(section,"grenade_remove_time");
	else
		m_dwGrenadeRemoveTime = GRENADE_REMOVE_TIME;
	m_grenade_detonation_threshold_hit=READ_IF_EXISTS(pSettings,r_float,section,"detonation_threshold_hit",default_grenade_detonation_threshold_hit);
}

void CGrenade::LoadSounds(LPCSTR section)
{
	inherited::LoadSounds(section);

	m_sounds.LoadSound(section, "snd_checkout", "sndCheckout", false, m_eSoundCheckout);
}

bool CGrenade::CheckGrenadeExplosionByHit(SHit* SHit)
{
	if (m_bExplosionOnHit)
	{
		if (m_grenade_detonation_threshold_hit < SHit->power)
		{
			if (!Useful() || m_bExplosionWhileNotActivated)
			{
				if (!m_eExplosionHitTypes.empty())
				{
					for (u32 i = 0; i < m_eExplosionHitTypes.size(); i++)
					{
						if (SHit->hit_type == static_cast<ALife::EHitType>(m_eExplosionHitTypes[i]))
							return true;
					}
				}
				else
				{
					if (SHit->hit_type == ALife::eHitTypeExplosion)
						return true;
				}
			}
		}
	}

	return false;
}

void CGrenade::Hit					(SHit* pHDS)
{
	if (ALife::eHitTypeExplosion == pHDS->hit_type && m_grenade_detonation_threshold_hit < pHDS->damage() && CExplosive::Initiator() == u16(-1) || CheckGrenadeExplosionByHit(pHDS))
	{
		CExplosive::SetCurrentParentID(pHDS->who->ID());
		Destroy();
	}
	inherited::Hit(pHDS);
}

BOOL CGrenade::net_Spawn(CSE_Abstract* DC) 
{
	m_dwGrenadeIndependencyTime			= 0;
	BOOL ret= inherited::net_Spawn		(DC);
	Fvector box;BoundingBox().getsize	(box);
	float max_size						= _max(_max(box.x,box.y),box.z);
	box.set								(max_size,max_size,max_size);
	box.mul								(3.f);
	CExplosive::SetExplosionSize		(box);
	m_thrown							= false;
	return								ret;
}

void CGrenade::net_Destroy() 
{
	if(m_destroy_callback)
	{
		m_destroy_callback				(this);
		m_destroy_callback				= destroy_callback(nullptr);
	}

	inherited::net_Destroy				();
	CExplosive::net_Destroy				();
}

void CGrenade::OnH_B_Independent(bool just_before_destroy) 
{
	inherited::OnH_B_Independent(just_before_destroy);
}

void CGrenade::OnH_A_Independent() 
{
	m_dwGrenadeIndependencyTime			= Level().timeServer();
	inherited::OnH_A_Independent		();	
}

void CGrenade::OnH_A_Chield()
{
	m_dwGrenadeIndependencyTime			= 0;
	m_dwDestroyTime						= 0xffffffff;
	inherited::OnH_A_Chield				();
}

void CGrenade::State(u32 state) 
{
	switch (state)
	{
	case eThrowStart:
	{
		if (H_Parent())
		{
			Fvector SndPos;

			if (H_Parent()->Local())
				Center(SndPos);
			else
				SndPos.set(H_Parent()->Position());

			PlaySound("sndCheckout", SndPos);
		}
	}break;
	case eThrowEnd:
		{
			if(m_thrown)
			{
				if (m_pPhysicsShell)
					m_pPhysicsShell->Deactivate();
				xr_delete	( m_pPhysicsShell );
				m_dwDestroyTime			= 0xffffffff;
				PutNextToSlot			();
				if (Local())
				{
					DestroyObject();
				}
				
			};
		}break;
	};
	inherited::State( state );
}

bool CGrenade::DropGrenade()
{
	EMissileStates grenade_state = static_cast<EMissileStates>(GetState());
	if (((grenade_state == eThrowStart) ||
		(grenade_state == eReady) ||
		(grenade_state == eThrow)) &&
		(!m_thrown)
		)
	{
		Throw();
		return true;
	}
	return false;
}

void CGrenade::DiscardState()
{
	if(IsGameTypeSingle() && (GetState()==eReady || GetState()==eThrow) )
		OnStateSwitch(eIdle);
}

bool CGrenade::SendDeactivateItem()
{
	CActor* pActor = m_pInventory->GetOwner() ? m_pInventory->GetOwner()->cast_actor() : nullptr;
	if (pActor && (GetState() == eReady || GetState() == eThrow))
		return false;

	return inherited::SendDeactivateItem();
}

void CGrenade::Throw() 
{
	if (m_thrown)
		return;

	if (!m_fake_missile)
		return;

	CGrenade					*pGrenade = smart_cast<CGrenade*>( m_fake_missile );
	VERIFY						(pGrenade);
	
	if (pGrenade) 
	{
		pGrenade->set_destroy_time(m_dwDestroyTimeMax);
//установить ID того кто кинул гранату
		pGrenade->SetInitiator( H_Parent()->ID() );
	}
	inherited::Throw			();
	m_fake_missile->processing_activate();//@sliph
	m_thrown = true;
}



void CGrenade::Destroy() 
{
	//Generate Expode event
	Fvector						normal;

	if(m_destroy_callback)
	{
		m_destroy_callback		(this);
		m_destroy_callback	=	destroy_callback(nullptr);
	}

	FindNormal					(normal);
	CExplosive::GenExplodeEvent	(Position(), normal);
}



bool CGrenade::Useful() const
{

	bool res = (/* !m_throw && */ m_dwDestroyTime == 0xffffffff && CExplosive::Useful() && TestServerFlag(CSE_ALifeObject::flCanSave));

	return res;
}

void CGrenade::OnEvent(NET_Packet& P, u16 type) 
{
	inherited::OnEvent			(P,type);
	CExplosive::OnEvent			(P,type);
}

void CGrenade::PutNextToSlot()
{
	if (OnClient())
	{
		return;
	}

	VERIFY(!getDestroy());
	//выкинуть гранату из инвентаря
	NET_Packet P;
	if (m_pInventory)
	{
		m_pInventory->Ruck(this);

		this->u_EventGen(P, GEG_PLAYER_ITEM2RUCK, this->H_Parent()->ID());
		P.w_u16(this->ID());
		this->u_EventSend(P);
	}
	else
	{
		Msg("! PutNextToSlot : m_pInventory = nullptr [%d][%d]", ID(), Device.dwFrame);
	}

	if (H_Parent() && H_Parent()->cast_inventory_owner() && m_pInventory)
	{
		PIItem finded_item = m_pInventory->Same(this, true);

		CGrenade* pNext = finded_item ? finded_item->cast_grenade() : nullptr;
		if (!pNext)
		{
			finded_item = m_pInventory->SameSlot(GRENADE_SLOT, this, true);
			pNext = finded_item ? finded_item->cast_grenade() : nullptr;
		}

		VERIFY(pNext != this);

		if (pNext && m_pInventory->Slot(pNext->BaseSlot(), pNext))
		{
			pNext->u_EventGen(P, GEG_PLAYER_ITEM2SLOT, pNext->H_Parent()->ID());
			P.w_u16(pNext->ID());
			P.w_u16(pNext->BaseSlot());
			pNext->u_EventSend(P);
			m_pInventory->SetActiveSlot(pNext->BaseSlot());
		}
		else
		{
			if (CActor* pActor = m_pInventory->GetOwner() ? m_pInventory->GetOwner()->cast_actor() : nullptr)
			{
				pActor->OnPrevWeaponSlot();
			}
		}

		m_thrown = false;
	}
}

void CGrenade::OnAnimationEnd(u32 state) 
{
	switch(state)
	{
	case eThrowEnd: SwitchState(eHidden);	break;
	default : inherited::OnAnimationEnd(state);
	}
}


void CGrenade::UpdateCL() 
{
	PROF_EVENT("CGrenade::UpdateCL")
	inherited::UpdateCL			();
	CExplosive::UpdateCL		();

	if(!IsGameTypeSingle())	make_Interpolation();
}


bool CGrenade::Action(u16 cmd, u32 flags) 
{
	if(inherited::Action(cmd, flags)) return true;

	switch(cmd) 
	{
	//переключение типа гранаты
	case kWPN_NEXT:
		{
            if (flags & CMD_START) 
			{
				if (m_pInventory)
				{
					for (PIItem item : m_pInventory->m_ruck)
					{
						CGrenade* pGrenade = item->cast_grenade();
						if (pGrenade && xr_strcmp(pGrenade->cNameSect(), cNameSect())) 
						{
							m_pInventory->PutGrenade(pGrenade);
							return true;
						}
					}
					return true;
				}
			}
			return true;
		};
	}
	return false;
}


bool CGrenade::NeedToDestroyObject()	const
{
	if ( IsGameTypeSingle()			) return false;
	if ( Remote()					) return false;
	if ( TimePassedAfterIndependant() > m_dwGrenadeRemoveTime)
		return true;

	return false;
}

ALife::_TIME_ID	 CGrenade::TimePassedAfterIndependant()	const
{
	if(!H_Parent() && m_dwGrenadeIndependencyTime != 0)
		return Level().timeServer() - m_dwGrenadeIndependencyTime;
	else
		return 0;
}

BOOL CGrenade::UsedAI_Locations		()
{
#pragma todo("Dima to Yura : It crashes, because on net_Spawn object doesn't use AI locations, but on net_Destroy it does use them")
	return inherited::UsedAI_Locations( );//m_dwDestroyTime == 0xffffffff;
}

void CGrenade::net_Relcase(CObject* O )
{
	CExplosive::net_Relcase(O);
	inherited::net_Relcase(O);
}

void CGrenade::DeactivateItem()
{
	//Drop grenade if primed
	StopCurrentAnimWithoutCallback();
	if (!GetTmpPreDestroy() && Local() && (GetState() == eThrowStart || GetState() == eReady || GetState() == eThrow))
	{
		if (m_fake_missile)
		{
			CGrenade* pGrenade	= m_fake_missile->cast_grenade();
			if (pGrenade)
			{
				if (m_pInventory->GetOwner())
				{
					if (CActor* pActor = m_pInventory->GetOwner()->cast_actor())
					{
						if (!pActor->g_Alive())
						{
							m_constpower = false;
							m_fThrowForce = 0;
						}
					}
				}				
				Throw();
			};
		};
	};

	inherited::DeactivateItem();
}

bool CGrenade::GetBriefInfo( II_BriefInfo& info )
{
	VERIFY( m_pInventory );
	info.clear();

	info.name._set( m_nameShort );
	info.icon._set( cNameSect() );

	u32 ThisGrenadeCount	= m_pInventory->dwfGetSameItemCount( cNameSect().c_str(), true );
	
	string16 stmp;
	xr_sprintf( stmp, "%d", ThisGrenadeCount );
	info.cur_ammo._set( stmp );
	return true;
}
