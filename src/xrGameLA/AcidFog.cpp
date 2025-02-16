/////////////////////////////////////
// file name: AcidFog.cpp
// author: lost alpha
// created: 18/08/2011
// edited: 18/08/2011
// purpose: rusty fog anomaly
//////////////////////////////////////
#include "stdafx.h"
#include "AcidFog.h"
#include "hudmanager.h"
#include "ParticlesObject.h"
#include "level.h"
#include "actor.h"
#include "inventory.h"
#include "physicsshellholder.h"
#include "../xr_collide_form.h"

#define DEFAULT_RUST_DAMAGE 0.15f

CRustyFog::CRustyFog(void) 
{
	m_dwDeltaTime			= 0;
	m_fHitImpulseScale		= 1.f;

	m_bLastBlowoutUpdate	= false;
}

CRustyFog::~CRustyFog(void) 
{
}

void CRustyFog::Load(LPCSTR section) 
{
	inherited::Load(section);
	m_rust_damage = READ_IF_EXISTS(pSettings, r_float, section, "rusty_damage", DEFAULT_RUST_DAMAGE);
}


void CRustyFog::Postprocess(f32 /**val/**/) 
{
}

bool CRustyFog::BlowoutState()
{
	bool result = inherited::BlowoutState();
	if(!result)
	{
		m_bLastBlowoutUpdate = false;
		UpdateBlowout();
	}
	else if(!m_bLastBlowoutUpdate)
	{
		m_bLastBlowoutUpdate = true;
		UpdateBlowout();
	}

	return result;
}

void CRustyFog::Affect(SZoneObjectInfo* O) 
{
	CPhysicsShellHolder *pGameObject = smart_cast<CPhysicsShellHolder*>(O->object);
	if(!pGameObject) return;

	if(O->zone_ignore) return;

	Fvector P; 
	XFORM().transform_tiny(P,CFORM()->getSphere().P);

#ifdef DEBUG
	char l_pow[255]; 
	xr_sprintf(l_pow, "zone hit. %.1f", Power(pGameObject->Position().distance_to(P)));
	if(bDebug) Msg("%s %s",*pGameObject->cName(), l_pow);
#endif

	Fvector hit_dir; 
	hit_dir.set(::Random.randF(-.5f,.5f), 
		::Random.randF(.0f,1.f), 
		::Random.randF(-.5f,.5f)); 
	hit_dir.normalize();


	Fvector position_in_bone_space;

	VERIFY(!pGameObject->getDestroy());

	float dist = pGameObject->Position().distance_to(P) - pGameObject->Radius();
	float power = Power(dist>0.f?dist:0.f);
	float impulse = m_fHitImpulseScale*power*pGameObject->GetMass();

	//статистика по объекту
	O->total_damage += power;
	O->hit_num++;

	if(power > 0.01f) 
	{
		m_dwDeltaTime = 0;
		position_in_bone_space.set(0.f,0.f,0.f);

		CreateHit(pGameObject->ID(),ID(),hit_dir,power,0,position_in_bone_space,impulse,m_eHitTypeBlowout);

		PlayHitParticles(pGameObject);
	}
	if (pGameObject->ID() == Actor()->ID())
		RustyItems();
}

#define RUSTY_ITEM(item, val) \
	if (item) \
		item->ChangeCondition(item->GetCondition() - val); \
		

void CRustyFog::RustyItems()
{
	auto& slots = Actor()->inventory().m_slots;
	RUSTY_ITEM(slots[PISTOL_SLOT].m_pIItem, m_rust_damage);
	RUSTY_ITEM(slots[RIFLE_SLOT].m_pIItem, m_rust_damage);
	RUSTY_ITEM(slots[RIFLE_2_SLOT].m_pIItem, m_rust_damage);
	RUSTY_ITEM(slots[OUTFIT_SLOT].m_pIItem, m_rust_damage);
}