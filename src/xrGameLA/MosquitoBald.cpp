#include "stdafx.h"
#include "mosquitobald.h"
#include "hudmanager.h"
#include "ParticlesObject.h"
#include "level.h"
#include "physicsshellholder.h"
#include "../xr_collide_form.h"
#include "Actor.h"
#include "Car.h"

CMosquitoBald::CMosquitoBald(void) 
{
	m_dwDeltaTime			= 0;
	m_fHitImpulseScale		= 1.f;

	m_bLastBlowoutUpdate	= false;
}

CMosquitoBald::~CMosquitoBald(void) 
{
}

void CMosquitoBald::Load(LPCSTR section) 
{
	inherited::Load(section);

	m_killCarEngine = !!READ_IF_EXISTS(pSettings, r_bool, section, "car_kill_engine", false);
	m_hitKoefCar = READ_IF_EXISTS(pSettings, r_float, section, "hit_koef_car", 1.f);
}


void CMosquitoBald::Postprocess(f32 /**val/**/) 
{
}

bool CMosquitoBald::BlowoutState()
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

bool CMosquitoBald::ShouldIgnoreObject(CGameObject* pObject)
{
	auto pCar = smart_cast<CCar*>(pObject);
	if (pCar) return false;

	return inherited::ShouldIgnoreObject(pObject);
}

void CMosquitoBald::Affect(SZoneObjectInfo* O) 
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


	if (power > EPS_L)
	{
		m_dwDeltaTime = 0;
		position_in_bone_space.set(0.f,0.f,0.f);

		auto car = smart_cast<CCar*>(pGameObject);
		if (car != nullptr)
		{
			if (m_killCarEngine)
			{
				car->KillEngine();
			}
			power *= m_hitKoefCar;
		}
		CreateHit(pGameObject->ID(),ID(),hit_dir,power,0,position_in_bone_space,impulse,m_eHitTypeBlowout);

		PlayHitParticles(pGameObject);

		O->total_damage += power;
	}
	
	//статистика по объекту
	O->hit_num++;
}
