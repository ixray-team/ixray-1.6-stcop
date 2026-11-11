#include "StdAfx.h"
#include "MosquitoBald.h"
#include "Level.h"
#include "PhysicsShellHolder.h"
#include "Car.h"
#include "../xrEngine/xr_collide_form.h"

namespace
{
	// Fire and burn anomalies damage the body but must not stall the engine.
	// Other hit types keep the original engine hit behaviour.
	bool IsFireOrBurnHitType(ALife::EHitType HitType)
	{
		switch (HitType)
		{
		case ALife::eHitTypeBurn:
		case ALife::eHitTypeLightBurn:
		case ALife::eHitTypeChemicalBurn:
			return true;
		default:
			return false;
		}
	}

	// The car's bounding sphere is much larger than its body, so the old
	// Position() - Radius() distance made anomalies react from several metres
	// away. Use the nearest point of the car body box instead.
	float ObjectDistanceToZone(CPhysicsShellHolder* Object, const Fvector& ZoneCenter)
	{
		if (CCar* Car = Object->cast_car())
		{
			return Car->GetNearestPoint(ZoneCenter).distance_to(ZoneCenter);
		}

		return Object->Position().distance_to(ZoneCenter) - Object->Radius();
	}
}

CMosquitoBald::CMosquitoBald(void) 
{
	m_fHitImpulseScale		= 1.f;
	m_bLastBlowoutUpdate	= false;
}

CMosquitoBald::~CMosquitoBald(void) 
{
}

void CMosquitoBald::Load(const char* section) 
{
	inherited::Load(section);
}

bool CMosquitoBald::ShouldIgnoreObject(CGameObject* pObject)
{
	if (pObject != nullptr && pObject->cast_car() != nullptr)
	{
		return false;
	}

	return inherited::ShouldIgnoreObject(pObject);
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

void CMosquitoBald::Affect(SZoneObjectInfo* O) 
{
	CPhysicsShellHolder *pGameObject = O->object != nullptr ? O->object->cast_physics_shell_holder() : nullptr;
	if(!pGameObject) return;

	if(O->zone_ignore) return;

	Fvector P; 
	XFORM().transform_tiny(P,CFORM()->getSphere().P);

	Fvector hit_dir; 
	hit_dir.set(	::Random.randF(-.5f,.5f), 
					::Random.randF(.0f,1.f), 
					::Random.randF(-.5f,.5f)); 
	hit_dir.normalize();

	Fvector position_in_bone_space;

	VERIFY(!pGameObject->getDestroy());

	float dist = ObjectDistanceToZone(pGameObject, P);
	float power = Power(dist>0.f?dist:0.f, Radius());
	float impulse = m_fHitImpulseScale*power*pGameObject->GetMass();

	if(power > 0.01f) 
	{
		position_in_bone_space.set(0.f,0.f,0.f);

		ALife::EHitType hit_type = m_eHitTypeBlowout;
		if (CCar* car = pGameObject->cast_car())
		{
			// A non-zero hit_stall_threshold enables the LA anomaly engine hit:
			// HitEngine lowers current power/RPM and can stop the engine.
			if (car->HitStallThreshold() != 0.f && !IsFireOrBurnHitType(hit_type))
			{
				car->HitEngine(this, power);
			}

			// Collision-like body damage is reserved for anomalies that use
			// strike damage (gravity zones). Fire, acid, shock and other zones
			// keep their own hit type so CCar::Hit applies the matching immunity.
			if (hit_type == ALife::eHitTypeStrike)
			{
				const float MassScale = pGameObject->GetMass() / 1000.f;
				power *= MassScale * car->HitAnomalyScale();
			}
		}

		CreateHit(pGameObject->ID(),ID(),hit_dir,power,0,position_in_bone_space,impulse,hit_type);
		PlayHitParticles(pGameObject);
	}
}

void CMosquitoBald::UpdateSecondaryHit()
{
	if(m_dwAffectFrameNum == Device.dwFrame)	
		return;

	m_dwAffectFrameNum	= Device.dwFrame;
	if(Device.dwPrecacheFrame)					
		return;

	for (SZoneObjectInfo& info : m_ObjectInfoMap)
	{
		if(info.object && !info.object->getDestroy())
		{
			CPhysicsShellHolder *pGameObject = info.object->cast_physics_shell_holder();
			if(!pGameObject) return;

			if(info.zone_ignore) return;
			Fvector P; 
			XFORM().transform_tiny(P,CFORM()->getSphere().P);

			Fvector hit_dir; 
			hit_dir.set(	::Random.randF(-.5f,.5f), 
							::Random.randF(.0f,1.f), 
							::Random.randF(-.5f,.5f)); 
			hit_dir.normalize();

			Fvector position_in_bone_space;

			VERIFY(!pGameObject->getDestroy());

			float dist = ObjectDistanceToZone(pGameObject, P);
			float power = m_fSecondaryHitPower * RelativePower(dist>0.f?dist:0.f, Radius());
			if(power<0.0f)
				return;

			float impulse = m_fHitImpulseScale*power*pGameObject->GetMass();
			position_in_bone_space.set(0.f,0.f,0.f);
			CreateHit(pGameObject->ID(),ID(),hit_dir,power,0,position_in_bone_space,impulse,m_eHitTypeBlowout);
		}
	}
}
