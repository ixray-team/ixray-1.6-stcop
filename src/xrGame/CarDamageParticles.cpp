#include "StdAfx.h"
#include "CarDamageParticles.h"
#ifdef DEBUG

#include "PHDebug.h"
#endif
#include "alife_space.h"
#include "Hit.h"
#include "PHDestroyable.h"
#include "Car.h"
#include "../Include/xrRender/Kinematics.h"
#include "../xrPhysics/IPHWorld.h"
//#include "PHWorld.h"
//extern CPHWorld*	ph_world;
void read_bones(IKinematics *K, const char* S , xr_vector<u16>& bones)
{
	string64					S1;
	int count =					_GetItemCount(S);
	for (int i=0 ;i<count; ++i) 
	{
		_GetItem					(S,i,S1);

		u16 bone_id	=				K->LL_BoneID(S1);
		R_ASSERT3(bone_id!=BI_NONE,"wrong bone",S1);
		xr_vector<u16>::iterator iter=std::find(bones.begin(),bones.end(),bone_id);
		R_ASSERT3(iter==bones.end(),"double bone",S1);
		bones.push_back					(bone_id);
	}
}
void CCarDamageParticles::Init(CCar *car)
{
	m_playing1 = false;
	m_playing2 = false;
	m_playing3 = false;
	IKinematics *K=PKinematics(car->Visual());
	CInifile	*ini=K->LL_UserData();
	if(ini->section_exist("damage_particles"))
	{
		m_car_damage_particles1=ini->r_string("damage_particles","car_damage_particles1");
		m_car_damage_particles2=ini->r_string("damage_particles","car_damage_particles2");
		m_wheels_damage_particles1=ini->r_string("damage_particles","wheels_damage_particles1");
		m_wheels_damage_particles2=ini->r_string("damage_particles","wheels_damage_particles2");

		read_bones(K,ini->r_string("damage_particles","particle_bones1"),bones1);
		read_bones(K,ini->r_string("damage_particles","particle_bones2"),bones2);

		// The zero-health fire effect is optional for custom car packs.
		if (ini->line_exist("damage_particles", "car_damage_particles3"))
			m_car_damage_particles3 = ini->r_string("damage_particles", "car_damage_particles3");
		if (ini->line_exist("damage_particles", "particle_bones3"))
			read_bones(K, ini->r_string("damage_particles", "particle_bones3"), bones3);
	}
}

void CCarDamageParticles::Play1(CCar* car)
{
	if (m_playing1)
	{
		return;
	}

	if (*m_car_damage_particles1)
	{
		BIDS_I i = bones1.begin(), e = bones1.end();
		TParticlesPlayer* PPlayer = car->GetOrCreateComponent<TParticlesPlayer>();
		for (; e != i; ++i)
		{
			PPlayer->StartParticles(m_car_damage_particles1, *i, Fvector().set(0, 1, 0), car->ID());
		}
	}
	m_playing1 = true;
}

void CCarDamageParticles::Play2(CCar* car)
{
	VERIFY(!physics_world()->Processing());
	if (m_playing2)
	{
		return;
	}

	if (*m_car_damage_particles2)
	{
		BIDS_I i = bones2.begin(), e = bones2.end();
		TParticlesPlayer* PPlayer = car->GetOrCreateComponent<TParticlesPlayer>();
		for (; e != i; ++i)
		{
			PPlayer->StartParticles(m_car_damage_particles2, *i, Fvector().set(0, 1, 0), car->ID());
		}
	}
	m_playing2 = true;
}

void CCarDamageParticles::Play3(CCar* car)
{
	VERIFY(!physics_world()->Processing());
	if (m_playing3)
	{
		return;
	}

	if (*m_car_damage_particles3 && !bones3.empty())
	{
		BIDS_I i = bones3.begin(), e = bones3.end();
		TParticlesPlayer* PPlayer = car->GetOrCreateComponent<TParticlesPlayer>();
		for (; e != i; ++i)
		{
			PPlayer->StartParticles(m_car_damage_particles3, *i, Fvector().set(0, 1, 0), car->ID());
		}
		m_playing3 = true;
	}
}

void CCarDamageParticles::PlayWheel1(CCar* car, u16 bone_id)
{
	VERIFY(!physics_world()->Processing());
	if (*m_wheels_damage_particles1)
	{
		TParticlesPlayer* PPlayer = car->GetOrCreateComponent<TParticlesPlayer>();
		PPlayer->StartParticles(m_wheels_damage_particles1, bone_id, Fvector().set(0, 1, 0), car->ID());
	}
}

void CCarDamageParticles::PlayWheel2(CCar* car, u16 bone_id)
{
	VERIFY(!physics_world()->Processing());
	if (*m_wheels_damage_particles2)
	{
		TParticlesPlayer* PPlayer = car->GetOrCreateComponent<TParticlesPlayer>();
		PPlayer->StartParticles(m_wheels_damage_particles2, bone_id, Fvector().set(0, 1, 0), car->ID());
	}
}

void CCarDamageParticles::StopWheel1(CCar* car, u16 bone_id)
{
	if (*m_wheels_damage_particles1)
	{
		TParticlesPlayer* PPlayer = car->GetOrCreateComponent<TParticlesPlayer>();
		PPlayer->StopParticles(car->ID(), bone_id, false);
	}
}

void CCarDamageParticles::StopWheel2(CCar* car, u16 bone_id)
{
	VERIFY(!physics_world()->Processing());
	if (*m_wheels_damage_particles2)
	{
		TParticlesPlayer* PPlayer = car->GetOrCreateComponent<TParticlesPlayer>();
		PPlayer->StopParticles(car->ID(), bone_id, false);
	}
}


void CCarDamageParticles::Clear()
{
	bones1.clear();
	bones2.clear();
	bones3.clear();
	m_playing1 = false;
	m_playing2 = false;
	m_playing3 = false;
}

void CCarDamageParticles::Stop1(CCar* car)
{
	if (!m_playing1)
	{
		return;
	}

	if (*m_car_damage_particles1)
	{
		TParticlesPlayer* PPlayer = car->GetOrCreateComponent<TParticlesPlayer>();
		auto i = bones1.begin(), e = bones1.end();
		for (; e != i; ++i)
		{
			PPlayer->StopParticles(m_car_damage_particles1, *i, false);
		}
	}
	m_playing1 = false;
}

void CCarDamageParticles::Stop2(CCar* car)
{
	VERIFY(!physics_world()->Processing());
	if (!m_playing2)
	{
		return;
	}

	if (*m_car_damage_particles2)
	{
		TParticlesPlayer* PPlayer = car->GetOrCreateComponent<TParticlesPlayer>();
		auto i = bones2.begin(), e = bones2.end();
		for (; e != i; ++i)
		{
			PPlayer->StopParticles(m_car_damage_particles2, *i, false);
		}
	}
	m_playing2 = false;
}

void CCarDamageParticles::Stop3(CCar* car)
{
	VERIFY(!physics_world()->Processing());
	if (!m_playing3)
	{
		return;
	}

	if (*m_car_damage_particles3)
	{
		TParticlesPlayer* PPlayer = car->GetOrCreateComponent<TParticlesPlayer>();
		auto i = bones3.begin(), e = bones3.end();
		for (; e != i; ++i)
		{
			PPlayer->StopParticles(m_car_damage_particles3, *i, false);
		}
	}
	m_playing3 = false;
}
