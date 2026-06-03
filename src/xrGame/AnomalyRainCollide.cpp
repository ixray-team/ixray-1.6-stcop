#include "stdafx.h"
#include "object_broker.h"
#include "AnomalyRainCollide.h"

void TAnomalyRainCollide::Load(const char* section)
{
	shared_str rain_collide_particles_section = pSettings->read_if_exists<str_c>(section, "rain_collide_particles_section", nullptr);
	if (rain_collide_particles_section)
	{
		m_ground_particle_path = pSettings->read_if_exists<str_c>(rain_collide_particles_section, "ground_particle_path", nullptr);
		m_use_ground_rain_collide_particles = m_ground_particle_path != nullptr;

		m_air_particle_path = pSettings->read_if_exists<str_c>(rain_collide_particles_section, "air_particle_path", nullptr);
		m_use_air_rain_collide_particles = m_air_particle_path != nullptr;
	}
}

void TAnomalyRainCollide::OnRainCollide(Fvector rainCollisionPosition)
{
	Fvector GroundPosition;
	GroundPosition.set(rainCollisionPosition);
	GroundPosition.y = XFORM().c.y;

	if (m_use_air_rain_collide_particles)
	{
		SpawnAirParticle(rainCollisionPosition);
	}

	if (m_use_ground_rain_collide_particles)
	{
		SpawnGroundParticle(GroundPosition);
	}
}

void TAnomalyRainCollide::SpawnGroundParticle(Fvector position)
{
	PlayNewPG(m_ground_particle_path, position);
}

void TAnomalyRainCollide::SpawnAirParticle(Fvector position)
{
	PlayNewPG(m_air_particle_path, position);
}

CParticlesObject* TAnomalyRainCollide::PlayNewPG(shared_str path, Fvector position)
{
	CParticlesObject* pParticle = Particles::Details::Create(path.c_str(), true).get();

	Fvector dir;
	dir.set(0, 1, 0);

	Fmatrix XF;
	XF.j.set(dir);
	Fvector::generate_orthonormal_basis(XF.j, XF.k, XF.i);
	XF.c.set(position);
	pParticle->SetXFORM(XF);
	pParticle->Play(false);

	return pParticle;
}

void TAnomalyRainCollide::Update()
{

}