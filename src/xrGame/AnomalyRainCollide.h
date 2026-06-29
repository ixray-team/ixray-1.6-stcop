#pragma once

#include "AnomalyZone.h"

class CGameObject;
class CArtefact;

struct TAnomalyRainCollide final
{
private:
	CAnomalyZone* m_currentAnomalyObject = nullptr;
	bool m_use_ground_rain_collide_particles = false;
	bool m_use_air_rain_collide_particles = false;

	shared_str m_ground_particle_path;
	shared_str m_air_particle_path;

	Fmatrix& XFORM() { return m_currentAnomalyObject->XFORM(); }

	bool IsEnabled() { return m_currentAnomalyObject && m_currentAnomalyObject->IsEnabled(); }
	void SpawnGroundParticle(Fvector position);
	void SpawnAirParticle(Fvector position);
	CParticlesObject* PlayNewPG(shared_str path, Fvector position);

public:
	void BeginComponent(IECSOwner* O) { m_currentAnomalyObject = smart_cast<CAnomalyZone*>(O); }
	void EndComponent() {}
	void Load(const char* section);
	void OnRainCollide(Fvector rainCollisionPosition);
	void Update();
	bool AlwaysTheCrow() { return false; }

private:
	ECS_COMPONENT(TAnomalyRainCollide)
		ECS_END
};