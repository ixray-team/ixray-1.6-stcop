#pragma once

#include "AnomalyZone.h"

class CAnomalyZone;
class CGameObject;
class CArtefact;

struct TAnomalyMovement final
{
private:
	CAnomalyZone* m_currentAnomalyObject = nullptr;
	CGameObject* lastDamagedObject = nullptr;

	Fvector m_initial_spawn_position;
	bool m_use_movement = false;
	float max_processing_distance = 200.0f;
	bool m_use_movement_magnetic_on_take_artefacts_mode = false;
	bool draw_dbg = false;
	bool m_use_movement_always_mode = false;
	bool m_use_movement_magnetic_on_inside_alive_mode = false;
	float movement_magnetic_on_inside_alive_mode_speed = 0.f;
	float m_timer_magnetic_on_take_artefacts = 0.f;
	float m_max_timer_magnetic_on_take_artefacts = 0.f;
	float movement_magnetic_on_take_artefacts_mode_speed = 0.f;
	float m_movement_speed = 0.f;
	float m_movement_radius = 0.f;
	Fvector m_target_position;
	Fvector lastPosition;
	Fmatrix& XFORM() { return m_currentAnomalyObject->XFORM(); }
	void MoveToFromDelta(Fvector newPos, float speed);
	Fvector GetLVPos(Fvector newPos);


public:
	void BeginComponent(IECSOwner* O);
	void EndComponent();

	void Load(const char* section);
	void SetInitialSpawnPosition(Fvector vector) { m_initial_spawn_position = Fvector(vector.x, vector.y, vector.z); } // clone
	bool AlwaysTheCrow();
	bool IsEnabled();
	void OnActorTakeArtefact(float scan_radius, CArtefact* artefact, Fvector actorPos);
	void AffectBlast(CGameObject* blastedObject);
	bool IsNeedScanObjects();
	float GetScanRadius() { return m_movement_radius / 2; }
	float GetBarierRadius() { return m_movement_radius; }
	void Update(CGameObject* m_best_magnetic_target, bool isUpdateCL);

private:
	ECS_COMPONENT(TAnomalyMovement)
		ECS_END
};