#pragma once

#include "AnomalyZone.h"
#include "ElectricCurve.h"
#include "RandomSoundEmmiter.h"

class CAnomalyZone;
class CArtefact;
class CGameObject;

struct TAnomalyElectricCurve final
{
private:
	xr_vector<CRandomSoundEmmiter*> m_snd_emmiter_electric_core_target_damage;
	xr_vector<CRandomSoundEmmiter*> m_snd_emmiter_electric_core_loop;
	xr_vector<CRandomSoundEmmiter*> m_snd_emmiter_electric_curve_start;
	xr_vector<CRandomSoundEmmiter*> m_snd_emmiter_electric_curve_loop;
	xr_vector<CRandomSoundEmmiter*> m_snd_emmiter_electric_curve_end;

	CAnomalyZone* m_currentAnomalyObject = nullptr;
	shared_str m_electric_curve_particle_path;
	Fvector m_initial_spawn_position;
	xr_vector<SElectricCurve> m_electric_curves;
	CGameObject* lastDamagedObject = nullptr;

	bool draw_dbg = false;

	bool m_use_electric_curve = false;
	bool m_cascade_curves = false;
	bool m_touch_objects_by_curves = false;

	u8 m_max_count_electric_curves = 1;
	float max_trace_curve_distance = 15.f;
	float m_max_curve_damage = 0.005f;
	float m_max_curve_impulse = 0.012f;
	float max_processing_distance = 200.0f;
	float blastTimeProcessing = 0.0f;
	float max_blastTimeProcessing = 5000.0f;
	float m_max_curve_radius = 5.0f;

	Fmatrix& XFORM() { return m_currentAnomalyObject->XFORM(); }
	void AffectCurveDamade(CGameObject* obj);
	void OnBlastElectricCurvesUpdate(CGameObject* obj);

public:
	void BeginComponent(IECSOwner* O);
	void EndComponent();

	void Load(const char* section);
	void SetInitialSpawnPosition(Fvector vector) { m_initial_spawn_position = Fvector(vector.x, vector.y, vector.z);} // clone
	bool AlwaysTheCrow();
	bool IsEnabled();
	void InitElectricCurves();
	void OnActorTakeArtefact(float scan_radius, CArtefact* artefact, Fvector actorPos);
	void AffectBlast(CGameObject* blastedObject);
	bool IsNeedScanObjects();
	float GetScanRadius() { return m_max_curve_radius / 2; }
	float GetBarierRadius() { return m_max_curve_radius; }
	void Update(bool isUpdateCL);

private:
	ECS_COMPONENT(TAnomalyElectricCurve)
		ECS_END
};
