#pragma once

#include "AnomalyZone.h"

class CGameObject;

struct SWanderState
{
	float angle = 0.f;
	float speed = 0.f;
	float a = 0.f, b = 0.f; // полуоси эллипса
	Fvector u, v;			// ортогональный базис плоскости орбиты
	float moveSpeed = 0.f;
	float rotSpeed = 0.f;
	float rollSpeed = 0.f;
	bool initialized = false;
};

struct TAnomalyGravity final
{
private:
	CAnomalyZone* m_current_anomaly = nullptr;
	bool m_use_gravity_effect = false;
	u16 m_max_count_spawn_trash = 0;
	float m_gravity_radius = 10.f;
	float centerOffsetY = 1.0f;
	float m_max_processing_distance = 200.0f;
	bool m_is_trash_spawned = false;

	std::unordered_map<u32, SWanderState> m_wanderStates;
	xr_vector<xr_string> m_trash_items_sections;
	float m_min_gravity_radius_factor = 0.3f; 

	Fmatrix& XFORM() { return m_current_anomaly->XFORM(); }
	void SetForce(CGameObject* obj, Fvector dir, float value, float rotationSpeed, float rollSpeed);

public:
	void BeginComponent(IECSOwner* O) { m_current_anomaly = smart_cast<CAnomalyZone*>(O); }
	void EndComponent() {}
	bool IsEnabled() { return m_use_gravity_effect && m_current_anomaly->IsEnabled(); }
	void Load(const char* section);
	void Update();

	void save(NET_Packet& output_packet);
	void load(IReader& input_packet);
	void net_Spawn(CSE_Abstract* DC);

	bool AlwaysTheCrow();

	bool IsNeedScanObjects() { return IsEnabled(); }
	float GetScanRadius() { return IsEnabled() ? m_gravity_radius : 0.f; }
	float GetBarierRadius() { return IsEnabled() ? m_gravity_radius : 0.f; }

	bool IsObjectIgnored(CGameObject* obj);
	bool IsAllowPlayEntranceSmallParticles() { return false; }

private:
	ECS_COMPONENT(TAnomalyGravity)
		ECS_END
};