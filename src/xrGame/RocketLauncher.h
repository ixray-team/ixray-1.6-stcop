#pragma once

class CCustomRocket;
class CGameObject;

class CRocketLauncher
{
public:
	CRocketLauncher() = default;
	virtual ~CRocketLauncher() = default;

	virtual void Load(const char* section);

	void AttachRocket(ALife::_OBJECT_ID rocket_id, CGameObject* parent_rocket_launcher);
	void DetachRocket(ALife::_OBJECT_ID rocket_id, bool bLaunch);

	void SpawnRocket(const shared_str& rocket_section, CGameObject* parent_rocket_launcher);
	void LaunchRocket(const Fmatrix& xform, const Fvector& vel, const Fvector& angular_vel);

	virtual CRocketLauncher* cast_rocket_launcher() { return this; }

protected:
	using ROCKET_VECTOR = xr_vector<CCustomRocket*>;
	using ROCKETIT = ROCKET_VECTOR::iterator;

	ROCKET_VECTOR m_rockets = {};
	ROCKET_VECTOR m_launched_rockets = {};

	CCustomRocket* getCurrentRocket();
	void dropCurrentRocket();
	u32	getRocketCount();
	float m_fLaunchSpeed = 0.0f;

};