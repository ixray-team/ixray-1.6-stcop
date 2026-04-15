#pragma once

class IRestoresOwner
{
public:
	IRestoresOwner() = default;
	virtual ~IRestoresOwner() = default;

	void Load(const char* section);

	float m_fHealthRestoreSpeed = 0.0f;
	float m_fRadiationRestoreSpeed = 0.0f;
	float m_fSatietyRestoreSpeed = 0.0f;
	float m_fThirstRestoreSpeed = 0.0f;
	float m_fPowerRestoreSpeed = 0.0f;
	float m_fBleedingRestoreSpeed = 0.0f;
};
