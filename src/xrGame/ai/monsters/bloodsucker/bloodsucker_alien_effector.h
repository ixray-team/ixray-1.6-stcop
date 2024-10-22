#pragma once

class CustomBloodsuckerAlienEffector : public CEffectorCam
{
protected:
	using inherited = CEffectorCam;

	float	m_time_total;
	Fvector	dangle_target;
	Fvector dangle_current;

	CBloodsuckerBase* object;

	float		m_current_fov;
	Fmatrix		m_prev_eye_matrix;
	float		m_inertion;

public:
	CustomBloodsuckerAlienEffector(ECamEffectorType type, CBloodsuckerBase* object);
	virtual ~CustomBloodsuckerAlienEffector() override;

	virtual BOOL	ProcessCam(SCamEffectorInfo& info) override;
};