#pragma once

class CustomBloodsuckerAlienEffector : public CEffectorCam
{
	using inherited = CEffectorCam;

	float	m_time_total;
	Fvector	dangle_target;
	Fvector dangle_current;

	CustomBloodsucker* object;

	float		m_current_fov;
	Fmatrix		m_prev_eye_matrix;
	float		m_inertion;

public:
	CustomBloodsuckerAlienEffector(ECamEffectorType type, CustomBloodsucker* object);
	virtual ~CustomBloodsuckerAlienEffector();

	virtual BOOL	ProcessCam(SCamEffectorInfo& info);

	struct SBloodsuckerAlienCameraEffectorProperies
	{
		static constexpr float DeltaAngleX = 10.f * M_PI / 180.f;
		static constexpr float DeltaAngleY = 10.f * M_PI / 180.f;
		static constexpr float DeltaAngleZ = 10.f * M_PI / 180.f;
		static constexpr float AngleSpeed = 0.2f;

		static constexpr float MinFov = 70.0f;
		static constexpr float MaxFov = 175.0f;
		static constexpr float FovSpeed = 80.0f;
		static constexpr float MaxCameraDist = 3.5f;
	};
};