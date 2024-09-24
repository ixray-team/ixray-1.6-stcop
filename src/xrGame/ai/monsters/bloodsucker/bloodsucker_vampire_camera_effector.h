#pragma once

#include "../../../../xrEngine/effectorPP.h"
#include "../../../CameraEffector.h"
#include "../../../../xrEngine/cameramanager.h"

class CustomBloodsuckerVampireCameraEffector : public CEffectorCam
{
	using inherited = CEffectorCam;

	float	m_time_total;
	Fvector	dangle_target;
	Fvector dangle_current;

	float	m_dist;
	Fvector m_direction;

public:
	CustomBloodsuckerVampireCameraEffector(float time, const Fvector& src, const Fvector& tgt);
	virtual				~CustomBloodsuckerVampireCameraEffector();

	virtual BOOL	ProcessCam(SCamEffectorInfo& info);

	struct SBloodsuckerVampireCameraEffectorProperies
	{
		// Константы углов и скоростей
		static constexpr float PI = 3.14159265358979323846f;

		// Константные углы
		static constexpr float DeltaAngleX = 10.0f * PI / 180.0f;
		static constexpr float DeltaAngleY = DeltaAngleX;
		static constexpr float DeltaAngleZ = DeltaAngleX;

		// Константная скорость угла
		static constexpr float AngleSpeed = 0.2f;

		// Константное лучшее расстояние
		static constexpr float BestDistance = 0.3f;
	};
};