#pragma once

#include "../../../../xrEngine/effectorPP.h"
#include "../../../CameraEffector.h"
#include "../../../../xrEngine/cameramanager.h"

class CustomBloodsuckerVampirePPEffector : public CEffectorPP 
{
	using inherited  = CEffectorPP;

	SPPInfo state;			//current state
	float	m_total;		// total PP time

public:
	CustomBloodsuckerVampirePPEffector(const SPPInfo &ppi, float life_time);
	virtual ~CustomBloodsuckerVampirePPEffector();

	virtual	BOOL	Process					(SPPInfo& pp);

	struct SBloodsuckerVampirePPEffectorProperies
	{
		static constexpr float PI = 3.14159265358979323846f;
		static constexpr float PI_DIV_2 = PI / 2.0f;
		static constexpr float PI_MUL_2 = 2.0f * PI;

		static constexpr float TimeAttack = 0.2f;
		static constexpr int Periods = 2;

		static constexpr float RadToPerc(float rad)
		{
			return (rad - PI_DIV_2) / (Periods * PI_MUL_2);
		}

		static constexpr float PercToRad(float perc)
		{
			return perc * (Periods * PI_MUL_2) + PI_DIV_2;
		}
	};
};
