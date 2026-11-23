#include "StdAfx.h"
#include "FlashlightInterference.h"
#include "AnomalyZone.h"
#include "RadioactiveZone.h"
#include "ZoneCampfire.h"

int g_FlashlightInterference = 1;

void FlashlightInterference::Load(const char* Section)
{
	Enabled = pSettings->read_if_exists<bool>(Section, "anomaly_interference", false);
	Radius = pSettings->read_if_exists<float>(Section, "anomaly_radius", Radius);
	ShockFactor = pSettings->read_if_exists<float>(Section, "anomaly_shock_factor", ShockFactor);
	BlowoutFactor = pSettings->read_if_exists<float>(Section, "anomaly_blowout_factor", BlowoutFactor);
	DimLevel = pSettings->read_if_exists<float>(Section, "anomaly_dim", DimLevel);
	JitterLevel = pSettings->read_if_exists<float>(Section, "anomaly_jitter", JitterLevel);
	FlickerRate = pSettings->read_if_exists<float>(Section, "anomaly_flicker_rate", FlickerRate);
	BlackoutRate = pSettings->read_if_exists<float>(Section, "anomaly_blackout_rate", BlackoutRate);
	ResponseTime = pSettings->read_if_exists<float>(Section, "anomaly_response_time", ResponseTime);

	Radius = std::max(Radius, 0.1f);
	Reset();
}

void FlashlightInterference::Reset()
{
	ScanTimer = 0.0f;
	Target = 0.0f;
	Disturbance = 0.0f;
	DipTime = 0.0f;
	DipDepth = 0.0f;
	BlackoutTime = 0.0f;
}

float FlashlightInterference::Scan(const Fvector& Position) const
{
	static xr_vector<ISpatialShared> Nearest;
	Nearest.clear();
	g_SpatialSpace->q_sphere(Nearest, 0, ESPATIAL_TYPE::ANOMALY_ZONE, Position, Radius + 20.0f);

	float Result = 0.0f;
	for (ISpatialShared& Spatial : Nearest)
	{
		CObject* Object = Spatial ? Spatial->dcast_CObject() : nullptr;
		CGameObject* GameObject = Object != nullptr && !Object->getDestroy() ? Object->cast_game_object() : nullptr;
		CAnomalyZone* Zone = GameObject != nullptr ? GameObject->cast_anomaly_zone() : nullptr;

		if (Zone == nullptr || !Zone->IsEnabled() || Zone->GetHitType() == ALife::eHitTypeRadiation)
		{
			continue;
		}

		if (smart_cast<CRadioactiveZone*>(Zone) != nullptr || smart_cast<CZoneCampfire*>(Zone) != nullptr)
		{
			continue;
		}

		const float Distance = std::max(Position.distance_to(Zone->Position()) - Zone->Radius(), 0.0f);
		float Proximity = 1.0f - clampr(Distance / Radius, 0.0f, 1.0f);
		Proximity *= Proximity;

		if (Zone->GetHitType() == ALife::eHitTypeShock)
		{
			Proximity *= ShockFactor;
		}

		if (Zone->ZoneState() == CAnomalyZone::eZoneStateBlowout)
		{
			Proximity *= BlowoutFactor;
		}

		Result = std::max(Result, Proximity);
	}

	return clampr(Result, 0.0f, 1.0f);
}

float FlashlightInterference::Update(const Fvector& Position, float TimeDelta)
{
	if (!Enabled || g_FlashlightInterference == 0 || TimeDelta <= 0.0f)
	{
		if (!Enabled || g_FlashlightInterference == 0)
		{
			Reset();
		}

		return BlackoutTime > 0.0f ? 0.0f : 1.0f - DimLevel * Disturbance;
	}

	ScanTimer -= TimeDelta;
	if (ScanTimer <= 0.0f)
	{
		ScanTimer = ScanInterval;
		Target = Scan(Position);
	}

	const float Blend = ResponseTime > EPS ? 1.0f - std::exp(-TimeDelta / ResponseTime) : 1.0f;
	Disturbance += (Target - Disturbance) * Blend;

	if (Disturbance < 0.01f && BlackoutTime <= 0.0f && DipTime <= 0.0f)
	{
		return 1.0f;
	}

	const float Level = Disturbance;

	if (BlackoutTime > 0.0f)
	{
		BlackoutTime -= TimeDelta;
		if (BlackoutTime <= 0.0f)
		{
			DipTime = ::Random.randF(0.08f, 0.2f);
			DipDepth = ::Random.randF(0.5f, 0.9f);
		}
	}
	else if (::Random.randF(0.0f, 1.0f) < BlackoutRate * Level * Level * TimeDelta)
	{
		BlackoutTime = ::Random.randF(0.12f, 0.25f + 0.6f * Level);
	}

	if (DipTime > 0.0f)
	{
		DipTime -= TimeDelta;
	}
	else if (::Random.randF(0.0f, 1.0f) < FlickerRate * Level * TimeDelta)
	{
		DipTime = ::Random.randF(0.03f, 0.12f);
		DipDepth = ::Random.randF(0.3f, 1.0f) * Level;
	}

	if (BlackoutTime > 0.0f)
	{
		return 0.0f;
	}

	float Value = 1.0f - DimLevel * Level;
	if (DipTime > 0.0f)
	{
		Value *= 1.0f - DipDepth;
	}

	Value *= 1.0f - JitterLevel * Level * ::Random.randF(0.0f, 1.0f);
	return clampr(Value, 0.0f, 1.0f);
}
