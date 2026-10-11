#pragma once

class CObject;
class CBaseMonster;
class CEntityAlive;

// Per-species policy and per-instance decisions. No rendering data lives here.
struct SMonsterBehavior
{
	enum EStrategy : u32
	{
		Direct = 1,
		Flank = 2,
		Wide = 4,
		Cautious = 8,
		Lunge = 16,
		Charge = 32,
		Encircle = 64,
		Pressure = 128,
		Retreat = 256
	};
	bool Enabled = false;
	u32 Strategies = Direct | Flank | Pressure;
	u32 SearchMin = 15000, SearchMax = 25000, ApproachMax = 90000;
	u32 PeripheralInterval = 1000, BodyFreshTime = 90000, HazardTime = 120000;
	float SearchRadius = 12.f, SearchAngle = 60.f, TrailLength = 5.f;
	float ApproachWidth = 5.f, PeripheralAngle = 150.f, PeripheralRadius = 25.f;
	float DeathRadius = 20.f, BodyRadius = 25.f, HazardRadius = 12.f;
	u32 HazardThreshold = 3, InvestigatorLimit = 2;
	bool Sniff = false;
	bool TrailEnabled = true, PeripheralEnabled = true, DeathAlertEnabled = true;
	bool BodiesEnabled = true, HazardsEnabled = true;
	bool AnomalyAvoidance = false;
	u32 FightQuietTime = 15000, NextAnomalyScan = 0, AnomalyCount = 0, HazardMoveUntil = 0;
	float AnomalyScanRadius = 60.f, AnomalyMargin = 1.f;
	struct SAnomaly { Fvector Position{}; float Radius = 0.f; };
	SAnomaly Anomalies[32]{};
	struct SNoiseAttention { u32 Source = u32(-1), Until = 0, NextCheck = 0, LastInterest = 0, LastCheckedEvent = 0; Fvector Position{}; float Interest = 0.f, Fear = 0.f, Power = 0.f; bool Explosion = false; };
	SNoiseAttention NoiseAttention[8]{};
	u32 NoiseCursor = 0, NoiseCheckCursor = 0;
	float NoiseInterestDecay = .1f;
	u32 NoiseInterestInterval = 100;
	u32 HearNoise(CBaseMonster& Monster, CObject* Source, const Fvector& Position, float Power, bool Explosion);
	bool CrossesDanger(CBaseMonster& Monster, const Fvector& Point) const;
	float ApproachSpeed = 2.f, CloseVisualRadius = 10.f, CrowdDistance = 20.f;
	u32 CloseVisualTime = 5000, NoiseSearchTime = 10000;
	u32 PeripheralCursor = 0, LastDeathSource = u32(-1), LastDeathTime = 0;
	u32 NextSniff = 0, SniffUntil = 0, RecentPointCursor = 0;
	Fvector RecentPoints[8]{};
	u32 RecentPointTimes[8]{};
	u32 NextUpdate = 0, NextPeripheral = 0, NextPoint = 0;
	u32 TargetID = u32(-1), ObservationTime = 0, ApproachUntil = 0, SearchUntil = 0;
	u32 AttentionUntil = 0, NoiseLookUntil = 0, ProgressTime = 0, Phase = 0, Strategy = Direct;
	u32 DeathTime = 0, BodyID = u32(-1), ActionUntil = 0;
	Fvector Goal = {0.f, 0.f, 0.f}, Anchor = {0.f, 0.f, 0.f};
	Fvector Attention = {0.f, 0.f, 0.f}, ProgressPosition = {0.f, 0.f, 0.f};
	u32 GoalVertex = u32(-1);
	const char* ActionMode = "DEATH ALERT SEARCH";
	const char* Mode = "NATIVE";
	const char* DecisionMode = "NATIVE";
	bool Moving = false, Running = false, Searching = false;
	static bool TakeNavigationSample();
	static bool TakeSpatialQuery();
	void Apply(CBaseMonster& Monster);
	void RecordDebug(CBaseMonster& Monster);
	void Load(CBaseMonster& Monster, const char* Section);
	void Update(CBaseMonster& Monster);
	void HearDeath(CBaseMonster& Monster, CBaseMonster* Source, const Fvector& Position, float Power);
	void OnDeath(CBaseMonster& Monster);
	void Remove(CBaseMonster& Monster);
	void ShareAreas(CBaseMonster& Receiver, const CBaseMonster& Source);
	bool HasAreas(const CBaseMonster& Monster) const;
	bool Avoids(CBaseMonster& Monster, const Fvector& Point) const;
};
