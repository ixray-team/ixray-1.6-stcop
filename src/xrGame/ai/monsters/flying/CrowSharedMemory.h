#pragma once

class CGameObject;
class CEntityAlive;

// Level-local shared observations; IDs are resolved before use, never retained pointers.
class CCrowSharedMemory
{
public:
	struct SPerch
	{
		Fvector Contact;
		Fvector Flesh;
		bool Valid = true;
	};
	struct SObservation
	{
		int X, Y, Z;
		float Radius;
		u32 Created, LastUse;
		xr_vector<u32> Threats, Food;
	};
	static CCrowSharedMemory& Get();
	bool ReloadConfig();
	void Update();
	void RegisterBird(u32 Bird);
	void UnregisterBird(u32 Bird);
	void InvalidateObject(u32 Object);
	void ClaimFood(u32 Bird, u32 Corpse, const Fvector* Contact = nullptr);
	void RejectPerch(u32 Corpse, const Fvector& Contact);
	void ReleaseFood(u32 Bird);
	u32 FoodUsers(u32 Corpse) const;
	const SObservation* Observe(const Fvector& Point, float Radius);
	const xr_vector<SPerch>* CorpsePerches(CGameObject& Bird, u32 Corpse, float Gap, float GroundOffset);
	static bool FoodContact(CGameObject& Bird, CEntityAlive& Corpse, const Fvector& From, Fvector& Point);
	static bool BodyOutsideCorpse(CGameObject& Bird, CEntityAlive& Corpse, const Fvector& Root, float Radius);

private:
	struct SCellKey
	{
		int X, Y, Z;
		bool operator<(const SCellKey& Other) const
		{
			if (X != Other.X)
			{
				return X < Other.X;
			}
			if (Y != Other.Y)
			{
				return Y < Other.Y;
			}
			return Z < Other.Z;
		}
	};
	struct SConfig
	{
		// Zero disables only the optional count cap; idle expiration still frees entries.
		u32 MaxCachedCorpses = 0;
		u32 MaxCachedObservations = 0;
		u32 PerchCount = 0;
		u32 CorpseRefreshMs = 0;
		u32 ObservationRefreshMs = 250;
		u32 PerchAttempts = 32;
		u32 PerchRetryMs = 2000;
		u32 CorpseIdleMs = 15000;
		u32 ObservationIdleMs = 5000;
		u32 MaintenanceMs = 250;
		u32 PerchProbesPerFrame = 2;
		u32 ObservationQueriesPerFrame = 8;
		float CellSize = 16.f;
		float CorpseMoveDistance = .05f;
	};
	SConfig Config;
	bool ConfigLoaded = false;
	struct SFoodClaim
	{
		u32 Bird, Corpse;
		bool HasPerch = false;
		Fvector Contact;
	};
	struct SCorpse
	{
		u32 Id;
		Fvector Centre, Position, Forward, BoxMin, BoxMax;
		float Gap, GroundOffset;
		u32 Created, LastUse, Attempt = 0;
		u32 FailedAttempts = 0, RetryStarted = 0;
		xr_vector<SPerch> Perches;
	};
	void Maintain();
	xr_vector<u32> Birds;
	xr_vector<SFoodClaim> Claims;
	xr_map<u32, SCorpse> Corpses;
	xr_map<SCellKey, SObservation> Observations;
	u32 InvalidationFrame = u32(-1);
	xr_hash_set<u32> InvalidatedObjects;
	u32 MaintenanceTime = 0, ProbeFrame = u32(-1), ProbesRemaining = 0;
	u32 ObservationFrame = u32(-1), ObservationsRemaining = 0;
};
