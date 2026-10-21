#pragma once
class CGameObject;
class CEntityAlive;

class CELSharedCrowMemory
{
public:
	struct SObservation
	{
		Fvector Center = {0.f, 0.f, 0.f};
		float Radius = 0.f;
		u32 Created = 0;
		u32 LastUse = 0;
		bool Pending = false;
		xr_vector<u16> Threats;
		xr_vector<u16> Food;
	};
	struct SPerch
	{
		Fvector Contact;
		Fvector Flesh;
		bool Valid = true;
	};
	static CELSharedCrowMemory& Instance();
	void Register(u16 BirdIdentifier);
	void Unregister(u16 BirdIdentifier);
	const SObservation* Observe(const Fvector& Point, float Radius);
	const xr_vector<SPerch>* CorpsePerches(CGameObject& Bird, u16 CorpseIdentifier, float Gap, float GroundOffset);
	bool ClaimPerch(u16 BirdIdentifier, const Fvector& Contact, float Spacing);
	bool HasFoodClaim(u16 BirdIdentifier, u16 CorpseIdentifier);
	bool ClaimFood(u16 BirdIdentifier, u16 CorpseIdentifier, const Fvector& Contact, u32 SlotLimit);
	void ReleaseFood(u16 BirdIdentifier);
	u32 FoodUsers(u16 CorpseIdentifier) const;
	void RejectPerch(u16 CorpseIdentifier, const Fvector& Contact);
	void Invalidate(u16 ObjectIdentifier);
	void BeginFrame();
	bool NearestTree(const Fvector& Point, float Radius, Fvector& TreePosition) const;
	void Clear();
	static bool FoodContact(CGameObject& Bird, CEntityAlive& Corpse, const Fvector& From, Fvector& Point, bool HasReservedRay = false);
	static bool BodyOutsideCorpse(CEntityAlive& Corpse, const Fvector& Root, float Radius, bool HasReservedRays = false);

private:
	struct SCellKey
	{
		int Column = 0;
		int Height = 0;
		int Row = 0;
		bool operator<(const SCellKey& Other) const;
	};
	struct SClaim
	{
		u16 BirdIdentifier = u16(-1);
		u16 CorpseIdentifier = u16(-1);
		Fvector Contact;
		float Spacing = .2f;
	};
	struct SCorpse
	{
		Fvector Center;
		Fvector Position;
		Fvector Forward;
		Fbox Bounds;
		float Gap = .22f;
		float GroundOffset = .24f;
		u32 Created = 0;
		u32 LastUse = 0;
		u32 Attempt = 0;
		u32 Failures = 0;
		u32 RetryAfter = 0;
		bool Pending = false;
		u16 Requester = u16(-1);
		xr_vector<SPerch> Perches;
	};
	struct SSettings
	{
		u32 CorpseLimit = 0;
		u32 ObservationLimit = 0;
		u32 PerchCount = 0;
		u32 CorpseRefresh = 0;
		u32 ObservationRefresh = 250;
		u32 PerchAttempts = 32;
		u32 PerchRetry = 2000;
		u32 CorpseIdle = 15000;
		u32 ObservationIdle = 5000;
		u32 MaintenanceInterval = 250;
		u32 PerchProbes = 2;
		u32 ObservationQueries = 8;
		u32 ScanEntries = 128;
		float CellSize = 16.f;
		float CorpseMoveDistance = .05f;
	} Settings;
	void LoadSettings();
	void UpdateTrees();
	void Maintain();
	bool BuildPerch(u16 CorpseIdentifier, SCorpse& CorpseRecord);
	xr_map<SCellKey, SObservation> Observations;
	xr_map<u16, SCorpse> Corpses;
	xr_vector<SClaim> Claims;
	xr_vector<u16> Birds;
	xr_map<u64, xr_vector<Fvector>> Trees;
	u32 NextTreeRefresh = 0;
	size_t CachedTreeCount = 0;
	u32 NextMaintenance = 0;
	SCellKey LastObservation;
	u16 LastCorpse = u16(-1);
	bool HasObservationCursor = false;
	bool HasSettings = false;
};
