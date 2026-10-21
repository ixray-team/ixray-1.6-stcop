#pragma once

class CEntityAlive;

enum class EELObservationSource : u8
{
	Vision,
	Sound,
	Damage,
	Sharing,
	CloseContact,
	CloseScent
};

enum class EELObservationKind : u8
{
	Enemy,
	Body,
	Hazard
};

struct SELObservation
{
	EELObservationKind Kind = EELObservationKind::Enemy;
	bool IsEnvironmentalDeath = false;
	float Radius = 0.f;
	u16 TargetIdentifier = u16(-1);
	Fvector Position = {0.f, 0.f, 0.f};
	u32 VertexIdentifier = u32(-1);
	u32 ObservedAt = 0;
	u32 ExpiresAt = 0;
	u32 RetentionTime = 0;
	u32 VisualGraceUntil = 0;
	EELObservationSource Source = EELObservationSource::Vision;
	bool HasLivePosition = false;
	u16 SenderIdentifier = u16(-1);
	u32 SenderGeneration = 0;
	bool IsPsiShared = false;
	bool HasCall = false;
	bool HasParentalChannel = false;
	xr_vector<Fvector> Trail;
};

class CELSharedMemory
{
public:
	using Records = xr_vector<SELObservation>;
	bool Remember(const SELObservation& Observation, u32 CurrentTime, u32 RetentionTime, float TrailLength);
	void Expire(u32 CurrentTime);
	void Remove(u16 TargetIdentifier);
	void Clear();
	const Records& GetRecords() const { return Observations; }
	const SELObservation* Find(u16 TargetIdentifier) const;

private:
	Records Observations;
};
