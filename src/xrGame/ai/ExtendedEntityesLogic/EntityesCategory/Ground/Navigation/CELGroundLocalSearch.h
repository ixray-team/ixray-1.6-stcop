#pragma once
class IELAdapter;
class CELGroundMovement;
struct SELPerceptionProfile;

// Bounded, collision-checked local movement while the main graph request remains pending.
class CELGroundLocalSearch final
{
public:
	void Reset();
	bool Move(IELAdapter& Adapter, CELGroundMovement& Movement, const SELPerceptionProfile& Profile, const Fvector& Center, bool Runs);
	static bool Resolve(IELAdapter& Adapter, const Fvector& Position, Fvector& Result, u32& Vertex);

private:
	Fvector Point = {0.f, 0.f, 0.f};
	u32 PointVertex = u32(-1);
	u32 NextAttempt = 0;
	u32 Phase = 0;
	u32 PointUntil = 0;
	u32 ProgressAt = 0;
	Fvector ProgressPosition = {0.f, 0.f, 0.f};
	bool HasPoint = false;
};
