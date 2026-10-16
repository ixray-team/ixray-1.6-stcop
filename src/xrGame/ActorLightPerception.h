#pragma once

#include "../xrCore/Collision/xr_collide_defs.h"

class CActor;
class CCreature;

class ActorLightPerception
{
public:
	void Load();
	void Reset();
	void Update(CActor& Player, CCreature& Observer);

private:
	enum class Visibility
	{
		None,
		Suspicious,
		Revealed
	};

	void UpdateSource(CActor& Player);
	Visibility SeePoint(CCreature& Observer, const Fvector& Point, CObject* Target = nullptr);

	bool Enabled = false;
	float WeaponDistance = 45.f;
	float HeadlampDistance = 45.f;
	float HandDistance = 20.f;
	float RevealDistance = 20.f;
	float NoticeDistance = 60.f;
	u32 SourceFrame = u32(-1);
	float SourceDistance = -1.f;
	Fvector SourcePosition = {};
	Fvector LightPoint = {};
	ALife::_OBJECT_ID HitObjectId = ALife::INVALID_OBJECT_ID;
	bool HasLightPoint = false;
	collide::rq_results RayResults;
};
