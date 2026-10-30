#pragma once
class CEntityAlive;
struct SELDebugRecord;

// A bounded post-hit physical effect. It never creates damage or moves bodies by teleportation.
class CELGroundFlyingGrab
{
public:
	static void ConfirmHit(CEntityAlive& Victim, u16 Attacker, float PreviousHealth);
	static void Update();
	static void FillDebug(u16 OwnerIdentifier, SELDebugRecord& Record);
	static void ReleaseOwner(u16 Identifier);
	static void Remove(u16 Identifier);
	static void Clear();
};
