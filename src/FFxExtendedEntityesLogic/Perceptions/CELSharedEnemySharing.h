#pragma once
class CBaseMonster;
class CELSharedMemory;
struct SELPerceptionProfile;

class CELSharedEnemySharing
{
public:
	void Update(CBaseMonster& SourceMonster, const CELSharedMemory& Memory, const SELPerceptionProfile& Profile);
	void Clear();
	void OnReceivedCall(u32 Cooldown);
	static bool IgnoresOrdinarySound(const CBaseMonster& Receiver, const CBaseMonster& Source);
	static bool AreFriends(const CBaseMonster& Receiver, const CBaseMonster& Source);
	static bool CanReceive(CBaseMonster& Receiver, CBaseMonster& Source);

private:
	u32 NextSession = 0;
	u32 NextCall = 0;
	u32 ReceiverCursor = 0;
	u32 ObservationCursor = 0;
};
