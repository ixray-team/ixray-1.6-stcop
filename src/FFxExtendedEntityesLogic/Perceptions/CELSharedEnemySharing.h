#pragma once
class CBaseMonster;
class CELSharedMemory;
class CELSharedHearing;
struct SELPerceptionProfile;

class CELSharedEnemySharing
{
public:
	void Update(CBaseMonster& SourceMonster, const CELSharedMemory& Memory, const SELPerceptionProfile& Profile, const CELSharedHearing& Hearing, u16 PriorityTarget);
	void Clear();
	void NotifyOwnContact(u32 ObservedAt);
	void RequestHelp(CBaseMonster& SourceMonster, const SELPerceptionProfile& Profile);
	static bool IgnoresOrdinarySound(const CBaseMonster& Receiver, const CBaseMonster& Source);
	static bool AreFriends(const CBaseMonster& Receiver, const CBaseMonster& Source);
	static bool CanReceive(CBaseMonster& Receiver, CBaseMonster& Source);

private:
	bool TryCall(CBaseMonster& SourceMonster, const SELPerceptionProfile& Profile, bool IsHelp);
	u32 HelpCallUntil = 0;
	u32 NextSession = 0;
	u32 LastUrgentAt = 0;
	u32 LastOwnContactAt = 0;
	u32 NextCall = 0;
	u32 ReceiverCursor = 0;
	u32 ObservationCursor = 0;
};
