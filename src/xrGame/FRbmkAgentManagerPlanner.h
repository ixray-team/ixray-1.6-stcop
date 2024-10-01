#pragma once

class CAgentManager;
class FRbmkAgentManagerPlanner
{
public:
					FRbmkAgentManagerPlanner	(CAgentManager *InOwner);
					~FRbmkAgentManagerPlanner	();
	void			Update						();
private:
	bool			IsItemSelected				() const;
	bool			IsEnemySelected				() const;
	bool			IsDangerSelected			() const;
	void			RefreshState				(u32 NewState);
	u32				CurrentState = -1;
	CAgentManager*	Owner;
};
