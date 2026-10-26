////////////////////////////////////////////////////////////////////////////
//	Module 		: stalker_kill_wounded_actions.h
//	Created 	: 25.05.2006
//  Modified 	: 25.05.2006
//	Author		: Dmitriy Iassenev
//	Description : Stalker kill wounded action classes
////////////////////////////////////////////////////////////////////////////

#pragma once

#include "stalker_combat_actions.h"

//////////////////////////////////////////////////////////////////////////
// CStalkerActionReachWounded
//////////////////////////////////////////////////////////////////////////

class CStalkerActionReachWounded : public CStalkerActionCombatBase {
protected:
	typedef CStalkerActionCombatBase inherited;

public:
						CStalkerActionReachWounded	(CAI_Stalker *object, const char* action_name = "");
	virtual void		initialize					() override;
	virtual void		execute						() override;
	virtual void		finalize					() override;
};

//////////////////////////////////////////////////////////////////////////
// CStalkerActionAimWounded
//////////////////////////////////////////////////////////////////////////

class CStalkerActionAimWounded : public CStalkerActionCombatBase {
protected:
	typedef CStalkerActionCombatBase inherited;

private:
//	float				m_speed;

public:
						CStalkerActionAimWounded	(CAI_Stalker *object, const char* action_name = "");
	virtual void		initialize					() override;
	virtual void		execute						() override;
	virtual void		finalize					() override;
};

//////////////////////////////////////////////////////////////////////////
// CStalkerActionPrepareWounded
//////////////////////////////////////////////////////////////////////////

class CStalkerActionPrepareWounded : public CStalkerActionCombatBase {
protected:
	typedef CStalkerActionCombatBase inherited;

public:
						CStalkerActionPrepareWounded(CAI_Stalker *object, const char* action_name = "");
	virtual void		initialize					() override;
	virtual void		execute						() override;
	virtual void		finalize					() override;
};

//////////////////////////////////////////////////////////////////////////
// CStalkerActionKillWounded
//////////////////////////////////////////////////////////////////////////

class CStalkerActionKillWounded : public CStalkerActionCombatBase {
protected:
	typedef CStalkerActionCombatBase inherited;

public:
						CStalkerActionKillWounded	(CAI_Stalker *object, const char* action_name = "");
	virtual void		initialize					() override;
	virtual void		execute						() override;
	virtual void		finalize					() override;
};

//////////////////////////////////////////////////////////////////////////
// CStalkerActionPauseAfterKill
//////////////////////////////////////////////////////////////////////////

class CStalkerActionPauseAfterKill : public CStalkerActionCombatBase {
protected:
	typedef CStalkerActionCombatBase inherited;

public:
						CStalkerActionPauseAfterKill(CAI_Stalker *object, const char* action_name = "");
	virtual void		initialize					() override;
	virtual void		execute						() override;
};
