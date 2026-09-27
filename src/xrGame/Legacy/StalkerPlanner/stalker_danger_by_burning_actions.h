#pragma once

#include "stalker_combat_actions.h"

//////////////////////////////////////////////////////////////////////////
// CStalkerActionDangerByBurningRunAway
//////////////////////////////////////////////////////////////////////////

class CStalkerActionDangerByBurningRunAway : public CStalkerActionCombatBase {
protected:
	using inherited = CStalkerActionCombatBase;

public:
						CStalkerActionDangerByBurningRunAway	(CAI_Stalker *object, const char* action_name = "");
	virtual void		initialize							();
	virtual void		execute								();
	virtual void		finalize							();
};

//////////////////////////////////////////////////////////////////////////
// CStalkerActionDangerBySoundCheck
//////////////////////////////////////////////////////////////////////////

/*class CStalkerActionDangerBySoundCheck : public CStalkerActionCombatBase {
protected:
	typedef CStalkerActionCombatBase inherited;

public:
						CStalkerActionDangerBySoundCheck	(CAI_Stalker *object, const char* action_name = "");
	virtual void		initialize							();
	virtual void		execute								();
	virtual void		finalize							();
};

//////////////////////////////////////////////////////////////////////////
// CStalkerActionDangerBySoundTakeCover
//////////////////////////////////////////////////////////////////////////

class CStalkerActionDangerBySoundTakeCover : public CStalkerActionCombatBase {
protected:
	typedef CStalkerActionCombatBase inherited;

public:
						CStalkerActionDangerBySoundTakeCover(CAI_Stalker *object, const char* action_name = "");
	virtual void		initialize							();
	virtual void		execute								();
	virtual void		finalize							();
};

//////////////////////////////////////////////////////////////////////////
// CStalkerActionDangerBySoundLookOut
//////////////////////////////////////////////////////////////////////////

class CStalkerActionDangerBySoundLookOut : public CStalkerActionCombatBase {
protected:
	typedef CStalkerActionCombatBase inherited;

public:
						CStalkerActionDangerBySoundLookOut	(CAI_Stalker *object, const char* action_name = "");
	virtual void		initialize							();
	virtual void		execute								();
	virtual void		finalize							();
};

//////////////////////////////////////////////////////////////////////////
// CStalkerActionDangerBySoundLookAround
//////////////////////////////////////////////////////////////////////////

class CStalkerActionDangerBySoundLookAround : public CStalkerActionCombatBase {
protected:
	typedef CStalkerActionCombatBase inherited;

public:
						CStalkerActionDangerBySoundLookAround	(CAI_Stalker *object, const char* action_name = "");
	virtual void		initialize								();
	virtual void		execute									();
	virtual void		finalize								();
};*/
