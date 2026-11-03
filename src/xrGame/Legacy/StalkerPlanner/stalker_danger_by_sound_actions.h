////////////////////////////////////////////////////////////////////////////
//	Module 		: stalker_danger_by_sound_actions.h
//	Created 	: 31.05.2005
//  Modified 	: 31.05.2005
//	Author		: Dmitriy Iassenev
//	Description : Stalker danger by sound actions classes
////////////////////////////////////////////////////////////////////////////

#pragma once

#include "stalker_combat_actions.h"

//////////////////////////////////////////////////////////////////////////
// CStalkerActionDangerBySoundListenTo
//////////////////////////////////////////////////////////////////////////

class CStalkerActionDangerBySoundListenTo : public CStalkerActionCombatBase {
protected:
	typedef CStalkerActionCombatBase inherited;

public:
						CStalkerActionDangerBySoundListenTo	(CAI_Stalker *object, const char* action_name = "");
	virtual void		initialize							() override;
	virtual void		execute								() override;
	virtual void		finalize							() override;
};

//////////////////////////////////////////////////////////////////////////
// CStalkerActionDangerBySoundCheck
//////////////////////////////////////////////////////////////////////////

class CStalkerActionDangerBySoundCheck : public CStalkerActionCombatBase {
protected:
	typedef CStalkerActionCombatBase inherited;

public:
						CStalkerActionDangerBySoundCheck	(CAI_Stalker *object, const char* action_name = "");
	virtual void		initialize							() override;
	virtual void		execute								() override;
	virtual void		finalize							() override;
};

//////////////////////////////////////////////////////////////////////////
// CStalkerActionDangerBySoundTakeCover
//////////////////////////////////////////////////////////////////////////

class CStalkerActionDangerBySoundTakeCover : public CStalkerActionCombatBase {
protected:
	typedef CStalkerActionCombatBase inherited;

public:
						CStalkerActionDangerBySoundTakeCover(CAI_Stalker *object, const char* action_name = "");
	virtual void		initialize							() override;
	virtual void		execute								() override;
	virtual void		finalize							() override;
};

//////////////////////////////////////////////////////////////////////////
// CStalkerActionDangerBySoundLookOut
//////////////////////////////////////////////////////////////////////////

class CStalkerActionDangerBySoundLookOut : public CStalkerActionCombatBase {
protected:
	typedef CStalkerActionCombatBase inherited;

public:
						CStalkerActionDangerBySoundLookOut	(CAI_Stalker *object, const char* action_name = "");
	virtual void		initialize							() override;
	virtual void		execute								() override;
	virtual void		finalize							() override;
};

//////////////////////////////////////////////////////////////////////////
// CStalkerActionDangerBySoundLookAround
//////////////////////////////////////////////////////////////////////////

class CStalkerActionDangerBySoundLookAround : public CStalkerActionCombatBase {
protected:
	typedef CStalkerActionCombatBase inherited;

public:
						CStalkerActionDangerBySoundLookAround	(CAI_Stalker *object, const char* action_name = "");
	virtual void		initialize								() override;
	virtual void		execute									() override;
	virtual void		finalize								() override;
};
