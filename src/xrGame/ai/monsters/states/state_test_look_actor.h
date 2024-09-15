#pragma once
#include "../state.h"


class CStateMonsterLookActor : public CState {
	typedef CState inherited;
public:
						CStateMonsterLookActor	(CBaseMonster*obj) : inherited(obj) {}
	virtual	void		execute					();
	virtual void		remove_links			(CObject* object_) { inherited::remove_links(object_);}
};



class CStateMonsterTurnAwayFromActor : public CState {
	typedef CState inherited;
public:
						CStateMonsterTurnAwayFromActor	(CBaseMonster*obj) : inherited(obj) {}
	virtual	void		execute					();
};


class CStateMonstertTestIdle : public CState {
	typedef CState inherited;
public:
						CStateMonstertTestIdle	(CBaseMonster*obj) : inherited(obj) {}
	virtual	void		execute					();
};

#include "state_test_look_actor_inline.h"