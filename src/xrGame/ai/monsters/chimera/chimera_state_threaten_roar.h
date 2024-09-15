#pragma once
#include "../state.h"


class CStateChimeraThreatenRoar : public CState {
	typedef CState		inherited;

public:
	IC					CStateChimeraThreatenRoar	(CBaseMonster *obj) : inherited(obj){}
	
	virtual	void		initialize					();	
	virtual	void		execute						();
	virtual bool		check_completion			();
	virtual void		remove_links				(CObject* object) { inherited::remove_links(object);}
};

#include "chimera_state_threaten_roar_inline.h"
