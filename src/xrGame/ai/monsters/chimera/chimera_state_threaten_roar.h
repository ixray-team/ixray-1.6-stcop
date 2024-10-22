#pragma once
#include "../state.h"
#include "../../ai_entity_definitions.h"

class CStateChimeraThreatenRoar : public CState {
protected:
	using inherited = CState		;

public:
	CStateChimeraThreatenRoar(CBaseMonster* object);
	virtual ~CStateChimeraThreatenRoar() override;
	
	virtual	void		initialize					() override;
	virtual	void		execute						() override;
	virtual bool		check_completion			() override;
	virtual void		remove_links				(CObject* object) override { inherited::remove_links(object);}
};
