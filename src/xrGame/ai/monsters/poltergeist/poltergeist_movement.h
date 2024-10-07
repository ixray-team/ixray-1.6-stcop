#pragma once

#include "../control_path_builder.h"

class CPoltergeistBase;
class CCustomMonster;

class CPoltergeisMovementManager : public CControlPathBuilder {
	typedef CControlPathBuilder inherited;

	CPoltergeistBase	*m_monster;

public:
					CPoltergeisMovementManager	(CPoltergeistBase *monster) : inherited((CCustomMonster*)monster), m_monster(monster) {}
	virtual			~CPoltergeisMovementManager	(){}

	virtual	void	move_along_path				(CPHMovementControl *movement_control, Fvector &dest_position, float time_delta);

			Fvector	CalculateRealPosition		();
};

