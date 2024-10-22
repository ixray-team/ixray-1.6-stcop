#pragma once

#include "../control_path_builder.h"

class CPoltergeistBase;
class CCustomMonster;

class CPoltergeisMovementManager : public CControlPathBuilder {
protected:
	using inherited = CControlPathBuilder;

	CPoltergeistBase	*m_monster;

public:
	CPoltergeisMovementManager(CPoltergeistBase* object);
					virtual			~CPoltergeisMovementManager() override;

	virtual	void	move_along_path				(CPHMovementControl *movement_control, Fvector &dest_position, float time_delta) override;

			Fvector	CalculateRealPosition		();
};

