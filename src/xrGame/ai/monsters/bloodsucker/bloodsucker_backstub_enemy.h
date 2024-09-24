#pragma once
#include "../states/monster_state_attack.h"

class CustomBloodsuckerBackstubEnemy : public CState
{
	using inherited = CState;

public:
	struct StateParams : SStateDataMoveToPointEx
	{
		bool   start_with_encircle;
		StateParams() : start_with_encircle(false) {}
	} data;

protected:

	float                   m_last_health;
	bool                    m_encircle;
	TTime                   m_encircle_end_tick;
	TTime                   m_next_change_behaviour_tick;

public:
	CustomBloodsuckerBackstubEnemy(CustomBloodsucker* object);
	virtual				~CustomBloodsuckerBackstubEnemy();

	virtual void		initialize();
	virtual	void		execute();
	virtual bool 		check_start_conditions();
	virtual bool		check_completion();
	virtual void		remove_links(CObject* object) { inherited::remove_links(object); }

	struct SBloodsuckerStateBackstubEnemyProperies
	{
		static constexpr int encircle_time = 3000;
		static constexpr float loose_health_diff = 0.15f;
		static constexpr int change_behaviour_time = 1000;
	};
};