#pragma once
#include "../state.h"

class	CStateBurerAttack : public CState
{
protected:
	using inherited = CState;
	using state_ptr = CState *;

	CBurerBase* pBurerBase;

public:
						CStateBurerAttack			(CBaseMonster* object);
						virtual ~CStateBurerAttack() override;

	virtual	void		initialize					() override;
	virtual	void		execute						() override;
	virtual void		remove_links				(CObject* object) override { inherited::remove_links(object); }

	virtual void		finalize					() override;
	virtual void		critical_finalize			() override;
	virtual bool		check_control_start_conditions	(ControlCom::EControlType type) override;

private:
	bool				m_wait_state_end;
	bool				m_lost_delta_health;
	bool				m_allow_anti_aim;
	float				m_last_health;
	TTime				m_next_runaway_allowed_tick;
};
