#pragma once

#include "../state.h"

class CStateGroupAttackRun : public CState {
	using inherited = CState;

	TTime				m_time_path_rebuild;
	
	TTime               m_next_encircle_tick;
	TTime               m_encircle_time;
	Fvector             m_encircle_dir;

	TTime               m_intercept_tick;
	TTime               m_intercept_length;
	Fvector             m_intercept;

	TTime               m_memorized_tick;
	Fvector             m_memorized_pos;
	Fvector             m_predicted_vel;

	CDogBase* m_pDog;

public:
						CStateGroupAttackRun	(CBaseMonster *object);
	virtual~		CStateGroupAttackRun	();

	virtual void		initialize				();
	virtual	void		execute					();
	virtual void		finalize				();
	virtual void		critical_finalize		();

	virtual bool 		check_completion		();
	virtual bool 		check_start_conditions	();
	virtual void		remove_links			(CObject* object) { inherited::remove_links(object);}
};

