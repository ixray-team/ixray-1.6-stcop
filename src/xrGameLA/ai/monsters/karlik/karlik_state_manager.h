#pragma once
#include "../monster_state_manager.h"

//class CBurer;

class CStateManagerKarlik : public CMonsterStateManager<CKarlik> {
	typedef CMonsterStateManager<CKarlik> inherited;
public:
					CStateManagerKarlik		(CKarlik *monster); 
	virtual			~CStateManagerKarlik	();

	virtual void	execute					();

//private:
	//float				m_last_health;
};

