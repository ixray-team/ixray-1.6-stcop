#pragma once
#include "../monster_state_manager.h"

class CPoltergeistBase;

class CStateManagerPoltergeist : public CMonsterStateManager {
	typedef CMonsterStateManager inherited;

	CPoltergeistBase* m_pPoltergeist;
public:
						CStateManagerPoltergeist		(CPoltergeistBase *obj);
	virtual				~CStateManagerPoltergeist	();

	virtual void		reinit						();
	virtual	void		execute						();
	virtual void		remove_links				(CObject* object_) { inherited::remove_links(object_);}

private:

			u32			time_next_flame_attack;
			u32			time_next_tele_attack;
			u32			time_next_scare_attack;

			void		polter_attack				();
			


};
