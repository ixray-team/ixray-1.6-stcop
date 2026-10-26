#pragma once
#include "../monster_state_manager.h"

class CPoltergeist;

class CStateManagerPoltergeist : public CMonsterStateManager<CPoltergeist> {
	typedef CMonsterStateManager<CPoltergeist> inherited;


public:
						CStateManagerPoltergeist		(CPoltergeist *obj);
	virtual ~CStateManagerPoltergeist	() override;

	virtual void		reinit						() override;
	virtual void		execute						() override;
	virtual void		remove_links				(CObject* object_) override { inherited::remove_links(object_);}

private:

			u32			time_next_flame_attack;
			u32			time_next_tele_attack;
			u32			time_next_scare_attack;

			void		polter_attack				();
			


};
