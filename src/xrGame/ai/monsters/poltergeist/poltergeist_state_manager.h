#pragma once
#include "../monster_state_manager.h"

class CPoltergeistBase;

class CPoltergeistBaseStateManager : public CMonsterStateManager {
protected:
	using inherited = CMonsterStateManager;

	CPoltergeistBase* pPoltergeistBase;

public:
	CPoltergeistBaseStateManager(CPoltergeistBase *object);
	virtual				~CPoltergeistBaseStateManager() override;

	virtual void		reinit						() override;
	virtual	void		execute						() override;
	virtual void		remove_links				(CObject* object) override { inherited::remove_links(object); }

private:
			u32			time_next_flame_attack;
			u32			time_next_tele_attack;
			u32			time_next_scare_attack;
};
