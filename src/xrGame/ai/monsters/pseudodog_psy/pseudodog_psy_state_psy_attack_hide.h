#pragma once

#include "../state.h"

class CStatePsyDogHide : public CState 
{
protected:
	using inherited = CState;
	using state_ptr = CState*;

	struct 
	{
		Fvector position;
		u32		node;
	} target;

public:
	CStatePsyDogHide(CBaseMonster* object);
	virtual			~CStatePsyDogHide() override;

	virtual void	initialize				() override;
	virtual void	execute					() override;
	virtual void	remove_links			(CObject* object) override { inherited::remove_links(object);}

	virtual bool 	check_completion		() override;
	virtual bool 	check_start_conditions	() override;

private:
			void	select_target_point		();
};
