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
					CStatePsyDogHide		(CBaseMonster*obj) : inherited(obj) {}
	virtual			~CStatePsyDogHide		() {}

	virtual void	initialize				();
	virtual void	execute					();
	virtual void	remove_links			(CObject* object) { inherited::remove_links(object);}

	virtual bool 	check_completion		();
	virtual bool 	check_start_conditions	();

private:
			void	select_target_point		();
};
