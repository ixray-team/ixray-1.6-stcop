#pragma once

#include "../state.h"

class CustomDog;

class	CStateCustomGroup : public CState{
protected:
	using inherited = CState		;
	using state_ptr = CState*;

private:
	CustomDog* m_pDog;

public:
	CStateCustomGroup		(CBaseMonster *object);
	virtual				~CStateCustomGroup		();

	virtual	void		execute					();
	virtual void		setup_substates			();
	virtual bool 		check_completion		() 
	{
		bool result {};

		if (m_pDog) result = m_pDog->b_state_end;

		return result;
	}

	virtual void		remove_links			(CObject* object) { inherited::remove_links(object);}
};
