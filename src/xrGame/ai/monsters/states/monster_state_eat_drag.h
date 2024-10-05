#pragma once

#include "../state.h"

class CStateMonsterDrag : public CState {
	typedef CState		inherited;

	Fvector				m_cover_position;
	u32					m_cover_vertex_id;
	
	bool				m_failed;
	Fvector				m_corpse_start_position;

public:
						CStateMonsterDrag		(CBaseMonster *obj);
	virtual				~CStateMonsterDrag		();

	virtual void		initialize				();
	virtual	void		execute					();
	virtual void		finalize				();
	virtual void		critical_finalize		();
	virtual void		remove_links			(CObject* object_) { inherited::remove_links(object_);}

	virtual bool		check_completion		();
};

