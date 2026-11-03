#pragma once

template<typename _Object>
class CStateMonsterDrag : public CState<_Object> {
	typedef CState<_Object>		inherited;

	Fvector				m_cover_position;
	u32					m_cover_vertex_id;
	
	bool				m_failed;
	Fvector				m_corpse_start_position;

public:
						CStateMonsterDrag		(_Object *obj);
	virtual ~CStateMonsterDrag		() override;

	virtual void		initialize				() override;
	virtual void		execute					() override;
	virtual void		finalize				() override;
	virtual void		critical_finalize		() override;
	virtual void		remove_links			(CObject* object_) override { inherited::remove_links(object_);}

	virtual bool		check_completion		() override;
};


#include "monster_state_eat_drag_inline.h"