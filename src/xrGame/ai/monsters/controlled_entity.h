#pragma once
#include "controller/controller.h"

class CEntity;

enum ETask {
	eTaskFollow		= u32(0),
	eTaskAttack,
	eTaskNone		= u32(-1)
};

struct SControlledInfo {
	ETask			m_task;
	const CEntity	*m_object;
	Fvector			m_position;
	u32				m_node;		
	float			m_radius;
};

class CControlledEntityBase {
public:
	virtual bool			is_under_control	()								= 0;

	virtual void			set_data			(const SControlledInfo &info)	= 0;
	virtual SControlledInfo &get_data			()								= 0;

	virtual void			set_task_follow		(const CEntity *e)				= 0;
	virtual void			set_task_attack		(const CEntity *e)				= 0;
	
	virtual void			set_under_control	(CControllerBase *controller)		= 0;
	virtual void			free_from_control	()								= 0;

	virtual void			on_reinit			()								= 0;
	virtual void			on_die				()								= 0;
	virtual void			on_destroy			()								= 0;
};

class CControlledEntity : public CControlledEntityBase {
	
	SControlledInfo		m_data;
	
	struct SGroupID {
		int team_id;
		int squad_id;
		int group_id;
	} saved_id;

	CBaseMonster		*m_object;
	CControllerBase			*m_controller;

public:

	virtual bool			is_under_control		() {return (m_controller != 0);}

	virtual void			set_data				(const SControlledInfo &info) {m_data = info;}
	virtual SControlledInfo &get_data				(){return m_data;}

	virtual void			set_task_follow			(const CEntity *e);
	virtual void			set_task_attack			(const CEntity *e);

	virtual void			set_under_control		(CControllerBase *controller); 
	virtual void			free_from_control		();

	virtual void			on_reinit				();
	virtual void			on_die					();
	virtual void			on_destroy				();

			void			init_external			(CBaseMonster *obj) {m_object = obj;}
};

