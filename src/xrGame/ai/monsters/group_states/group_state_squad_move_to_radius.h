#pragma once
#include "../state.h"
#include "../states/state_data.h"

template<typename _Object>
class CStateGroupSquadMoveToRadiusEx : public CState<_Object> {
	typedef CState<_Object> inherited;

protected:
	
	SStateDataMoveToPointEx data;

public:
						CStateGroupSquadMoveToRadiusEx	(_Object *obj) : inherited(obj, &data) {}
	virtual ~CStateGroupSquadMoveToRadiusEx	() override = default;
	virtual void		initialize					() override;
	virtual void		execute						() override;
	virtual bool		check_completion			() override;
	virtual void		remove_links				(CObject* object_) override { inherited::remove_links(object_);}
};

template<typename _Object>
class CStateGroupSquadMoveToRadius : public CState<_Object> {
	typedef CState<_Object> inherited;

protected:

	SStateDataMoveToPointEx data;

public:
	CStateGroupSquadMoveToRadius	(_Object *obj) : inherited(obj, &data) {}
	virtual ~CStateGroupSquadMoveToRadius	() override = default;
	virtual void		initialize					() override;
	virtual void		execute						() override;
	virtual bool		check_completion			() override;
	virtual void		remove_links				(CObject* object_) override { inherited::remove_links(object_);}
};


#include "group_state_squad_move_to_radius_inline.h"
