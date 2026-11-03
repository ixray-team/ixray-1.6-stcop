#pragma once
#include "../state.h"
#include "state_data.h"

template<typename _Object>
class CStateMonsterMoveToPoint : public CState<_Object> {
	typedef CState<_Object> inherited;
	
	SStateDataMoveToPoint data;

public:
						CStateMonsterMoveToPoint	(_Object *obj) : inherited(obj, &data) {}
	virtual ~CStateMonsterMoveToPoint	() override = default;

	virtual void		initialize					() override;
	virtual void		execute						() override;
	virtual void		remove_links				(CObject* object_) override { inherited::remove_links(object_);}

	virtual bool		check_completion			() override;

};


template<typename _Object>
class CStateMonsterMoveToPointEx : public CState<_Object> {
	typedef CState<_Object> inherited;

protected:
	
	SStateDataMoveToPointEx data;

public:
						CStateMonsterMoveToPointEx	(_Object *obj) : inherited(obj, &data) {}
	virtual ~CStateMonsterMoveToPointEx	() override = default;
	virtual void		initialize					() override;
	virtual void		execute						() override;
	virtual bool		check_completion			() override;
	virtual void		remove_links				(CObject* object_) override { inherited::remove_links(object_);}
};

#include "state_move_to_point_inline.h"
