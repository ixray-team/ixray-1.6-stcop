#pragma once
#include "AnomalyZone.h"

class CNoGravityZone final :
	public CAnomalyZone
{
typedef CAnomalyZone inherited;
public:
protected:
	virtual		void	enter_Zone						(SZoneObjectInfo& io)				;
	virtual		void	exit_Zone						(SZoneObjectInfo& io)				;
private:
				void	switchGravity					(SZoneObjectInfo& io,bool val)		;
	virtual		void	UpdateWorkload					(u32	dt	)						;
};