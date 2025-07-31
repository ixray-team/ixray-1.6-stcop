#pragma once

#include "../xrForms/xrThread.h"
#include "detail_slot_calculate.h"

class	LightThread : public CThread
{
 	DWORDVec	box_result;
public:
	LightThread			(u32 ID) : CThread(ID)
	{
 
	}
	virtual void		Execute();

};
