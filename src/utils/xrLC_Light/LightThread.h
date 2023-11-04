#pragma once

#include "../xrForms/xrThread.h"
#include "detail_slot_calculate.h"

class	LightThread : public CThread
{
	u32		Nstart, Nend;
	DWORDVec	box_result;
public:
	LightThread			(u32 ID, u32 _start, u32 _end) : CThread(ID)
	{
		Nstart	= _start;
		Nend	= _end;
	}
	virtual void		Execute();

};
