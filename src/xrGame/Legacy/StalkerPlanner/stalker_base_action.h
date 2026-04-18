////////////////////////////////////////////////////////////////////////////
//	Module 		: stalker_base_action.h
//	Created 	: 25.03.2004
//  Modified 	: 27.09.2004
//	Author		: Dmitriy Iassenev
//	Description : Stalker base action
////////////////////////////////////////////////////////////////////////////

#pragma once

#include "action_script_base.h"

class CAI_Stalker;

//////////////////////////////////////////////////////////////////////////
// CStalkerActionBase
//////////////////////////////////////////////////////////////////////////

class CStalkerActionBase : public CActionScriptBase<CAI_Stalker>
{
protected:
	typedef CActionScriptBase<CAI_Stalker> inherited;

public:
						CStalkerActionBase			(CAI_Stalker *object, const char* action_name = "");
	virtual void		initialize					();
	virtual void		execute						();
	virtual void		finalize					();
	IC		CAI_Stalker	&object						() const
	{
		VERIFY			(m_object);
		return			(*m_object);
	}
};
