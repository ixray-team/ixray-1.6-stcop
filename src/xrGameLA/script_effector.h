////////////////////////////////////////////////////////////////////////////
//	Module 		: script_effector.h
//	Created 	: 06.02.2004
//  Modified 	: 06.02.2004
//	Author		: Dmitriy Iassenev
//	Description : XRay Script effector class
////////////////////////////////////////////////////////////////////////////

#pragma once

#include "../effectorpp.h"
#include "../xrScripts/script_export_space.h"
#include "../cameramanager.h"

class CScriptEffector : public CEffectorPP {
public:
	typedef CEffectorPP inherited;
	EEffectorPPType		m_tEffectorType;

	IC					CScriptEffector		(int iType, float time);
	virtual				~CScriptEffector	();
	virtual	BOOL		Process				(SPPInfo &pp);
	virtual	bool		process				(SPPInfo *pp);
	virtual	void		Add					();
	virtual	void		Remove				();
	DECLARE_SCRIPT_REGISTER_FUNCTION
};
#include "script_effector_inline.h"