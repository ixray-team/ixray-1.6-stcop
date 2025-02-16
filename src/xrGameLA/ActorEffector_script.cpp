#include "stdafx.h"
#include "pch_script.h"
#include "ai_space.h"
#include "script_engine.h"
#include "ActorEffector.h"
#include "../ObjectAnimator.h"

void CAnimatorCamEffectorScriptCB::ProcessIfInvalid(SCamEffectorInfo& info)
{
	if(m_bAbsolutePositioning)
	{
		const Fmatrix& m			= m_objectAnimator->XFORM();
		info.d							= m.k;
		info.n							= m.j;
		info.p							= m.c;
	}
}

BOOL CAnimatorCamEffectorScriptCB::Valid()
{
	BOOL res = inherited::Valid();
	if(!res && cb_name.size() )
	{
		luabind::functor<LPCSTR>			fl;
		R_ASSERT							(ai().script_engine().functor<LPCSTR>(*cb_name,fl));
		fl									();
		cb_name								= "";
	}
	return res;
}
