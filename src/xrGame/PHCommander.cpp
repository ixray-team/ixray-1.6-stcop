#include "StdAfx.h"
#include "PHCommander.h"

#include "PHSimpleCalls.h"
#ifdef DEBUG
#include "../xrPhysics/IPHWorld.h"
#endif

CPHCall::CPHCall(CPHCondition* condition,CPHAction* action)	
{
	m_action=action;
	m_condition=condition;
}

CPHCall::~CPHCall()
{
	xr_delete(m_action);
	xr_delete(m_condition);
}

bool CPHCall::obsolete()
{
	return !m_action || m_action->obsolete() || !m_condition || m_condition->obsolete();
}

void CPHCall::check()
{
	if(!m_condition) return;
	if(!m_condition->is_true()) return;

	if(!m_action) return;

	m_action->run();
}

bool CPHCall::equal(CPHReqComparerV* cmp_condition,CPHReqComparerV* cmp_action)
{
	return m_action && m_condition && m_action->compare(cmp_action)&&m_condition->compare(cmp_condition);
}
bool CPHCall::is_any(CPHReqComparerV* v)
{
	return (m_action && m_action->compare(v)) || (m_condition && m_condition->compare(v));
}

void delete_call(CPHCall* &call)
{
	try{
		xr_delete(call);
	}
	catch(...)
	{
		call= nullptr;
	}
}
/////////////////////////////////////////////////////////////////////////////////
CPHCommander::~CPHCommander()
{
	clear();
}

void CPHCommander::scheduleDelete(CPHCall* call)
{
	if (!call)
	{
		return;
	}

	if (m_isUpdating)
	{
		m_callsDeferredDelete.push_back(call);
		return;
	}

	delete_call(call);
}

void CPHCommander::flushDeferredDeletes()
{
	while (!m_callsDeferredDelete.empty())
	{
		CPHCall* call = m_callsDeferredDelete.back();
		m_callsDeferredDelete.pop_back();
		delete_call(call);
	}
}

void CPHCommander::clear	()
{
	xrCriticalSectionGuard guard(&lock);
	while (m_calls.size())	{
		remove_call(m_calls.end()-1);
	}
	flushDeferredDeletes();
}

void CPHCommander::update	()
{
	xrCriticalSectionGuard guard(&lock);
	PROF_EVENT("CPHCommander::update");

	m_isUpdating = true;

	for(u32 i=0; i<m_calls.size();)
	{
		CPHCall* call = m_calls[i];
		bool hasException = false;

		try
		{
			call->check();
		} 
		catch(...)
		{
			hasException = true;
		}

		PHCALL_I it = std::find(m_calls.begin(), m_calls.end(), call);
		if (it == m_calls.end())
		{
			continue;
		}

		if (hasException || (*it)->obsolete())
		{
			remove_call(it);
			continue;
		}

		++i;
	}

	m_isUpdating = false;
	flushDeferredDeletes();
}

void CPHCommander::add_call(CPHCondition* condition,CPHAction* action)
{
	xrCriticalSectionGuard guard(&lock);
	m_calls.push_back(new CPHCall(condition, action));
}

void CPHCommander::remove_call(PHCALL_I i)
{
	xrCriticalSectionGuard guard(&lock);
#ifdef DEBUG
	const CPHCallOnStepCondition	* esc = smart_cast<const CPHCallOnStepCondition*>((*i)->condition());
	const CPHConstForceAction		* cfa = smart_cast<const CPHConstForceAction*>((*i)->action());
	if (esc&&cfa)
	{
		Fvector f = cfa->force();
		float m = f.magnitude();
		if (m>EPS_S)
			f.mul(1.f / m);
		//Msg(" const force removed: force: %f,  remove step: %d  world step: %d ,dir(%f,%f,%f) ", m, esc->step(), (u32)physics_world()->StepsNum(), f.x, f.y , f.z ); 
	}
#endif
	CPHCall* call = *i;
	m_calls.erase(i);
	scheduleDelete(call);
}

struct SFEqualPred
{
	CPHReqComparerV* cmp_condition,*cmp_action;
	SFEqualPred(CPHReqComparerV* cmp_c,CPHReqComparerV* cmp_a)
	{
		cmp_condition=cmp_c;cmp_action=cmp_a;
	}
	bool operator()(CPHCall* call)
	{
		return	call->equal(cmp_condition,cmp_action);
	}
};

PHCALL_I CPHCommander::find_call(CPHReqComparerV* cmp_condition,CPHReqComparerV* cmp_action)
{
	xrCriticalSectionGuard guard(&lock);
	return std::find_if(m_calls.begin(), m_calls.end(), SFEqualPred(cmp_condition, cmp_action));
}

bool CPHCommander::has_call(CPHReqComparerV* cmp_condition,CPHReqComparerV* cmp_action)
{
	return find_call(cmp_condition,cmp_action) != m_calls.end();
}

void CPHCommander::remove_call(CPHReqComparerV* cmp_condition,CPHReqComparerV* cmp_action)
{
	xrCriticalSectionGuard guard(&lock);
	for (PHCALL_I it = m_calls.begin(); it != m_calls.end();)
	{
		if ((*it)->equal(cmp_condition, cmp_action))
		{
			CPHCall* call = *it;
			it = m_calls.erase(it);
			scheduleDelete(call);
		}
		else
		{
			++it;
		}
	}
}

bool CPHCommander::add_call_unique(CPHCondition* condition,CPHReqComparerV* cmp_condition,CPHAction* action,CPHReqComparerV* cmp_action)
{
	if (m_calls.end() == find_call(cmp_condition, cmp_action))
	{
		add_call(condition, action);
		return true;
	}
	return false;
}

void CPHCommander::remove_calls(CPHReqComparerV* cmp_object)
{
	xrCriticalSectionGuard guard(&lock);
	for (PHCALL_I it = m_calls.begin(); it != m_calls.end();)
	{
		if ((*it)->is_any(cmp_object))
		{
			CPHCall* call = *it;
			it = m_calls.erase(it);
			scheduleDelete(call);
		}
		else
		{
			++it;
		}
	}
}

void CPHCommander::phys_shell_relcase(CPhysicsShell* sh)
{
	CPHReqComparerHasShell c(sh);
	remove_calls(&c);
}
