#include "stdafx.h"
#pragma hdrstop

XRCORE_API BOOL			g_bEnableStatGather	= FALSE;

CStatTimer::CStatTimer()
{
	accum	= 0;
	result	= 0.f;
	count	= 0;
}

void	CStatTimer::FrameStart	()
{
	accum	= 0;
	count	= 0;
}
void	CStatTimer::FrameEnd	()
{
	result = GetElapsed_ms_f();
}

XRCORE_API pauseMngr	g_pauseMngr;


pauseMngr::pauseMngr	():m_paused(FALSE)
{
	m_timers.reserve	(3);
}

void pauseMngr::Pause(BOOL b)
{
	if(m_paused == b)return;

	for (CTimer_paused* Timer : m_timers) {
		Timer->Pause(b);
	}

	m_paused = b;
}

void pauseMngr::Register (CTimer_paused* t){
		m_timers.push_back(t);
}

void pauseMngr::UnRegister (CTimer_paused* t){
	xr_vector<CTimer_paused*>::iterator it = std::find(m_timers.begin(),m_timers.end(),t);
	if( it!=m_timers.end() )
		m_timers.erase(it);
}
