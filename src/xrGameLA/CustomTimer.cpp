#include "stdafx.h"
#include "CustomTimer.h"
#include "alife_space.h"
#include "level.h"


u64	generate_add_time	(u32 days, u32 hours, u32 minutes, u32 seconds)
{
	u64						result = 0;

	result					+= u64(days);
	result					= result*u64(24) + u64(hours);
	result					= result*u64(60) + u64(minutes);
	result					= result*u64(60) + u64(seconds);
	result					*=u64(1000);

	return					(result);
}

CTimerCustom::CTimerCustom(CTimersManager *parent) 
 {
	m_time = 0;
	m_game_time = 0;
	m_day = 0;
	m_hour = 0;
	m_min = 0;
	m_sec = 0;
	m_ms = 0;
	m_flags.zero();
	m_parent = parent;
	m_name = "@";
	m_action = "";
}

CTimerCustom::~CTimerCustom() 
{
}

bool CTimerCustom::Prepare()
{
	if (isGameTimer() && !isHUD())
	{
		PrepareGameTime();
		return true;
	}
	else
	{
		PrepareTime();
		return false;
	}
}

xrTime CTimerCustom::TimeObject()
{
	return xrTime(m_time);
}

bool CTimerCustom::valide()
{
	if (isGameTimer() && !isHUD())
		return (m_game_time!=0);
	else
		return (m_time!=0 || m_day!=0 || m_hour!=0 || m_min!=0 || m_sec!=0 || m_ms!=0);
}

xrTime CTimerCustom::TimeElapsed()
{
	xrTime timeElapsed(m_time);
	xrTime time = get_time_struct();
	if (time < timeElapsed)
		timeElapsed.sub(time);
	else
		timeElapsed.set(0,0,0,0,0,0,0);
	return timeElapsed;
}

void CTimerCustom::SetTimerType(bool value)
{
	m_flags.set(lmGameTimer, value);
}

bool CTimerCustom::CheckTime(ALife::_TIME_ID time)
{
	if (!m_time) return false;

	if (time > m_time)  return true;
	return false;
}

bool CTimerCustom::CheckGameTime()
{
	return (Device.dwTimeGlobal > m_game_time);
}

void CTimerCustom::PrepareTime()
{
	ALife::_TIME_ID time_now = ai().get_alife() ? ai().alife().time().game_time() : Level().GetGameTime();

	ALife::_TIME_ID time_plus = generate_add_time(m_day,m_hour,m_min,m_sec) + m_ms;

	time_now+=time_plus;

	m_time = time_now;
}

void CTimerCustom::PrepareGameTime()
{
	m_game_time += Device.dwTimeGlobal;
}



#include "CustomTimersManager.h"
void CTimerCustom::SetHUD(bool b)
{
	m_flags.set(lmHUD,b);
	if (m_parent) m_parent->OnHud(this,b);
}

void CTimerCustom::save (NET_Packet &stream)
{
	u32 m_save_game_time = m_game_time;
	if (m_save_game_time > 0)
		m_save_game_time -= Device.dwTimeGlobal;

	save_data(m_name,	stream);
	save_data(m_action,	stream);
	save_data(m_time,	stream);
	save_data(m_save_game_time,	stream);
	save_data(m_flags,	stream);
}

void CTimerCustom::load (IReader &stream)
{
	load_data(m_name,	stream);
	load_data(m_action,	stream);
	load_data(m_time,	stream);
	load_data(m_game_time,	stream);
	load_data(m_flags,	stream);
	if (isHUD()) m_parent->OnHud(this,true);
}



