#include "stdafx.h"
#include "xr_time.h"
#include "ui/UIInventoryUtilities.h"
#include "Level.h"
#include "../xrEngine/date_time.h"
#include "ai_space.h"
#include "alife_simulator.h"
#include "alife_time_manager.h"

#define sec2ms		1000
#define min2ms		60*sec2ms
#define hour2ms		60*min2ms
#define day2ms		24*hour2ms

ALife::_TIME_ID __game_time()
{
	return	(ai().get_alife() ? ai().alife().time().game_time() : Level().GetGameTime());
}
u32 get_time()
{
	return u32(__game_time() & u32(-1));
}

xrTime get_time_struct()
{
	return xrTime(__game_time());
}

LPCSTR	xrTime::dateToString	(int mode)								
{ 
	return *InventoryUtilities::GetDateAsString(m_time,(InventoryUtilities::EDatePrecision)mode);
}
LPCSTR	xrTime::timeToString	(int mode)								
{ 
	return *InventoryUtilities::GetTimeAsString(m_time,(InventoryUtilities::ETimePrecision)mode);
}

void	xrTime::add				(const xrTime& other)					
{  
	m_time += other.m_time;				
}
void	xrTime::sub				(const xrTime& other)					
{  
	if(*this>other)
		m_time -= other.m_time; 
	else 
		m_time=0;	
}

void	xrTime::setHMS			(int h, int m, int s)					
{ 
	m_time=0; 
	m_time+=generate_time(1,1,1,h,m,s);
}

void	xrTime::setHMSms		(int h, int m, int s, int ms)			
{ 
	m_time=0; 
	m_time+=generate_time(1,1,1,h,m,s,ms);
}

void	xrTime::set				(int y, int mo, int d, int h, int mi, int s, int ms)
{ 
	m_time=0; 
	m_time+=generate_time(y,mo,d,h,mi,s,ms);
}

void xrTime::get(u32& y, u32& mo, u32& d, u32& h, u32& mi, u32& s, u32& ms)
{
	split_time(m_time, y, mo, d, h, mi, s, ms);
}

void xrTime::Save(NET_Packet& Packet)
{
	u32 y;
	u32 mo;
	u32 d;
	u32 h;
	u32 mi;
	u32 s;
	u32 ms;

	get(y, mo, d, h, mi, s, ms);

	TimePacked Tm = {};
	Tm.Years = y - 2000;
	Tm.Month = mo;
	Tm.Days = d;

	Tm.Hours = h;
	Tm.Min = mi;
	Tm.Sec = s;

	Packet.w_u32(Tm.TimeTotal);
}

void xrTime::Load(NET_Packet& Packet)
{
	TimePacked Tm = {};
	Packet.r_u32(Tm.TimeTotal);

	set(Tm.Years + 2000, Tm.Month, Tm.Days, Tm.Hours, Tm.Min, Tm.Sec, 0);
}

float	xrTime::diffSec(const xrTime& other)
{
	if (*this > other)
		return (m_time - other.m_time) / (float)sec2ms;

	return ((other.m_time - m_time) / (float)sec2ms) * (-1.0f);
}
