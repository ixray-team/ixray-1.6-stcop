////////////////////////////////////////////////////////////////////////////
//	Module 		: alife_time_manager.cpp
//	Created 	: 05.01.2003
//  Modified 	: 12.05.2004
//	Author		: Dmitriy Iassenev
//	Description : ALfie time manager class
////////////////////////////////////////////////////////////////////////////

#include "StdAfx.h"
#include "alife_time_manager.h"
#include "../xrEngine/date_time.h"

CALifeTimeManager::CALifeTimeManager	(const char* section)
{
	init						(section);
}

CALifeTimeManager::~CALifeTimeManager	() = default;

void CALifeTimeManager::init			(const char* section)
{
	u32							years,months,days,hours,minutes,seconds;
	sscanf						(pSettings->r_string(section,"start_time"),"%u:%u:%u",&hours,&minutes,&seconds);
	sscanf						(pSettings->r_string(section,"start_date"),"%u.%u.%u",&days,&months,&years);
	m_start_game_time			= generate_time(years,months,days,hours,minutes,seconds);
	m_time_factor				= pSettings->r_float(section,"time_factor");
	m_normal_time_factor		= pSettings->r_float(section,"normal_time_factor");
	m_game_time					= m_start_game_time;
	PdaTimeOffset.store(0, std::memory_order_relaxed);
	m_start_time				= Device.dwTimeGlobal;
}

void CALifeTimeManager::save			(IWriter	&memory_stream)
{
	m_game_time					= game_time();
	m_start_time				= Device.dwTimeGlobal;
	memory_stream.open_chunk	(GAME_TIME_CHUNK_DATA);
	memory_stream.w				(&m_game_time,		sizeof(m_game_time));
	memory_stream.w_float		(m_time_factor);
	memory_stream.w_float		(m_normal_time_factor);
	memory_stream.close_chunk	();

	const s64 Offset = PdaTimeOffset.load(std::memory_order_relaxed);
	if (Offset != 0)
	{
		memory_stream.open_chunk(PDA_TIME_CHUNK_DATA);
		memory_stream.w_s64(Offset);
		memory_stream.close_chunk();
	}
};

void CALifeTimeManager::load			(IReader	&file_stream)
{
	R_ASSERT2					(file_stream.find_chunk(GAME_TIME_CHUNK_DATA),"Can't find chunk GAME_TIME_CHUNK_DATA!");
	file_stream.r				(&m_game_time,		sizeof(m_game_time));
	m_time_factor				= file_stream.r_float();
	m_normal_time_factor		= file_stream.r_float();
	m_start_time				= Device.dwTimeGlobal;

	s64 Offset = 0;
	if (file_stream.find_chunk(PDA_TIME_CHUNK_DATA) == static_cast<intptr_t>(sizeof(Offset)))
	{
		Offset = file_stream.r_s64();
	}
	PdaTimeOffset.store(Offset, std::memory_order_relaxed);
};

ALife::_TIME_ID CALifeTimeManager::GetPdaTime() const
{
	const s64 Time = static_cast<s64>(game_time());
	const s64 Offset = PdaTimeOffset.load(std::memory_order_relaxed);
	return static_cast<ALife::_TIME_ID>(std::max(Time + Offset, s64(0)));
}

void CALifeTimeManager::SetPdaTime(ALife::_TIME_ID Time)
{
	const s64 Offset = static_cast<s64>(Time) - static_cast<s64>(game_time());
	PdaTimeOffset.store(Offset, std::memory_order_relaxed);
}
