#include "stdafx.h"
#include "CustomTimersManager.h"
#include "ui\uistatic.h"
#include "profiler.h"


CTimersManager::CTimersManager() 
{
	b_HUDTimerActive = false;
	hud_timer = NULL;
	b_GameLoaded = false;
}

template <typename T>
void delete_container(xr_vector<T>& container)
{
	xr_vector<T>::iterator	I = container.begin();
	xr_vector<T>::iterator	E = container.end();
	for ( ; I != E; ++I)
	{
		try 
		{
			xr_delete			(*I);
		}
		catch(...) 
		{
			*I			= 0;
		}
	}
	container.clear();
}

CTimersManager::~CTimersManager() 
{
	delete_container(game_timers);
	delete_container(timers);
}

void CTimersManager::OnHud(CTimerCustom *t,bool b)
{
	if (b) {
		if (!t) return;
		if (IsAnyHUDTimerActive()) {
			FATAL("Trying to add more than one HUD timer");
		} else {
			b_HUDTimerActive = true;
			ui_hud_timer = CurrentGameUI()->AddCustomStatic("hud_timer", true)->wnd();
			hud_timer = t;
		}
	} else {
		if (IsAnyHUDTimerActive()) {
			CurrentGameUI()->RemoveCustomStatic("hud_timer");
			ui_hud_timer = NULL;
			b_HUDTimerActive = false;
			hud_timer = NULL;
		}
	}
}

bool CTimersManager::AddTimer (CTimerCustom *timer)
{

	if (xr_strcmp(timer->Name(), "@") != 0 && TimerExist(timer->Name()))
	{
		Msg("! adding a timer with same name: %s", timer->Name());
		return false;
	}

	bool b = timer->Prepare();
	timer->SetParent(this);

	if (timer->isHUD()) 
		OnHud(timers.back(), true);

	if (b)
	{
		game_timers.push_back(timer);
		std::push_heap(game_timers.begin(), game_timers.end(), m_game_timer_pred);
	}
	else
	{
		timers.push_back(timer);
		std::push_heap(timers.begin(), timers.end(), m_timer_pred);
	}

	return true;
}

void CTimersManager::RemoveTimer (LPCSTR name)
{
	TIMERS_IT it0 = std::find_if(timers.begin(), timers.end(), STimerPred(name));
	TIMERS_IT it1 = std::find_if(game_timers.begin(), game_timers.end(), STimerPred(name));
	
	TIMERS_IT *it = (it0 != timers.end()) ? &it0 : ((it1 != game_timers.end()) ? &it1 : NULL);

	R_ASSERT3(it != NULL, "Can't find timer with name ", name);
	
	if ((**it)->isHUD()) 
		OnHud(NULL, false);
	if (it0 != timers.end())
		timers.erase(it0);
	if (it1 != game_timers.end())
		game_timers.erase(it1);
	xr_delete(**it);
	std::make_heap(timers.begin(), timers.end(), m_timer_pred);
	std::make_heap(game_timers.begin(), game_timers.end(), m_game_timer_pred);
}

CTimerCustom* CTimersManager::SearchTimer(LPCSTR name)
{
	TIMERS_IT it0 = std::find_if(timers.begin(), timers.end(), STimerPred(name));
	TIMERS_IT it1 = std::find_if(game_timers.begin(), game_timers.end(), STimerPred(name));
	
	TIMERS_IT *it = (it0 != timers.end()) ? &it0 : ((it1 != game_timers.end()) ? &it1 : NULL);

	if (it)
		return **it;
	return NULL;
}

CTimerCustom* CTimersManager::GetTimerByName(LPCSTR name)
{
	CTimerCustom* timer = SearchTimer(name);
	R_ASSERT3(timer,"Can't find timer with name ",name);
	return timer;
}


bool CTimersManager::TimerExist(LPCSTR name) 
{
	return SearchTimer(name)!=NULL;
}

void CTimersManager::save(IWriter &memory_stream)
{
	Msg							("* Saving timers...");

	memory_stream.open_chunk	(TIMERS_CHUNK_DATA);
	memory_stream.w_u16(timers.size());
	NET_Packet packet;
	packet.write_start();
	for (TIMERS_IT it = timers.begin(), it_end = timers.end(); it!=it_end; ++it) 
		(*it)->save(packet);
	if (packet.B.count)
		memory_stream.w(packet.B.data, packet.B.count);

	memory_stream.w_u16(game_timers.size());
	packet.write_start();
	for (TIMERS_IT it = game_timers.begin(), it_end = game_timers.end(); it!=it_end; ++it) 
		(*it)->save(packet);
	if (packet.B.count)
		memory_stream.w(packet.B.data, packet.B.count);
	memory_stream.close_chunk	();

	

	Msg							("* %d timers successfully saved",game_timers.size() + timers.size());
}

void CTimersManager::load(IReader &file_stream)
{
	Msg							("* Loading timers...");
	
	R_ASSERT2					(file_stream.find_chunk(TIMERS_CHUNK_DATA),"Can't find chunk TIMERS_CHUNK_DATA!");
	u16 size0 = file_stream.r_u16();
	for (int idx=0; idx<size0; idx++) {
		//CTimerCustom* timer = xr_new<CTimerCustom, CTimersManager*>(this);
		CTimerCustom* timer = new CTimerCustom(this);
		timer->load(file_stream);
		to_register.push_back(timer);
	}
	
	u16 size1 = file_stream.r_u16();
	for (int idx=0; idx<size1; idx++) {
		//CTimerCustom* timer = xr_new<CTimerCustom, CTimersManager*>(this);
		CTimerCustom* timer = new CTimerCustom(this);
		timer->load(file_stream);
		to_register.push_back(timer);
	}
	Msg							("* %d timers successfully loaded", size0 + size1);
}


void CTimersManager::Update ()
{
	ALife::_TIME_ID time_now = u64(0);

	if (!b_GameLoaded)
		return;
	START_PROFILE("ALife/Timers")
	u64 start = CPU::QPC();
	time_now = ai().get_alife() ? ai().alife().time().game_time() : Level().GetGameTime();

	if (!game_timers.empty())
	{
		CTimerCustom *timer = game_timers.front();
		if (timer && timer->CheckGameTime())
		{
			//
			std::pop_heap(game_timers.begin(), game_timers.end(), m_game_timer_pred);
			game_timers.pop_back();
			timer->execute(timer->Action());
			//xr_delete(timer);
		}
	}

	if (!timers.empty())
	{
		CTimerCustom *timer = timers.front();
		if (timer && timer->CheckTime(time_now))
		{
			//timer->execute(timer->Action());
			if (timer->isHUD())
				OnHud(NULL,false);
			std::pop_heap(timers.begin(), timers.end(), m_timer_pred);
			timers.pop_back();
			timer->execute(timer->Action());
			//xr_delete(timer);
		}
	}


	if (b_GameLoaded)
	{
		while (!to_register.empty())
		{
			CTimerCustom *tmp = to_register.back();
			to_register.pop_back();
			if (tmp->Prepare())
			{
				game_timers.push_back(tmp);
				std::push_heap(game_timers.begin(), game_timers.end(), m_game_timer_pred);	
			}
			else
			{
				timers.push_back(tmp);
				std::push_heap(timers.begin(), timers.end(), m_timer_pred);
			}
		}
	}
	

	if (hud_timer) 
	{
		string64 str;
		
		ALife::_TIME_ID time_elapsed = hud_timer->Time() - time_now;

		u32 _years,_months,_days,_hours=0,_minutes=0,_seconds=0,_mseconds;
		split_time(time_elapsed,_years,_months,_days,_hours,_minutes,_seconds,_mseconds);

		if (hud_timer->isGameTimer())
			sprintf(str,"%02d:%02d",_hours,_minutes);
		else
			sprintf(str,"%02d:%02d",_minutes,_seconds);

		ui_hud_timer->SetText(str);
	}
	STOP_PROFILE
}

