#pragma once
#ifdef USE_TIMERS_MANAGER
#include "../xrScripts/script_export_space.h"
#include "CustomTimer.h"
#include "UIGameSP.h"

class CTimersManager : public IPureSerializeObject<IReader, IWriter>
{
	private:
		using TIMERS_STORAGE = xr_vector<CTimerCustom*>;
		using TIMERS_IT = TIMERS_STORAGE::iterator;

	public:
						CTimersManager			();
		virtual			~CTimersManager			();

		void			Update					();

		virtual void	save					(IWriter &memory_stream);
		virtual void	load					(IReader &file_stream);

		bool			AddTimer				(CTimerCustom *timer);
		void			RemoveTimer				(LPCSTR name);
		CTimerCustom*	GetTimerByName			(LPCSTR name);
		bool			TimerExist				(LPCSTR name);

		void			OnHud					(CTimerCustom *t,bool b);
		bool			IsAnyHUDTimerActive		() const					{ return b_HUDTimerActive; }

		void			GameLoaded				(bool val)					{ b_GameLoaded = val; }
		bool			IsGameLoaded			() const					{ return b_GameLoaded; }

	private:
		CTimerCustom*	SearchTimer				(LPCSTR name);

	public:
		DECLARE_SCRIPT_REGISTER_FUNCTION;
	protected:
		struct STimerPred
		{
			private:
				shared_str m_name;
			public:
				STimerPred(shared_str name) : m_name(name) {}
				bool operator() (CTimerCustom *t)		{ return xr_strcmp(t->Name(), m_name) == 0; }
		};
		struct SGameTimerHeapPred
		{
			public:
				bool operator() (CTimerCustom *t0, CTimerCustom *t1) { return t0->GameTime() > t1->GameTime(); }
		} m_game_timer_pred;
		struct STimerHeapPred
		{
			public:
				bool operator() (CTimerCustom *t0, CTimerCustom *t1) { return t0->Time() > t1->Time(); }
		} m_timer_pred;
	protected:
		TIMERS_STORAGE			game_timers, timers, to_register;
		CTimerCustom			*hud_timer;
		CUIStatic				*ui_hud_timer;
		bool					b_HUDTimerActive;
		bool					b_GameLoaded;
};
#endif