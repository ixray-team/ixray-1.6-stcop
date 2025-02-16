#pragma once
#ifdef USE_TIMERS_MANAGER
#include "pch_script.h"
#include "script_export_space.h"
#include "xr_time.h"
#include "date_time.h"
#include "ai_space.h"
#include "alife_simulator.h"
#include "alife_time_manager.h"
#include "level.h"
#include "script_engine.h"
#include "../xrCore/object_broker.h"
#include "script_space_forward.h"


class CTimersManager;


class CTimerCustom : public IPureSerializeObject<IReader, NET_Packet>
{ 

	public:
						
							CTimerCustom		(CTimersManager *parent = 0);
		virtual				~CTimerCustom		();
		

		virtual void		load				(IReader& stream);
		virtual void		save				(NET_Packet& stream);
	
		//Name
		virtual void		SetName				(LPCSTR _name ) {m_name = _name;}
		virtual LPCSTR		Name				() {return *m_name;}

		//Time
		virtual void		SetTimerType		(bool value);
		xrTime				TimeObject			();
		ALife::_TIME_ID		Time				() {return m_time;}
		u32					GameTime			() {return m_game_time;}
		xrTime				TimeElapsed			();
		bool				isGameTimer			() const {return (m_flags.test(lmGameTimer)==1)?true:false;}


		//Hud
		virtual void		SetHUD				(bool b);
		bool				isHUD				() {return (m_flags.test(lmHUD)==1)?true:false;}

		virtual void		execute				(LPCSTR action) { }

		virtual void		SetAction			(LPCSTR func) { m_action = func; }
		LPCSTR				Action				() { return *m_action; }

		//valide
		bool				valide				();
		bool				CheckTime			(ALife::_TIME_ID time);
		bool				CheckGameTime		();

		void				SetParent			(CTimersManager *parent) { m_parent = parent;}

		bool				Prepare				();
	private:
		void				PrepareTime			();
		void				PrepareGameTime		();
	public:	
		
		
		u32					m_day,m_hour,m_min,m_sec,m_ms;

	protected:

		shared_str			m_name;
		shared_str			m_action;

		
				u32					m_game_time;
				
			
			ALife::_TIME_ID			m_time;
		

		enum lm_flags	
		{	
			lmHUD			= (1<<0),
			lmRestart		= (1<<1),
			lmGameTimer		= (1<<2),
		};

		flags8				m_flags;
		CTimersManager*		m_parent;
		bool				m_bGameTimer;

	public:
		DECLARE_SCRIPT_REGISTER_FUNCTION;
};

class CTimerCustomWrapper : public CTimerCustom, public luabind::wrap_base 
{
	public:
								CTimerCustomWrapper	(CTimersManager *parent = 0);
		virtual					~CTimerCustomWrapper();
		virtual void			load				(IReader& stream);
		virtual void			save				(NET_Packet& stream);
		static	void			load_static			(CTimerCustom* self, IReader& stream);
		static  void			save_static			(CTimerCustom* self, NET_Packet& stream);

	

		virtual void			execute				(LPCSTR action);
		static void				execute_static		(CTimerCustom* self, LPCSTR action);

	

};


add_to_type_list(CTimerCustom)
#undef script_type_list
#define script_type_list save_type_list(CTimerCustom)

#endif