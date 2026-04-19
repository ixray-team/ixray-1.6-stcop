#pragma once

#include <chrono>

using high_resolution_clock = std::chrono::high_resolution_clock;
using microseconds = std::chrono::microseconds ;
#define chrono_duration std::chrono::duration_cast<microseconds>

class	CTimer_paused;

class XRCORE_API				pauseMngr
{
	xr_vector<CTimer_paused*>	m_timers;
	bool						m_paused;
public:
			pauseMngr			();
	bool	Paused				(){return m_paused;};
	void	Pause				(bool b);
	static pauseMngr& Instance	();
	void	Register			(CTimer_paused* t);
	void	UnRegister			(CTimer_paused* t);
};

extern XRCORE_API pauseMngr* g_pauseMngrPtr;

#define g_pauseMngr pauseMngr::Instance()

// Fast getting of time, but doesn't have time factor
class XRCORE_API CTimer {
protected:
	high_resolution_clock::time_point startTimePoint_;
	u64 pausedTime_;
	u64	pauseAccum_;

	bool		bPause			;
public:
	CTimer() : startTimePoint_(high_resolution_clock::now()), pausedTime_(0), pauseAccum_(0), bPause(false) {
	}

	ICF virtual void Start() 	{
		if (bPause)
			return;

		startTimePoint_ = high_resolution_clock::now();
		pauseAccum_ = 0;
		pausedTime_ = 0;
	}

	ICF virtual u64	GetElapsed_mcs() const {
		if (bPause)
			return pausedTime_;
		else
			return u64(chrono_duration(high_resolution_clock::now() - startTimePoint_).count()) - pauseAccum_;
	}

	IC virtual u32 GetElapsed_ms() const {
		return u32(GetElapsed_mcs() / 1000);
	}

	ICF virtual float GetElapsed_ms_f() const {
		return float(double(GetElapsed_mcs()) / 1000);
	}

	IC virtual float GetElapsed_sec() const {
		return float(double(GetElapsed_mcs()) / 1000000);
	}

	IC virtual void Dump() const {
		Msg("* Elapsed time (sec): %f", GetElapsed_sec());
	}

	IC CTimer& operator=(const CTimer& other_timer) {
		startTimePoint_ = other_timer.startTimePoint_;
		pausedTime_ = other_timer.pausedTime_;
		pauseAccum_ = other_timer.pauseAccum_;

		return (*this);
	}
};

// Has time factor, thus slower
class XRCORE_API CTimerFactored : 
	public CTimer
{
	using inherited = CTimer;

private:
	float m_time_factor;
	u64   m_accum_ticks;
	u64   m_last_ticks; 
	bool  m_started;    

public:
	IC CTimerFactored()
		: m_time_factor(1.f), m_accum_ticks(0), m_last_ticks(0), m_started(false) {
	}

	ICF void Start() override
	{
		if (bPause)
			return;

		inherited::Start();
		m_last_ticks = inherited::GetElapsed_mcs();
		m_accum_ticks = 0;
		m_started = true;
	}

	IC bool IsStarted() const
	{
		return m_started;
	}

	IC const float& time_factor() const
	{
		return m_time_factor;
	}

	IC void time_factor(const float& new_factor)
	{
		if (!m_started)
		{
			m_time_factor = new_factor;
			return;
		}

		u64 current = inherited::GetElapsed_mcs();
		m_accum_ticks += u64((current - m_last_ticks) * double(m_time_factor));
		m_last_ticks = current;

		m_time_factor = new_factor;
	}

	IC u64 GetElapsed_mcs() const override
	{
		if (!m_started)
		{
			return 0;
		}

		u64 current = inherited::GetElapsed_mcs();
		u64 delta = current - m_last_ticks;

		u64 elapsed = m_accum_ticks + u64(double(delta) * m_time_factor);

		if (bPause)
		{
			return pausedTime_;
		}

		return elapsed;
	}

	IC u32 GetElapsed_ms() const override
	{
		return u32(GetElapsed_mcs() / 1000);
	}

	IC float GetElapsed_ms_f() const override
	{
		return float(double(GetElapsed_mcs()) / 1000.0);
	}

	IC float GetElapsed_sec() const override
	{
		return float(double(GetElapsed_mcs()) / 1000000.0);
	}

	IC void Dump() const override
	{
		Msg("* Elapsed time (sec): %f", GetElapsed_sec());
	}

	IC CTimerFactored& operator=(const CTimerFactored& other)
	{
		inherited::operator=(other);
		m_time_factor = other.m_time_factor;
		m_accum_ticks = other.m_accum_ticks;
		m_last_ticks = other.m_last_ticks;
		m_started = other.m_started;
		return *this;
	}
};


class XRCORE_API CTimer_paused_ex : public CTimerFactored {
	u64							save_clock;

public:
	CTimer_paused_ex			()		{ }
	virtual ~CTimer_paused_ex	()		{ }

	IC bool		Paused() const { return bPause; }

	IC void		Pause(bool b)
	{
		if (bPause == b)
			return;

		u64 _current = u64(chrono_duration(high_resolution_clock::now() - startTimePoint_).count());

		if (b)
		{
			save_clock = _current;
			pausedTime_ = GetElapsed_mcs();
		}
		else
		{
			pauseAccum_ += _current - save_clock;
		}

		bPause = b;
	}
};

// A timer that is influenced by Pause manager
class XRCORE_API CTimer_paused : public CTimer_paused_ex {
public:
	CTimer_paused				()		{ g_pauseMngr.Register(this);	}
	virtual ~CTimer_paused		()		{ g_pauseMngr.UnRegister(this);	}
};

extern XRCORE_API bool			g_bEnableStatGather;
class XRCORE_API CStatTimer
{
private:
	CTimer		selfTimer_;

public:
	u64			accum;
	float		result;
	u32			count;
public:
				CStatTimer		();
	void		FrameStart		();
	void		FrameEnd		();

	void	Begin() { if (!g_bEnableStatGather) return;	count++; selfTimer_.Start(); }
	void	End() { if (!g_bEnableStatGather) return;	accum += selfTimer_.GetElapsed_mcs(); }

	ICF u64		GetElapsed_mcs() const { return accum; }

	IC u32 GetElapsed_ms() const {
		return u32(GetElapsed_mcs() / 1000);
	}

	ICF float GetElapsed_ms_f() const {
		return float(double(GetElapsed_mcs()) / 1000);
	}

	IC float GetElapsed_sec() const {
		float _result = float(double(GetElapsed_mcs()) / 1000000);
		return _result;
	}
};

#include "FTimerScope.h"