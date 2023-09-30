#pragma once

#include <chrono>

using high_resolution_clock = std::chrono::high_resolution_clock;
using microseconds = std::chrono::microseconds ;
#define chrono_duration std::chrono::duration_cast<microseconds>

class	CTimer_paused;

class XRCORE_API				pauseMngr
{
	xr_vector<CTimer_paused*>	m_timers;
	BOOL						m_paused;
public:
			pauseMngr			();
	BOOL	Paused				(){return m_paused;};
	void	Pause				(BOOL b);
	void	Register			(CTimer_paused* t);
	void	UnRegister			(CTimer_paused* t);
};

extern XRCORE_API pauseMngr		g_pauseMngr;

// Fast getting of time, but doesn't have time factor
class XRCORE_API CTimer {
protected:
	high_resolution_clock::time_point startTimePoint_;
	u64 pausedTime_;
	u64	pauseAccum_;

	BOOL		bPause			;
public:
	CTimer() : startTimePoint_(high_resolution_clock::now()), pausedTime_(0), pauseAccum_(0), bPause(FALSE) {
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
class XRCORE_API CTimerFactored : public CTimer {
private:
	typedef CTimer inherited;

private:
	float				m_time_factor;
	u64					m_real_ticks;
	u64					m_ticks;
	bool				timerStarted_;

private:
	IC	u64 GetElapsed_mcs(const u64& current_ticks) const
	{
		u64				delta = current_ticks - m_real_ticks;
		double			delta_d = (double)delta;
		double			time_factor_d = time_factor();
		double			time = delta_d * time_factor_d; // +.5;
		u64				result = (u64)time;

		return			(m_ticks + result);
	}

public:
	IC CTimerFactored() : m_time_factor(1.f), m_real_ticks(0), m_ticks(0), timerStarted_(false) { }

	ICF	void Start() override
	{
		if (bPause)
			return;

		inherited::Start();

		m_real_ticks	= 0;
		m_ticks			= 0;
		timerStarted_ = true;
	}

	IC	const bool IsStarted() const
	{
		return timerStarted_;
	}

	IC	const float& time_factor() const {
		return m_time_factor;
	}

	IC	void time_factor(const float& time_factor) {
		u64 current = inherited::GetElapsed_mcs();

		m_ticks = GetElapsed_mcs(current);
		m_real_ticks = current;
		m_time_factor = time_factor;
	}

	IC	u64 GetElapsed_mcs() const override {
		u64 result = GetElapsed_mcs(inherited::GetElapsed_mcs());
		return result;
	}

	IC CTimerFactored& operator=(const CTimerFactored& other_timer) {
		inherited::operator=(other_timer);

		m_time_factor = other_timer.m_time_factor;
		m_real_ticks = other_timer.m_real_ticks;
		m_ticks = other_timer.m_ticks;
		timerStarted_ = other_timer.timerStarted_;

		return (*this);
	}
};

class XRCORE_API CTimer_paused_ex : public CTimerFactored {
	u64							save_clock;

public:
	CTimer_paused_ex			()		{ }
	virtual ~CTimer_paused_ex	()		{ }

	IC BOOL		Paused() const { return bPause; }

	IC void		Pause(BOOL b)
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

extern XRCORE_API BOOL			g_bEnableStatGather;
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
