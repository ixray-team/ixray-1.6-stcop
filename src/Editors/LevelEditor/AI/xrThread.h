#pragma once

class  CThread
{
	static void			startup(void* P);
public:
	volatile u32		thID;
	volatile float		thProgress;
	volatile bool		thCompleted;
	volatile bool		thMessages;
	volatile bool		thMonitor;
	volatile float		thPerformance;
	volatile bool		thDestroyOnComplete;

	CThread				(u32 _ID)	
	{
		thID				= _ID;
		thProgress			= 0;
		thCompleted			= false;
		thMessages			= true;
		thMonitor			= false;
		thDestroyOnComplete	= true;
	}
	virtual				~CThread(){}
	void				Start	()
	{
		thread_spawn	(startup,"worker-thread",1024*1024,this);
	}
	virtual		void	Execute	()	= 0;
};

class  CThreadManager
{
	xr_vector<CThread*>	threads;
public:
	void				start	(CThread*	T);
	void				wait	(u32		sleep_time=1000);
};