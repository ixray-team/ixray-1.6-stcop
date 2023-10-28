#include "stdafx.h"
#include "xrlight_implicitrun.h"
#include "xrThread.h"
#include "xrLight_Implicit.h"
#include "xrlight_implicitdeflector.h"
#include "xrHardwareLight.h"

class ImplicitThread : public CThread
{
public:

	ImplicitExecute		execute;
	ImplicitThread(u32 ID, ImplicitDeflector* _DATA, u32 _y_start, u32 _y_end) :
		CThread(ID), execute(_y_start, _y_end) {

	}
	virtual void		Execute();


};

void	ImplicitThread::Execute() {
	// Priority
	SetThreadPriority(GetCurrentThread(), THREAD_PRIORITY_BELOW_NORMAL);
	Sleep(0);
	execute.Execute(0);
}

// 2 : Mainthread + UI thread
#define	NUM_THREADS	 CPU::ID.n_threads - 2

void RunImplicitMultithread(ImplicitDeflector& defl, u32 thCount) {
	// Start threads
	CThreadManager			tmanager;
	if (xrHardwareLight::IsEnabled())
	{
		tmanager.start(xr_new<ImplicitThread>(0, &defl, 0, defl.Height()));
	}
	else
	{
		u32	stride = defl.Height() / thCount;
		for (u32 thID = 0; thID < thCount; thID++)
			tmanager.start(xr_new<ImplicitThread>(thID, &defl, thID * stride, thID * stride + stride));
	}

	tmanager.wait();
}
