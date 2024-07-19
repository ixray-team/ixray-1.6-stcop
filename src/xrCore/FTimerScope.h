#pragma once

template <class TimerClass>
class CScopeTimer
{
	TimerClass* Handle = nullptr;

public:
	CScopeTimer(TimerClass* Ptr) : Handle(Ptr)
	{
		Handle->Begin();
	}

	CScopeTimer(TimerClass& Ptr) : Handle(&Ptr)
	{
		Handle->Begin();
	}

	~CScopeTimer()
	{
		Handle->End();
		Handle = nullptr;
	}
};