#pragma once
#include "EventAPI.h"

class XRCORE_API CEventManager {
	friend class CApplication;

public:
	CEventAPI Event;

private:
	EVENT eQuit;
	EVENT eStart;
	EVENT eAssert;
	EVENT eStartLoad;
	EVENT eDisconnect;
	EVENT eConsole;
	EVENT eStartMPDemo;

	size_t EventMainThread;

public:
	CEventManager();

	void Attach(IEventReceiver* Holder) noexcept;
	void Detach(IEventReceiver* Holder) noexcept;
	void OnEvent(EVENT E, u64 P1, u64 P2);

	bool IsEventThread() const;
};

extern XRCORE_API CEventManager* g_pEventManager;
