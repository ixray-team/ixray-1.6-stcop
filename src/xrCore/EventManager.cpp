#include "stdafx.h"
#include "EventManager.h"

XRCORE_API CEventManager* g_pEventManager = nullptr;

CEventManager::CEventManager() {
	EventMainThread = GetCurrentThreadId();
}

void CEventManager::Attach(IEventReceiver* Holder) noexcept {
	eQuit			= Event.Handler_Attach("KERNEL:quit", Holder);
	eStart			= Event.Handler_Attach("KERNEL:start", Holder);
	eAssert			= Event.Handler_Attach("KERNEL:assert", Holder);
	eStartLoad		= Event.Handler_Attach("KERNEL:load", Holder);
	eDisconnect		= Event.Handler_Attach("KERNEL:disconnect", Holder);
	eConsole		= Event.Handler_Attach("KERNEL:console", Holder);
	eStartMPDemo	= Event.Handler_Attach("KERNEL:start_mp_demo", Holder);
}

void CEventManager::Detach(IEventReceiver* Holder) noexcept {
	Event.Handler_Detach(eConsole, Holder);
	Event.Handler_Detach(eDisconnect, Holder);
	Event.Handler_Detach(eStartLoad, Holder);
	Event.Handler_Detach(eStart, Holder);
	Event.Handler_Detach(eQuit, Holder);
	Event.Handler_Detach(eAssert, Holder);
	Event.Handler_Detach(eStartMPDemo, Holder);
}

void CEventManager::OnEvent(EVENT E, u64 P1, u64 P2) {
	if (E == eAssert) {
		Debug.show_dialog(*(std::string*)P1, *(bool*)P2);
	}
}

bool CEventManager::IsEventThread() const {
	return EventMainThread == GetCurrentThreadId();
}
