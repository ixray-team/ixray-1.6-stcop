#pragma once

#ifndef IXRAY_NO_LUA
#include "../xrScripts/script_callback_ex.h"
#endif

class CUIWindow;

struct SCallbackInfo
{
#ifndef IXRAY_NO_LUA
	CScriptCallbackEx<void>	m_callback;
#endif
	xr_delegate<void(CUIWindow*,void*)> m_cpp_callback;
	CUIWindow* 				m_control_ptr;
	shared_str 				m_control_name;
	s16						m_event;
	SCallbackInfo():m_control_ptr(NULL),m_event(-1){};
};

struct UI_API event_comparer
{
	CUIWindow*			pWnd;
	s16					evt;

	event_comparer(CUIWindow* w, s16 e):pWnd(w),evt(e){}
	bool operator ()(SCallbackInfo* i);
};