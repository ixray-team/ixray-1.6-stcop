#include "StdAfx.h"
#include "pch_script.h"
#include "UIScriptWnd.h"
#include "object_broker.h"
#include "../callback_info.h"
#include "../../xrUI/Widgets/UIStatic.h"


CUIDialogWndEx::CUIDialogWndEx()
{}

CUIDialogWndEx::~CUIDialogWndEx()
{
	DetachFromDialogHolders();
	delete_data(m_callbacks);
}

void CUIDialogWndEx::Register			(CUIWindow* pChild)
{
	pChild->SetMessageTarget(this);
	if (CUIStatic* Static = pChild->ui_cast_static())
	{
		Static->EnableMouseEvents(true);
	}
}

void CUIDialogWndEx::Register(CUIWindow* pChild, const char* name)
{
	pChild->SetWindowName(name);
	Register(pChild);
}

void CUIDialogWndEx::SendMessage(CUIWindow* pWnd, s16 msg, void* pData)
{
	event_comparer ec(pWnd,msg);

	CALLBACK_IT it = std::find_if(m_callbacks.begin(),m_callbacks.end(),ec);
	if(it==m_callbacks.end())
		return inherited::SendMessage(pWnd, msg, pData);

	((*it)->m_callback)();

//	if ( (*it)->m_cpp_callback )	
//		(*it)->m_cpp_callback(pData);
}

bool CUIDialogWndEx::Load(const char* xml_name)
{
	return true;
}

SCallbackInfo*	CUIDialogWndEx::NewCallback ()
{
	m_callbacks.push_back( new SCallbackInfo() );
	return m_callbacks.back();
}

void CUIDialogWndEx::AddCallback(const char* ControlId, s16 Event, const luabind::functor<void>& Functor)
{
	SCallbackInfo* Callback = NewCallback();
	Callback->m_callback.set(Functor);
	Callback->m_control_name = ControlId;
	Callback->m_event = Event;
}

void CUIDialogWndEx::AddCallback (const char* control_id, s16 evt, const luabind::functor<void> &functor, const luabind::object &object)
{
	SCallbackInfo* c	= NewCallback ();
	c->m_callback.set	(functor,object);
	c->m_control_name	= control_id;
	c->m_event			= evt;
}



bool CUIDialogWndEx::OnKeyboardAction(int dik, EUIMessages keyboard_action)
{
	return inherited::OnKeyboardAction(dik,keyboard_action);
}
void CUIDialogWndEx::Update()
{
	inherited::Update();
}
